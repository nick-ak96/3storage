% HEADER =============================================================
%
%
-module(pfs).
-behaviour(gen_server).


% gen_server API
-export([ init/1, start_link/2, start_link/3, terminate/2, handle_call/3, handle_cast/2 ]).


% Constants
-define(PAGE_SIZE, 4). % page size in bytes


% Structures
-record(state, { q }).

%% TODO
%% - check how the data is stored in the state of the gen_server. if copied -> optimize the write queries
%% - add configuration
%% - add logger
%% --- write logs to a file, specify log level from configuration or as a start parameter
%% - check if it is possible to optimize binary concatenation on continuous requests
%% - implement caching (buffer manager layer)
%% - add module documentation
%% - server timeout handling -> hibernation state
%% - add request timeout handling
%% - add error and exception management
%% - write tests
%% - implement code_change interface
%% - implement handle_info interface
%
%
% END (HEADER) =======================================================
%
%
% IMPLEMENTATION =====================================================
%
%
% Server API ---------------------------------------------------------

init([ ServerName, Filepath ]) ->
	init([ ServerName, Filepath, notice ]);
init([ ServerName, Filepath, LogLevel ]) ->
	process_flag(trap_exit, true),
	% initialize logger
	logger:update_primary_config(maps:merge(logger:get_primary_config(), #{ level => LogLevel })),
	% open page file
	{ Status, Result } = file:open(Filepath, [raw, binary, read, write]),
	logger:info("Initialization with file ~w", [ Filepath ]),
	case Status of
		error ->
			logger:error("Could not open page file. Error: ~p", [ Result ]),
			logger:critical("Unexpected PFS termination"),
			{ stop, Result };
		ok ->
			put(pfs_file, Result),
			put(pfs_name, ServerName),
			logger:info("Initialization finished successfully."),
			{ ok, #state{ q = queue:new() } }
	end.

start_link(ServerName, File) ->
	start_link(ServerName, File, notice).
start_link(ServerName, File, LogLevel) ->
	gen_server:start_link({ local, ServerName }, ?MODULE, [ ServerName, File, LogLevel ], []).

terminate(Reason, State) ->
	logger:info("Terminating. Reason: ~p", [ Reason ]),
	logger:debug("State before termination: ~p", [ State ]),
	ok.

handle_call(_Request, _From, State) ->
	logger:notice("Unhandled call request received"),
	{ noreply, State }.

handle_cast({ read, _Receiver, _Payload } = Request, State) ->
	{ noreply, handle_request(Request, State) };
handle_cast({ write, _Receiver, _Payload } = Request, State) ->
	{ noreply, handle_request(Request, State) };
handle_cast({ append, _Receiver, _Payload } = Request, State) ->
	{ noreply, handle_request(Request, State) };
handle_cast(_Request, State) ->
	logger:notice("Unhandled cast request received"),
	{ noreply, State }.

% END (Server API) ---------------------------------------------------
%
%
% Internal functions -------------------------------------------------

handle_request(Request, State) ->
	logger:debug("Received request: ~p", [ Request ]),
	logger:debug("Current state: ~p", [ State ]),
	% put the incoming request to the internal queue
	IntQ = queue:in(Request, State#state.q),
	logger:debug("Request is queued"),
	% process requests from the internal queue
	State#state { q = process_queue(IntQ) }.

process_queue(Q) ->
	logger:debug("Processing internal queue"),
	NewQ = process_internal_queue(Q),
	% check mailbox
	case process_info(self(), message_queue_len) of
		{ message_queue_len, 0 } when NewQ =/= Q ->
			logger:debug("No messages in the mailbox"),
			% no messages -> continue to process the internal queue
			process_queue(NewQ);
		_ ->
			logger:debug("No messages in the mailbox, idling"),
			% there is a message -> exit
			NewQ
	end.

process_internal_queue(Q) ->
	case queue:out(Q) of
		{ empty, _ } ->
			logger:debug("Internal queue is empty"),
			Q;
		{ { value, { Fun, Receiver, Payload } = Request }, NewQ } ->
			logger:debug("Processing the request: ~p", [ Request ]),
			Result = process_request(Fun, Payload),
			case Result of
				{ done, Message } ->
					logger:debug("Request resulted in: ~p", [ Message ]),
					Receiver ! Message,
					NewQ;
				{ continue, Message, Continue } ->
					logger:debug("Request is processed partially and resulted in: ~p", [ Message ]),
					Receiver ! Message,
					queue:in(Continue, NewQ)
			end
	end.

process_request(read, { _PageId } = P) ->
	read(P);
process_request(write, { _PageId, _Data } = P) ->
	write(P);
process_request(append, { _Data } = P) ->
	append(P);
process_request(_Type, _P) ->
	{ done, { error, "Unsupported request type" } }.


%% Page File Input/Output


read({ PageId }) when is_integer(PageId) ->
	File = get(pfs_file),
	Result = file:pread(File, PageId * ?PAGE_SIZE, ?PAGE_SIZE),
	case Result of
		eof ->
			{ done, { error, "End of stream is reached" } };
		_ -> % either { error, Reason } or { ok, Data }
			{ done, Result }
	end;
read(_P) ->
	{ done, { error, "Invalid input" } }.

write({ PageId, Data }) when is_binary(Data), is_integer(PageId), size(Data) =:= ?PAGE_SIZE ->
	File = get(pfs_file),
	Result = file:pwrite(File, PageId * ?PAGE_SIZE, prepare_write(Data)),
	case Result of
		ok ->
			{ done, { ok, PageId } };
		_ -> % { error, Reason }
			{ done, Result }
	end;
write(_P) ->
	{ done, { error, "Invalid input" } }.

append({ Data }) when is_binary(Data), size(Data) =:= ?PAGE_SIZE ->
	File = get(pfs_file),
	{ ok, Pos } = file:position(File, eof),
	Result = file:pwrite(File, Pos, prepare_write(Data)),
	case Result of
		ok ->
			{ ok, OffSet } = file:position(File, eof),
			{ done, { ok, (OffSet - ?PAGE_SIZE) div ?PAGE_SIZE } };
		_ -> % { error, Reason }
			{ done, Result }
	end;
append(_P) ->
	{ done, { error, "Invalid input" } }.

prepare_write(Data) ->
	% TODO add custom header that will contain the following properties
	% - size of the actual data in the page
	% - method to unpack the data into erlang term()
	Data.

% END (Internal functions) -------------------------------------------
%
%
% END (IMPLEMENTATION) ===============================================
