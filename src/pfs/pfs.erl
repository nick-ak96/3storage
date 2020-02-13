% HEADER =============================================================
%
%
-module(pfs).
-behaviour(gen_server).
-include("pfs.hrl").

% gen_server API
-export([ init/1, start_link/2, start_link/3, terminate/2, handle_call/3, handle_cast/2 ]).

%% TODO
%% - Add trace ID when request isbeing queued into internal queue.
%% This will help to combine multiple responces from single request into one.
%% - Check how the data is stored in the state of the gen_server. If copied -> optimize the write queries.
%% - Add configuration file that will be tight to the ServerName parameter on initialization of the PFS instance.
%%
%% - check if it is possible to optimize binary concatenation on continuous requests
%% - implement caching (buffer manager layer)
%% - add module documentation
%% - server timeout handling -> hibernation state
%% - add request timeout handling
%% - add error and exception management
%% - write tests
%% - implement code_change interface
%
%
% END (HEADER) =======================================================
%
%
% IMPLEMENTATION =====================================================
%
%
% Server API ---------------------------------------------------------

% Page File Server internal initialization.
% ServerName - atom(), that represents a server name.
% Filepath - string(), a relative or absolute path to the page file.
% LogLevel - level(), a logging level treshold. level() = emergency | alert | critical | error | warning | notice | info | debug
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
			% store file IO device and server name
			put(pfs_file, Result),
			% store file size in pages
			put(pages_cnt, get_pages_count(Filepath)),
			put(pfs_name, ServerName),
			put(rq, queue:new()),
			SID = generate_session_id(ServerName),
			put(sid, SID),
			State = 0,
			logger:info("Initialization finished successfully."),
			logger:info("Session ID: ~w", [ SID ]),
			{ ok, State }
	end.

generate_session_id(ServerName) when is_atom(ServerName) ->
	Timestamp = erlang:system_time(nanosecond),
	string:concat(string:concat(atom_to_list(ServerName), "_"), integer_to_list(Timestamp)).

get_pages_count(Filepath) ->
	{ ok, FileInfo } = file:read_file_info(Filepath),
	element(2, FileInfo) / ?PAGE_SIZE.


% Page File Server external initialization.
% ServerName - atom(), that represents a server name.
% Filepath - string(), a relative or absolute path to the page file.
% LogLevel - level(), a logging level treshold. level() = emergency | alert | critical | error | warning | notice | info | debug
start_link(ServerName, File) ->
	start_link(ServerName, File, notice).
start_link(ServerName, File, LogLevel) ->
	gen_server:start_link({ local, ServerName }, ?MODULE, [ ServerName, File, LogLevel ], []).

% Page File Server termination handler.
terminate(Reason, State) ->
	logger:info("Terminating. Reason: ~p", [ Reason ]),
	logger:debug("State before termination: ~p", [ State ]),
	Result = file:close(get(pfs_file)),
	logger:info("Page file is closed with response: ~p", [ Result ]),
	ok.

% Page File Server message handlers
handle_call(_Request, _From, State) ->
	logger:notice("Unhandled call request received"),
	{ noreply, State }.

handle_cast({ read, _Receiver, _Payload } = Request, State) ->
	{ noreply, handle_request(Request, State) };
handle_cast({ write, _Receiver, _Payload } = Request, State) ->
	{ noreply, handle_request(Request, State) };
handle_cast({ append, _Receiver, _Payload } = Request, State) ->
	{ noreply, handle_request(Request, State) };
handle_cast({ get_current_state, Receiver }, State) ->
	Receiver ! { get(sid), State },
	{ noreply, State };
% TODO request cancellation
handle_cast(_Request, State) ->
	logger:notice("Unhandled cast request received"),
	{ noreply, State }.

% END (Server API) ---------------------------------------------------
%
%
% Internal functions -------------------------------------------------

% Main request handler function.
% Request - message that is received from client.
% State - current server state (contains internal queue).
handle_request(Request, State) ->
	logger:debug("Received request: ~p", [ Request ]),
	logger:debug("Current state: ~p", [ State ]),
	% put the incoming request to the internal queue
	InternalQueue = queue:in(Request, get(rq)),
	logger:debug("Request is queued"),
	% process requests from the internal queue and update internal queue
	put(rq, process_queue(InternalQueue)),
	% increase request counter
	State + 1.

% Internal loop for processing requests. This function handles the order the requests are processed in.
% 1. The front request from internal queue is processed.
% 2. If there are no incoming messages in process message queue the internal queue is processed again, otherwise the loop is exited.
% 3. When there are no messages in internal queue the loop is exited and server is idling until the message is received in the process message queue.
% Queue - queue(), internal queue of requests.
process_queue(Queue) ->
	logger:debug("Processing internal queue"),
	NewQueue = process_internal_queue(Queue),
	% check mailbox
	case process_info(self(), message_queue_len) of
		{ message_queue_len, 0 } when NewQueue =/= Queue ->
			logger:debug("No messages in the mailbox"),
			% no messages -> continue to process the internal queue
			process_queue(NewQueue);
		_ ->
			logger:debug("Exiting internal loop"),
			% there is a message -> exit
			NewQueue
	end.

% Processing of a single message from the internal queue.
% Queue - queue(), internal queue of requests.
process_internal_queue(Queue) ->
	case queue:out(Queue) of
		{ empty, _ } ->
			logger:debug("Internal queue is empty"),
			Queue;
		{ { value, { Action, Receiver, Payload } = Request }, NewQueue } ->
			logger:debug("Processing the request: ~p", [ Request ]),
			Result = process_request(Action, Payload),
			logger:debug("Request resulted in ~p", [ Result ]),
			case Result of
				{ done, Message } ->
					logger:debug("Sending result to: ~p", [ Receiver ]),
					Receiver ! Message,
					NewQueue;
				{ continue, Message, { ContinueAction, ContinuePayload } } ->
					logger:debug("Sending result to: ~p", [ Receiver ]),
					Receiver ! Message,
					queue:in({ ContinueAction, Receiver, ContinuePayload }, NewQueue)
			end
	end.

% Proxy for request messages. Forwarding the requests to specific private methods for processing.
% Type - atom(), requested type of action. Supported actions: read | write | append.
% Payload - term(), parameters for a specific action.
%	Supported payloads: { PageId } (for read) | { PageId, Data } (for write) | { Data } (for append).
process_request(read, { PageId }) ->
	process_request(read, { PageId, 1 });
process_request(read, { _PageId, _N } = P) ->
	read(P);
process_request(write, { _PageId, _Data } = P) ->
	write(P);
process_request(append, { _Data } = P) ->
	append(P);
process_request(_Type, _P) ->
	{ done, { error, "Unsupported request type" } }.


%% Page File Input/Output


% Read page file.
% Parameters:
%	Payload - term() = { PageId, N }, parameters for reading the page file.
%	PageId - integer(), offset of pages. Identifies the position in the page file to read from.
%	N - integer(), number of pages to read.
% Returns:
%	{ done, Result } | { continue, Result, Continue }
%	Result = { ok, Data } | { partial, Data } | { error, Error }
%	Continue = { read, Payload }
%	Data = binary()
%	Error = string()
read({ PageId, N }) when is_integer(PageId), is_integer(N), N >= 1 ->
	case PageId + N - 1 >= get(pages_cnt) of
		true ->
			{ done, { error, "Invalid input" } };
		_ ->
			Result = file:pread(get(pfs_file), PageId * ?PAGE_SIZE, ?PAGE_SIZE),
			case Result of
				eof ->
					{ done, { error, "End of stream is reached" } };
				{ ok, Data } when N > 1 ->
					{ continue, { partial, Data }, { read, { PageId + 1, N - 1 } } };
				_ -> % Result = { ok, Data } | { error, Error }
					{ done, Result }
			end
	end;
read(_P) ->
	{ done, { error, "Invalid input" } }.

% Override a page in a page file.
% Parameters:
%	Payload - term() = { PageId, Data }, parameters for writing to the page file.
%	PageId - integer(), offset of pages. Identifies the position in the page file to write to.
%	Data - binary(), data to write.
% Returns:
%	{ done, Result } | { continue, Result, Continue }
%	Result = { ok, PageId } | { partial, PageId } | { error, Error }
%	PageId = integer(), PageId that was overriden
%	Error = string()
write({ PageId, Data }) when is_binary(Data), is_integer(PageId), PageId >= 0 ->
	% check if data fits to file
	case PageId >= get(pages_cnt) of
		true ->
			{ done, { error, "Invalid input" } };
		_ ->
			{ PageData, Leftover } = prepare_write(Data),
			Result = file:pwrite(get(pfs_file), PageId * ?PAGE_SIZE, PageData),
			case Result of
				ok when Leftover =:= empty ->
					{ done, { ok, PageId } };
				ok when Leftover =/= empty ->
					{ continue, { partial, PageId }, { write, { PageId + 1, Leftover } } };
				_ -> % { error, Reason }
					{ done, Result }
			end
	end;
write(_P) ->
	{ done, { error, "Invalid input" } }.

% Append a page to a page file.
% Parameters:
%	Payload - term() = { Data }, parameters for appending the page to a page file.
%	Data - binary(), data to append.
% Returns:
%	{ done, Result } | { continue, Result, Continue }
%	Result = { ok, Data } | { partial, Data } | { error, ErrorDescription }
%	Data = binary()
%	ErrorDescription = string()
append({ Data }) when is_binary(Data) ->
	logger:debug("Preparing data for append"),
	{ PageData, Leftover } = prepare_write(Data),
	File = get(pfs_file),
	{ ok, Pos } = file:position(File, eof),
	Result = file:pwrite(File, Pos, PageData),
	case Result of
		ok when Leftover =:= empty ->
			% update pages counter
			put(pages_cnt, get(pages_cnt) + 1),
			{ done, { ok, Pos div ?PAGE_SIZE } };
		ok when Leftover =/= empty ->
			{ continue, { partial, Pos div ?PAGE_SIZE }, { append, { Leftover } } };
		_ -> % { error, Reason }
			{ done, Result }
	end;
append(_P) ->
	{ done, { error, "Invalid input" } }.


prepare_write(Data) when byte_size(Data) >= ?PAGE_DATA_SIZE ->
	<<PageData:?PAGE_DATA_SIZE/binary, Leftover/binary>> = Data,
	{ <<(create_page_header())/binary, PageData/binary>>, Leftover };
prepare_write(Data) ->
	DataSize = byte_size(Data),
	{ <<(create_page_header(DataSize))/binary, Data/binary, 0:((?PAGE_DATA_SIZE - DataSize) * 8)>>, empty }.

create_page_header() ->
	create_page_header(?PAGE_DATA_SIZE).
create_page_header(Size) ->
	% decoding header -> binary:decode_unsigned(Header, big).
	<<Size:(?PAGE_HEADER_SIZE * 8)>>.

% END (Internal functions) -------------------------------------------
%
%
% END (IMPLEMENTATION) ===============================================
