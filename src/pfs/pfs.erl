%%%-----------------------------------------------------------------------------
%%% TODO module information will be placed here
%%%-----------------------------------------------------------------------------

-module(pfs).
-behaviour(gen_server).
-include("pfs.hrl").

% gen_server API
-export([ init/1, terminate/2, handle_call/3, handle_cast/2 ]).
% PFS API
-export([ start/1, get_data/2, get_data/3, write_data/2, write_data/3 ]).

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


% PFS API implementation -------------------------------------------------------


%% Page File Server external initialization.
%% PageFile - string(), a relative or absolute path to the page file.
start(PageFile) ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [ PageFile ], []).

%% Retrieve page data for a specified PageId
get_data(ForPid, PageId) -> get_data(ForPid, PageId, 1).

%% Retrieve NumberOfPages pages of data starting from PageId
get_data(ForPid, PageId, NumberOfPages) ->
	gen_server:cast(?MODULE, { read, ForPid, { PageId, NumberOfPages } }).

%% Create a new page an the end of page file with data Data
write_data(ForPid, Data) ->
	gen_server:cast(?MODULE, { append, ForPid, { Data } }).

%% Override page PageId with data Data
write_data(ForPid, PageId, Data) ->
	gen_server:cast(?MODULE, { write, ForPid, { PageId, Data } }).


% gen_server API implementation ------------------------------------------------


%% Page File Server internal initialization.
%% PageFile - string(), a relative or absolute path to the page file.
init([ PageFile ]) ->
	configure_logger(?LOG_LEVEL),
	logger:debug("Initialization with page file: ~p", [ PageFile ]),
	try
		process_flag(trap_exit, true),
		State = pfs_init(PageFile),
		logger:info("PFS initialization completed."),
		{ ok, State }
	catch
		_ExceptionClass:Error:Trace ->
			logger:critical("Error initializing PFS. Error: ~p Trace: ~p", [ Error, Trace ]),
			{ stop, ?GENERIC_ERROR }
	end.

%% Page File Server termination handler.
terminate(Reason, State) ->
	logger:info("Terminating. Reason: ~p", [ Reason ]),
	logger:debug("State before termination: ~p", [ State ]),
	Result = file:close(get(?PAGE_FILE)),
	logger:debug("Page file is closed with response: ~p", [ Result ]),
	ok.

%% Handle asyncronous requests
handle_cast({ read, _Receiver, _Payload } = Request, State) ->
	{ noreply, handle_request(Request, State) };
handle_cast({ write, _Receiver, _Payload } = Request, State) ->
	{ noreply, handle_request(Request, State) };
handle_cast({ append, _Receiver, _Payload } = Request, State) ->
	{ noreply, handle_request(Request, State) };
handle_cast(_Request, State) ->
	logger:warning("Unknown cast request received"),
	{ noreply, State }.

%% Handle syncronous calls
handle_call(_Request, _From, State) ->
	logger:warning("Unknown call request received"),
	{ noreply, State }.


% Internal functions -----------------------------------------------------------


%% Configure logger.
configure_logger(LogLevel) ->
	logger:update_primary_config(maps:merge(logger:get_primary_config(), #{ level => LogLevel })).

%% Initialization
pfs_init(PageFile) ->
	open_page_file(PageFile),
	initialize_page_file_parameters(),
	initialize_internal_request_queue(),
	get_current_state().

%% Open page file and store it.
open_page_file(PageFile) ->
	{ ok, File } = file:open(PageFile, [raw, binary, read, write]),
	put(?PAGE_FILE, File).

initialize_page_file_parameters() ->
	PageHeaderSize = trunc(math:ceil(math:log2(?PAGE_SIZE))),
	put(?PAGE_HEADER_SIZE, PageHeaderSize),
	put(?PAGE_DATA_SIZE, ?PAGE_SIZE - PageHeaderSize).

%% Initialize internal queue as empty.
initialize_internal_request_queue() ->
	put(?INTERNAL_QUEUE, queue:new()).

%% Generate server State.
get_current_state() ->
	get_pages_count().

%% Calculate the number of pages in the page file.
get_pages_count() ->
	{ ok, FileInfo } = file:read_file_info(get(?PAGE_FILE)),
	element(2, FileInfo) / ?PAGE_SIZE.

%% Main request handler function.
%% Request - message that is received from client.
%% State - current server state (contains internal queue).
handle_request(Request, _CurrentState) ->
	logger:debug("Received request: ~p", [ Request ]),
	try
		% put the incoming request to the internal queue
		put(?INTERNAL_QUEUE, queue:in(Request, get(?INTERNAL_QUEUE))),
		% enter process loop
		process_loop()
	catch
		_ExceptionClass:Error:Trace ->
			logger:error("Unexpected error occured. Error: ~p Trace: ~p", [ Error, Trace ])
	end.

%% Internal loop for processing requests. This function handles the order the requests are processed in.
%% 1. The front request from internal queue is processed.
%% 2. If there are no incoming messages in process message queue the internal queue is processed again, otherwise the loop is exited.
%% 3. When there are no messages in internal queue the loop is exited and server is idling until the message is received in the process message queue.
%% Queue - queue(), internal queue of requests.
process_loop() ->
	MessagesLeft = process_internal_queue(),
	case process_info(self(), message_queue_len) of
		{ message_queue_len, 0 } when MessagesLeft > 0 ->
			process_loop();
		_ -> get_current_state()
	end.

%% Process requests from internal queue and return the remaining queue size.
process_internal_queue() ->
	process_internal_queue(queue:out(get(?INTERNAL_QUEUE))).
process_internal_queue({ empty, _ }) -> 0;
process_internal_queue({ { value, { Action, Receiver, Payload } = Request }, Q }) ->
	logger:debug("Processing request: ~p", [ Request ]),
	{ Status, Response } = execute_request(Action, Payload),
	logger:debug("Request resulted in ~p", [ { Status, Response } ]),
	logger:debug("Sending result to: ~p", [ Receiver ]),
	Receiver ! Response,
	process_request_status(Receiver, Q, Status),
	queue:len(get(?INTERNAL_QUEUE)).

%% Process request status and update internal queue.
process_request_status(_Receiver, Q, done) ->
	put(?INTERNAL_QUEUE, Q);
process_request_status(Receiver, Q, { continue, ContinueAction, ContinuePayload }) ->
	logger:debug("Creating a follow-up request ~p", [ { ContinueAction, Receiver, ContinuePayload } ]),
	put(?INTERNAL_QUEUE, queue:in({ ContinueAction, Receiver, ContinuePayload }, Q)).

%% Proxy for request messages. Forwarding the requests to specific private methods for processing.
%% Type - atom(), requested type of action. Supported actions: read | write | append.
%% Payload - term(), parameters for a specific action.
%% Supported payloads: { PageId, N } (for read) | { PageId, Data } (for write) | { Data } (for append).
execute_request(read, P) -> read(P);
execute_request(write, P) -> write(P);
execute_request(append, P) -> append(P);
execute_request(_Action, _P) ->
	{ done, { error, "Invalid input" } }.


% Page file I/O functions ------------------------------------------------------


%% Check if input for accessing page file is valid.
check_input(PageId, NPagesToAccess) when is_integer(PageId), is_integer(NPagesToAccess) ->
	TotalPages = get_pages_count(),
	case
		PageId < 0
		orelse NPagesToAccess < 1
		orelse PageId >= TotalPages
		orelse PageId + NPagesToAccess - 1 >= TotalPages of
		true -> false;
		_ -> true
	end.

%% Read page file.
%% Parameters:
%%	Payload - term() = { PageId, N }, parameters for reading the page file.
%%	PageId - integer(), offset of pages. Identifies the position in the page file to read from.
%%	N - integer(), number of pages to read.
%% Returns:
%%	{ done, Result } | { continue, Result, Continue }
%%	Result = { ok, Data } | { partial, Data } | { error, Error }
%%	Continue = { read, Payload }
%%	Data = binary()
%%	Error = string()
read({ PageId, all }) ->
	read({ PageId, get_pages_count() - PageId });
read({ PageId, N }) ->
	logger:debug("Read ~p pages from page ID ~p", [ N, PageId ]),
	try
		true = check_input(PageId, N),
		{ ok, Data } = file:pread(get(?PAGE_FILE), PageId * ?PAGE_SIZE, ?PAGE_SIZE),
		case N > 1 of
			true -> { { continue, read, { PageId + 1, N - 1 } }, { partial, Data } };
			_ -> { done, { ok, Data } }
		end
	catch
		_ExceptionClass:Error:Trace ->
			logger:error("Error processing read request. Error: ~p Trace: ~p", [ Error, Trace ]),
			{ done, { error, ?GENERIC_ERROR } }
	end.

%% Override a page in a page file.
%% Parameters:
%%	Payload - term() = { PageId, Data }, parameters for writing to the page file.
%%	PageId - integer(), offset of pages. Identifies the position in the page file to write to.
%%	Data - binary(), data to write.
%% Returns:
%%	{ done, Result } | { continue, Result, Continue }
%%	Result = { ok, PageId } | { partial, PageId } | { error, Error }
%%	PageId = integer(), PageId that was overriden
%%	Error = string()
write({ PageId, Data }) ->
	logger:debug("Write to page ID", [ PageId ]),
	try
		true = check_input(PageId, 1),
		% { PageData, Leftover } = prepare_write(Data),
		{ PageData, empty } = prepare_write(Data),
		ok = file:pwrite(get(?PAGE_FILE), PageId * ?PAGE_SIZE, PageData),
		% case Leftover =/= empty of
		% 	true -> { { continue, write, { PageId + 1, Leftover } }, { partial, PageId } };
		% 	_ -> { done, { ok, PageId } }
		% end
		{ done, { ok, PageId } }
	catch
		_ExceptionClass:Error:Trace ->
			logger:error("Error processing write request. Error: ~p Trace: ~p", [ Error, Trace ]),
			{ done, { error, ?GENERIC_ERROR } }
	end.

%% Append a page to a page file.
%% Parameters:
%%	Payload - term() = { Data }, parameters for appending the page to a page file.
%%	Data - binary(), data to append.
%% Returns:
%%	{ done, Result } | { continue, Result, Continue }
%%	Result = { ok, Data } | { partial, Data } | { error, ErrorDescription }
%%	Data = binary()
%%	ErrorDescription = string()
append({ Data }) ->
	logger:debug("Append data"),
	try
		{ PageData, Leftover } = prepare_write(Data),
		File = get(?PAGE_FILE),
		{ ok, Pos } = file:position(File, eof),
		ok = file:pwrite(File, Pos, PageData),
		case Leftover =/= empty of
			true -> { { continue, append, { Leftover } }, { partial, Pos div ?PAGE_SIZE } };
			_ -> { done, { ok, Pos div ?PAGE_SIZE } }
		end
	catch
		_ExceptionClass:Error:Trace ->
			logger:error("Error processing append request. Error: ~p Trace: ~p", [ Error, Trace ]),
			{ done, { error, ?GENERIC_ERROR } }
	end.

%% Prepare page data for writing. Create page header and make sure the data fits the page size.
prepare_write(Data) when is_binary(Data) ->
	PageDataSize = get(?PAGE_DATA_SIZE),
	DataSize = byte_size(Data),
	case DataSize > PageDataSize of
		true ->
			<<PageData:PageDataSize/binary, Leftover/binary>> = Data,
			{ <<(create_page_header(PageDataSize))/binary, PageData/binary>>, Leftover };
		_ ->
			{ <<(create_page_header(DataSize))/binary, Data/binary, (padding(PageDataSize - DataSize))/binary>>, empty }
	end.

%% Padding with zeros.
padding(Size) -> <<0:(Size * 8)>>.

%% Page header.
create_page_header(Size) ->
	% decoding header -> binary:decode_unsigned(Header, big).
	<<Size:(get(?PAGE_HEADER_SIZE) * 8)>>.
