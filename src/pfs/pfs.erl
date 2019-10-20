%% --------------------------------------------------------------------
%% Header
%% --------------------------------------------------------------------
-module(pfs).
-behaviour(gen_server).


% includes
%-include_lib("kernel/include/logger.hrl").


% gen_server API
-export([ init/1, terminate/2, handle_call/3, handle_cast/2 ]).
% public API
-export([ start_link/2, stop/1, read_request/2, write_request/2 ]).


% Constants
-define(PAGE_SIZE, 4). % page size in bytes
-define(N_CHUNKS, 1). % number of chunks/pages that can be processed at once


% Structures
% sn = ServerName, f = PageFile, rq = RequestQueue
-record(state, { sn, f, rq }).

%% TODO
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

% ====================================================================


%% --------------------------------------------------------------------
%% Server API
%% --------------------------------------------------------------------

init([ ServerName, Filepath ]) ->
	process_flag(trap_exit, true),
	{ Status, Result } = file:open(Filepath, [raw, binary, read, write]),
	case Status of
		error ->
			%logger:error(Result),
			%logger:critical("Initialization failed. Page file: ~w", [ File ]),
			{ stop, Result };
		ok ->
			%logger:info("Initialization with file ~w", [ File ]),
			%logger:info("Initialization finished successfully."),
			{ ok, #state{ sn = ServerName, f = Result, rq = queue:new() } }
	end.

terminate(_Reason, _State) ->
	% TODO termination protocol, if the request queue is non-empty
	ok.

handle_call({ read, { PageId, N } }, { _PID, Tag }, State) ->
	process_request({ read, { PageId, N } }, Tag, State);
handle_call({ write, { PageId, N, Data } }, { _PID, Tag }, State) ->
	process_request({ write, { PageId, N, Data } }, Tag, State);
handle_call({ continue, RequestId }, _From, State) ->
	process_request({ continue, RequestId }, RequestId, State);
handle_call({ read_request, { PageId, N } }, _From, State) ->
	read_request(State#state.sn, { PageId, N });
handle_call({ write_request, { PageId, N, Data } }, _From, State) ->
	write_request(State#state.sn, { PageId, N, Data });
handle_call(_Request, _From, State) ->
	{ noreply, State }.

handle_cast(_Request, State) ->
	{ noreply, State }.

% ====================================================================


%% --------------------------------------------------------------------
%% Public API
%% --------------------------------------------------------------------

start_link(ServerName, File) ->
	gen_server:start_link({ local, ServerName }, ?MODULE, [ ServerName, File ], []).

stop(ServerName) ->
	gen_server:stop(ServerName).

read_request(ServerName, Request) ->
	process_read_request(ServerName, { read, Request }).

write_request(ServerName, Request) ->
	process_write_request(ServerName, { write, Request }).

% ====================================================================


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

process_read_request(ServerName, Request) ->
	io:fwrite("process_read_request(~w, ~w)~n", [ ServerName, Request ]),
	Reply = gen_server:call(ServerName, Request),
	io:fwrite("reply: ~w~n", [ Reply ]),
		case Reply of
			{ continue, Data, Continue } ->
				{ NewStatus, NewResult } = process_read_request(ServerName, Continue),
				case NewStatus of
					ok ->
						{ ok, <<Data/binary, NewResult/binary>> };
					error ->
						{ error, NewResult }
				end;
			{ busy, Continue } ->
				process_read_request(ServerName, Continue);
			{ Status, Result } ->
				{ Status, Result }
		end.

process_write_request(ServerName, Request) ->
	io:fwrite("process_write_request(~w, ~w)~n", [ ServerName, Request ]),
	Reply = gen_server:call(ServerName, Request),
	io:fwrite("reply: ~w~n", [ Reply ]),
	case Reply of
		{ continue, Continue } ->
			process_write_request(ServerName, Continue);
		{ busy, Continue } ->
			process_write_request(ServerName, Continue);
		{ Status, Result } ->
			{ Status, Result }
	end.

process_request({ read, Args }, Tag, State) ->
	process_response(Tag, read(Args, State));
process_request({ write, Args }, Tag, State) ->
	process_response(Tag, write(Args, State));
process_request({ continue, Args }, Tag, State) ->
	process_response(Tag, continue(Args, State)).

process_response(_Tag, { Status, Result, NewState }) ->
	{ reply, { Status, Result }, NewState };
process_response(Tag, { continue, Result, NewRequest, NewState }) ->
	{ reply, { continue, Result, { continue, Tag } }, NewState#state{ rq = queue:in({ Tag, NewRequest }, NewState#state.rq) } }.

read({ PageId, N }, State) when N > ?N_CHUNKS ->
	{ Status, Result } = read_page_file(State#state.f, PageId, ?N_CHUNKS),
	case Status of
		ok ->
			{ continue, Result, { read, { PageId + ?N_CHUNKS, N - ?N_CHUNKS } }, State };
		error ->
			{ Status, Result, State }
	end;
read({ PageId, N }, State) ->
	{ Status, Result } = read_page_file(State#state.f, PageId, N),
	{ Status, Result, State }.

write({ _PageId, N, Data }, State) when size(Data) =/= N * ?PAGE_SIZE ->
	{ error, "Invalid data to process", State };
write({ PageId, N, Data }, State) when N > ?N_CHUNKS ->
	<<Chunk:(?N_CHUNKS * ?PAGE_SIZE)/binary, Rest/binary>> = Data,
	{ Status, Result } = write_page_file(State#state.f, PageId, ?N_CHUNKS, Chunk),
	case Status of
		ok ->
			{ continue, { write, { PageId + ?N_CHUNKS, N - ?N_CHUNKS, Rest } }, State };
		error ->
			{ Status, Result, State }
	end;
write({ PageId, N, Data }, State) ->
	{ Status, Result } = write_page_file(State#state.f, PageId, N, Data),
	{ Status, Result, State }.

continue(Tag, State) ->
	Q = queue:filter(fun(Item) -> { T, _ } = Item, Tag =:= T end, State#state.rq),
	case queue:is_empty(Q) of
		true ->
			{ error, "No request was found to continue" };
		false ->
			QItem = queue:peek(State#state.rq),
			case QItem of
				{ value, { Tag, Request } } ->
					continue_request(Request, State#state{ rq = queue:drop(State#state.rq)});
				_ ->
					{ busy, { continue, Tag }, State }
			end
	end.

continue_request({ read, Args }, State) ->
	io:fwrite("continue_request(~w)~n", [ { read, Args } ]),
	read(Args, State);
continue_request({ write, Args }, State) ->
	io:fwrite("continue_request(~w)~n", [ { write, Args } ]),
	write(Args, State).

read_page_file(File, PageId, N) ->
	Result = file:pread(File, PageId * ?PAGE_SIZE, N * ?PAGE_SIZE),
	case Result of
		eof ->
			{ error, "End of stream is reached" };
		_ ->
			Result
	end.

write_page_file(File, PageId, _N, Data) ->
	Result = file:pwrite(File, PageId * ?PAGE_SIZE, Data),
	case Result of
		ok ->
			{ Result, ok };
		_ ->
			Result
	end.

% ====================================================================
