-module(allocator_robust).
-export([start/1,server/2,allocate/0,free/1]).

start(Resources) ->
    process_flag(trap_exit, true),
    Pid = spawn(allocator_robust, server, [Resources,[]]),
    register(resource_alloc, Pid).

% The access functions.
allocate() ->
    request(alloc).

free(Resource) ->
    resource_alloc ! {self(),{free,Resource}},
    receive
        {resource_alloc, error} ->
            exit(bad_allocation); % exit added here
        {resource_alloc, Reply} ->
            Reply
    end.
  
request(Request) ->
    resource_alloc ! {self(),Request},
    receive
        {resource_alloc,Reply} ->
            Reply
    end.

% The server.

server(Free, Allocated) ->
    receive
        {From,alloc} ->
            allocate(Free, Allocated, From);
        {From,{free,R}} ->
            free(Free, Allocated, From, R);
        {'EXIT', From, _ } ->
            check(Free, Allocated, From)
    end.

allocate([R|Free], Allocated, From) ->
    link(From),
    From ! {resource_alloc,{yes,R}},
    server(Free, [{R,From}|Allocated]);

allocate([], Allocated, From) ->
    From ! {resource_alloc,no},
    server([], Allocated).

free(Free, Allocated, From, R) ->
    case lists:member({R, From}, Allocated) of
        true ->
            From ! {resource_alloc, yes},
            Allocated1 = lists:delete({R, From}, Allocated),
            case lists:keysearch(From, 2, Allocated1) of
                false ->
                    unlink(From);
                _ ->
                    true
            end,
            server([R|Free], Allocated1);
        false ->
            From ! {resource_alloc, error},
            server(Free, Allocated)
    end.

check(Free, Allocated, From) ->
    case lists:keysearch(From, 2, Allocated) of
        false ->
            server(Free, Allocated);
        {value, {R, From}} ->
            check([R|Free], lists:delete({R, From}, Allocated), From)
    end.
