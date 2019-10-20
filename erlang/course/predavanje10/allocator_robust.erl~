-module(allocator).
-export([start/1,server/2,allocate/0,free/1]).

start(Resources) ->
    Pid = spawn(allocator, server, [Resources,[]]),
    register(resource_alloc, Pid).

% The interface functions.
allocate() ->
    request(alloc).

free(Resource) ->
    request({free,Resource}).

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
            free(Free, Allocated, From, R)
    end.

allocate([R|Free], Allocated, From) ->
    From ! {resource_alloc,{yes,R}},
    server(Free, [{R,From}|Allocated]);

allocate([], Allocated, From) ->
    From ! {resource_alloc,no},
    server([], Allocated).

free(Free, Allocated, From, R) ->
    case lists:member({R,From}, Allocated) of
        true ->
            From ! {resource_alloc,ok},
            server([R|Free], lists:delete({R,From}, Allocated));
        false ->
            From ! {resource_alloc,error},
            server(Free, Allocated)
    end.