-module(bank_server).
-export([start/0, server/1]).

start() ->
    register(bank_server, spawn(bank_server, server, [[]])).

server(Data) ->
    receive
        {From, {deposit, Who, Amount}} ->
            From ! {bank_server, ok},
            server(deposit(Who, Amount, Data));
        {From, {ask, Who}} ->
            From ! {bank_server, lookup(Who, Data)},
            server(Data);
        {From, {withdraw, Who, Amount}} ->
            case lookup(Who, Data) of
                undefined ->
                    From ! {bank_server, no},
                    server(Data);
                Balance when Balance > Amount ->
                    From ! {bank_server, ok},
                    server(deposit(Who, -Amount, Data));
                _ ->
                    From ! {bank_server, no},
                    server(Data)
            end
    end.

lookup(Who, [{Who, Value}|_]) -> Value;
lookup(Who, [_|T]) -> lookup(Who, T);
lookup(_, _) -> undefined.

deposit(Who, X, [{Who, Balance}|T]) ->
    [{Who, Balance+X}|T];
deposit(Who, X, [H|T]) ->
    [H|deposit(Who, X, T)];
deposit(Who, X, []) ->
    [{Who, X}].
