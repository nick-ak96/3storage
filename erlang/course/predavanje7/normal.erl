-module(normal).
-export([start/1, p1/1, test/1]).

start(N) ->
    register(start, spawn_link(normal, p1, [N - 1])).

p1(0) ->
    top1();
  
p1(N) ->
    top(spawn_link(normal, p1, [N - 1]),N).

top(Next, N) ->
    receive
        X ->
            Next ! X,
            io:format("Process ~w received ~w~n", [N,X]),
            top(Next,N)
    end.

top1() ->
    receive
        stop ->
            io:format("Last process now exiting ~n", []),
            exit(finished);
        X ->
            io:format("Last process received ~w~n", [X]),
            top1()
    end.
 
test(Mess) ->
    start ! Mess.

