%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(kvs).
-export([start/0, store/2, lookup/1]).

start() -> register(kvs, spawn(fun() -> loop() end)).

store(Key, Value) -> rpc({store, Key, Value}).

lookup(Key) -> rpc({lookup, Key}).

rpc(Q) ->
    kvs ! {self(), Q},
    receive
	{kvs, Reply} ->
	    Reply
    end.

loop() ->
    receive
	{From, {store, Key, Value}} ->
            io:format("From=~w,Command=store,Key=~w,Value=~w~n",[From,Key,Value]),
	    put(Key, {ok, Value}),
	    From ! {kvs, true},
	    loop();
	{From, {lookup, Key}} ->
            io:format("From=~w,Command=lookup,Key=~w~n",[From,Key]),
	    From ! {kvs, get(Key)},
	    loop();
        M -> 
            io:format("Message=~w~n",[M]),
            loop()
    end.


% 1> kvs:start().
% true
% 2> kvs:store({location, joe}, "Stockholm").
% true
% 3> kvs:store(weather, raining).
% true
% 4> kvs:lookup(weather).
% {ok,raining}
% 5> kvs:lookup({location, joe}).
% {ok,"Stockholm"}
% 6> kvs:lookup({location, jane}).
% undefined
