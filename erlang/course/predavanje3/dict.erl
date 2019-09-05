-module(dict).
-export([new/0,lookup/2,add/3,delete/2]).

new() ->
    [].

lookup(Key, [{Key,Value}|Rest]) ->
    {value,Value};
lookup(Key, [Pair|Rest]) ->
    lookup(Key, Rest);
lookup(Key, []) ->
    undefined.

add(Key, Value, Dict) ->
    NewDict = delete(Key, Dict),
    [{Key,Value}|NewDict].

delete(Key, [{Key,Value}|Rest]) ->
    Rest;
delete(Key, [Pair|Rest]) ->
    [Pair|delete(Key, Rest)];
delete(Key, []) ->
    [].

