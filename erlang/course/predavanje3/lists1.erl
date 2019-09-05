-module(lists1).
-export([member/2,append/2,reverse/1,delete_all/2]).

member(X, [X|_]) -> true;
member(X, [_|T]) -> member(X, T);
member(X, []) -> false.

append([H|L1], L2) -> [H|append(L1, L2)];
append([], L) -> L.

reverse(L) -> reverse(L, []).

reverse([H|T], Acc) ->
    reverse(T, [H|Acc]);
reverse([], Acc) ->
    Acc.

delete_all(X, [X|T]) ->
    delete_all(X, T);
delete_all(X, [Y|T]) ->
    [Y | delete_all(X, T)];
delete_all(_, []) ->
    [].

