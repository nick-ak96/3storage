-module(sets1).
-export([new/0, add_element/2, del_element/2,
         is_element/2, is_empty/1, union/2, intersection/2]).
    
new() -> [].

add_element(X, Set) ->
    case is_element(X, Set) of
        true -> Set;
        false -> [X|Set]
    end.

del_element(X, [X|T]) -> T;
del_element(X, [Y|T]) -> [Y|del_element(X,T)];
del_element(_, []) -> [].

is_element(H, [H|_]) -> true;
is_element(H, [_|Set]) -> is_element(H, Set);
is_element(_, []) -> false.

is_empty([]) -> true;
is_empty(_) -> false.

union([H|T], Set) -> union(T, add_element(H, Set));
union([], Set) -> Set.

intersection(S1, S2) -> 
    intersection(S1, S2, []).

intersection([], _, S) -> 
    S;
intersection([H|T], S1, S) ->
    case is_element(H,S1) of
        true -> intersection(T, S1, [H|S]);
        false -> intersection(T, S1, S)
    end.
