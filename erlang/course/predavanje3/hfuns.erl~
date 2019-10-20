-module(hfuns).
-compile(export_all).
    
map(F, [H|T]) ->
    [F(H)|map(F, T)];
map(F, []) ->
    [].

filter(Pred, [H|T]) ->
    case Pred(H) of    
        true ->
            [H|filter(Pred, T)];
        false ->
            filter(Pred, T)
    end;
filter(Pred, []) -> [].

foreach(F, [H|T]) ->
    F(H),
    foreach(F, T);
foreach(F, []) ->
    ok.
%foreach(fun(H) -> io:format(S, "~p~n",[H]) end, L)

any(Pred, [H|T]) ->
    case Pred(H) of
        true  ->  true;
        false ->  any(Pred, T)
    end;
any(Pred, []) ->
    false.
%> Big =  fun(X) -> if X > 10 -> true; true -> false end end.
%#Fun<erl_eval.6.72228031>
%> lists:any(Big, [1,2,3,4]).
%false
%> lists:any(Big, [1,2,3,12,5]).
%true

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) -> Accu.
%> L = ["I","like","Erlang"].
%["I","like","Erlang"]
%10> lists:foldl(fun(X, Sum) -> length(X) + Sum end, 0, L).                    
%11

foldr(F, Accu, [Hd|Tail]) ->
    F(Hd, foldr(F, Accu, Tail));
foldr(F, Accu, []) -> Accu.

