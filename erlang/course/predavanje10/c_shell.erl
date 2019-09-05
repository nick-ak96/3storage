-module(c_shell).
-export([start/0, eval/2]).

start() ->
    process_flag(trap_exit, true),
    go().

go() ->
    eval(io:parse_erl_exprs('-> ')),

go().

eval({ok, Exprs, _}) ->
    Id = spawn_link(c_shell, eval, [self(), Exprs]),
    receive
        {value, Res, _} ->
            {ok,R} = Res,
            io:format("Result: ~s~n", [R]),
            receive
                {'EXIT', Id, _ } ->
                    true
            end;
        {'EXIT', Id, Reason} ->
            io:format("Error: ~w!~n", [Reason])
    end;

eval(Pres) ->
    io:format("Syntax Error: ~w !~n", [Pres]).

eval(Id, Exprs) ->
    Id ! erl_eval:exprs(Exprs, []).
