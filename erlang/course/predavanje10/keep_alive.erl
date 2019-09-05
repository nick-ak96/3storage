-module(keep_alive).
-export([start/0, start1/0, new_process/3]).

start() ->
    register(keep_alive, spawn(keep_alive, start1, [])).

start1() ->
    process_flag(trap_exit, true),
    loop([]).

loop(Processes) ->
    receive
        {From, {new_proc, Mod, Func, Args}} ->
            Id = spawn_link(Mod, Func, Args),
            From ! {keep_alive, started},
            loop([{Id, Mod, Func, Args}|Processes]);
        {'EXIT', Id, _} ->
            case lists:keysearch(Id, 1, Processes) of
               false ->
                   loop(Processes);
               {value, {Id, Mod, Func, Args}} ->
                   P = lists:delete({Id,Mod,Func,Args},Processes),
                   Id1 = spawn_link(Mod, Func, Args),
                   loop([{Id1, Mod, Func, Args} | P])
           end
    end.

new_process(Mod, Func, Args) ->
    keep_alive ! {self(), {new_proc, Mod, Func, Args}},    
    receive
        {keep_alive, started} ->
            true
    end.
