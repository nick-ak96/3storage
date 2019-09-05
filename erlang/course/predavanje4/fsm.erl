-module(fsm).
-compile(export_all).

start() ->
    spawn(fsm, s1, []).

s1() ->
    put(cstate,1),
    receive
        msg_a -> 
            s2();
        msg_c -> 
            s3();
        state -> 
            io:format("State=~w~n",[get(cstate)]),
            s1();
        {From,state} -> 
            io:format("Message=~w ! {~w,~w}~n",[From,self(),get(cstate)]),
            From ! {self(),get(cstate)},
            s1();
        stop -> 
            ok;
        _ -> 
            s1()
    end.

s2() ->
    put(cstate,2),
    receive
        msg_x -> 
            s3();
        msg_h -> 
            s4();
        state -> 
            io:format("State=~w~n",[get(cstate)]),
            s2();
        {From,state} -> 
            io:format("Send=~w ! {~w,~w}~n",[From,self(),get(cstate)]),
            From ! {self(),get(cstate)},
            s2();
        stop -> 
            ok;
        _ -> 
            s2()
    end.

s3() ->
    put(cstate,3),
    receive
        msg_b -> 
            s1();
        msg_y -> 
            s2();
        state -> 
            io:format("State=~w~n",[get(cstate)]),
            s3();
        {From,state} -> 
            io:format("Send=~w ! {~w,~w}~n",[From,self(),get(cstate)]),
            From ! {self(),get(cstate)},
            s3();
        stop -> 
            ok;
        _ -> 
            s3()
    end.

s4() ->
    put(cstate,4),
    receive
        msg_i -> 
            s3();
        state -> 
              io:format("State=~w~n",[get(cstate)]),
              s4();
        {From,state} -> 
            io:format("Send=~w ! {~w,~w}~n",[From,self(),get(cstate)]),
            From ! {self(),get(cstate)},
            s4();
        stop -> 
            ok;
        _ -> 
            s4()
    end.
