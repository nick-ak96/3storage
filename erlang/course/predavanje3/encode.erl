-module(encode).
-export([encode/2,test/0]).

encode(Pin, Password) ->
    Code = {nil,nil,nil,nil,nil,nil,nil,nil,nil,
            nil,nil,nil,nil,nil,nil,nil,nil,nil,
            nil,nil,nil,nil,nil,nil,nil,nil},
    encode(Pin, Password, Code).

encode([], _, Code) ->
    Code;
encode(_, [], _) ->
    io:format("Out of Letters~n",[]);
encode([H|T], [Letter|T1], Code) ->
    Arg = index(Letter) + 1,
    case element(Arg, Code) of
        nil ->
           encode(T, T1, setelement(Arg, Code, index(H)));
        _ ->
           encode([H|T], T1, Code)
        end.

index(X) when X >= $0, X =< $9 ->
    X - $0;
index(X) when X >= $A, X =< $Z ->
    X - $A.

print_code([], Seed) ->
    Seed;
print_code([nil|T], Seed) ->
    NewSeed = ran(Seed),
    Digit = NewSeed rem 10,
    io:format("~w ",[Digit]),
    print_code(T, NewSeed);
print_code([H|T],Seed) ->
    io:format("~w ",[H]),
    print_code(T, Seed).
    
ran(Seed) ->
    (125 * Seed + 1) rem 4096.

test() ->
    title(),
    Password = "DECLARATIVE",
    entries([{"3451",Password,lisa},
             {"1234",Password,carwash},
             {"4321",Password,bigbank},
             {"7568",Password,doorcode1},
             {"8832",Password,doorcode2},
             {"4278",Password,cashcard},
             {"4278",Password,chequecard}]).

title() ->
    io:format("a b c d e f g h i j k l m n o p q r s t u v w x y z~n",[]).

entries(List) ->
    {_,_,Seed} = time(),
    entries(List, Seed).

entries([], _) -> true;
entries([{Pin,Password,Title}|T], Seed) ->
    Code = encode(Pin, Password),
    NewSeed = print_code(tuple_to_list(Code), Seed),
    io:format(" ~w~n",[Title]),
    entries(T, NewSeed).
