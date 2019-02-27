-module(lab3).
-export([do/1, nil/0, p1/0, p2/0, p3/0, p4/0, p5/0, q5/0]).

do(X) -> case X of
              {output, E} ->
                  io:format("~~~p~n", [E]);
              {input, E}  ->
                  io:format("~p~n", [E]);
              [] ->
                  ok;
              [E | MoreEs] ->
                  do(E), do(MoreEs);
              _Else ->
                  io:format("Unknown argument to do/1~n")
         end.

nil() -> ok.

p1() ->
        do({input, a}),
        do({input, b}),
        do({output, a}),
        do({output, b}),
        nil().

p2() ->
        do({input, a}),
        do({output, b}),
        p2().

p3() -> case rand:uniform(2) of
            1 ->
                do({input, a}), nil();
            2 ->
                do({input, b}), nil()
        end.
p4() -> case rand:uniform(2) of
            1 ->
                do({input, a}), p4();
            2 ->
                do({input, b}), nil()
        end.
p5() -> case rand:uniform(2) of
            1 ->
                do({input, a}), q5();
            2 ->
                do({output, b}), nil()
        end.

q5() -> do({output, b}), p5().