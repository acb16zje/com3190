-module(lab3).
-export([do/1, nil/0, p1/0, p2/0, p3/0, p4/0, p5/0, q5/0]).

%do(X) -> case X of
%              {output, E} ->
%                  io:format("~~~p~n", [E]);
%              {input, E}  ->
%                  io:format("~p~n", [E]);
%              [] ->
%                  ok;
%              [E | MoreEs] ->
%                  do(E), do(MoreEs);
%              _Else ->
%                  io:format("Unknown argument to do/1~n")
%         end.

do("a") -> io:format("a");
do("~a") -> io:format("~~a");
do("b") -> io:format("b");
do("~b") -> io:format("~~b").
nil() -> ok.

p1() -> do("a"), do("b"), do("~a"), do("~b"), nil().
p2() -> do("a"), do("~b"), p2().
p3() -> case rand:uniform(2) of
            1 ->
                do("a"), nil();
            2 ->
                do("b"), nil()
        end.
p4() -> case rand:uniform(2) of
            1 ->
                do("a"), p4();
            2 ->
                do("b"), nil()
        end.
p5() -> case rand:uniform(2) of
            1 ->
                do("a"), q5();
            2 ->
                do("~b"), nil()
        end.
q5() -> do("~b"), p5().