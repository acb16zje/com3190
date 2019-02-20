-module(lab1).
-import(lists,[nth/2]).
-export([say/1, respond/1, broken/1, extra/1, recursion/1, getNth/2]).

say(A) -> io:format("Hello, ~p~n", [A]).

respond(mike)        -> "Hi mike!";
respond(42)          -> meaningoflife;
respond({mytuple,_}) -> "You gave me a pair but I ignored half of it".

broken({1,A}) -> 1 + A;
broken({1,0}) -> 1;
broken(A)     -> {error, "I can't handle that!", A}.

extra({_, A, _}) -> A.

recursion(L)          -> recursion(L, 0).
recursion([H|T], Sum) -> recursion(T, Sum + H);
recursion([], Sum)    -> Sum.

getNth(L, N) -> nth(N, L).
