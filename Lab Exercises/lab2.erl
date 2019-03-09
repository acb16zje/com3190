-module(lab2).
-export([do/1, f/0]).

do({output, E})  -> io:format("~~~p~n", [E]);
do({input, E})   -> io:format("~p~n", [E]);
do([])           -> ok;
do([E | MoreEs]) -> do(E), do(MoreEs);
do(_)            -> io:format("Unknown argument to do/1~n").

f() ->
  X = 5,
  Y = X + 1,
  Z = Y + 1,
  Y + X.