-module(lab9).
-export([p/1, q/0, week3/0]).

p(PID) ->
  PID ! {a, self()},
  receive
    b ->
      io:format("P received b from ~p~n", [PID])
  end.

q() ->
  receive
    {a, PID} ->
      io:format("Q received a from ~p~n", [PID]),
      PID ! b
  end.

week3() ->
  PID = spawn(lab9, q, []),
  spawn(lab9, p, [PID]).
