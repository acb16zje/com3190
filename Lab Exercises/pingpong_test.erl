-module(pingpong_test).
-import(pingpong, [start/0, ping/2, pong/0]).

-include_lib("eunit/include/eunit.hrl").

expect(Val) ->
  receive
    Val -> ok;
    Other -> {error, Other}
  end.

ping1_test() ->
  PID = spawn(pingpong, ping, [1, self()]),
  ok = expect({ping, PID}),
  PID ! pong,
  ok = expect(finished).

ping1_expect2_test() ->
  PID = spawn(pingpong, ping, [2, self()]),
  ok = expect({ping, PID}),
  PID ! pong,
  ok = expect({ping, PID}),
  PID ! pong,
  ok = expect(finished).

