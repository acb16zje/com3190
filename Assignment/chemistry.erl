-module(chemistry).
-export([react/2]).

ch3oh(0, _) -> io:format("Run out of CH3OH~n");
ch3oh(N, O2_PID) ->
  case rand:uniform(3) of
    1 ->
      O2_PID ! c,
      h3oh(O2_PID);
    2 ->
      O2_PID ! h,
      ch2oh(O2_PID);
    3 ->
      O2_PID ! oh,
      ch3(O2_PID)
  end,
  ch3oh(N - 1, O2_PID).

ch2oh(O2_PID) ->
  case rand:uniform(3) of
    1 ->
      O2_PID ! c,
      h2oh(O2_PID);
    2 ->
      O2_PID ! h,
      choh(O2_PID);
    3 ->
      O2_PID ! oh,
      ch2(O2_PID)
  end.

choh(O2_PID) ->
  case rand:uniform(3) of
    1 ->
      O2_PID ! c,
      hoh(O2_PID);
    2 ->
      O2_PID ! h,
      coh(O2_PID);
    3 ->
      O2_PID ! oh,
      ch(O2_PID)
  end.

ch3(O2_PID) ->
  case rand:uniform(2) of
    1 ->
      O2_PID ! c
      h3(O2_PID);
    2 ->
      O2_PID ! h,
      ch2(O2_PID)
  end.

ch2(O2_PID) ->
  case rand:uniform(2) of
    1 ->
      O2_PID ! c,
      h2(O2_PID);
    2 ->
      O2_PID ! h,
      ch(O2_PID)
  end.

ch(O2_PID) ->
  case rand:uniform(2) of
    1 ->
      O2_PID ! c,
      h(O2_PID);
    2 ->
      O2_PID ! h,
      c()
  end.

coh(O2_PID) ->
  case rand:uniform(2) of
    1 ->
      O2_PID ! c,
      oh()
  end.

h3oh(O2_PID) -> ok.
h2oh() -> ok.
hoh() -> ok.

h3() -> ok.
h2() -> ok.
h() -> ok.

o2(0) ->
  io:format("Run out of O2~n"),
  ok;

o2(N) -> ok.
o() -> ok.
oh() -> ok.
c() -> ok.
co() -> ok.

h2o() -> io:format("H2O~n").
co2() -> io:format("CO2~n").

react(Methanol, Oxygen) ->
  O2_PID = spawn(?MODULE, o2, [Oxygen]),
  spawn(?MODULE, ch3oh, [Methanol, O2_PID]).
