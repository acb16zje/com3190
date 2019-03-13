-module(pingpong).
-export([start/0, ping/2, pong/0, count/1, pinger/2, ping2/2]).

ping(0, Pong_PID) ->
  Pong_PID ! finished,
  io:format("Ping finished~n", []);

ping(N, Pong_PID) ->
  Pong_PID ! {ping, self()},
  receive
    pong -> io:format("Ping received pong~n", [])
  end,
  ping(N - 1, Pong_PID).

pong() ->
  receive
    finished -> io:format("Pong finished~n", []);
    {ping, Ping_PID} ->
      io:format("Pong received ping~n", []),
      Ping_PID ! pong,
      pong()
  end.

start() ->
  Pong_PID = spawn(pingpong, pong, []),
  spawn(pingpong, ping, [3, Pong_PID]).
  % Pong2_PID = spawn(pingpong, pong, []),
  % spawn(pingpong, ping, [5, Pong2_PID]).

count(1) ->
  receive
    {dec, PID} -> PID ! done
  end;

count(N) ->
  receive
    {dec, PID} ->
      PID ! more,
      count(N - 1)
  end.

pinger(Pong_PID, Count_PID) ->
  Pong_PID ! {ping, self()},
  receive
    pong ->
      Count_PID ! {dec, self()},
      receive
        more -> pinger(Pong_PID, Count_PID);
        done -> Pong_PID ! finished
      end
  end.

ping2(N, Pong_PID) ->
  Count_PID = spawn(pingpong, count, [N]),
  pinger(Pong_PID, Count_PID).
