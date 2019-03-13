-module(farm).
-export([farmer/4, worker/0, farm/0]).

farmer(0, N, Worker1_PID, Worker2_PID) ->
  case rand:uniform(2) of
    1 ->
      io:format("Submit~n"),
      farmer(1, N, Worker1_PID, Worker2_PID);
    2 ->
      receive
        complete ->
            io:format("Complete~n"),
            farmer(0, N + 1, Worker1_PID, Worker2_PID)
      end
  end;

farmer(J, 0, Worker1_PID, Worker2_PID) ->
  case rand:uniform(2) of
    1 ->
      io:format("Submit~n"),
      farmer(J + 1, 0, Worker1_PID, Worker2_PID);
    2 ->
      receive
        complete -> io:format("Complete~n"),
        farmer(J, 1, Worker1_PID, Worker2_PID)
      end
  end;

farmer(J, N, Worker1_PID, Worker2_PID) ->
  case rand:uniform(3) of
    1 ->
      io:format("Submit~n"),
      farmer(J + 1, N, Worker1_PID, Worker2_PID);
    2 ->
      receive
        complete ->
          io:format("Complete~n"),
          farmer(J, N + 1, Worker1_PID, Worker2_PID)
      end;
    3 ->
      io:format("Assign~n"),
      case rand:uniform(2) of
        1 -> Worker1_PID ! {assign, self()};
        2 -> Worker2_PID ! {assign, self()}
      end,
      farmer(J - 1, N - 1, Worker1_PID, Worker2_PID)
  end.

worker() ->
  receive
    {assign, Farmer_PID} -> io:format("Worker~p received Assign~n", [self()])
  end,
  Farmer_PID ! complete,
  worker().

farm() ->
  Worker1_PID = spawn(farm, worker, []),
  Worker2_PID = spawn(farm, worker, []),
  spawn(farm, farmer, [0, 2, Worker1_PID, Worker2_PID]).
