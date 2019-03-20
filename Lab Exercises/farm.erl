-module(farm).

-export([farm/0,submit/2,report/0]).
-export([worker/0,farmer/2]).

worker() ->
  receive
    {assign, Job, From} ->
      From ! {complete, Job, self()},
      worker()
  end.

farmer([], Workers) ->
  receive
	  {submit, Job} ->
	    farmer([Job], Workers);
	  {complete, {J, Sender}, W} ->
	    Sender ! {report, J},
	    farmer([], [W | Workers])
  end;

farmer(Jobs, []) ->
  receive
    {submit, Job} ->
      farmer([Job|Jobs], []);
    {complete, {J, Sender}, W} ->
      Sender ! {report, J},
      farmer(Jobs, [W])
  end;

farmer([J|Jobs], [W|Workers]) ->
  W ! {assign, J, self()},
  farmer(Jobs, Workers).

farm() ->
  W1 = spawn(?MODULE, worker, []),
  W2 = spawn(?MODULE, worker, []),
  spawn(?MODULE, farmer, [[], []]).

%% Jobs need to be paired with our PID so we can get the report back again
%% This will just bind to the Job variable in most places, but the
%% complete event in farmer needs to know its a pair and fish out the return
%% address. This is the biggest change from the CCS
% lists:map(fun(L) -> farm:submit(F,L) end, lists:seq(1,100)).
% lists:map(fun(L) -> farm:report() end, lists:seq(1,100)).
% The order of the report is not ordered because we only have 2 workers, when
% first worker finishes its job, then it do the one it receives next (ie. 30 more or so)
submit(Farmer, Job) ->
  Farmer ! {submit, {Job, self()}},
  ok.

report() ->
  receive
	  {report, Job} ->
	    Job
    after 1000 ->
	    no_report
    end.

