% @author Zer Jun Eng
-module(chemistry).
-export([react/2]).

-type reactant() :: {atom(), pid()}.
-type reaction() :: {{reactant(), atom()}, {reactant(), atom()}, atom()}.

% -----------------------------------------------------------------------
-spec get_rule(atom()) -> list().
% @doc Get the reaction rule of a given reactant
% @param Reactant The reactant
% @returns The reaction rule of the given reactant

get_rule(Reactant) ->
  Rules = #{
    ch3oh => [{snd, c}, {snd, h}, {snd, oh}],
    ch2oh => [{snd, c}, {snd, h}, {snd, oh}],
    coh => [{snd, c}, {snd, oh}],
    ch3 => [{snd, c}, {snd, h}],
    h2 => [{rcv, o}, {snd, h}],
    h => [{rcv, oh}, {snd, h}],
    o2 => [{rcv, c}, {snd, o}],
    o => [{rcv, h}, {snd, o}],
    oh => [{rcv, h}, {snd, o}],
    c => [{rcv, o}],
    co => [{rcv, o}]
  },
  maps:get(Reactant, Rules).

% -----------------------------------------------------------------------
-spec spawn_reactant(atom()) -> reactant().
% @doc Spawn a new process of a given reactant
% @param Reactant The reactant to spawn
% @returns {Reactant atom, Reactant PID}

spawn_reactant(Reactant) ->
  case Reactant of
    ch3oh -> {ch3oh, spawn(fun() -> ch3oh() end)};
    ch2oh -> {ch2oh, spawn(fun() -> ch2oh() end)};
    coh -> {coh, spawn(fun() -> coh() end)};
    ch3 -> {ch3, spawn(fun() -> ch3() end)};
    h2 -> {h2, spawn(fun() -> h2() end)};
    h -> {h, spawn(fun() -> h() end)};
    o2 -> {o2, spawn(fun() -> o2() end)};
    o -> {o, spawn(fun() -> o() end)};
    oh -> {oh, spawn(fun() -> oh() end)};
    c -> {c, spawn(fun() -> c() end)};
    co -> {co, spawn(fun() -> co() end)}
  end.

% -----------------------------------------------------------------------
-spec spawn_reactants([atom()]) -> [reactant()].
% @doc Spawn a list of reactants recursively
% @param Elements A list of reactants to spawn
% @returns A list of reactant processes, in the form {Reactant atom, Reactant PID}

spawn_reactants([]) -> [];

spawn_reactants([Reactant]) -> [spawn_reactant(Reactant)];

spawn_reactants([H | T]) -> [spawn_reactant(H) | spawn_reactants(T)].

% -----------------------------------------------------------------------
-spec remove_reactants(reactant(), reactant(), [reactant()]) -> [reactant()].
% @doc Remove X and Y from a list of reactants
% @param X A reactant
% @param Y A reactant
% @param Reactants A list of reactants
% @returns A new list of updated reactants

remove_reactants(R1, R2, Reactants) ->
  [Reactant || Reactant <- Reactants, Reactant =/= R1, Reactant =/= R2].

% -----------------------------------------------------------------------
-spec show_reactants([reactant()]) -> none().
% @doc Shows the reactants left
% @param A list of reactants left

show_reactants(Reactants) ->
  io:format("~nReactants left: ~p~n", [[R || {R, _} <- Reactants]]).

% -----------------------------------------------------------------------
-spec find_reaction([reactant()]) -> none().
% @doc Find possible reaction in a list of reactants
% @param A list of reactants
% @returns A randomly chosen reaction

find_reaction([]) -> [];

find_reaction([{R1, R1_Pid} | Reactants]) ->
  % find snd, rcv pair that sync on the same action
  Reactions = [{{{R1, R1_Pid}, R1_Comm}, {{R2, R2_Pid}, R2_Comm}, R1_Action} ||
                {R2, R2_Pid} <- Reactants,
                {R1_Comm, R1_Action} <- get_rule(R1),
                {R2_Comm, R2_Action} <- get_rule(R2),
                R1_Comm =/= R2_Comm,
                R1_Action =:= R2_Action],
  case Reactions of
    [] -> find_reaction(Reactants);
    [Reaction] -> Reaction;
    [_ | _] ->
      Index = rand:uniform(length(Reactions)),
      lists:nth(Index, Reactions)
  end.

% -----------------------------------------------------------------------
-spec perform_reaction(reaction()) -> none().
% @doc Perform a reaction
% @param A reaction

perform_reaction({{{R1, R1_Pid}, R1_Comm}, {{R2, R2_Pid}, R2_Comm}, Action}) ->
  case {R1_Comm, R2_Comm} of
    {snd, rcv} ->
      % Reactant 1 sends Action to Reactant 2
      R1_Pid ! {snd, Action, R2_Pid},
      io:format("~p --~p--> ~p~n", [R1, Action, R2]);
    {rcv, snd} ->
      % Reactant 2 sends Action to Reactant 1
      R2_Pid ! {snd, Action, R1_Pid},
      io:format("~p --~p--> ~p~n", [R2, Action, R1])
  end.

% -----------------------------------------------------------------------
-spec start_reaction([{atom(), pid()}]) -> none().
% @doc
% @param Reactants The list of reactants

start_reaction(Reactants) ->
  % Show traces
  show_reactants(Reactants),

  Reaction = find_reaction(Reactants),
  case Reaction of
    % No possible reactions left, end and show results
    [] -> whereis(result) ! {finished, Reactants};

    % Randomly chosen reaction returned
    {{R1, _}, {R2, _}, _} ->
      perform_reaction(Reaction),

      % Remove the two reactans reacted after the reaction
      NewReactants = remove_reactants(R1, R2, Reactants),

      % Wait until the reaction has finished, then repeat again
      receive
        NewProcesses ->
          start_reaction(lists:merge(NewProcesses, NewReactants))
      end
  end.

% -----------------------------------------------------------------------
-spec show_result(integer(), integer()) -> none().
% @doc Show the final results when
% @param CO2 The number of CO2 produced
% @param H2O The number of H2O produced

show_result(CO2, H2O) ->
  receive
    {finished, []} ->
      io:format("Final products: ~pCO2 + ~pH2O~n", [CO2, H2O]),
      io:format("Status        : Complete combustion~n");
    {finished, Reactants} ->
      io:format("Final products: ~pCO2 + ~pH2O", [CO2, H2O]),
      count_reactants_left(Reactants),
      show_status(Reactants);
    co2 ->
      show_result(CO2 + 1, H2O);
    h2o ->
      show_result(CO2, H2O + 1)
  end.

% -----------------------------------------------------------------------
-spec count_reactants_left([reactant()]) -> none().
% @doc Count the number of each reactant left
% @param The list of reactants left

count_reactants_left(Reactants) ->
  % No duplicate reactant
  ReactantsAtom = [R || {R, _} <- Reactants],
  NoDup = sets:to_list(sets:from_list(ReactantsAtom)),
  count_reactants_left(NoDup, ReactantsAtom).

count_reactants_left([], _) -> io:format("~n");

count_reactants_left([H | T], Reactants) ->
  count_reactant_in_list(H, Reactants),
  count_reactants_left(T, Reactants).

-spec count_reactant_in_list(atom(), [reactant()]) -> none().
% @doc Count the number of the given reactant in the list of reactants left
% @param Reactant The reactant to count
% @param Reactants The list of reactants left

count_reactant_in_list(Reactant, Reactants) ->
  Count = length([1 || R <- Reactants, R =:= Reactant]),
  io:format(" + ~p~s", [Count, string:uppercase(atom_to_list(Reactant))]).

% -----------------------------------------------------------------------
-spec show_status([reactant()]) -> none().
% @doc Show the final reaction status
% @param Reactants The list of reactants left

show_status(Reactants) ->
  ReactantsAtom = [R || {R, _} <- Reactants],
  HasOxide = lists:member(o, ReactantsAtom) or lists:member(o2, ReactantsAtom),
  if
    HasOxide  ->
      io:format("Status        : Run out of CH3OH, excess O2 provided~n");
    true ->
      io:format("Status        : Run out of O2~n")
  end.

% -----------------------------------------------------------------------

ch3oh() ->
  receive
    {snd, c, To} ->
      To ! {rcv, c, ch3oh, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([h2, h, oh] ++ R)
      end;

    {snd, h, To} ->
      To ! {rcv, h, ch3oh, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([ch2oh | R])
      end;

    {snd, oh, To} ->
      To ! {rcv, oh, ch3oh, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([ch3 | R])
      end
  end.

ch2oh() ->
  receive
    {snd, c, To} ->
      To ! {rcv, c, ch2oh, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([h, h, oh] ++ R)
      end;

    {snd, h, To} ->
      To ! {rcv, h, ch2oh, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([coh, h] ++ R)
      end;

    {snd, oh, To} ->
      To ! {rcv, oh, ch2oh, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([c, h2] ++ R)
      end
  end.

coh() ->
  receive
    {snd, c, To} ->
      To ! {rcv, c, coh, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([oh | R])
      end;

    {snd, oh, To} ->
      To ! {rcv, oh, coh, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([c | R])
      end
  end.

ch3() ->
  receive
    {snd, c, To} ->
      To ! {rcv, c, ch3, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([h2, h] ++ R)
      end;

    {snd, h, To} ->
      To ! {rcv, h, ch3, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([c, h2] ++ R)
      end
  end.

h2() ->
  receive
    {rcv, o, From, Pid} ->
      io:format("h2 <--o-- ~p~n", [From]),
      h2o(),
      Pid ! {ok, []};

    {snd, h, To} ->
      To ! {rcv, h, h2, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([h | R])
      end
  end.

h() ->
  receive
    {rcv, oh, From, Pid} ->
      io:format("h <--oh-- ~p~n", [From]),
      h2o(),
      Pid ! {ok, []};

    {snd, h, To} ->
      To ! {rcv, h, h, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants(R)
      end
  end.

o2() ->
  receive
    {rcv, c, From, Pid} ->
      io:format("o2 <--c-- ~p~n", [From]),
      co2(),
      Pid ! {ok, []};

    {snd, o, To} ->
      To ! {rcv, o, o2, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([o | R])
      end
  end.

o() ->
  receive
    {rcv, h, From, Pid} ->
      io:format("o <--h-- ~p~n", [From]),
      Pid ! {ok, [oh]};

    {snd, o, To} ->
      To ! {rcv, o, o, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants(R)
      end
  end.

oh() ->
  receive
    {rcv, h, From, Pid} ->
      io:format("oh <--h-- ~p~n", [From]),
      h2o(),
      Pid ! {ok, []};

    {snd, o, To} ->
      To ! {rcv, o, oh, self()},
      receive
        {ok, R} -> whereis(start) ! spawn_reactants([h | R])
      end
  end.

c() ->
  receive
    {rcv, o, From, Pid} ->
      io:format("c <--o-- ~p~n", [From]),
      Pid ! {ok, [co]}
  end.

co() ->
  receive
    {rcv, o, From, Pid} ->
      io:format("co <--o-- ~p~n", [From]),
      co2(),
      Pid ! {ok, []}
  end.

h2o() -> whereis(result) ! h2o.

co2() -> whereis(result) ! co2.

% -----------------------------------------------------------------------
-spec react(integer(), integer()) -> none().
% @doc Starts the combustion of Methanol
% @param Methanol The number of CH3OH molecules
% @param Oxygen The number of O2 molecules

react(0, 0) ->
  io:format("No CH3OH provided~n"),
  io:format("No O2 provided~n");

react(0, _) ->
  io:format("No CH3OH provided~n");

react(_, 0) ->
  io:format("No O2 provided~n");

react(Methanol, Oxygen) ->
  Reactants = lists:merge(
    [spawn_reactant(ch3oh) || _ <- lists:seq(1, Methanol)],
    [spawn_reactant(o2) || _ <- lists:seq(1, Oxygen)]
  ),
  register(start, spawn(fun() -> start_reaction(Reactants) end)),
  register(result, spawn(fun() -> show_result(0, 0) end)).
