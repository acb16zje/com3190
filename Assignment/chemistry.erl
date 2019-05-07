% @author Zer Jun Eng
-module(chemistry).
-export([react/2, get_rule/1]).

-type element() :: {atom(), pid()}.
-type reaction() :: {{element(), atom()}, {element(), atom()}, atom()}.

% -----------------------------------------------------------------------
-spec get_rule(atom()) -> list().
% @doc Get the reaction rule of a given element
% @param Elem The Element
% @returns The reaction rule of the given element

get_rule(Elem) ->
  Rules = #{
    ch3oh => [{snd, c}, {snd, h}, {snd, oh}],
    ch2oh => [{snd, c}, {snd, h}, {snd, oh}],
    coh => [{snd, c}, {snd, oh}],
    ch3 => [{snd, c}, {snd, h}],
    h2 => [{rcv, o}, {snd, h}],
    h => [{rcv, oh}, {snd, h}],
    o2 => [{rcv, c}, {snd, o}],
    o => [{rcv, h}, {snd, o}],
    oh => [{rcv, h}, {snd, o}, {snd, h}],
    c => [{rcv, o}],
    co => [{rcv, o}]
  },
  maps:get(Elem, Rules).

% -----------------------------------------------------------------------
-spec spawn_element(atom()) -> element().
% @doc Spawn a process of the given element
% @param Elem The element to spawn
% @returns {Element name atom, Element PID}

spawn_element(Elem) ->
  case Elem of
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
-spec spawn_elements([atom()]) -> [element()].
% @doc Spawn a list of elements recursively
% @param Elements A list of elements to spawn
% @returns A list of element processes, in the form {Element name atom, Element PID}

spawn_elements([]) -> [];

spawn_elements([Elem]) -> [spawn_element(Elem)];

spawn_elements([Elem | Elements]) -> [spawn_element(Elem) | spawn_elements(Elements)].

% -----------------------------------------------------------------------
-spec remove_elements(element(), element(), [element()]) -> [element()].
% @doc
% @param
% @returns

remove_elements(ElemX, ElemY, Elements) ->
  [Elem || Elem <- Elements, Elem =/= ElemX, Elem =/= ElemY].

% -----------------------------------------------------------------------
-spec show_elements([element()]) -> none().
% @doc
% @param
% @returns

show_elements(Elements) ->
  io:format("~nReactants left: ~p~n", [[Atom || {Atom, _} <- Elements]]).

% -----------------------------------------------------------------------
-spec find_reaction([element()]) -> none().
% @doc
% @param
% @returns

find_reaction([]) -> [];

find_reaction([{ElemX, PidX} | Elems]) ->
  Reactions = [{{{ElemX, PidX}, CommX}, {{ElemY, PidY}, CommY}, AtomX} ||
                {ElemY, PidY} <- Elems,
                {CommX, AtomX} <- get_rule(ElemX),
                {CommY, AtomY} <- get_rule(ElemY),
                CommX =/= CommY,
                AtomX =:= AtomY],
  case Reactions of
    [] -> find_reaction(Elems);
    [Reaction] -> Reaction;
    [_ | _] ->
      Index = rand:uniform(length(Reactions)),
      lists:nth(Index, Reactions)
  end.

% -----------------------------------------------------------------------
-spec perform_reaction(reaction()) -> none().
% @doc
% @param
% @returns

perform_reaction({{{ElemX, PidX}, CommX}, {{ElemY, PidY}, CommY}, Atom}) ->
  case {CommX, CommY} of
    {snd, rcv} ->
      PidX ! {snd, Atom, PidY},
      io:format("~p --~p--> ~p~n", [ElemX, Atom, ElemY]);
    {rcv, snd} ->
      PidY ! {snd, Atom, PidX},
      io:format("~p --~p--> ~p~n", [ElemY, Atom, ElemX])
  end.

% -----------------------------------------------------------------------
-spec start_reaction([{atom(), pid()}]) -> none().
% @doc
% @param
% @returns

start_reaction(Elements) ->
  show_elements(Elements),
  Reaction = find_reaction(Elements),

  case Reaction of
    [] ->
      whereis(result) ! {finished, Elements};

    {{ElemX, _}, {ElemY, _}, _} ->
      perform_reaction(Reaction),
      NewElements = remove_elements(ElemX, ElemY, Elements),

      receive
        NewProcs ->
          start_reaction(lists:merge(NewProcs, NewElements))
      end
  end.

% -----------------------------------------------------------------------
-spec show_result(integer(), integer()) -> none().
% @doc
% @param
% @returns

show_result(CO2, H2O) ->
  receive
    {finished, []} ->
      io:format("Final products: ~pCO2 + ~pH2O~n", [CO2, H2O]),
      io:format("Status        : Complete combustion~n");
    {finished, Elements} ->
      io:format("Final products: ~pCO2 + ~pH2O~n", [CO2, H2O]),
      io:format("Status        : Incomplete combustion~n");
    co2 ->
      show_result(CO2 + 1, H2O);
    h2o ->
      show_result(CO2, H2O + 1)
  end.

% -----------------------------------------------------------------------

ch3oh() ->
  receive
    {snd, c, To} ->
      To ! {rcv, c, ch3oh, self()},
      receive
        {ok, E} -> whereis(start) ! spawn_elements([h2, h, oh] ++ E)
      end;

    {snd, h, To} ->
      To ! {rcv, h, ch3oh, self()},
      receive
        {ok, E} -> whereis(start) ! spawn_elements([ch2oh | E])
      end;

    {snd, oh, To} ->
      To ! {rcv, oh, ch3oh, self()},
      receive
        {ok, E} -> whereis(start) ! spawn_elements([ch3 | E])
      end
  end.

ch2oh() ->
  receive
    {snd, c, To} ->
      To ! {rcv, c, ch2oh, self()},
      receive
        {ok, E} -> whereis(start) ! spawn_elements([h, h, oh] ++ E)
      end;

    {snd, h, To} ->
      To ! {rcv, h, ch2oh, self()},
      receive
        {ok, E} -> whereis(start) ! spawn_elements([coh, h] ++ E)
      end;

    {snd, oh, To} ->
      To ! {rcv, oh, ch2oh, self()},
      receive
        {ok, E} -> whereis(start) ! spawn_elements([c, h2] ++ E)
      end
  end.

coh() ->
  receive
    {snd, c, To} ->
      To ! {rcv, c, coh, self()},
      receive
        {ok, E} -> whereis(start) ! spawn_elements([oh | E])
      end;

    {snd, oh, To} ->
      To ! {rcv, oh, coh, self()},
      receive
        {ok, E} -> whereis(start) ! spawn_elements([c | E])
      end
  end.

ch3() ->
  receive
    {snd, c, To} ->
      To ! {rcv, c, ch3, self()},
      receive
        {ok, E} -> whereis(start) ! spawn_elements([h2, h] ++ E)
      end;

    {snd, h, To} ->
      To ! {rcv, h, ch3, self()},
      receive
        {ok, E} -> whereis(start) ! spawn_elements([c, h2] ++ E)
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
        {ok, E} -> whereis(start) ! spawn_elements([h | E])
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
        {ok, E} -> whereis(start) ! spawn_elements(E)
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
        {ok, E} -> whereis(start) ! spawn_elements([o | E])
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
        {ok, E} -> whereis(start) ! spawn_elements(E)
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
        {ok, E} -> whereis(start) ! spawn_elements([h | E])
      end;

    {snd, h, To} ->
      To ! {rcv, h, oh, self()},
      receive
        {ok, E} -> whereis(start) ! spawn_elements([o | E])
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
% @doc
% @param
% @returns

react(0, 0) ->
  io:format("No CH3OH provided~n"),
  io:format("No O2 provided~n");

react(0, _) ->
  io:format("No CH3OH provided~n");

react(_, 0) ->
  io:format("No O2 provided~n");

react(Methanol, Oxygen) ->
  Elements = lists:merge(
    [spawn_element(ch3oh) || _ <- lists:seq(1, Methanol)],
    [spawn_element(o2) || _ <- lists:seq(1, Oxygen)]
  ),
  register(start, spawn(fun() -> start_reaction(Elements) end)),
  register(result, spawn(fun() -> show_result(0, 0) end)).
