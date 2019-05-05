-module(chemistry).
-author("Zer Jun Eng").
-export([react/2]).

get_rule(Elem) ->
  Rules = #{
    ch3oh => [{snd, c}],
    h2 => [{rcv, o}],
    o2 => [{rcv, c}, {snd, o}],
    o => [{snd, o}]
  },
  maps:get(Elem, Rules).

% -----------------------------------------------------------------------

remove_elements(ElemX, ElemY, Elements) ->
  [Elem || Elem <- Elements, Elem =/= ElemX, Elem =/= ElemY].

show_elements(Elements) ->
  io:format("~nCurrent elements: ~p~n", [[Atom || {Atom, _} <- Elements]]).

% -----------------------------------------------------------------------

find_reaction([]) -> [];

find_reaction([{ElemX, PidX} | Elems]) ->
  Reactions = [{{ElemX, PidX, CommX}, {ElemY, PidY, CommY}, AtomX} ||
                {ElemY, PidY} <- Elems,
                {CommX, AtomX} <- get_rule(ElemX),
                {CommY, AtomY} <- get_rule(ElemY),
                CommX =/= CommY,
                AtomX =:= AtomY],
  case Reactions of
    [] -> find_reaction(Elems);
    [Reaction] -> Reaction;
    [Reaction | _] -> Reaction
  end.

% -----------------------------------------------------------------------

send_reaction({{ElemX, PidX, CommX}, {ElemY, PidY, CommY}, Atom}) ->
  case {CommX, CommY} of
    {snd, rcv} ->
      PidX ! {Atom, PidY},
      io:format("~p --~p--> ~p~n", [ElemX, Atom, ElemY]);
    {rcv, snd} ->
      PidY ! {Atom, PidX},
      io:format("~p --~p--> ~p~n", [ElemY, Atom, ElemX])
  end.

% -----------------------------------------------------------------------

start_reaction(Elements) ->
  show_elements(Elements),
  Reaction = find_reaction(Elements),
  case Reaction of
    [] -> ok;
    {{ElemX, PidX, _}, {ElemY, PidY, _}, _} ->
      send_reaction(Reaction),
      UpdatedElements = remove_elements({ElemX, PidX}, {ElemY, PidY}, Elements),
      receive
        nil -> start_reaction(UpdatedElements);
        NewProc -> start_reaction([NewProc | UpdatedElements])
      end
  end.

% -----------------------------------------------------------------------

ch3oh() ->
  receive
    {c, To} ->
      To ! {c, ch3oh},
      whereis(start) ! {h2, spawn(fun() -> h2() end)},
      h2o()
  end.

h2() ->
  receive
    {o, From} ->
      io:format("h2 <--o-- ~p~n", [From]),
      h2o()
  end.

o2() ->
  receive
    {c, From} ->
      io:format("o2 <--c-- ~p~n", [From]),
      co2();
    {o, To} ->
      To ! {o, o2},
      whereis(start) ! {o, spawn(fun() -> o() end)}
  end.

o() ->
  receive
    {o, To} ->
      To ! {o, o},
      whereis(start) ! nil
  end.

h2o() -> io:format("").

co2() -> io:format("").

% -----------------------------------------------------------------------
react(0, 0) ->
  io:format("No CH3OH provided~n"),
  io:format("No O2 provided~n");
react(0, _) ->
  io:format("No CH3OH provided~n");
react(_, 0) ->
  io:format("No O2 provided~n");
react(Methanol, Oxygen) ->
  Elements = lists:merge(
    [{ch3oh, spawn(fun() -> ch3oh() end)} || _ <- lists:seq(1, Methanol)],
    [{o2, spawn(fun() -> o2() end)} || _ <- lists:seq(1, Oxygen)]
  ),
  register(start, spawn(fun() -> start_reaction(Elements) end)).
