-module(chemistry_test).
-import(chemistry, [
  get_rule/1,
  spawn_reactant/1,
  spawn_reactants/1,
  remove_reactants/3,
  find_reaction/1,
  ch3oh/0,
  c/0]
).

-include_lib("eunit/include/eunit.hrl").

get_rule_test_() -> [
  ?_assertError(_, get_rule(wrong)),
  ?_assertEqual([{snd, c}, {snd, h}, {snd, oh}], get_rule(ch3oh)),
  ?_assertEqual([{snd, c}, {snd, h}, {snd, oh}], get_rule(ch2oh)),
  ?_assertEqual([{snd, c}, {snd, oh}], get_rule(coh)),
  ?_assertEqual([{snd, c}, {snd, h}], get_rule(ch3)),
  ?_assertEqual([{rcv, o}, {snd, h}], get_rule(h2)),
  ?_assertEqual([{rcv, oh}, {snd, h}], get_rule(h)),
  ?_assertEqual([{rcv, c}, {snd, o}], get_rule(o2)),
  ?_assertEqual([{rcv, h}, {snd, o}], get_rule(o)),
  ?_assertEqual([{rcv, h}, {snd, o}], get_rule(oh)),
  ?_assertEqual([{rcv, o}], get_rule(c)),
  ?_assertEqual([{rcv, o}], get_rule(co))
].

spawn_reactant_test_() -> [
  ?_assertError(_, spawn_reactant(wrong)),
  ?_assertMatch({ch3oh, _}, spawn_reactant(ch3oh)),
  ?_assertMatch({ch2oh, _}, spawn_reactant(ch2oh)),
  ?_assertMatch({coh, _}, spawn_reactant(coh)),
  ?_assertMatch({ch3, _}, spawn_reactant(ch3)),
  ?_assertMatch({h2, _}, spawn_reactant(h2)),
  ?_assertMatch({h, _}, spawn_reactant(h)),
  ?_assertMatch({o2, _}, spawn_reactant(o2)),
  ?_assertMatch({o, _}, spawn_reactant(o)),
  ?_assertMatch({oh, _}, spawn_reactant(oh)),
  ?_assertMatch({c, _}, spawn_reactant(c)),
  ?_assertMatch({co, _}, spawn_reactant(co))
].

spawn_reactants_test_() -> [
  ?_assertError(_, spawn_reactants(wrong)),
  ?_assertError(_, spawn_reactants([wrong])),
  ?_assertError(_, spawn_reactants([fail, wrong])),
  ?_assertMatch([], spawn_reactants([])),
  ?_assertMatch([{ch3oh, _}], spawn_reactants([ch3oh])),
  ?_assertMatch([{ch2oh, _}], spawn_reactants([ch2oh])),
  ?_assertMatch([{coh, _}], spawn_reactants([coh])),
  ?_assertMatch([{ch3, _}], spawn_reactants([ch3])),
  ?_assertMatch(
    [{ch3oh, _}, {o2, _}],
    spawn_reactants([ch3oh, o2])
  ),
  ?_assertMatch(
    [{ch3oh, _}, {ch3oh, _}, {o2, _}, {o2, _}],
    spawn_reactants([ch3oh, ch3oh, o2, o2])
  ),
  ?_assertMatch(
    [{h2, _}, {h, _}, {oh, _}],
    spawn_reactants([h2, h, oh])
  ),
  ?_assertMatch([{c, _}, {h2, _}], spawn_reactants([c, h2]))
].

remove_reactants_test() ->
  CH3OH = spawn_reactant(ch3oh),
  O2 = spawn_reactant(o2),
  ?assertEqual(
    [],
    remove_reactants(CH3OH, O2, [CH3OH, O2])
  ),
  ?assertNotEqual(
    [],
    remove_reactants(CH3OH, O2, [CH3OH, O2, spawn_reactant(o2)])
  ).

find_reaction_test() ->
  CH3OH = spawn_reactant(ch3oh),
  O2 = spawn_reactant(o2),
  ?assertEqual(
    {{CH3OH, snd}, {O2, rcv}, c},
    find_reaction([CH3OH, O2])
  ),
  ?assertEqual(
    [],
    find_reaction([CH3OH])
  ),
  ?assertEqual(
    [],
    find_reaction([O2])
  ),
  ?assertEqual(
    [],
    find_reaction([CH3OH, CH3OH])
  ).

ch3oh_snd_c_test() ->
  PID = spawn(fun() -> ch3oh() end),
  PID ! {snd, c, self()},
  ok = expect({rcv, c, ch3oh, PID}).

ch3oh_snd_h_test() ->
  PID = spawn(fun() -> ch3oh() end),
  PID ! {snd, h, self()},
  ok = expect({rcv, h, ch3oh, PID}).

ch3oh_snd_oh_test() ->
  PID = spawn(fun() -> ch3oh() end),
  PID ! {snd, oh, self()},
  ok = expect({rcv, oh, ch3oh, PID}).

c_rcv_o_test() ->
  PID = spawn(fun() -> c() end),
  PID ! {rcv, o, o2, self()},
  ok = expect({ok, [co]}).

expect(Val) ->
  receive
    Val -> ok;
    Other -> {error, Other}
  end.
