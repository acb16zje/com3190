-module(farm_test).

-import([farm/0,submit/2,report/0]).
-import([worker/0,farmer/2]).

-include_lib("eunit/include/eunit.hrl").

expect(Val) ->
  receive
    Val -> ok;
    Other -> {error, Other}
  end.

