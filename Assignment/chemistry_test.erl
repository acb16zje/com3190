-module(chemistry_test).
-import(chemistry, [react/2]).

-include_lib("eunit/include/eunit.hrl").

expect(Val) ->
  receive
    Val -> ok;
    Other -> {error, Other}
  end.
