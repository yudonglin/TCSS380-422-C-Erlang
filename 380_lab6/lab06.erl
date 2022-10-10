%%%-------------------------------------------------------------------
%%% @author Yudong Lin & Wei Wei Chien & Michael Theisen
%%%-------------------------------------------------------------------

-module(lab06).
-export([eqWrapper/1, pyramidVolume/1, filterReduce/4, isPositive/1, twoMaps/3]).
-import(lists, [filter/2, foldl/3, map/2]).

eqWrapper(Value) ->
  case Value of
    "o" -> fun(Value1, Value2) -> Value1 or Value2 end;
    "a" -> fun(Value1, Value2) -> Value1 and Value2 end
  end.

pyramidVolume(Base) -> fun(Height) -> Base * Height / 3 end.

% lab06:filterReduce(fun(N) -> N > 0 end, fun(X, Prod) -> X * Prod end, 1, [-1, 0, 3, 6]).
filterReduce(F1, F2, Accumulator, ListIn) ->
  lists:foldl(F2, Accumulator, lists:filter(F1, ListIn)).

% lab06:filterReduce(fun lab06:isPositive/1, fun(X, Prod) -> X * Prod end, 1, [-1, 0, 3, 6]).
isPositive(N) -> N > 0.

% lab06:twoMaps(fun(X) -> X + 1 end, fun(X) -> X * 2 end, [1,2,3]).
twoMaps(F1, F2, ListIn) ->
  R1 = lists:map(F2, lists:map(F1, ListIn)),
  R2 = lists:map(F1, lists:map(F2, ListIn)),
  {R1, R2, R1 == R2}.
