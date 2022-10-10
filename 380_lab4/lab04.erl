% Team 19 - Yudong Lin & Wei Wei Chien

-module(lab04).

-export([fibTerm/1, isSame/2, isNegative/1, largest/3, beverageType/1, beverageType2/1, myPower/2, mySum/1, mySeries/3]).

-import(io, [fwrite/2]).

-import(math, [sqrt/2, pow/2]).

fibTerm(N) ->
  trunc((math:pow(1 + math:sqrt(5), N) - math:pow(1 - math:sqrt(5), N)) / (math:pow(2, N) * math:sqrt(5))).

isSame(Val1, Val2) ->
  Val1 =:= Val2.

isNegative(X) ->
  if
    X < 0 -> true;
    X > 0 -> false;
    true -> zero
  end.

%%noinspection ErlangUnresolvedFunction
largest(N1, N2, N3) ->
  io:format("Input numbers: ~p, ~p, ~p~n", [N1, N2, N3]),
  if
    N1 > N2 ->
      if
        N1 > N3 -> N1;
        true -> N3
      end;
    true ->
      if
        N2 > N3 -> N2;
        true -> N3
      end
  end.

beverageType(Index) ->
  case Index of
    1 -> soda;
    2 -> water;
    3 -> juice;
    4 -> shake;
    5 -> smoothie;
    _ -> no_match
  end.

beverageType2(1) -> soda;
beverageType2(2) -> water;
beverageType2(3) -> juice;
beverageType2(4) -> shake;
beverageType2(5) -> smoothie;
beverageType2(_) -> no_match.

myPower(X, N) when N > 0 ->
  X * myPower(X, N - 1);
myPower(X, N) when N < 0 ->
  myPower(X, -N);
myPower(_, 0) -> 1.

mySum(Boundary) when Boundary > 1 ->
  Boundary + mySum(Boundary - 1);
mySum(1) -> 1;
mySum(_) -> 0.

mySeries(FirstTerm, Multiplier, ReturnNum) ->
  trunc(FirstTerm * math:pow(Multiplier, ReturnNum - 1)).
