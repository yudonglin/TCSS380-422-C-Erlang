% Yudong Lin & Wei Wei Chien & Michael Theisen
-module(lab05).
-compile(export_all).

%Function consolidateLoan that that takes two tuples, where each tuple represents a laon object (each
%tuple consists of 3 values: balance, APR, length – in this order) and calculates and returns a new tuple
%that represents a new consolidated loan, constructed using the same logic as function consolidate in
%lab 3. Assume valid input (no need to error check on the parameter values to this function. (Hint: to
%pattern match a function on a tuple consisting of 3 elements, you can use a function header like so:
%functionName ( {R, X, Y} ) -> … )
consolidateLoan({BAL1, APR1, LEN1}, {BAL2, APR2, LEN2}) ->
  BAL3 = BAL1 + BAL2,
  if
    APR1 > APR2 andalso LEN1 < LEN2 -> {BAL3, APR2, LEN2};
    APR1 > APR2 -> {BAL3, APR2, LEN1};
    LEN1 < LEN2 -> {BAL3, APR1, LEN2};
    true -> {BAL3, APR1, LEN1}
  end.
%Function isOlder that takes two dates (each date is a 3-int tuple dd, mm, yyyy) and evaluates to true
%or false. It evaluates to true if the first argument is a date that comes before the second argument (e.g.
%30/3/2012 comes before 1/4/2012, hence a person born on 30/3/2012 is older). (If two dates are the
%same, the result is false.). Assume the user of your function will enter valid input. Think about how you
%can use relational operators on entire tuples to simplify processing instead of writing nested ifs
isOlder({D1, M1, Y1}, {D2, M2, Y2}) ->
  if
    {Y1, M1, D1} < {Y2, M2, D2} -> true;
    true -> false
  end.

%Function scaleEach that takes two arguments: a list and a numerical value, and returns the new list
%constructed by applying the second argument as a multiplier to each list element. For example, if the
%original list contains [4, 9, -13] and the second argument to the function is 2, then the resulting list will
%contain [8, 18, -26].
scaleEach([], _) -> [];
scaleEach([X | Xs], Multiplier) ->
  [X * Multiplier | scaleEach(Xs, Multiplier)].

%Function myMin that takes a list and returns its minimum value – use tail recursion to find list minimum
%and return an atom empty_list if a list is empty; you are NOT allowed to use ++ or built-in functions or
%list comprehension; add appropriate test cases to the test file that use assertEqual macro
myMin([]) -> empty;
myMin([EL]) -> EL;
myMin([Head | Tail]) ->
  Temp = myMin(Tail),
  if
    Head =< Temp -> Head;
    true -> Temp
  end.

%Function removeValues that takes a list of numbers and a threshold value, and returns the original list
%with values lower than the threshold removed, e.g. if the original list contains [-1, 0, 1, 2, -3], and the
%threshold is 1, then [1, 2] is returned; Assume all incoming arguments will be valid for this operation
%(no need to error check)
removeValues([], _) -> [];
removeValues([X | Xs], Threshold) ->
  if
    X < Threshold -> removeValues(Xs, Threshold);
    true -> [X | removeValues(Xs, Threshold)]
  end.
%Function calculateSugar that takes a list of tuples, where each tuple is of the form food, grams,
%sugar_grams and returns a list of tuples, where each tuple is of the form food, grams, sugar_ratio,
%where sugar_ratio = sugar_grams / grams
calculateSugar({F, G, S}) -> {F, G, S / G}.

%Function generate – takes three ints as arguments and generates a list of integers from arg1 to arg2
%(inclusive) in increments indicated by arg3, e .g. if arg1 = 3, arg2 = 8, and arg 3 = 2, then the function
%returns [3, 5, 7]. If arg1 > arg2, returns an empty list; assume arg3 will be a valid number
generate(_, _, Arg3) when Arg3 =< 0 -> [];
generate(Arg1, Arg2, _) when Arg1 > Arg2 -> [];
generate(Arg1, Arg2, Arg3) -> [Arg1 | generate(Arg1 + Arg3, Arg2, Arg3)].

%Function getnth that takes a list and an int n and returns the nth element of the list, where the head
%of the list is the 1st element. You are only allowed to use list functions hd and tl in your solution – no
%other built-in functions are allowed. If the list is empty or n is invalid, return a tuple
getnth([], _) -> {error, no_such_element};
getnth([X | _], 1) -> X;
getnth([_ | Xs], N) -> getnth(Xs, N - 1).

%Function repeat that takes a list of integers and a list of nonnegative integers and returns a list that
%repeats the integers in the first list according to the numbers indicated by the second list; add the
%following test cases to the test file – these test files also illustrate the logic of repetition to be followed
repeat(_, []) -> [];
repeat([], _) -> [];
repeat([L1 | L1s], [L2 | L2s]) ->
  if
    L2 > 0 -> [L1 | repeat([L1 | L1s], [L2 - 1 | L2s])];
    true -> repeat(L1s, L2s)
  end.
