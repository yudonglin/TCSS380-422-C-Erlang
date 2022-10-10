% Yudong Lin & Wei Wei Chien & Michael Theisen
-module(lab05_tests).
-include_lib("eunit/include/eunit.hrl").

loan_1_test() ->
  ?assertEqual({5, 1, 2}, lab05:consolidateLoan({4, 1, 2}, {1, 1, 1})).

loan_2_test() ->
  ?assertEqual({3, 4, 6}, lab05:consolidateLoan({1, 5, 2}, {2, 4, 6})).

loan_3_test() ->
  ?assertEqual({9, 1, 2}, lab05:consolidateLoan({4, 1, 2}, {5, 2, 2})).

isOlder_1_test() ->
  false = lab05:isOlder({10, 3, 2016}, {10, 3, 2016}).

isOlder_2_test() ->
  true = lab05:isOlder({10, 3, 2016}, {11, 3, 2016}).

isOlder_3_test() ->
  true = lab05:isOlder({10, 1, 2016}, {10, 2, 2016}).

isOlder_4_test() ->
  false = lab05:isOlder({10, 3, 2016}, {11, 2, 2016}).

isOlder_5_test() ->
  true = lab05:isOlder({10, 12, 2015}, {10, 12, 2016}).

isOlder_6_test() ->
  true = lab05:isOlder({11, 12, 2015}, {10, 12, 2016}).

isOlder_7_test() ->
  true = lab05:isOlder({12, 11, 2016}, {12, 10, 2017}).

isOlder_8_test() ->
  true = lab05:isOlder({12, 10, 2016}, {11, 3, 2017}).

scaleEach_1_test() ->
  [] = lab05:scaleEach([], 4).

scaleEach_2_test() ->
  [8, 18, -26] = lab05:scaleEach([4, 9, -13], 2).

scaleEach_3_test() ->
  [15] = lab05:scaleEach([5], 3).

scaleEach_4_test() ->
  [12, 27, 15, 18] = lab05:scaleEach([4, 9, 5, 6], 3).

myMin_1_test() ->
  ?assertEqual(empty, lab05:myMin([])).

myMin_2_test() ->
  ?assertEqual(-99, lab05:myMin([-99])).

myMin_3_test() ->
  ?assertEqual(4, lab05:myMin([4, 9, 5, 6])).

myMin_4_test() ->
  ?assertEqual(-9, lab05:myMin([4, 9, -3, 6, 12, -9])).

myMin_5_test() ->
  ?assertEqual(-3, lab05:myMin([4, 9, -3, 6, 12, -1])).

removeValues_1_test() ->
  ?assertEqual([], lab05:removeValues([], 99)).

removeValues_2_test() ->
  ?assertEqual([], lab05:removeValues([18], 19)).

removeValues_3_test() ->
  ?assertEqual([19], lab05:removeValues([19], 19)).

removeValues_5_test() ->
  ?assertEqual([1, 2], lab05:removeValues([-1, 0, 1, 2, -3], 1)).

calculateSugar_1_test() ->
  ?assertEqual({apple, 100, 0.1}, lab05:calculateSugar({apple, 100, 10})).

calculateSugar_2_test() ->
  ?assertEqual({orange, 100, 0.0}, lab05:calculateSugar({orange, 100, 0})).

calculateSugar_3_test() ->
  ?assertEqual({raspberry, -100, -0.1}, lab05:calculateSugar({raspberry, -100, 10})).

calculateSugar_4_test() ->
  ?assertEqual({watermelon, -100, 0.1}, lab05:calculateSugar({watermelon, -100, -10})).

generate_1_test() ->
  ?assertEqual([], lab05:generate(1, 5, 0)).

generate_2_test() ->
  ?assertEqual([1, 2, 3, 4, 5], lab05:generate(1, 5, 1)).

generate_3_test() ->
  ?assertEqual([], lab05:generate(15, 2, 1)).

generate_4_test() ->
  ?assertEqual([3, 5, 7], lab05:generate(3, 8, 2)).

generate_5_test() ->
  ?assertEqual([], lab05:generate(3, 8, -2)).

getnth_1_test() -> {error, no_such_element} = lab05:getnth([], 2).
getnth_2_test() -> {error, no_such_element} = lab05:getnth(["hello", "there"], 3).
getnth_3_test() -> {error, no_such_element} = lab05:getnth(["hello", "there"], 0).
getnth_4_test() -> "there" = lab05:getnth(["hello", "there"], 2).
getnth_5_test() -> "there" = lab05:getnth(["hello", "there", "where"], 2).
getnth_6_test() -> "where" = lab05:getnth(["hello", "there", "where"], 3).
getnth_7_test() -> "where" = lab05:getnth(["hello", "there", "where", "here"], 3).

repeat_1_test() -> [2, 2, 2, 2, 3] = lab05:repeat([1, 2, 3], [0, 4, 1]).
repeat_2_test() -> [] = lab05:repeat([], [0, 4, 1]).
repeat_3_test() -> [] = lab05:repeat([1, 2, 3], []).
repeat_4_test() -> [4, 4] = lab05:repeat([4, 5, 6], [2]).