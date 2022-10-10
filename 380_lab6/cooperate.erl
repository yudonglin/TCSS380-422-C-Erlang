%%%-------------------------------------------------------------------
%%% @author Yudong Lin & Wei Wei Chien & Michael Theisen
%%%-------------------------------------------------------------------

-module(cooperate).
-compile(export_all).
-import(rand, [uniform/1]).

go() ->
  PidConsumer = spawn(cooperate, consumer, []),
  PidProducer = spawn(cooperate, producer, [PidConsumer, 5]),
  register(consumerPid, PidConsumer),
  register(producerPid, PidProducer).

producer(Pid, N) ->
  if
    N > 0 ->
      RandNum = rand:uniform(100),
      io:format("process ~w producing ~w~n", [self(), RandNum]),
      Pid ! RandNum,
      producer(Pid, N - 1);
    true -> ok
  end.

consumer() ->
  timer:sleep(10),
  receive
    Num ->
      io:format("process ~w consuming ~w~n", [self(), Num]),
      consumer()
  after 40 ->
    io:format("process ~w has been terminated~n", [self()])
  end.