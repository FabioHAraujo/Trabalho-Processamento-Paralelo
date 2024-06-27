-module(run).
-compile([producer_consumer]).  % Compila o módulo producer_consumer.erl

-export([start/0]).

start() ->
    producer_consumer:start().
