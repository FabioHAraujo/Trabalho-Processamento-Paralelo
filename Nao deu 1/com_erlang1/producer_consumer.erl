-module(producer_consumer).
-export([start/0, spawn_prod_consumers/2, spawn_producers/2, producer/1, consumer/1]).

start() ->
    Pids = spawn_prod_consumers(2, 4),
    io:format("Sistema de produtor-consumidor iniciado.~n"),
    spawn_producers(2, Pids),
    init_consumers(2, Pids).

spawn_prod_consumers(NumProducers, NumConsumers) ->
    Producers = spawn_processes(producer, NumProducers, []),
    Consumers = spawn_processes(consumer, NumConsumers, []),
    io:format("Iniciando ~B produtores e ~B consumidores.~n", [NumProducers, NumConsumers]),
    Producers ++ Consumers.

spawn_processes(Module, Num, Acc) when Num > 0 ->
    Pid = spawn(?MODULE, Module, [self()]),
    spawn_processes(Module, Num - 1, [Pid | Acc]);
spawn_processes(_, _, Acc) ->
    lists:reverse(Acc).

spawn_producers(NumProducers, ConsumerPids) ->
    lists:foreach(
        fun(Pid) ->
            Pid ! {register_producer, self()},
            io:format("Produtor ~p registrado.~n", [self()])
        end,
        ConsumerPids).

init_consumers(NumConsumers, ConsumerPids) ->
    lists:foreach(
        fun(_Idx) ->
            spawn_consumer(ConsumerPids)
        end,
        lists:seq(1, NumConsumers)).

spawn_consumer(ConsumerPids) ->
    spawn(fun() -> consumer_loop(ConsumerPids) end).

consumer_loop(ConsumerPids) ->
    receive
        {start_producing, Pid} ->
            io:format("Consumidor ~p iniciado.~n", [self()]),
            consume_product(Pid),
            consumer_loop(ConsumerPids)
    end.

consume_product(MainPid) ->
    receive
        {produce, ProductType} ->
            io:format("Consumidor ~p consumindo produto de tipo ~p.~n", [self(), ProductType]),
            timer:sleep(7500),
            MainPid ! {start_producing, self()}
    end.

producer(MainPid) ->
    receive
        {register_producer, Pid} ->
            io:format("Produtor ~p iniciado.~n", [self()]),
            producer_loop(Pid, MainPid)
    end.

producer_loop(Pid, MainPid) ->
    ProductType = random_product(),
    io:format("Produtor ~p produzindo produto de tipo ~p.~n", [self(), ProductType]),
    timer:sleep(3500),
    Pid ! {produce, ProductType},
    producer_loop(Pid, MainPid).

random_product() ->
    rand:uniform(2).  % Retorna 1 ou 2 aleatoriamente, representando os dois tipos de produtos.
