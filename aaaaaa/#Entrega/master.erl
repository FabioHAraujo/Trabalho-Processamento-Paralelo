-module(master).
-export([init/0]).

init() ->
  {ok, [Num_Prod]} = io:fread("Digite o número de produtores: ", "~d"),
  {ok, [Num_Cons]} = io:fread("Digite o número de consumidores: ", "~d"),
  Buffer = spawn(buffer, init, []),
  start_prod(1, Num_Prod, Buffer),
  start_cons(1, Num_Cons, Buffer).

start_prod(Prod_Id, Num_Prod, Buffer) when Prod_Id =< Num_Prod ->
  spawn(producer, init, [Buffer, Prod_Id]),
  start_prod(Prod_Id + 1, Num_Prod, Buffer);

start_prod(_, _, _) ->
  ok.

start_cons(Cons_Id, Num_Cons, Buffer) when Cons_Id =< Num_Cons ->
  spawn(consumer, init, [Buffer, Cons_Id]),
  start_cons(Cons_Id + 1, Num_Cons, Buffer);

start_cons(_, _, _) ->
  ok.
