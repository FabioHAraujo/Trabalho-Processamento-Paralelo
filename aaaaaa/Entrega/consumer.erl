-module(consumer).
-export([init/2]).

init(Buffer, Cons_Id) ->
  io:format("Consumidor número ~w criado~n", [Cons_Id]),
  listen(Buffer, Cons_Id).

listen(Buffer, Cons_Id) ->
  Buffer ! {occupied, self(), Cons_Id},
  receive
    {removeItem, ItemId, ItemType} ->
      case ItemType of
        1 ->
          timer:sleep(7000),  % Espera 7 segundos para tipo 1
          io:format("Consumidor ~w removeu o item ~w do tipo ~w.~n", [Cons_Id, ItemId, ItemType]);
        2 ->
          timer:sleep(15000),  % Espera 15 segundos para tipo 2
          io:format("Consumidor ~w removeu o item ~w do tipo ~w.~n", [Cons_Id, ItemId, ItemType])
      end,
      listen(Buffer, Cons_Id);
    empty ->
      io:format("Consumidor ~w tentou remover um item, mas o buffer está vazio. Esperando 10 segundos...~n", [Cons_Id]),
      timer:sleep(10000),  % Espera 10 segundos antes de tentar novamente
      listen(Buffer, Cons_Id)
  end.
