-module(buffer).
-export([init/0, listen/4, output/1, getItemId/1]).

init() ->
  io:format("Buffer criado~n"),
  MaxSize = 40,
  BufferArray = [],
  ItemId_Prod = 1,
  listen(MaxSize, BufferArray, ItemId_Prod, 0).

listen(MaxSize, BufferArray, ItemId_Prod, Type) ->
  receive
    {available, From, Item, Prod_Id, ItemType} ->
      case length(BufferArray) < MaxSize of
        true ->
          Item ! {assignId, ItemId_Prod},
          NewBufferArray = BufferArray ++ [{Item, ItemType}],  % Adiciona item e tipo ao buffer
          From ! putItem,
          io:format("Produtor ~w inseriu o item ~w do tipo ~w. Há ~w itens no buffer.~n", [Prod_Id, ItemId_Prod, ItemType, length(NewBufferArray)]),
          output(NewBufferArray),
          listen(MaxSize, NewBufferArray, ItemId_Prod + 1, Type);
        false ->
          From ! full,
          listen(MaxSize, BufferArray, ItemId_Prod, Type)
      end;
    {occupied, From, Cons_Id} ->
      case BufferArray of
        [{Item, ItemType} | NewBufferArray] when length(BufferArray) > 0 ->
          ItemId_Cons = getItemId(Item),
          From ! {removeItem, ItemId_Cons, ItemType},  % Envia o tipo do item para o consumidor
          io:format("Consumidor ~w solicitou remoção do item ~w. Há ~w itens no buffer.~n", [Cons_Id, ItemId_Cons, length(NewBufferArray)]),
          output(NewBufferArray),
          listen(MaxSize, NewBufferArray, ItemId_Prod, Type);
        [] ->
          From ! empty,
          listen(MaxSize, BufferArray, ItemId_Prod, Type)
      end
  end.

output(Buffer) ->
  io:format("Buffer: ["),
  fnl(Buffer).

fnl([{Item, ItemType}]) ->
  io:format("{OP: ~w, Tipo: ~w}]~n", [getItemId(Item), ItemType]);
fnl([{Item, ItemType} | T]) ->
  io:format("{OP: ~w, Tipo: ~w}, ", [getItemId(Item), ItemType]),
  fnl(T);
fnl([]) ->
  io:format("]~n").

getItemId(Item) ->
  Item ! {self(), getId},
  receive
    {itemid, Id} ->
      Id
  end.
