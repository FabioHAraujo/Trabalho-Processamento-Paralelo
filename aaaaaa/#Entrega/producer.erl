-module(producer).
-export([init/2]).

init(Buffer, Prod_Id) ->
  io:format("Produtor ~w criado~n", [Prod_Id]),
  listen(Buffer, Prod_Id).

listen(Buffer, Prod_Id) ->
  ItemType = rand:uniform(2),  % Escolhe aleatoriamente entre 1 e 2

  TempoEspera = case ItemType of
    1 -> 3500;  % Tempo de produção para tipo 1 (3.5 segundos)
    2 -> 7500   % Tempo de produção para tipo 2 (7.5 segundos)
  end,

  timer:sleep(TempoEspera),
  
  Item = spawn(item, init, []),
  Buffer ! {available, self(), Item, Prod_Id, ItemType},
  receive
    full ->
      io:format("Produtor ~w tentou inserir um item, mas o buffer está cheio.~n", [Prod_Id]),
      listen(Buffer, Prod_Id);
    putItem ->
      listen(Buffer, Prod_Id)
  end.
