defmodule ProdutorConsumidor.Application do
  use Application

  def start(_type, _args) do
    children = [
      {ProdutorConsumidor, []},
      {Registry, keys: :unique, name: Consumidor.Registry},
      Supervisor.child_spec({Consumidor, 1}, id: :consumidor_1),
      Supervisor.child_spec({Consumidor, 2}, id: :consumidor_2),
      Supervisor.child_spec({Consumidor, 3}, id: :consumidor_3),
      Supervisor.child_spec({Consumidor, 4}, id: :consumidor_4)
    ]

    opts = [strategy: :one_for_one, name: ProdutorConsumidor.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
