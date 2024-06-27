defmodule ProducerConsumer.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: :consumer_registry},
      {ProducerConsumer.Cache, []},
      {ProducerConsumer.Producer, []},
      Supervisor.child_spec({ProducerConsumer.Consumer, 1}, id: :consumer1),
      Supervisor.child_spec({ProducerConsumer.Consumer, 2}, id: :consumer2),
      Supervisor.child_spec({ProducerConsumer.Consumer, 3}, id: :consumer3),
      Supervisor.child_spec({ProducerConsumer.Consumer, 4}, id: :consumer4)
    ]

    opts = [strategy: :one_for_one, name: ProducerConsumer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
