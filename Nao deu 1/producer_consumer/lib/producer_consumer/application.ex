defmodule ProducerConsumer.Application do
  use Application

  def start(_type, _args) do
    children = [
      {ProducerConsumer.ProducerA, []},
      {ProducerConsumer.ProducerB, []},
      Supervisor.child_spec({ProducerConsumer.ConsumerA, []}, id: :consumer_a1),
      Supervisor.child_spec({ProducerConsumer.ConsumerA, []}, id: :consumer_a2),
      Supervisor.child_spec({ProducerConsumer.ConsumerB, []}, id: :consumer_b1),
      Supervisor.child_spec({ProducerConsumer.ConsumerB, []}, id: :consumer_b2)
    ]

    opts = [strategy: :one_for_one, name: ProducerConsumer.Supervisor]
    {:ok, sup} = Supervisor.start_link(children, opts)

    # Set up subscriptions
    GenStage.sync_subscribe(:consumer_a1, to: ProducerConsumer.ProducerA)
    GenStage.sync_subscribe(:consumer_a2, to: ProducerConsumer.ProducerA)
    GenStage.sync_subscribe(:consumer_b1, to: ProducerConsumer.ProducerB)
    GenStage.sync_subscribe(:consumer_b2, to: ProducerConsumer.ProducerB)

    {:ok, sup}
  end
end
