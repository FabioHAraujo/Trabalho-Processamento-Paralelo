defmodule ProducerConsumer.ConsumerB do
  use GenStage

  require Logger

  def start_link(opts) do
    GenStage.start_link(__MODULE__, :ok, opts)
  end

  def init(:ok) do
    {:consumer, :ok}
  end

  def handle_events(events, _from, state) do
    for {:item, type} <- events do
      if type == :type2 do
        Process.sleep(7000)  # Simula tempo de consumo para type2
        Logger.info("#{inspect(self())} ConsumerB consumed item of type2")
      end
    end
    {:noreply, [], state}
  end
end
