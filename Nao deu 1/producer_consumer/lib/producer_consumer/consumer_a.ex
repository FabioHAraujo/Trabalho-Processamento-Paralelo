defmodule ProducerConsumer.ConsumerA do
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
      if type == :type1 do
        Process.sleep(7000)  # Simula tempo de consumo para type1
        Logger.info("#{inspect(self())} ConsumerA consumed item of type1")
      end
    end
    {:noreply, [], state}
  end
end
