defmodule ProducerConsumer.ProducerB do
  use GenStage

  def start_link(_) do
    GenStage.start_link(__MODULE__, :ok, name: :producer_b)
  end

  def init(:ok) do
    {:producer, :ok}
  end

  def handle_demand(demand, state) when demand > 0 do
    Process.sleep(3500)  # Simula tempo de produção
    events = for _ <- 1..demand, do: {:item, :type2}
    {:noreply, events, state}
  end
end
