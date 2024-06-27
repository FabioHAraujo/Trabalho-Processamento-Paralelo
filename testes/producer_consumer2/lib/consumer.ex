defmodule ProducerConsumer.Consumer do
  use GenServer

  def start_link(consumer_id) do
    GenServer.start_link(__MODULE__, consumer_id, name: via_tuple(consumer_id))
  end

  defp via_tuple(consumer_id) do
    {:via, Registry, {ProducerConsumer.Registry, consumer_id}}
  end

  def init(consumer_id) do
    Registry.register(ProducerConsumer.Registry, consumer_id, self())
    schedule_consume()
    {:ok, consumer_id}
  end

  def handle_info(:consume, consumer_id) do
    case ProducerConsumer.Cache.get_item() do
      {:ok, item} ->
        consume_item(item)
        schedule_consume()
      :empty ->
        schedule_consume()
    end
    {:noreply, consumer_id}
  end

  defp schedule_consume do
    Process.send_after(self(), :consume, 1000)  # Ajuste o intervalo conforme necessário
  end

  defp consume_item(%{tipo: :type1} = item) do
    Process.sleep(7000)  # Consumo de 7 segundos para tipo1
    IO.puts("Item consumido pelo consumidor #{self()}: #{inspect(item)}")
  end

  defp consume_item(%{tipo: :type2} = item) do
    Process.sleep(15000)  # Consumo de 15 segundos para tipo2
    IO.puts("Item consumido pelo consumidor #{self()}: #{inspect(item)}")
  end
end
