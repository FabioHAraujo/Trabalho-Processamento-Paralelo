defmodule Consumidor do
  use GenServer

  @consume_times %{tipo1: 7000, tipo2: 15000}

  def start_link(id) do
    GenServer.start_link(__MODULE__, id, name: via_tuple(id))
  end

  def init(id) do
    schedule_consume(id)
    {:ok, id}
  end

  def handle_info({:consume, id}, state) do
    task = Task.async(fn -> consume_item(id) end)
    {:noreply, state, {:continue, {:await_task, task}}}
  end

  def handle_continue({:await_task, task}, state) do
    Task.await(task, :infinity)
    {:noreply, state}
  end

  defp consume_item(id) do
    case GenServer.call(ProdutorConsumidor, :consume, 15000) do
      {:ok, {op, item_type, _producer_name}} ->
        consume_time = @consume_times[item_type]
        IO.puts("Consumidor #{id} consumindo item do tipo #{item_type} | OP: #{op}...")

        # Simulando consumo com :timer.sleep
        :timer.sleep(consume_time)

        IO.puts("Consumido: OP #{op}, Tipo #{item_type}, Consumidor #{id}")

      :empty ->
        IO.puts("Consumidor #{id}: Cache vazio, tentando novamente.")
    end
    schedule_consume(id)
  end

  defp schedule_consume(id) do
    Process.send_after(self(), {:consume, id}, 0)
  end

  defp via_tuple(id), do: {:via, Registry, {Consumidor.Registry, id}}
end
