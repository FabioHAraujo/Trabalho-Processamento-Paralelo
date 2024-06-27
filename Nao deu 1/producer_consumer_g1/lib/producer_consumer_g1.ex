defmodule Producer do
  use GenStage

  def start_link(id) do
    GenStage.start_link(__MODULE__, id) # Passa o ID como estado inicial
  end

  def init(id) do
    {:producer, %{counter: 0, item_type: :tipo1, id: id}}
  end

  def handle_demand(_demand, %{counter: counter, item_type: item_type, id: id} = state) do
    item = {item_type, "#{item_type} #{counter}"}
    IO.puts("Produtor #{id} produziu #{item}") # Exibe mensagem de produção

    new_state = %{
      state
      | counter: counter + 1,
        item_type: (if item_type == :tipo1, do: :tipo2, else: :tipo1)
    }
    production_time = if item_type == :tipo1, do: 3500, else: 7500
    Process.sleep(production_time)
    {:noreply, [item], new_state}
  end
end

defmodule Consumer do
  use GenStage

  def start_link(id) do
    GenStage.start_link(__MODULE__, id) # Passa o ID como estado inicial
  end

  def init(id) do
    {:consumer, %{id: id}}
  end

  def handle_events(events, _from, %{id: id} = state) do
    for {type, item} <- events do
      IO.puts("Consumidor #{id} consumiu #{item}") # Exibe mensagem de consumo
      consumption_time = if type == :tipo1, do: 7000, else: 15000
      Process.sleep(consumption_time)
    end
    {:noreply, [], state}
  end
end

# Inicia os produtores com IDs
produtores = Enum.map(1..2, fn id -> Producer.start_link(id) end)
# Mapeia os IDs para os PIDs dos produtores
produtores_pids = Enum.map(produtores, fn {:ok, pid} -> pid end)

# Inicia os consumidores com IDs
consumers = Enum.map(1..4, fn id -> Consumer.start_link(id) end)
# Mapeia os IDs para os PIDs dos consumidores
consumers_pids = Enum.map(consumers, fn {:ok, pid} -> pid end)

Enum.each(consumers_pids, fn consumer_pid ->
  # Escolhe um produtor aleatório para cada consumidor
  producer_pid = Enum.random(produtores_pids)
  GenStage.sync_subscribe(consumer_pid, to: producer_pid)
end)

Process.sleep(30000) # Aguarda 30 segundos antes de finalizar
