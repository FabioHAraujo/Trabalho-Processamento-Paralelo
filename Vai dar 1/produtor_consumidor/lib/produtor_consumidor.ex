defmodule ProdutorConsumidor do
  use GenServer

  @item_types [:tipo1, :tipo2]
  @max_cache_size 200

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{
      cache: :queue.new(),
      next_op: 1
    }, name: __MODULE__)
  end

  def init(state) do
    schedule_producers()
    {:ok, state}
  end

  def handle_info({:produce, producer_name}, state) do
    task = Task.async(fn -> produce_item(producer_name, state) end)
    {:noreply, state, {:continue, {:await_task, task}}}
  end

  def handle_continue({:await_task, task}, _state) do
    {_item, new_state} = Task.await(task, :infinity)
    IO.inspect(new_state.cache, label: "Cache atual")
    {:noreply, new_state}
  end

  defp produce_item(producer_name, state) do
    {item_type, production_time} = random_item()
    IO.puts("Produtor #{producer_name} produzindo item do tipo #{item_type}...")

    # Simulando produção com :timer.sleep
    :timer.sleep(production_time)

    new_op = state.next_op
    item = {new_op, item_type, producer_name}
    new_cache = :queue.in(item, state.cache)

    IO.puts("Produzido: OP #{new_op}, Tipo #{item_type}, Produtor #{producer_name}")

    new_state = %{
      state
      | cache: limit_cache_size(new_cache),
        next_op: new_op + 1
    }

    schedule_producers()  # Corrigido para chamar a função correta
    {item, new_state}
  end

  defp schedule_producers do
    Enum.each([:produtor1, :produtor2], fn producer_name ->
      Process.send_after(self(), {:produce, producer_name}, random_production_time())
    end)
  end

  defp random_production_time do
    :rand.uniform(8000) + 2000
  end

  defp random_item do
    item_type = Enum.random(@item_types)
    production_time = if item_type == :tipo1, do: 3500, else: 7500
    {item_type, production_time}
  end

  defp limit_cache_size(cache) do
    if :queue.len(cache) > @max_cache_size do
      :queue.drop(cache)
    else
      cache
    end
  end

  def consume do
    GenServer.call(__MODULE__, :consume)
  end

  def handle_call(:consume, _from, state) do
    case :queue.out(state.cache) do
      {{:value, item}, new_cache} ->
        {:reply, {:ok, item}, %{state | cache: new_cache}}

      {:empty, _} ->
        {:reply, :empty, state}
    end
  end
end
