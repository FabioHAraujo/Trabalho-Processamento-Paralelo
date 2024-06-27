defmodule ProducerConsumer.Producer do
  use Agent

  @type op_number :: integer
  @type item_type :: :type1 | :type2
  @type producer_name :: :produtor1 | :produtor2
  @type cache_item :: %{op: String.t(), tipo: item_type, produtor: producer_name}

  @cache_limit 100

  def start_link(_) do
    Agent.start_link(fn -> %{cache: [], op_number: 1, consumers: []} end, name: __MODULE__)
  end

  def start_producers() do
    IO.puts("Iniciando produtores...")

    producer1 = Task.async(fn -> start_producer(:produtor1, 3500, 7500) end)
    producer2 = Task.async(fn -> start_producer(:produtor2, 3500, 7500) end)

    Task.await(producer1, 600_000)
    Task.await(producer2, 600_000)
  end

  defp start_producer(producer_name, time1, time2) do
    Enum.each(1..50, fn _ ->
      wait_if_cache_full()

      op_number = get_next_op_number()
      op = format_op_number(op_number)
      item_type = random_item_type()
      production_time = if Enum.random([true, false]), do: time1, else: time2

      IO.puts("OP: #{op} - Iniciado às #{format_timestamp()} pelo #{producer_name} produzindo #{item_type}")

      :timer.sleep(production_time)

      IO.puts("OP: #{op} - Finalizado às #{format_timestamp()} pelo #{producer_name} produzindo #{item_type}")

      cache_item = %{op: op, tipo: item_type, produtor: producer_name}
      store_in_cache(cache_item)
    end)
  end

  defp wait_if_cache_full() do
    Agent.get(__MODULE__, fn state ->
      if length(state.cache) >= @cache_limit do
        :timer.sleep(1000)
        wait_if_cache_full()
      else
        state
      end
    end)
  end

  defp get_next_op_number() do
    Agent.get_and_update(__MODULE__, fn state ->
      next_op = state.op_number
      {next_op, %{state | op_number: state.op_number + 1}}
    end)
  end

  defp random_item_type() do
    Enum.random([:type1, :type2])
  end

  defp format_op_number(op_number) do
    String.pad_leading(Integer.to_string(op_number), 8, "0")
  end

  defp format_timestamp() do
    {{_, _, _}, {hour, min, sec}} = :calendar.universal_time()
    "#{hour}:#{min}:#{sec}"
  end

  defp store_in_cache(cache_item) do
    Agent.update(__MODULE__, fn state ->
      %{state | cache: [cache_item | state.cache]}
    end)
    IO.inspect(cache_item, label: "Item armazenado no cache")
    IO.inspect(Agent.get(__MODULE__, & &1.cache), label: "Cache completo")

    notify_consumers(cache_item)
  end

  defp notify_consumers(cache_item) do
    consumers = Agent.get(__MODULE__, & &1.consumers)
    Enum.each(consumers, fn consumer_pid ->
      send(consumer_pid, {:new_item, cache_item})
    end)
  end

  def register_consumer(consumer_pid) do
    Agent.update(__MODULE__, fn state ->
      %{state | consumers: [consumer_pid | state.consumers]}
    end)
  end

  def get_cache() do
    Agent.get(__MODULE__, & &1.cache)
  end

  def remove_from_cache(cache_item) do
    Agent.update(__MODULE__, fn state ->
      %{state | cache: List.delete(state.cache, cache_item)}
    end)
  end
end
