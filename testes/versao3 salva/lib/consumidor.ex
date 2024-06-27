defmodule ProducerConsumer.Consumer do
  @type consumer_id :: 1 | 2 | 3 | 4
  @type cache_item :: %{op: String.t(), tipo: :type1 | :type2, produtor: :produtor1 | :produtor2}

  def start_consumers() do
    for id <- 1..4 do
      consumer_pid = spawn_link(fn -> consumer_loop(id) end)
      ProducerConsumer.Producer.register_consumer(consumer_pid)
    end
  end

  def notify_consumer(cache_item) do
    send(self(), {:new_item, cache_item})
  end

  defp consumer_loop(consumer_id) do
    receive do
      {:new_item, cache_item} ->
        consume_item(consumer_id, cache_item)
        consumer_loop(consumer_id)
    after
      0 -> # Caso não haja mensagens imediatamente, tenta novamente
        consumer_loop(consumer_id)
    end
  end

  defp consume_item(consumer_id, cache_item) do
    consumption_time = case cache_item.tipo do
      :type1 -> 7000
      :type2 -> 15000
    end

    IO.puts("Consumer #{consumer_id} - Iniciando consumo de #{cache_item.tipo} (OP: #{cache_item.op}) às #{format_timestamp()} - Tempo estimado: #{consumption_time / 1000}s")

    :timer.sleep(consumption_time)

    IO.puts("Consumer #{consumer_id} - Finalizou consumo de #{cache_item.tipo} (OP: #{cache_item.op}) às #{format_timestamp()}")

    # Remover item do cache após o consumo
    ProducerConsumer.Cache.remove_from_cache(cache_item)
  end

  defp format_timestamp() do
    {{_, _, _}, {hour, min, sec}} = :calendar.universal_time()
    "#{hour}:#{min}:#{sec}"
  end
end
