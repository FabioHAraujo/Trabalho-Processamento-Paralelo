defmodule ElixirParallelProduction do
  alias ElixirParallelProduction.Cache

  def start_production do
    cache = Cache.new()

    producers = [
      %{name: "produtor1", item_type: "tipo1", production_time: 3500},
      %{name: "produtor2", item_type: "tipo2", production_time: 7500}
    ]

    for producer <- producers do
      Task.async(fn -> produce_items(producer, cache, 1) end)
    end
  end

  defp produce_items(_producer, _cache, op_number) when op_number > 999_999_99, do: nil

  defp produce_items(producer, cache, op_number) do
    item = %{
      op: String.pad_leading("#{op_number}", 8, "0"),
      type: producer.item_type,
      producer: producer.name,
      start_time: DateTime.utc_now(),
      end_time: nil
    }

    IO.puts("OP #{item.op} - #{producer.name} iniciou produção de #{item.type} às #{item.start_time}")

    Cache.put(cache, item.op, item)

    Process.sleep(producer.production_time)

    item = Map.put(item, :end_time, DateTime.utc_now())

    IO.puts("OP #{item.op} - #{producer.name} finalizou produção de #{item.type} às #{item.end_time}")

    Cache.put(cache, item.op, item)

    produce_items(producer, cache, op_number + 1)
  end
end
