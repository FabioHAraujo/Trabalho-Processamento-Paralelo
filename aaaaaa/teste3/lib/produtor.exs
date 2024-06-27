defmodule ProducerConsumer do
  defmodule Producer do
    @interval 3000  # 3000 milliseconds = 3 seconds

    def start_link do
      Task.start_link(fn -> produce_items([]) end)
    end

    defp produce_items(items) do
      send_items(items)
      :timer.sleep(@interval)
      produce_items(["Item" | items])
    end

    defp send_items(items) do
      Consumer.consume(items)
    end
  end

  defmodule Consumer do
    @interval 4000  # 4000 milliseconds = 4 seconds

    def start_link do
      Task.start_link(fn -> consume_items([]) end)
    end

    defp consume_items(items) do
      IO.puts("Items consumidos: #{items}")
      :timer.sleep(@interval)
      consume_items(tail(items))
    end

    defp tail([]), do: []
    defp tail([_ | t]), do: t
  end

  def start do
    Consumer.start_link()
    Producer.start_link()
  end
end

ProducerConsumer.start()
