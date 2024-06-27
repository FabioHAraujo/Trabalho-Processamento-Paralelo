defmodule Buffer do
  def init do
    IO.puts("Buffer created.")
    max_size = 10
    buffer_array = []
    item_id_prod = 1
    listen(max_size, buffer_array, item_id_prod)
  end

  defp listen(max_size, buffer_array, item_id_prod) do
    receive do
      {:available, from, item, prod_id} when length(buffer_array) < max_size ->
        new_buffer_array = buffer_array ++ [{item_id_prod, item}]
        send(from, :put_item)
        IO.puts("Producer #{prod_id} inserted item #{item_id_prod}. There are #{length(new_buffer_array)} items in buffer.")
        output(new_buffer_array)
        listen(max_size, new_buffer_array, item_id_prod + 1)

      {:occupied, from, cons_id} when buffer_array != [] ->
        [{item_id, item} | new_buffer_array] = buffer_array
        send(from, :remove_item)
        IO.puts("Consumer #{cons_id} removed item #{item_id}. There are #{length(new_buffer_array)} items in buffer.")
        output(new_buffer_array)
        listen(max_size, new_buffer_array, item_id_prod)

      {:available, from, _item, _prod_id} ->
        send(from, :full)
        listen(max_size, buffer_array, item_id_prod)

      {:stop} ->
        IO.puts("Buffer stopping.")
    end
  end

  defp output(buffer) do
    item_ids = Enum.map(buffer, &elem(&1, 0))
    IO.puts("Buffer: [#{Enum.join(item_ids, ", ")}]")
  end

  defp fnl([h]) do
    IO.puts("#{get_item_id(h)}]")
  end

  defp fnl([h | t]) do
    IO.write("#{get_item_id(h)}, ")
    fnl(t)
  end

  defp fnl([]) do
    IO.puts("]")
  end

  defp get_item_id({item_id, _item}), do: item_id
end
