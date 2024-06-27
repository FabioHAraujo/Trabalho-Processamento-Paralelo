defmodule Item do
  def init do
    listen()
  end

  defp listen(item_id \\ nil) do
    receive do
      {:assign_id, new_id} ->
        listen(new_id)

      {from, :get_id} ->
        send(from, {:item_id, item_id || 0})
        listen(item_id)
    end
  end
end
