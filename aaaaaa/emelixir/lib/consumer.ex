defmodule Consumer do
  def init(buffer, cons_id) do
    IO.puts("Consumer #{cons_id} created.")
    listen(buffer, cons_id)
  end

  defp listen(buffer, cons_id) do
    time_ms = :rand.uniform(1000)  # Gerando um número aleatório entre 0 e 1000 (milissegundos)
    Process.sleep(time_ms)

    send(buffer, {:occupied, self(), cons_id})

    receive do
      :empty ->
        IO.puts("Consumer #{cons_id} tried to remove item, but buffer is empty.")
        listen(buffer, cons_id)

      :remove_item ->
        listen(buffer, cons_id)
    end
  end
end
