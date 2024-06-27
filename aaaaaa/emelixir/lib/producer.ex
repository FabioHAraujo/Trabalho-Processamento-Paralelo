defmodule Producer do
  def init(buffer, prod_id) do
    IO.puts("Producer #{prod_id} created.")
    listen(buffer, prod_id)
  end

  defp listen(buffer, prod_id) do
    time_ms = :rand.uniform(1000)  # Gerando um número aleatório entre 0 e 1000 (milissegundos)
    Process.sleep(time_ms)

    send(buffer, {:available, self(), prod_id})

    receive do
      :full ->
        IO.puts("Producer #{prod_id} tried to insert item, but buffer is full.")
        listen(buffer, prod_id)

      :put_item ->
        listen(buffer, prod_id)
    end
  end
end
