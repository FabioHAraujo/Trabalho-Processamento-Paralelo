defmodule ParallelProduction.Application do
  use Application

  @impl true
  def start(_type, _args) do
    IO.puts("Iniciando ParallelProduction...")

    children = [
      ParallelProduction.Producer
    ]

    opts = [strategy: :one_for_one, name: ParallelProduction.Supervisor]
    IO.puts("Iniciando supervisor...")
    Supervisor.start_link(children, opts)
  end

  def run() do
    IO.puts("Produção iniciada!")
    schedule_cache_print()
    wait_for_exit()
  end

  defp schedule_cache_print do
    IO.puts("Agendando próxima impressão do cache em 5s...")
    Process.send_after(self(), :print_cache, 5000)
  end

  def handle_info(:print_cache, state) do
    cache = ParallelProduction.Producer.get_cache()
    IO.puts("\nCache atual:")
    Enum.each(cache, fn {op, type, item} ->
      IO.puts("OP#{pad_op(op)} - Tipo: #{type} - Item: #{item}")
    end)
    schedule_cache_print()
    {:noreply, state}
  end

  defp wait_for_exit do
    receive do
      {:EXIT, _pid, reason} ->
        IO.puts("Application exited with reason: #{inspect(reason)}")
    end
  end

  defp pad_op(op), do: "#{:io_lib.format("~8..0B", [op])}"
end
