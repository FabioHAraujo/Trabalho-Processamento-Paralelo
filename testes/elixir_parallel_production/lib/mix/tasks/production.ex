defmodule Mix.Tasks.Production do
  use Mix.Task

  def run(_) do
    ElixirParallelProduction.start_production()
  end
end
