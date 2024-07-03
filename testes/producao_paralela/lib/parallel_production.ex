defmodule ParallelProduction do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  @impl true
  def init(:ok) do
    children = [
      {Registry, keys: :unique, name: ParallelProduction.ProducerRegistry},
      {ParallelProduction.Producer, :type1},
      {ParallelProduction.Producer, :type2}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def get_cache() do
    {:ok, type1_cache} = Registry.lookup(ParallelProduction.ProducerRegistry, :type1)
    {:ok, type2_cache} = Registry.lookup(ParallelProduction.ProducerRegistry, :type2)
    type1_cache ++ type2_cache
  end
end
