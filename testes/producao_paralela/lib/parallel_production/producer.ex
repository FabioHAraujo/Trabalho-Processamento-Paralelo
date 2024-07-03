defmodule ParallelProduction.Producer do
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, %{cache: []}, name: __MODULE__)
  end

  @impl true
  def init(state) do
    {:ok, state}
  end

  def produce(op, type, item) do
    GenServer.cast(__MODULE__, {:produce, op, type, item})
  end

  def get_cache() do
    GenServer.call(__MODULE__, :get_cache)
  end

  @impl true
  def handle_cast({:produce, op, type, item}, state) do
    new_item = generate_item_name(op, type, item)
    new_cache = [{op, type, new_item} | state.cache]
    {:noreply, %{state | cache: new_cache}}
  end

  @impl true
  def handle_call(:get_cache, _from, state) do
    {:reply, state.cache, state}
  end

  defp generate_item_name(op, type, item) do
    "OP#{pad_op(op)} - Tipo: #{type} - Item: #{item}"
  end

  defp pad_op(op), do: "#{:io_lib.format("~8..0B", [op])}"
end
