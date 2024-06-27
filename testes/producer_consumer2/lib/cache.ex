defmodule ProducerConsumer.Cache do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(_) do
    {:ok, %{cache: []}}
  end

  def store_item(item) do
    GenServer.call(__MODULE__, {:store_item, item})
  end

  def get_item do
    GenServer.call(__MODULE__, :get_item)
  end

  def handle_call({:store_item, item}, _from, state) do
    new_cache = [item | state.cache]
    if length(new_cache) >= 100 do
      IO.puts("Cache completo: #{length(new_cache)}")
    end
    {:reply, :ok, %{state | cache: new_cache}}
  end

  def handle_call(:get_item, _from, state) do
    case state.cache do
      [] ->
        {:reply, :empty, state}
      [item | rest] ->
        {:reply, {:ok, item}, %{state | cache: rest}}
    end
  end
end
