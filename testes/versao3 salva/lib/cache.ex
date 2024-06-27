defmodule ProducerConsumer.Cache do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, %{cache: [], consumers: []}, name: __MODULE__)
  end

  def init(state), do: {:ok, state}

  def handle_call(:get_cache, _from, state) do
    {:reply, state.cache, state}
  end

  def handle_cast({:add_item, item}, state) do
    new_state = Map.update!(state, :cache, fn cache -> [item | cache] end)
    {:noreply, new_state}
  end

  def handle_cast({:register_consumer, pid}, state) do
    new_state = Map.update!(state, :consumers, fn consumers -> [pid | consumers] end)
    {:noreply, new_state}
  end

  def register_consumer(pid) do
    GenServer.cast(__MODULE__, {:register_consumer, pid})
  end

  def add_item(item) do
    GenServer.cast(__MODULE__, {:add_item, item})
  end

  def get_cache() do
    GenServer.call(__MODULE__, :get_cache)
  end
end
