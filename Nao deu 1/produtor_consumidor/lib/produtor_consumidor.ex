defmodule ProdutorConsumidor do
  use GenServer

  # Inicialização do GenServer
  def start_link(initial_queue) do
    GenServer.start_link(__MODULE__, initial_queue, name: __MODULE__)
  end

  def init(initial_queue) do
    {:ok, initial_queue}
  end

  # Funções para o produtor
  def produce(item) do
    GenServer.call(__MODULE__, {:produce, item})
  end

  # Funções para o consumidor
  def consume do
    GenServer.call(__MODULE__, :consume)
  end

  # Implementação das funções de GenServer
  def handle_call({:produce, item}, _from, queue) do
    new_queue = :queue.in(item, queue)
    {:reply, :ok, new_queue}
  end

  def handle_call(:consume, _from, queue) do
    case :queue.out(queue) do
      {{:value, item}, new_queue} ->
        {:reply, {:ok, item}, new_queue}

      {:empty, _} ->
        {:reply, :empty, queue}
    end
  end
end
