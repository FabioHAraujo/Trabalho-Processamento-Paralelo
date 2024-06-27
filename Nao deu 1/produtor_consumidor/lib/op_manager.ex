defmodule OpManager do
  use GenServer

  # Inicialização do GenServer
  def start_link(initial_value) do
    GenServer.start_link(__MODULE__, initial_value, name: __MODULE__)
  end

  def init(initial_value) do
    {:ok, initial_value}
  end

  # Função para obter e incrementar a OP
  def get_and_increment_op do
    GenServer.call(__MODULE__, :get_and_increment_op)
  end

  # Implementação das funções de GenServer
  def handle_call(:get_and_increment_op, _from, op) do
    {:reply, op, op + 1}
  end
end
