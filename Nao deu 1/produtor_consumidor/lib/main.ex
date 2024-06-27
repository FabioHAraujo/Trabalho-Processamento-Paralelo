defmodule Main do
  def start do
    # Inicia o GenServer com uma fila vazia
    {:ok, _} = ProdutorConsumidor.start_link(:queue.new())

    # Inicia o GenServer para gerenciar a variável op
    {:ok, _} = OpManager.start_link(0)

    # Cria produtores
    for i <- 1..2 do
      spawn(fn -> producer(i) end)
    end

    # Cria consumidores
    for i <- 1..4 do
      spawn(fn -> consumer(i) end)
    end
  end

  def producer(id) do
    :timer.sleep(3500)
    item = OpManager.get_and_increment_op()
    IO.puts("Produtor #{id} produziu item referente à OP: #{item}")
    ProdutorConsumidor.produce(item)
    producer(id)
  end

  def consumer(id) do
    :timer.sleep(7000)
    case ProdutorConsumidor.consume() do
      {:ok, item} -> IO.puts("Consumidor #{id} consumiu item #{item}")
      :empty -> IO.puts("Consumidor #{id} encontrou a fila vazia")
    end
    consumer(id)
  end
end

Main.start()
