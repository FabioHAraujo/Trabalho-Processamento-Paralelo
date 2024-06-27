defmodule Master do
  def init do
    IO.puts("Input number of producers:")
    num_producers = get_int_input()

    IO.puts("Input number of consumers:")
    num_consumers = get_int_input()

    buffer = spawn(Buffer, :init, [])

    start_prod(1, num_producers, buffer)
    start_cons(1, num_consumers, buffer)

    loop(buffer)  # Adicionamos um loop para manter o programa em execução
  end

  defp loop(buffer) do
    receive do
      {:stop} ->
        IO.puts("Stopping the system.")
        send(buffer, :stop) # Envia mensagem para parar o buffer
      _ ->
        loop(buffer)
    end
  end

  defp get_int_input do
    input = IO.gets(" ") |> String.trim()

    case input do
      "" ->
        IO.puts("Invalid input. Please enter a positive integer.")
        get_int_input()

      _ ->
        case Regex.match?(~r/^\d+$/, input) do
          true ->
            case String.to_integer(input) do
              num when num > 0 ->
                num
              _ ->
                IO.puts("Invalid input. Please enter a positive integer.")
                get_int_input()
            end
          false ->
            IO.puts("Invalid input. Please enter a positive integer.")
            get_int_input()
        end
    end
  end

  defp start_prod(prod_id, num_producers, buffer) when prod_id <= num_producers do
    spawn(Producer, :init, [buffer, prod_id])
    start_prod(prod_id + 1, num_producers, buffer)
  end
  defp start_prod(_, _, _), do: :ok

  defp start_cons(cons_id, num_consumers, buffer) when cons_id <= num_consumers do
    spawn(Consumer, :init, [buffer, cons_id])
    start_cons(cons_id + 1, num_consumers, buffer)
  end
  defp start_cons(_, _, _), do: :ok
end
