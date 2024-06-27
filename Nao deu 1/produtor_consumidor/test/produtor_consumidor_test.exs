defmodule ProdutorConsumidorTest do
  use ExUnit.Case
  doctest ProdutorConsumidor

  test "greets the world" do
    assert ProdutorConsumidor.hello() == :world
  end
end
