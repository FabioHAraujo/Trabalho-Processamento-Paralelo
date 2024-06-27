defmodule Teste2Test do
  use ExUnit.Case
  doctest Teste2

  test "greets the world" do
    assert Teste2.hello() == :world
  end
end
