defmodule ElixirParallelProductionTest do
  use ExUnit.Case
  doctest ElixirParallelProduction

  test "greets the world" do
    assert ElixirParallelProduction.hello() == :world
  end
end
