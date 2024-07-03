defmodule ParallelProductionTest do
  use ExUnit.Case
  doctest ParallelProduction

  test "greets the world" do
    assert ParallelProduction.hello() == :world
  end
end
