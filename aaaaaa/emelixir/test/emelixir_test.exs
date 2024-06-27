defmodule EmelixirTest do
  use ExUnit.Case
  doctest Emelixir

  test "greets the world" do
    assert Emelixir.hello() == :world
  end
end
