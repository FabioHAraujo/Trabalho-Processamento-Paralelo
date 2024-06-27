defmodule ProducerConsumerTest do
  use ExUnit.Case
  doctest ProducerConsumer

  test "greets the world" do
    assert ProducerConsumer.hello() == :world
  end
end
