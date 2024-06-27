defmodule ProducerConsumer.MixProject do
  use Mix.Project

  def project do
    [
      app: :producer_consumer,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {ProducerConsumer.Application, []}
    ]
  end

  defp deps do
    [
      {:gen_stage, "~> 1.0"}
    ]
  end
end
