# Início da execução dos produtores
{:ok, _} = ProducerConsumer.Producer.start_link(nil)
ProducerConsumer.Producer.start_producers()

# Início da execução dos consumidores
ProducerConsumer.Consumer.start_consumers()
