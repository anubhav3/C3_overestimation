## 11.11.2021

library(NetworkExtinction)
library(network)

data("net")
ch_data <- net

net_sim <- SimulateExtinctions(Network = net, Order = c(3, 4, 7, 10, 1, 5, 8, 2, 6, 9), Method = "Ordered")

plot.network(x = ch_data, label = network.vertex.names(ch_data))
