# 07.12.2021
# We investigate the Broadstone Stream food web here

### Empirical Broadstone Stream
broad2008 <- readRDS("../C1_data/data/Broadstone Stream.web.RDS")
broad2008 <- broad2008$predation.matrix
broad2008_comm <- mat.to.comm(pred.mat = broad2008, fw_title = "Broadstone Stream")

ext_seq_2008 <- most_connected_ext(net = broad2008)
Plot.matrix(web = broad2008, title = "Broadstone Stream")
PlotWebByLevel(community = broad2008_comm)




