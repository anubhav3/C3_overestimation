# 07.12.2021
# We investigate the Broadstone Stream food web here

### Empirical Broadstone Stream
broad2008 <- readRDS("../C1_data/data/Broadstone Stream.web.RDS")
broad2008 <- broad2008$predation.matrix
broad2008_comm <- mat.to.comm(pred.mat = broad2008, fw_title = "Broadstone Stream")

ext_seq_2008 <- most_connected_ext(net = broad2008)
Plot.matrix(web = broad2008, title = "Broadstone Stream")
PlotWebByLevel(community = broad2008_comm)



### Empirical Broadstone Stream size agg
broad2008 <- readRDS("../C1_data/data/Broadstone Stream size_agg.web.RDS")
broad2008 <- broad2008$predation.matrix
broad2008_comm <- mat.to.comm(pred.mat = broad2008, fw_title = "Broadstone Stream size_agg obs")

Plot.matrix(web = broad2008, title = "Broadstone Stream size_agg obs")
PlotWebByLevel(community = broad2008_comm)


broad_ADBM_ABC <- readRDS("data/ADBM_ABC_predicted_foodwebs/Broadstone Stream size_agg.pred_mat.RDS")
broad_ADBM_ABC_comm <- mat.to.comm(pred.mat = broad_ADBM_ABC, fw_title = "Broadstone Stream size_agg")

Plot.matrix(web = broad_ADBM_ABC, title = "Broadstone Stream size_agg")

PlotWebByLevel(community = broad_ADBM_ABC_comm)


