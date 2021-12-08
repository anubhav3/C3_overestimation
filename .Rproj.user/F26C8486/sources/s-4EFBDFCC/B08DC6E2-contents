# 07.12.2021
# We investigate the Tuesday Lake food web here


### Empirical Tuesday Lake
tuesday2008 <- readRDS("../C1_data/data/Tuesday Lake.web.RDS")
tuesday2008 <- tuesday2008$predation.matrix
tuesday2008_comm <- mat.to.comm(pred.mat = tuesday2008, fw_title = "Tuesday Lake")

ext_seq_2008 <- most_connected_ext(net = tuesday2008)
Plot.matrix(web = tuesday2008, title = "Tuesday Lake")
PlotWebByLevel(community = tuesday2008_comm)


### ADBM_ABC Tuesday Lake
tuesday_ADBM_ABC <- readRDS("data/ADBM_ABC_predicted_foodwebs/Tuesday Lake.pred_mat.RDS")
tuesday_ADBM_ABC_comm <- mat.to.comm(pred.mat = tuesday_ADBM_ABC, fw_title = "Tuesday Lake")

ext_seq_ADBM_ABC <- most_connected_ext(net = tuesday_ADBM_ABC)
Plot.matrix(web = tuesday_ADBM_ABC, title = "Tuesday Lake")

PlotWebByLevel(community = tuesday_ADBM_ABC_comm)



