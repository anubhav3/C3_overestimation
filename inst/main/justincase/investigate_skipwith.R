# 06.12.2021
# We investigate the skipwtih pond food web here


### Empirical Skipwith Pond
skip2008 <- readRDS("../C1_data/data/Skipwith Pond.web.RDS")
skip2008 <- skip2008$predation.matrix
skip2008_comm <- mat.to.comm(pred.mat = skip2008, fw_title = "Skipwith Pond")

ext_seq_2008 <- most_connected_ext(net = skip2008)
Plot.matrix(web = skip2008, title = "Skipwith Pond")
PlotWebByLevel(community = skip2008_comm)


### ADBM_ABC Skipwith Pond
skip_ADBM_ABC <- readRDS("data/ADBM_ABC_predicted_foodwebs/Skipwith Pond.pred_mat.RDS")
skip_ADBM_ABC_comm <- mat.to.comm(pred.mat = skip_ADBM_ABC, fw_title = "Skipwith Pond")

ext_seq_ADBM_ABC <- most_connected_ext(net = skip_ADBM_ABC)
Plot.matrix(web = skip_ADBM_ABC, title = "Skipwith Pond")

PlotWebByLevel(community = skip_ADBM_ABC_comm)



