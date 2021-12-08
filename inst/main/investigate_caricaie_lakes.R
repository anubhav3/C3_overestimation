# 27.10.2021
# Investigate Secondary extinctions in Caricaie Lakes food web

caricaie_emp <- readRDS("../C1_method_v2/data/Caricaie Lakes.web.RDS")$predation.matrix
caricaie_2008 <-readRDS("../C3_overestimation/data/ADBM_2008_predicted_foodwebs/Caricaie Lakes.pred_mat.RDS")
caricaie_ABC <- readRDS("../C3_overestimation/data/ADBM_ABC_predicted_foodwebs/Caricaie Lakes.pred_mat.RDS")

Plot.matrix(caricaie_emp)
Plot.matrix(caricaie_2008)
Plot.matrix(caricaie_ABC)

conn_emp <- sum(caricaie_emp)/(dim(caricaie_emp)[1]^2)
conn_2008 <- sum(caricaie_2008)/(dim(caricaie_2008)[1]^2)
conn_ABC <- sum(caricaie_ABC)/(dim(caricaie_ABC)[1]^2)

net_caricaie_emp <- as.network(caricaie_emp)
net_caricaie_2008 <- as.network(caricaie_2008)

comm_caricaie_emp <- mat.to.comm(pred.mat = caricaie_emp, fw_title = "blabla")
comm_caricaie_2008 <- mat.to.comm(pred.mat = caricaie_2008, fw_title = "blabla")

indeg_caricaie_emp <- InDegree(community = comm_caricaie_emp)
indeg_caricaie_2008 <- InDegree(community = comm_caricaie_2008)

outdeg_caricaie_emp <- OutDegree(community = comm_caricaie_emp)
outdeg_caricaie_2008 <- OutDegree(community = comm_caricaie_2008)

hist(indeg_caricaie_emp, breaks = 50)
hist(indeg_caricaie_2008, breaks = 50)

hist(outdeg_caricaie_emp, breaks = 50)
hist(outdeg_caricaie_2008, breaks = 50)

emp <- DegreeDistribution(net_caricaie_emp)
DegreeDistribution(net_caricaie_2008)
