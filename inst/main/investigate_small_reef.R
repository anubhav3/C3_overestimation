# 27.10.2021
# Investigate Secondary extinctions in Small Reef food web

reef_emp <- readRDS("../C1_method_v2/data/Small Reef.web.RDS")$predation.matrix
reef_2008 <-readRDS("../C3_overestimation/data/ADBM_2008_predicted_foodwebs/Small Reef.pred_mat.RDS")
reef_ABC <- readRDS("../C3_overestimation/data/ADBM_ABC_predicted_foodwebs/Small Reef.pred_mat.RDS")


Plot.matrix(reef_emp)
Plot.matrix(reef_2008)
Plot.matrix(reef_ABC)

conn_emp <- sum(ythan_emp)/(dim(ythan_emp)[1]^2)
conn_2008 <- sum(reef_2008)/(dim(reef_2008)[1]^2)

