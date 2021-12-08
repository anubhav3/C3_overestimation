# 26.10.2021
# Investigate Secondary extinctions in Ythan food web

ythan_emp <- readRDS("../C1_method_v2/data/Ythan.web.RDS")$predation.matrix
ythan_2008 <-readRDS("../C3_overestimation/data/ADBM_2008_predicted_foodwebs/Ythan.pred_mat.RDS")
ythan_ABC <- readRDS("../C3_overestimation/data/ADBM_ABC_predicted_foodwebs/Ythan.pred_mat.RDS")


Plot.matrix(ythan_emp)
Plot.matrix(ythan_2008)
Plot.matrix(ythan_ABC)

conn_emp <- sum(ythan_emp)/(dim(ythan_emp)[1]^2)
conn_2008 <- sum(ythan_2008)/(dim(ythan_2008)[1]^2)

