# 25.11.2021


##### Investigating degree distribution of Skipwith Pond

skip_emp <- readRDS("../C1_method_v2/data/Skipwith Pond.web.RDS")$predation.matrix
skip_ABC <- readRDS("data/ADBM_ABC_predicted_foodwebs/Skipwith Pond.pred_mat.RDS")

skip_emp_net <- as.network(skip_emp)
skip_ABC_net <- as.network(skip_ABC)

NetworkExtinction::DegreeDistribution(skip_emp_net)

NetworkExtinction::DegreeDistribution(skip_ABC_net)
