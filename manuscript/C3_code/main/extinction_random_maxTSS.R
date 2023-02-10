# 2022.09.12
# We simulated primary extinction by removing random species in all the food web predicted by the ADBM with max TSS

library(R.utils)
library(network)
library(readxl)
library(dplyr)
library(ggplot2)
library(doParallel)

sourceDirectory("../C1_method_v2/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

# For removing random nodes
dd_rand <- data.frame(n_ext = double(),
                      acc_sec_ext = integer(),
                      nsim_rand = integer(),
                      type = character(),
                      fw_name = character(),
                      S = integer(),
                      L = integer())

# Getting the foodweb name and tolerance
metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
n_fw <- length(metadata_fw_tol$foodweb)
fw_ind <- 1:n_fw
Nsim <- 100

for(i in fw_ind){
  
  fw_name <- metadata_fw_tol$foodweb[i]
  fw_tol <- metadata_fw_tol$dist_rej[i]
  
  ### Real food webs
  real_pred_mat <- readRDS(paste0("../C1_method_v2/data/", fw_name, ".web.RDS"))$predation.matrix
  
  if(is.null(rownames(real_pred_mat))){
    n_nodes <- dim(real_pred_mat)[1]
    rownames(real_pred_mat) <- as.character(1:n_nodes)
    colnames(real_pred_mat) <- as.character(1:n_nodes)
  }
  
  S <- dim(real_pred_mat)[1]
  L_real <- sum(real_pred_mat)
  real_conn <- sum(real_pred_mat)/(dim(real_pred_mat)[1]^2)
  real_ext_rand <- random_ext_parallel(net = real_pred_mat, nsim = Nsim, n_cores = 5)
  
  dd_rand <- rbind(dd_rand,
                   data.frame(n_ext = real_ext_rand$acc_pri_ext,
                              acc_sec_ext = real_ext_rand$acc_sec_ext,
                              nsim_rand = real_ext_rand$nsim,
                              type = "Empirical",
                              fw_name = fw_name,
                              S = S,
                              L = L_real))
  
  ### ADBM predicted food webs
  ADBM_ABC_fw_calc <- ADBM_ABC_fw_maxTSS(fw_name = fw_name, fw_tol = fw_tol)
  ADBM_ABC_pred_mat <- ADBM_ABC_fw_calc$ADBM_pred_mat
  
  L_ADBM_ABC <- sum(ADBM_ABC_pred_mat)
  ADBM_ABC_conn <- sum(ADBM_ABC_pred_mat)/(dim(ADBM_ABC_pred_mat)[1]^2)
  ADBM_ABC_ext_rand <- random_ext_parallel(net = ADBM_ABC_pred_mat, nsim = Nsim, n_cores = 5)
  
  dd_rand <- rbind(dd_rand,
                   data.frame(n_ext = ADBM_ABC_ext_rand$acc_pri_ext,
                              acc_sec_ext = ADBM_ABC_ext_rand$acc_sec_ext,
                              nsim_rand = ADBM_ABC_ext_rand$nsim,
                              type = "ADBM_ABC",
                              fw_name = fw_name,
                              S = S,
                              L = L_ADBM_ABC))
  
  
  print(fw_name)
}

# saveRDS(dd_rand, file = "results/random_raw_uncertainty_maxTSS.RDS")

