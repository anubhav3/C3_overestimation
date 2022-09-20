# 2022.02.23
# We simulated primary extinction by removing max vulnerability species in all the 1000 food web predicted by the ADBM 

library(R.utils)
library(NetworkExtinction)
library(network)
library(readxl)
library(dplyr)
library(ggplot2)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

# For removing least connected nodes
dd_mg <- data.frame(n_ext = double(),
                    acc_sec_ext = integer(),
                    nsim = integer(),
                    type = character(),
                    fw_name = character(),
                    S = integer(),
                    L = integer())

# Getting the foodweb name and tolerance
metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
n_fw <- length(metadata_fw_tol$foodweb)
fw_ind <- 1:n_fw


for(i in fw_ind){
  fw_name <- metadata_fw_tol$foodweb[i]
  fw_tol <- metadata_fw_tol$dist_rej[i]
  
  real_pred_mat <- readRDS(paste0("../C1_data/data/", fw_name, ".web.RDS"))$predation.matrix
  if(is.null(rownames(real_pred_mat))){
    n_nodes <- dim(real_pred_mat)[1]
    rownames(real_pred_mat) <- as.character(1:n_nodes)
    colnames(real_pred_mat) <- as.character(1:n_nodes)
  }
  
  S <- dim(real_pred_mat)[1]
  L_real <- sum(real_pred_mat)
  real_conn <- sum(real_pred_mat)/(dim(real_pred_mat)[1]^2)
  real_ext_mc <- maxgenerality_ext(net = real_pred_mat)
  
  dd_mg <- rbind(dd_mg, 
                 data.frame(n_ext = real_ext_mc$acc_pri_ext,
                            acc_sec_ext = real_ext_mc$acc_sec_ext,
                            nsim = 1,
                            type = "Empirical",
                            fw_name = fw_name,
                            S = S,
                            L = L_real))
  
  for(j in 1:1000){
    ADBM_ABC_fw_calc <- ADBM_ABC_fw_all(fw_name = fw_name, fw_tol = fw_tol, nsim = j)
    ADBM_ABC_pred_mat <- ADBM_ABC_fw_calc$ADBM_pred_mat
    
    L_ADBM_ABC <- sum(ADBM_ABC_pred_mat)
    ADBM_ABC_conn <- sum(ADBM_ABC_pred_mat)/(dim(ADBM_ABC_pred_mat)[1]^2)
    ADBM_ABC_ext_mc <- maxgenerality_ext(net = ADBM_ABC_pred_mat)
    
    dd_mg <- rbind(dd_mg,
                   data.frame(n_ext = ADBM_ABC_ext_mc$acc_pri_ext,
                              acc_sec_ext = ADBM_ABC_ext_mc$acc_sec_ext,
                              nsim = j,
                              type = "ADBM",
                              fw_name = fw_name,
                              S = S,
                              L = L_ADBM_ABC))
  }
  print(fw_name)
}

# saveRDS(object = dd_mg, file = "results/maxgenerality_uncertainty.RDS")
