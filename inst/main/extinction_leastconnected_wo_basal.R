# 2022.02.09
# We simulated primary extinction by removing least connected species in food web predicted by the ADBM which had the maximum TSS

library(R.utils)
library(NetworkExtinction)
library(network)
library(readxl)
library(dplyr)
library(ggplot2)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

# For removing least connected nodes
dd_lc <- data.frame(n_ext = double(),
                    acc_sec_ext = integer(),
                    type = character(),
                    fw_name = character(),
                    S = integer(),
                    S_wo_basal = integer(),
                    L = integer())

# Getting the foodweb name and tolerance
metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
n_fw <- length(metadata_fw_tol$foodweb)
fw_ind <- 1:n_fw

for(i in fw_ind){
  fw_name <- metadata_fw_tol$foodweb[i]
  fw_tol <- metadata_fw_tol$dist_rej[i]
  
  ADBM_ABC_fw_calc <- ADBM_ABC_fw(fw_name = fw_name, fw_tol = fw_tol)
  
  real_pred_mat <- ADBM_ABC_fw_calc$real_pred_mat
  if(is.null(rownames(real_pred_mat))){
    n_nodes <- dim(real_pred_mat)[1]
    rownames(real_pred_mat) <- as.character(1:n_nodes)
    colnames(real_pred_mat) <- as.character(1:n_nodes)
  }
  ADBM_ABC_pred_mat <- ADBM_ABC_fw_calc$ADBM_pred_mat
  
  S <- dim(real_pred_mat)[1]
  S_wo_basal_emp <- S - length(basal_species_fun(real_pred_mat))
  L_real <- sum(real_pred_mat)
  L_ADBM_ABC <- sum(ADBM_ABC_pred_mat)
  S_wo_basal_ADBM <- S - length(basal_species_fun(ADBM_ABC_pred_mat))
  
  
  real_conn <- sum(real_pred_mat)/(dim(real_pred_mat)[1]^2)
  ADBM_ABC_conn <- sum(ADBM_ABC_pred_mat)/(dim(ADBM_ABC_pred_mat)[1]^2)
  
  real_ext_mc <- least_connected_ext_wo_basal(net = real_pred_mat)
  ADBM_ABC_ext_mc <- least_connected_ext_wo_basal(net = ADBM_ABC_pred_mat)
  
  dd_lc <- rbind(data.frame(n_ext = real_ext_mc$acc_pri_ext,
                            acc_sec_ext = real_ext_mc$acc_sec_ext,
                            type = "Empirical",
                            fw_name = fw_name,
                            S = S,
                            S_wo_basal = S_wo_basal_emp,
                            L = L_real),
                 data.frame(n_ext = ADBM_ABC_ext_mc$acc_pri_ext,
                            acc_sec_ext = ADBM_ABC_ext_mc$acc_sec_ext,
                            type = "ADBM",
                            fw_name = fw_name,
                            S = S,
                            S_wo_basal = S_wo_basal_ADBM,
                            L = L_ADBM_ABC),
                 dd_lc)
  print(fw_name)
}

# saveRDS(object = dd_lc, file = "results/least_connected_wo_basal_maxTSS.RDS")



