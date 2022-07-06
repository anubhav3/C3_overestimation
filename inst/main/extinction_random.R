# 2022.09.02
# We compute how the secondary extinctions vary when we remove random species in the food web predicted by ADBM

library(R.utils)
library(NetworkExtinction)
library(network)
library(readxl)
library(doParallel)
library(dplyr)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly = FALSE)

dd_raw <- data.frame(acc_pri_ext = integer(), acc_sec_ext = integer(),
                     nsim = integer(), type = character(), fw_name = character(),
                     S = integer(), L = integer())

dd_rand <- data.frame(acc_pri_ext = integer(),
                      mean_acc_sec_ext = double(),
                      l_acc_sec_ext = double(),
                      u_acc_sec_ext = double(),
                      type = character(),
                      fw_name = character(),
                      S = integer(),
                      L = integer(),
                      count_n = integer())

# Getting the foodweb name and tolerance
metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
n_fw <- length(metadata_fw_tol$foodweb)
fw_ind <- 1:n_fw


for(i in fw_ind){
  fw_name <- metadata_fw_tol$foodweb[i]
  fw_tol <- metadata_fw_tol$dist_rej[i]
  Nsim <- 1000
  
  ADBM_fw_calc <- ADBM_ABC_fw(fw_name = fw_name, fw_tol = fw_tol)
  
  real_pred_mat <- ADBM_fw_calc$real_pred_mat
  
  if(is.null(rownames(real_pred_mat))){
    n_nodes <- dim(real_pred_mat)[1]
    rownames(real_pred_mat) <- as.character(1:n_nodes)
    colnames(real_pred_mat) <- as.character(1:n_nodes)
  }
  ADBM_pred_mat <- ADBM_fw_calc$ADBM_pred_mat

  
  
  S <- dim(real_pred_mat)[1]
  L_real <- sum(real_pred_mat)
  L_ADBM <- sum(ADBM_pred_mat)

  
  
  real_conn <- sum(real_pred_mat)/(dim(real_pred_mat)[1]^2)
  ADBM_conn <- sum(ADBM_pred_mat)/(dim(ADBM_pred_mat)[1]^2)

  
  real_ext_rand_raw <- random_ext_parallel(net = real_pred_mat, nsim = Nsim, n_cores = 5)
  ADBM_ext_rand_raw <- random_ext_parallel(net = ADBM_pred_mat, nsim = Nsim, n_cores = 5)

  
  dd_raw <- rbind(dd_raw,
                  data.frame(acc_pri_ext = real_ext_rand_raw$acc_pri_ext, acc_sec_ext = real_ext_rand_raw$acc_sec_ext,
                             nsim = real_ext_rand_raw$nsim, type = "Empirical", fw_name = fw_name, 
                             S = S, L = L_real),
                  data.frame(acc_pri_ext = ADBM_ext_rand_raw$acc_pri_ext, acc_sec_ext = ADBM_ext_rand_raw$acc_sec_ext,
                             nsim = ADBM_ext_rand_raw$nsim, type = "ADBM", fw_name = fw_name,
                             S = S, L = L_ADBM))
  
  
  
  
  real_ext_rand <- random_ext_summary(ext_seq = real_ext_rand_raw)
  ADBM_ext_rand <- random_ext_summary(ext_seq = ADBM_ext_rand_raw)
  
  dd_rand <- rbind(data.frame(acc_pri_ext = real_ext_rand$acc_pri_ext,
                              mean_acc_sec_ext = real_ext_rand$mean_acc_sec_ext,
                              l_acc_sec_ext = real_ext_rand$l_acc_sec_ext,
                              u_acc_sec_ext = real_ext_rand$u_acc_sec_ext,
                              type = "Empirical",
                              fw_name = fw_name,
                              S = S,
                              L = L_real,
                              count_n = real_ext_rand$count_n),
                   data.frame(acc_pri_ext = ADBM_ext_rand$acc_pri_ext,
                              mean_acc_sec_ext = ADBM_ext_rand$mean_acc_sec_ext,
                              l_acc_sec_ext = ADBM_ext_rand$l_acc_sec_ext,
                              u_acc_sec_ext = ADBM_ext_rand$u_acc_sec_ext,
                              type = "ADBM",
                              fw_name = fw_name,
                              S = S,
                              L = L_ADBM,
                              count_n = ADBM_ext_rand$count_n),
                   dd_rand)
  print(fw_name)
}


# saveRDS(object = dd_rand, file = "results/random.RDS")
# saveRDS(object = dd_raw, file = "results/random_raw.RDS")