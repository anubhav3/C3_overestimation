# 22.10.2021
# We compute how the secondary extinctions vary when we remove random species in the food web predicted by ADBM

library(R.utils)
library(NetworkExtinction)
library(network)
library(readxl)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)

# For removing random nodes
dd_rand <- data.frame(mean_ext = double(),
                      CI_lower = double(),
                      CI_upper = double(),
                      n_ext = integer(),
                      type = character(),
                      fw_name = character(),
                      S = integer(),
                      L = integer())


ext_vs_n <- function(fw_name, fw_tol){
  
  real_fw <- readRDS(paste("../C1_method_v2/data/", fw_name, ".web.RDS", sep = ""))
  real_pred_mat <- real_fw$predation.matrix
  
  ADBM_fw_all <- readRDS(paste0("../C1_method_v2/results/rejection/", fw_name, "/rN=1000_tol=", fw_tol, "_TSS_lower_a/", fw_name, ".RDS"))
  
  sel_ind <- which.min(ADBM_fw_all$dist)
  ADBM_fw_par <- ADBM_fw_all$post_dists[sel_ind,]
  
  ## Generating the ADBM predicted food web
  fw_data <- real_fw
  
  n_species <- length(fw_data$species.sizes)
  
  M <- fw_data$species.sizes
  M <- sort(M)
  
  sim_a <- 10^ADBM_fw_par$a
  sim_ai <- ADBM_fw_par$ai
  sim_aj <- ADBM_fw_par$aj
  sim_r.b <- 10^ADBM_fw_par$r.b
  
  local_par <- data.frame(a = sim_a, ai = sim_ai, aj = sim_aj, r.b = sim_r.b)
  sim_model_core_par <- list(e = 1, n = 1, ni = -3/4, r.a = 1, M  = M)
  pred_mat <- ratio.power(opt=local_par, x = sim_model_core_par)
  rownames(pred_mat) <- as.character(seq(1, n_species))
  colnames(pred_mat) <- as.character(seq(1, n_species))
  
  ADBM_pred_mat <- pred_mat
  
  return(list(real_pred_mat = real_pred_mat,
              ADBM_pred_mat = ADBM_pred_mat))
}

# Getting the foodweb name and tolerance
metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/ADBM_2008_Petchey_par.xlsx")
n_fw <- dim(metadata_fw_tol)[1]
n_fw <- 11


for(i in 1:n_fw){
  fw_name <- metadata_fw_tol$foodweb[i]
  fw_tol <- metadata_fw_tol$dist_rej[i]
  
  fw_func_calc <- ext_vs_n(fw_name = fw_name, fw_tol = fw_tol)
  
  real_pred_mat <- fw_func_calc$real_pred_mat
  ADBM_pred_mat <- fw_func_calc$ADBM_pred_mat
  
  S <- dim(real_pred_mat)[1]
  L_real <- sum(real_pred_mat)
  L_ADBM <- sum(ADBM_pred_mat)
  
  real_conn <- sum(real_pred_mat)/(dim(real_pred_mat)[1]^2)
  ADBM_conn <- sum(ADBM_pred_mat)/(dim(ADBM_pred_mat)[1]^2)
  
  real_net <- as.network(real_pred_mat, loops = TRUE)
  ADBM_net <- as.network(ADBM_pred_mat, loops = TRUE)
  
  real_ext_rand <- RandomExtinctions(Network = real_net, nsim = 100, ncores = 5, parallel = TRUE)
  ADBM_ext_rand <- RandomExtinctions(Network = ADBM_net, nsim = 100, ncores = 5, parallel = TRUE)
  
  
  dd_rand <- rbind(data.frame(mean_ext = real_ext_rand$AccSecExt_mean,
                              CI_lower = real_ext_rand$Lower,
                              CI_upper = real_ext_rand$Upper,
                              n_ext = real_ext_rand$NumExt,
                              type = "Empirical",
                              fw_name = fw_name,
                              S = S,
                              L = L_real),
                   data.frame(mean_ext = ADBM_ext_rand$AccSecExt_mean,
                              CI_lower = ADBM_ext_rand$Lower,
                              CI_upper = ADBM_ext_rand$Upper,
                              n_ext = ADBM_ext_rand$NumExt,
                              type = "ADBM",
                              fw_name = fw_name,
                              S = S,
                              L = L_ADBM),
                   dd_rand)
  
  print(fw_name)
}

# saveRDS(object = dd_rand, file = "results/dd_rand.RDS")
