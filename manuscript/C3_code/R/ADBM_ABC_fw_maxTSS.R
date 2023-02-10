# Returns ADBM predicted food web with max TSS

ADBM_ABC_fw_maxTSS <- function(fw_name, fw_tol){
  
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