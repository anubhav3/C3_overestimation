ADBM_2008_fw <- function(fw_name){
  
  real_fw <- readRDS(paste("../C1_method_v2/data/", fw_name, ".web.RDS", sep = ""))
  real_pred_mat <- real_fw$predation.matrix
  
  ADBM_fw_all <- read_excel("../C1_method_v2/data/parameter_values.xlsx")
  
  ADBM_fw_par <- ADBM_fw_all[ADBM_fw_all$foodweb == fw_name,]
  
  ## Generating the ADBM predicted food web
  fw_data <- real_fw
  
  n_species <- length(fw_data$species.sizes)
  
  M <- fw_data$species.sizes
  M <- sort(M)
  
  sim_a <- ADBM_fw_par$a
  sim_ai <- ADBM_fw_par$ai
  sim_aj <- ADBM_fw_par$aj
  sim_r.b <- ADBM_fw_par$r.b
  
  local_par <- data.frame(a = sim_a, ai = sim_ai, aj = sim_aj, r.b = sim_r.b)
  sim_model_core_par <- list(e = 1, n = 1, ni = -3/4, r.a = 1, M  = M)
  pred_mat <- ratio.power(opt=local_par, x = sim_model_core_par)
  rownames(pred_mat) <- as.character(seq(1, n_species))
  colnames(pred_mat) <- as.character(seq(1, n_species))
  
  ADBM_pred_mat <- pred_mat
  
  return(list(real_pred_mat = real_pred_mat,
              ADBM_pred_mat = ADBM_pred_mat))
}