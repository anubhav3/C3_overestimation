# 2022.11.28
# Calculate proportion of basal species

prop_basal_ADBM_fun <- function(foodweb){
  
  fw <- foodweb
  prop_basal_ADBM_all <- c()
  
  library(R.utils)
  library(readxl)
  library(cheddar)
  
  sourceDirectory("R", modifiedOnly = FALSE)
  sourceDirectory("../C1_method_v2/R", modifiedOnly = FALSE)
  
  metadata <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
  fw_tol_loc <- metadata$dist_rej[metadata$foodweb == fw]
  
  for(nsim_loc in 1:1000){
    print(nsim_loc)
    pred_mat <- ADBM_ABC_fw_all(fw_name = fw, fw_tol = fw_tol_loc, nsim = nsim_loc)
    
    pred_mat_ADBM <- pred_mat$ADBM_pred_mat
    
    comm_ADBM <- mat.to.comm(pred.mat = pred_mat_ADBM, fw_title = fw)
    
    prop_basal_ADBM <- FractionBasalNodes(comm_ADBM)
    
    prop_basal_ADBM_all <- c(prop_basal_ADBM_all, prop_basal_ADBM)
  }
  
  return(prop_basal_ADBM_all)
}
