# 13.10.2021
# We simulate the best predicted (as per TSS) food web by the ADBM

library(readxl)

# Getting the foodweb name and tolerance
metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
n_fw <- dim(metadata_fw_tol)[1]


for(i in 1:n_fw){
  fw_name <- metadata_fw_tol$foodweb[i]
  fw_tol <- metadata_fw_tol$dist_rej[i]
  
  fw_func_ABC_calc <- ADBM_ABC_fw(fw_name = fw_name, fw_tol = fw_tol)
  
  # saveRDS(object = fw_func_ABC_calc$ADBM_pred_mat, file =
  #           paste0("data/ADBM_ABC_predicted_foodwebs/", fw_name, ".pred_mat.RDS"))
  
  fw_func_2008_calc <- ADBM_2008_fw(fw_name = fw_name)
  
  # saveRDS(object = fw_func_2008_calc$ADBM_pred_mat, file =
  #           paste0("data/ADBM_2008_predicted_foodwebs/", fw_name, ".pred_mat.RDS"))
  
}


