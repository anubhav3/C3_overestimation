# 2022.07.22
# Investigate ADBM predicted and observed food webs corresponding to random extinction scenario

library(R.utils)
library(cheddar)
sourceDirectory("../C1_method_v2/R", modifiedOnly = FALSE)

fw_name_loc <- "Tuesday Lake"
fw_tol_loc <- 0.65
  
fw_pred <- ADBM_ABC_fw(fw_name = fw_name_loc, fw_tol = fw_tol_loc)

fw_ADBM <- fw_pred$ADBM_pred_mat

fw_real <- fw_pred$real_pred_mat


fw_ADBM_comm <- mat.to.comm(pred.mat = fw_ADBM, fw_title = fw_name_loc)

fw_real_comm <- mat.to.comm(pred.mat = fw_real, fw_title = fw_name_loc)


PlotWebByLevel(community = fw_ADBM_comm)

PlotWebByLevel(community = fw_real_comm)
