# 22.10.2021
# We compute how the secondary extinctions vary when we remove the most connected species in the food web predicted by ADBM

library(R.utils)
library(NetworkExtinction)
library(network)
library(readxl)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

# For removing most connected nodes
dd_mc <- data.frame(sec_ext = double(),
                    n_ext = integer(),
                    type = character(),
                    fw_name = character(),
                    S = integer(),
                    L = integer())


# Getting the foodweb name and tolerance
metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/ADBM_2008_Petchey_par.xlsx")
n_fw <- dim(metadata_fw_tol)[1]
n_fw <- 11
fw_ind <- c(1:7, 9, 10, 11)


for(i in fw_ind){
  fw_name <- metadata_fw_tol$foodweb[i]
  fw_tol <- metadata_fw_tol$dist_rej[i]
  
  ADBM_ABC_fw_calc <- ADBM_ABC_fw(fw_name = fw_name, fw_tol = fw_tol)
  
  real_pred_mat <- ADBM_ABC_fw_calc$real_pred_mat
  ADBM_ABC_pred_mat <- ADBM_ABC_fw_calc$ADBM_pred_mat
  ADBM_2008_pred_mat <- ADBM_2008_fw(fw_name = fw_name)$ADBM_pred_mat
  
  
  S <- dim(real_pred_mat)[1]
  L_real <- sum(real_pred_mat)
  L_ADBM_ABC <- sum(ADBM_ABC_pred_mat)
  L_ADBM_2008 <- sum(ADBM_2008_pred_mat)
  
  
  real_conn <- sum(real_pred_mat)/(dim(real_pred_mat)[1]^2)
  ADBM_ABC_conn <- sum(ADBM_ABC_pred_mat)/(dim(ADBM_ABC_pred_mat)[1]^2)
  ADBM_2008_conn <- sum(ADBM_2008_pred_mat)/(dim(ADBM_2008_pred_mat)[1]^2)
  
  real_net <- as.network(real_pred_mat, loops = TRUE)
  ADBM_ABC_net <- as.network(ADBM_ABC_pred_mat, loops = TRUE)
  ADBM_2008_net <- as.network(ADBM_2008_pred_mat, loops = TRUE)
  
  real_ext_mc <- SimulateExtinctions(Network = real_net, Method = "Mostconnected")
  ADBM_ABC_ext_mc <- SimulateExtinctions(Network = ADBM_ABC_net, Method = "Mostconnected")
  ADBM_2008_ext_mc <- SimulateExtinctions(Network = ADBM_2008_net, Method = "Mostconnected")
  
  
  dd_mc <- rbind(data.frame(sec_ext = real_ext_mc$AccSecExt,
                            n_ext = real_ext_mc$NumExt,
                            type = "Empirical",
                            fw_name = fw_name,
                            S = S,
                            L = L_real),
                 data.frame(sec_ext = ADBM_ABC_ext_mc$AccSecExt,
                            n_ext = ADBM_ABC_ext_mc$NumExt,
                            type = "ADBM_ABC",
                            fw_name = fw_name,
                            S = S,
                            L = L_ADBM_ABC),
                 data.frame(sec_ext = ADBM_2008_ext_mc$AccSecExt,
                            n_ext = ADBM_2008_ext_mc$NumExt,
                            type = "ADBM_2008",
                            fw_name = fw_name,
                            S = S,
                            L = L_ADBM_2008),
                 dd_mc)
  print(fw_name)
}


# saveRDS(object = dd_mc, file = "results/dd_mc.RDS")

# dd_mc <- dd_mc %>%
#   filter(fw_name != "Broadstone Stream")
