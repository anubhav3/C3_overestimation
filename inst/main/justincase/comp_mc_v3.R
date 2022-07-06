# 13.12.2021
# Modified version of comp_mc_v2.R
# We consider all the 1000 predicted food webs from ADBM

library(R.utils)
library(NetworkExtinction)
library(network)
library(readxl)
library(dplyr)
library(ggplot2)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

# For removing most connected nodes
dd_mc <- data.frame(n_ext = double(),
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
  real_ext_mc <- most_connected_ext(net = real_pred_mat)
  
  dd_mc <- rbind(dd_mc, 
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
    ADBM_ABC_ext_mc <- most_connected_ext(net = ADBM_ABC_pred_mat)
    
    dd_mc <- rbind(dd_mc,
                   data.frame(n_ext = ADBM_ABC_ext_mc$acc_pri_ext,
                              acc_sec_ext = ADBM_ABC_ext_mc$acc_sec_ext,
                              nsim = j,
                              type = "ADBM_ABC",
                              fw_name = fw_name,
                              S = S,
                              L = L_ADBM_ABC))
  }
  print(fw_name)
}


saveRDS(object = dd_mc, file = "results/dd_mc_v3.RDS")
# dd_mc <- readRDS(file = "results/old/dd_mc_v3.RDS")

###### Plotting secondary extinction

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream \n (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream \n (size aggregation)")

dd_mc_mod <- dd_mc %>%
  mutate(n_ext_by_S = n_ext/S, acc_sec_ext_by_S = acc_sec_ext/S)

dd_mc_mod <- dd_mc_mod %>%
  group_by(n_ext_by_S, type, fw_name, S) %>%
  summarise(mean_acc_sec_ext_by_S = mean(acc_sec_ext_by_S), l_acc_sec_ext_by_S = min(acc_sec_ext_by_S),
            u_acc_sec_ext_by_S = max(acc_sec_ext_by_S))


plot_dd_mc_mod <- ggplot(dd_mc_mod) +
  geom_point(aes(x = n_ext_by_S, y = mean_acc_sec_ext_by_S, color = type), size = 2) +
  geom_ribbon(aes(x = n_ext_by_S, ymin = l_acc_sec_ext_by_S, ymax = u_acc_sec_ext_by_S, color = type), alpha = 0.3) +
  facet_wrap(~fw_name, scales = "free") +
  theme_classic() +
  xlab("Species removed/S") +
  ylab("Cumulative secondary extinction/S") +
  scale_color_discrete(name = "Type")
