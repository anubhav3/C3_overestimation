# 24.11.2021
# We investigate how the secondary extinctions vary when we remove the most connected species in the food web predicted by ADBM

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
  
  ADBM_ABC_fw_calc <- ADBM_ABC_fw(fw_name = fw_name, fw_tol = fw_tol)
  
  real_pred_mat <- ADBM_ABC_fw_calc$real_pred_mat
  if(is.null(rownames(real_pred_mat))){
    n_nodes <- dim(real_pred_mat)[1]
    rownames(real_pred_mat) <- as.character(1:n_nodes)
    colnames(real_pred_mat) <- as.character(1:n_nodes)
  }
  ADBM_ABC_pred_mat <- ADBM_ABC_fw_calc$ADBM_pred_mat
  ADBM_2008_pred_mat <- ADBM_2008_fw(fw_name = fw_name)$ADBM_pred_mat
  
  
  S <- dim(real_pred_mat)[1]
  L_real <- sum(real_pred_mat)
  L_ADBM_ABC <- sum(ADBM_ABC_pred_mat)
  L_ADBM_2008 <- sum(ADBM_2008_pred_mat)
  
  
  real_conn <- sum(real_pred_mat)/(dim(real_pred_mat)[1]^2)
  ADBM_ABC_conn <- sum(ADBM_ABC_pred_mat)/(dim(ADBM_ABC_pred_mat)[1]^2)
  ADBM_2008_conn <- sum(ADBM_2008_pred_mat)/(dim(ADBM_2008_pred_mat)[1]^2)
  
  # real_net <- as.network(real_pred_mat, loops = TRUE)
  # ADBM_ABC_net <- as.network(ADBM_ABC_pred_mat, loops = TRUE)
  # ADBM_2008_net <- as.network(ADBM_2008_pred_mat, loops = TRUE)
  
  real_ext_mc <- most_connected_ext(net = real_pred_mat)
  ADBM_ABC_ext_mc <- most_connected_ext(net = ADBM_ABC_pred_mat)
  ADBM_2008_ext_mc <- most_connected_ext(net = ADBM_2008_pred_mat)
  
  
  dd_mc <- rbind(data.frame(n_ext = real_ext_mc$acc_pri_ext,
                            acc_sec_ext = real_ext_mc$acc_sec_ext,
                            type = "Empirical",
                            fw_name = fw_name,
                            S = S,
                            L = L_real),
                 data.frame(n_ext = ADBM_2008_ext_mc$acc_pri_ext,
                            acc_sec_ext = ADBM_2008_ext_mc$acc_sec_ext,
                            type = "ADBM_2008",
                            fw_name = fw_name,
                            S = S,
                            L = L_ADBM_2008),
                 data.frame(n_ext = ADBM_ABC_ext_mc$acc_pri_ext,
                            acc_sec_ext = ADBM_ABC_ext_mc$acc_sec_ext,
                            type = "ADBM_ABC",
                            fw_name = fw_name,
                            S = S,
                            L = L_ADBM_ABC),
                 dd_mc)
  print(fw_name)
}


# saveRDS(object = dd_mc, file = "results/dd_mc_v2.RDS")
dd_mc <- readRDS(file = "results/dd_mc_v2.RDS")

###### Plotting secondary extinction

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream \n (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream \n (size aggregation)")

  
dd_mc_mod <- dd_mc %>%
  mutate(n_ext_by_S = n_ext/S, acc_sec_ext_by_S = acc_sec_ext/S)

plot_dd_mc_mod <- 
  dd_mc_mod %>%
  filter(type != "ADBM_2008") %>%
  ggplot() +
  geom_point(aes(x = n_ext_by_S, y = acc_sec_ext_by_S, color = type), size = 2, alpha = 0.8) +
  facet_wrap(~fw_name, scales = "free", labeller = labeller(fw_name = fw_labs), nrow = 2) +
  theme_classic() +
  xlab("Species removed/S") +
  ylab("Cumulative secondary extinction/S") +
  scale_color_manual(name = "Food web type", values = c("ADBM_ABC" = "red", "Empirical" = "blue"), 
                     labels = c("ADBM_ABC" = "ADBM food web",
                                "Empirical" = "Observed food web")) +
  theme(legend.position="bottom", legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

# ggsave(plot = plot_dd_mc_mod, filename = "results/plot_dd_mc_mod.png", width = 16, height = 8)

dd_mc_mod_2 <- dd_mc_mod %>%
  mutate(sum_ext  = n_ext_by_S + acc_sec_ext_by_S)

plot_dd_mc_mod_2 <- 
  dd_mc_mod_2 %>%
  filter(type != "ADBM_2008") %>%
  ggplot() +
  geom_point(aes(x = n_ext_by_S, y = sum_ext, color = type), size = 2) +
  facet_wrap(~fw_name) +
  theme_classic() +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Species removed/S") +
  ylab("Cumulative total extinction/S") +
  scale_color_discrete(name = "Type")

# ggsave(filename = "results/plot_dd_mc_mod_2.png", plot = plot_dd_mc_mod_2, width = 9, height = 6)
