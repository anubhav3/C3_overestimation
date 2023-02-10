# 2022.15.02

library(cheddar)
library(dplyr)
library(R.utils)
library(ggplot2)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

dd_rand_raw <- readRDS("results/random_raw.RDS")

fw_list <- unique(dd_rand_raw$fw_name)
sim_ind <- unique(dd_rand_raw$nsim)
list_all <- expand.grid(nsim = sim_ind, fw_name = fw_list)
N <- dim(list_all)[1]

rob_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                      S = double())

for(i in 1:N){
  
  row_sel <- list_all[i,]
  foodweb <- as.character(row_sel$fw_name)
  
  
  ###### ADBM_ABC #######
  type_fw = "ADBM"
  dd_fil <- dd_rand_raw %>%
    filter(fw_name == row_sel$fw_name, nsim == row_sel$nsim, type == type_fw) %>%
    mutate(acc_tot_ext = (acc_pri_ext + acc_sec_ext)/S)
  
  
  N_row <- dim(dd_fil)[1]
  for(i in 1:N_row){
    ith_row <- dd_fil[i,]
    if(ith_row$acc_tot_ext >= 0.5){
      robustness <- ith_row$acc_pri_ext/ith_row$S
      break
    }
  }
  
  pred_mat <- readRDS(paste0("data/", type_fw, "_predicted_foodwebs/", foodweb, ".pred_mat.RDS"))
  S <- dim(pred_mat)[1]
  conn <- sum(pred_mat)/(dim(pred_mat)[1]^2)
  
  rob_all <- rbind(rob_all,
                   data.frame(robustness = robustness, fw_name = foodweb, type = type_fw, connectance = conn,
                              S = S)
  )
  
  ###### Empirical #######
  type_fw = "Empirical"
  dd_fil <- dd_rand_raw %>%
    filter(fw_name == row_sel$fw_name, nsim == row_sel$nsim, type == type_fw) %>%
    mutate(acc_tot_ext = (acc_pri_ext + acc_sec_ext)/S)
  
  
  N_row <- dim(dd_fil)[1]
  for(i in 1:N_row){
    ith_row <- dd_fil[i,]
    if(ith_row$acc_tot_ext >= 0.5){
      robustness <- ith_row$acc_pri_ext/ith_row$S
      break
    }
  }
  
  pred_mat <- readRDS(paste0("../C1_method_v2/data/", foodweb, ".web.RDS"))$predation.matrix
  S <- dim(pred_mat)[1]
  conn <- sum(pred_mat)/(dim(pred_mat)[1]^2)
  
  rob_all <- rbind(rob_all,
                   data.frame(robustness = robustness, fw_name = foodweb, type = type_fw, connectance = conn,
                              S = S)
  )
  
}

# saveRDS(object = rob_all, file = "results/robustness_random.RDS")

# rob_all <- readRDS("results/robustness_random.RDS")

rob_all_mean <- rob_all %>%
  group_by(fw_name, type, connectance, S) %>%
  summarise(mean_rob = mean(robustness))

########## Plotting robustness ##########

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

plot_rob_all <- 
  rob_all_mean %>%
  ggplot() +
  geom_line(aes(x = connectance, y = mean_rob, color = fw_name)) +
  geom_point(aes(x = connectance, y = mean_rob, shape = type, color = fw_name), size = 5) +
  theme_classic() +
  scale_color_brewer(type = "qual", name = "Food web", palette = "Paired", labels = fw_labs) +
  scale_shape_discrete(name = "Type", labels = c("ADBM_ABC" = "ADBM food web", "Empirical" = "Observed food web")) +
  scale_x_log10() +
  ylab("Robustness") +
  xlab("Connectance") +
  ylim(c(0, 0.53)) +
  theme(legend.position = "bottom")

# saveRDS(object = plot_rob_all, file = "results/ggplot_rob_random.RDS")


# ggsave(plot = plot_rob_all, filename = "results/plot_robustness_random.png", width = 11, height = 6)




