# 2022.09.02
# We investigate how the robustness vary across food webs

library(cheddar)
library(dplyr)
library(R.utils)
library(ggplot2)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

dd_lc <- readRDS("results/least_connected.RDS")

fw_list <- unique(dd_lc$fw_name)
rob_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                      S = double(), intervality = double())

for(foodweb in fw_list){
  
  #### ADBM ABC predicted food web ####
  dd_lc_ADBM_ABC <- dd_lc %>%
    filter(fw_name == foodweb, type == "ADBM_ABC") %>%
    mutate(total_ext = (n_ext + acc_sec_ext)/S)
  
  N_row <- dim(dd_lc_ADBM_ABC)[1]
  for(i in 1:N_row){
    ith_row <- dd_lc_ADBM_ABC[i,]
    if(ith_row$total_ext >= 0.5){
      robustness <- ith_row$n_ext/ith_row$S
      break
    }
  }
  
  pred_mat_ADBM_ABC <- readRDS(paste0("data/ADBM_predicted_foodwebs/", foodweb, ".pred_mat.RDS"))
  S <- dim(pred_mat_ADBM_ABC)[1]
  conn_ADBM_ABC <- sum(pred_mat_ADBM_ABC)/(dim(pred_mat_ADBM_ABC)[1]^2)
  intervality_ADBM_ABC <- intervality(pred_mat_ADBM_ABC)
  rob_all <- rbind(rob_all,
                   data.frame(robustness = robustness, fw_name = foodweb, type = "ADBM", connectance = conn_ADBM_ABC,
                              S = S, intervality = intervality_ADBM_ABC)
  )
  
  #### Empirical food web ####
  dd_lc_ADBM <- dd_lc %>%
    filter(fw_name == foodweb, type == "Empirical")
  
  dd_lc_ADBM <- dd_lc_ADBM %>%
    mutate(total_ext = (n_ext + acc_sec_ext)/S)
  
  N_row <- dim(dd_lc_ADBM)[1]
  for(i in 1:N_row){
    ith_row <- dd_lc_ADBM[i,]
    if(ith_row$total_ext >= 0.5){
      robustness <- ith_row$n_ext/ith_row$S
      break
    }
  }
  
  pred_mat_emp <- readRDS(paste0("../C1_method_v2/data/", foodweb, ".web.RDS"))$predation.matrix
  conn_emp <- sum(pred_mat_emp)/(dim(pred_mat_emp)[1]^2)
  intervality_emp <- intervality(pred_mat_emp)
  rob_all <- rbind(rob_all,
                   data.frame(robustness = robustness, fw_name = foodweb, type = "Empirical", connectance = conn_emp,
                              S = S, intervality = intervality_emp)
  )
}

# saveRDS(object = rob_all, file = "results/robustness_leastconnected.RDS")

# rob_all <- readRDS(file = "results/robustness_leastconnected.RDS")



########## Plotting robustness ##########


fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

plot_rob_all <- 
  rob_all %>%
  ggplot() +
  geom_line(aes(x = connectance, y = robustness, color = fw_name)) +
  geom_point(aes(x = connectance, y = robustness, shape = type, color = fw_name), size = 5) +
  theme_classic() +
  scale_color_brewer(type = "qual", name = "Food web", palette = "Paired", labels = fw_labs) +
  scale_shape_discrete(name = "Type", labels = c("ADBM" = "ADBM food web", "Empirical" = "Observed food web")) +
  scale_x_log10() +
  ylab("Robustness") +
  xlab("Connectance") +
  ylim(c(0, 0.53)) +
  theme(legend.position = "bottom")

# saveRDS(object = plot_rob_all, file = "results/ggplot_rob_leastconnected.RDS")

# ggsave(plot = plot_rob_all, filename = "results/plot_robustness_leastconnected.png", width = 11, height = 6)




