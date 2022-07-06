# 14.12.2021
# We investigate how the robustness vary across food webs 
# We use the output produced by using the script comp_mc_v3.R

library(cheddar)
library(R.utils)
library(ggplot2)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

dd_mc <- readRDS("results/dd_mc_v3.RDS")

fw_list <- unique(dd_mc$fw_name)
rob_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                      S = double(), intervality = double(), nsim = integer())

for(foodweb in fw_list){
  
  for(nsim_local in 1:1000){
    
    #### ADBM ABC predicted food web first ####
    dd_mc_ADBM_ABC <- dd_mc %>%
      filter(fw_name == foodweb, type == "ADBM_ABC", nsim == nsim_local) %>%
      mutate(total_ext = (n_ext + acc_sec_ext)/S)
    
    N_row <- dim(dd_mc_ADBM_ABC)[1]
    for(i in 1:N_row){
      ith_row <- dd_mc_ADBM_ABC[i,]
      if(ith_row$total_ext >= 0.5){
        robustness <- ith_row$n_ext/ith_row$S
        break
      }
    }
    
    pred_mat_ADBM_ABC <- readRDS(paste0("data/ADBM_ABC_predicted_foodwebs/", foodweb, ".pred_mat.RDS"))
    S <- dim(pred_mat_ADBM_ABC)[1]
    conn_ADBM_ABC <- sum(pred_mat_ADBM_ABC)/(dim(pred_mat_ADBM_ABC)[1]^2)
    intervality_ADBM_ABC <- intervality(pred_mat_ADBM_ABC)
    rob_all <- rbind(rob_all,
                     data.frame(robustness = robustness, fw_name = foodweb, type = "ADBM_ABC", connectance = conn_ADBM_ABC,
                                S = S, intervality = intervality_ADBM_ABC, nsim = nsim_local)
    )
    
    
    
    #### Emprirical food web second ####
    dd_mc_ADBM <- dd_mc %>%
      filter(fw_name == foodweb, type == "Empirical", nsim == 1)
    
    dd_mc_ADBM <- dd_mc_ADBM %>%
      mutate(total_ext = (n_ext + acc_sec_ext)/S)
    
    N_row <- dim(dd_mc_ADBM)[1]
    for(i in 1:N_row){
      ith_row <- dd_mc_ADBM[i,]
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
                                S = S, intervality = intervality_emp, nsim = 1)
    )
  print(nsim_local)  
  }
  print(fw_name)
}

# saveRDS(object = rob_all, file = "results/rob_mc_all_v3.RDS")

##### Plotting robustness #####

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")


rob_all_mean <- rob_all %>%
  group_by(fw_name, type) %>%
  summarise(mean_robustness = mean(robustness))

rob_all_mean %>%
  ggplot() +
  # geom_line(aes(x = connectance, y = robustness, color = fw_name)) +
  geom_point(aes(x = fw_name, y = mean_robustness, color = type), size = 5, position = position_dodge(width = 0.1)) +
  theme_classic() +
  scale_color_manual(name = "Type", labels = c("ADBM_ABC" = "ADBM food web", "Empirical" = "Observed food web"), 
                     values = c("ADBM_ABC" = "red", "Empirical" = "blue")) +
  scale_x_discrete(labels = fw_labs) +
  ylab("Robustness") +
  xlab("Food web") +
  theme(legend.position = "bottom")


