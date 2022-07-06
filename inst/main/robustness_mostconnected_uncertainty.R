# 2022.02.24

library(cheddar)
library(R.utils)
library(ggplot2)
library(HDInterval)
library(readxl)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

dd_mc <- readRDS("results/most_connnected_uncertainty.RDS")

fw_list <- unique(dd_mc$fw_name)
rob_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                      S = integer(), nsim = integer())

metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")

for(foodweb in fw_list){
  
  for(nsim_local in 1:1000){
    
    #### ADBM ABC predicted food web first ####
    dd_mc_ADBM_ABC <- dd_mc %>%
      filter(fw_name == foodweb, type == "ADBM", nsim == nsim_local) %>%
      mutate(total_ext = (n_ext + acc_sec_ext)/S)
    
    N_row <- dim(dd_mc_ADBM_ABC)[1]
    for(i in 1:N_row){
      ith_row <- dd_mc_ADBM_ABC[i,]
      if(ith_row$total_ext >= 0.5){
        robustness <- ith_row$n_ext/ith_row$S
        break
      }
    }
    
    fw_tol <- metadata_fw_tol$dist_rej[which(metadata_fw_tol$foodweb == foodweb)]
    pred_mat_ADBM_ABC <- ADBM_ABC_fw_all(fw_name = foodweb, fw_tol = fw_tol, nsim = nsim_local)$ADBM_pred_mat
    S <- dim(pred_mat_ADBM_ABC)[1]
    conn_ADBM_ABC <- sum(pred_mat_ADBM_ABC)/(dim(pred_mat_ADBM_ABC)[1]^2)
    # intervality_ADBM_ABC <- intervality(pred_mat_ADBM_ABC)
    rob_all <- rbind(rob_all,
                     data.frame(robustness = robustness, fw_name = foodweb, type = "ADBM_ABC", connectance = conn_ADBM_ABC,
                                S = S, nsim = nsim_local)
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
    # intervality_emp <- intervality(pred_mat_emp)
    rob_all <- rbind(rob_all,
                     data.frame(robustness = robustness, fw_name = foodweb, type = "Empirical", connectance = conn_emp,
                                S = S, nsim = 1)
    )
    print(nsim_local)  
  }
  print(foodweb)
}


# saveRDS(object = rob_all, file = "results/robustness_mostconnected_uncertainty.RDS")


rob_all_raw1 <- readRDS("results/robustness_leastconnected_uncertainty.RDS")
rob_all_raw2 <- readRDS("results/robustness_random_uncertainty.RDS")
rob_all_raw3 <- readRDS("results/robustness_mostconnected_uncertainty.RDS")

rob_all_raw1 <- rob_all_raw1 %>%
  mutate(study = "(a) Least connected")

rob_all_raw2 <- rob_all_raw2 %>%
  mutate(study = "(b) Random")

rob_all_raw3 <- rob_all_raw3 %>%
  mutate(study = "(c) Most connected")

rob_all_raw <- rbind(rob_all_raw1, rob_all_raw2, rob_all_raw3)

rob_all_raw <- rob_all_raw %>%
  mutate(robustness = ifelse(robustness > 0.5, 0.5, robustness))

library(raincloudplots)


rob_all_rain <- rob_all_raw %>%
  group_by(fw_name, type, S, study) %>%
  summarise(mean_connectance = mean(connectance), robustness = robustness, mean_robustness = mean(robustness))

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

plot_rob_all_rain <- ggplot(rob_all_rain) +
  # ggdist::stat_slab(aes(x = mean_connectance, y = robustness, fill = fw_name), width = 1.5, point_colour = NA,
                       # alpha = 1.5) +
  geom_boxplot(aes(x = mean_connectance, y = robustness, group = mean_connectance), width = 0.003) +
  geom_point(aes(x = mean_connectance, y = mean_robustness, shape = type, color = fw_name), size = 2) +
  geom_line(aes(x = mean_connectance, y = mean_robustness, color = fw_name)) +
  facet_wrap(~study, scale = "free_y", nrow = 3) +
  # scale_x_log10() +
  ylab("Robustness") +
  xlab("Connectance") +
  theme(legend.position = "bottom") +
  scale_shape_discrete(name = "Type", labels = c("ADBM_ABC" = "ADBM food web", "Empirical" = "Observed food web")) +
  scale_color_brewer(type = "qual", name = "Food web", palette = "Paired", labels = fw_labs) +
  scale_fill_brewer(type = "qual", name = "Food web", palette = "Paired", labels = fw_labs) +
  theme_classic() +
  theme(legend.position = "right")

# ggsave(plot = plot_rob_all_rain, filename = "results/plot_robustness_all_uncertainty.png", width = 9, height = 9)


