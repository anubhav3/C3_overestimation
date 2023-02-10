# 2022.08.30

library(cheddar)
library(dplyr)
library(R.utils)
library(ggplot2)
library(tidyr)
library(HDInterval)

sourceDirectory("../C1_method/R", modifiedOnly = FALSE)
sourceDirectory("R", modifiedOnly = FALSE)

dd_rand_raw <- readRDS("results/random_raw_maxTSS.RDS")

fw_list <- unique(dd_rand_raw$fw_name)
sim_ind <- unique(dd_rand_raw$nsim)
list_all <- expand.grid(nsim = sim_ind, fw_name = fw_list)
N <- dim(list_all)[1]

rob_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                      S = double())

Nsim <- 1000

metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")

for(foodweb in fw_list){
  
  #### Emprirical food web first ####
  dd_rand_emp <- dd_rand_raw %>%
    filter(fw_name == foodweb, type == "Empirical")
  
  dd_rand_emp <- dd_rand_emp %>%
    mutate(total_ext = (acc_pri_ext + acc_sec_ext)/S)
  
  pred_mat_emp <- readRDS(paste0("../C1_method_v2/data/", foodweb, ".web.RDS"))$predation.matrix
  conn_emp <- sum(pred_mat_emp)/(dim(pred_mat_emp)[1]^2)
  S <- dim(pred_mat_emp)[1]
  

  for(k in 1:Nsim){
    robustness <- NA
    dd_rand_emp_local <- dd_rand_emp %>%
      filter(nsim == k)
    
    N_row <- dim(dd_rand_emp_local)[1]
    for(i in 1:N_row){
      ith_row <- dd_rand_emp_local[i,]
      if(ith_row$total_ext >= 0.5){
        robustness <- ith_row$acc_pri_ext/ith_row$S
        # print(robustness)
        break
      }
    }
    
    robustness <- min(robustness, 0.5)
    
    rob_all <- rbind(rob_all,
                     data.frame(robustness = robustness, fw_name = foodweb, type = "Empirical", connectance = conn_emp,
                                S = S, nsim = k)
    )
    
  }
  
  
  #### ADBM food web second ####
  dd_rand_ADBM <- dd_rand_raw %>%
    filter(fw_name == foodweb, type == "ADBM")
  
  dd_rand_ADBM <- dd_rand_ADBM %>%
    mutate(total_ext = (acc_pri_ext + acc_sec_ext)/S)
  
  fw_tol <- metadata_fw_tol$dist_rej[which(metadata_fw_tol$foodweb == foodweb)]
  pred_mat_ADBM <- ADBM_ABC_fw_maxTSS(fw_name = foodweb, fw_tol = fw_tol)$ADBM_pred_mat
  conn_ADBM <- sum(pred_mat_ADBM)/(dim(pred_mat_ADBM)[1]^2)
  S <- dim(pred_mat_ADBM)[1]
  
  for(k in 1:Nsim){
    robustness <- NA
    dd_rand_ADBM_local <- dd_rand_ADBM %>%
      filter(nsim == k)
    
    N_row <- dim(dd_rand_ADBM_local)[1]
    for(i in 1:N_row){
      ith_row <- dd_rand_ADBM_local[i,]
      if(ith_row$total_ext >= 0.5){
        robustness <- ith_row$acc_pri_ext/ith_row$S
        # print(robustness)
        break
      }
    }
    
    robustness <- min(robustness, 0.5)
    
    rob_all <- rbind(rob_all,
                     data.frame(robustness = robustness, fw_name = foodweb, type = "ADBM", connectance = conn_ADBM,
                                S = S, nsim = k)
    )
    
  }
  
  print(foodweb)
  
}

# saveRDS(object = rob_all, file = "results/OP_2022_08_10/robustness_ra_max_TSS.RDS")


#### Median ####

rob_all_smry <- rob_all %>%
  group_by(fw_name, type, S) %>%
  summarise(median_rob = median(robustness), l_rob = as.numeric(hdi(robustness)[1]),
            r_rob = as.numeric(hdi(robustness)[2]))


rob_ra_maxTSS_median <- rob_all_smry %>%
  pivot_wider(names_from = type, values_from = c(median_rob, l_rob, r_rob)) %>%
  ggplot() +
  geom_point(aes(x = median_rob_Empirical, y = median_rob_ADBM, color = fw_name), size = 4) +
  geom_errorbar(aes(x = median_rob_Empirical, y = median_rob_ADBM, color = fw_name, ymin = l_rob_ADBM, ymax = r_rob_ADBM)) +
  geom_linerange(aes(x = median_rob_Empirical, y = median_rob_ADBM, color = fw_name, xmin = l_rob_Empirical, xmax = r_rob_Empirical),
                 position = position_jitter(height = 0.01)) +
  geom_abline(slope = 1, intercept = 0) +
  xlim(c(0, 0.51)) +
  ylim(c(0, 0.51)) +
  scale_color_brewer(type = "qual", palette = "Set3", name = "Food web") +
  theme_classic() +
  xlab("Empirical robustness (Median)") +
  ylab("ADBM robustness (Median)")
  

# ggsave(plot = rob_ra_maxTSS_median, filename = "results/OP_2022_08_10/plot_rob_ra_maxTSS_median.png")

#### Mean ####

rob_all_smry_mean <- rob_all %>%
  group_by(fw_name, type, S) %>%
  summarise(mean_rob = mean(robustness), l_rob = as.numeric(hdi(robustness)[1]),
            r_rob = as.numeric(hdi(robustness)[2]))


rob_ra_maxTSS_mean <- rob_all_smry_mean %>%
  pivot_wider(names_from = type, values_from = c(mean_rob, l_rob, r_rob)) %>%
  ggplot() +
  geom_point(aes(x = mean_rob_Empirical, y = mean_rob_ADBM, color = fw_name), size = 4) +
  geom_errorbar(aes(x = mean_rob_Empirical, y = mean_rob_ADBM, color = fw_name, ymin = l_rob_ADBM, ymax = r_rob_ADBM)) +
  geom_linerange(aes(x = mean_rob_Empirical, y = mean_rob_ADBM, color = fw_name, xmin = l_rob_Empirical, xmax = r_rob_Empirical),
                 position = position_jitter(height = 0.01)) +
  geom_abline(slope = 1, intercept = 0) +
  xlim(c(0, 0.51)) +
  ylim(c(0, 0.51)) +
  scale_color_brewer(type = "qual", palette = "Set3", name = "Food web") +
  theme_classic() +
  xlab("Empirical robustness (Mean)") +
  ylab("ADBM robustness (Mean)")


# ggsave(plot = rob_ra_maxTSS_mean, filename = "results/OP_2022_08_10/plot_rob_ra_maxTSS_mean.png")



minTSS %>%
  ggplot() +
  geom_histogram(aes(robustness)) +
  facet_wrap(~fw_name + type)
