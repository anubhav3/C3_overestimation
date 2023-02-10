# 2022.07.04
# We investigate how the robustness (R100: the number of species required to achieve a total loss of all the species) vary across food webs for most connected extinction scenario

library(cheddar)
library(R.utils)
library(ggplot2)
library(HDInterval)
library(readxl)
library(dplyr)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

dd_mc <- readRDS("results/most_connnected_uncertainty_new.RDS")

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
      if(ith_row$total_ext >= 1){
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
    print(nsim_local)
  }
  
  #### Emprirical food web second ####
  dd_mc_ADBM <- dd_mc %>%
    filter(fw_name == foodweb, type == "Empirical", nsim == 1)
  
  dd_mc_ADBM <- dd_mc_ADBM %>%
    mutate(total_ext = (n_ext + acc_sec_ext)/S)
  
  N_row <- dim(dd_mc_ADBM)[1]
  for(i in 1:N_row){
    ith_row <- dd_mc_ADBM[i,]
    if(ith_row$total_ext >= 1){
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
  
  print(foodweb)
}


# saveRDS(object = rob_all, file = "results/robustness_R100_mostconnected_uncertainty.RDS")




rob_all_raw1 <- rob_all

rob_all_raw1 <- rob_all_raw1 %>%
  mutate(study = "(a) Most connected")

rob_all_raw <- rob_all_raw1

rob_ADBM <- rob_all_raw %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness) %>%
  select(-c(robustness, connectance, type))


rob_obs <- rob_all_raw %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness) %>%
  select(-c(robustness, connectance, type)) %>%
  unique()

rob_all_merged <- merge(x = rob_ADBM, rob_obs, by = c("fw_name", "study")) %>%
  mutate(S = S.x, nsim = nsim.x) %>%
  select(-c(S.x, S.y, nsim.x, nsim.y))


rob_all_merged_mean <- rob_all_merged %>%
  group_by(fw_name, robustness_emp, study) %>%
  summarise(mean_robustness_ADBM = mean(robustness_ADBM), median_robustness_ADBM = median(robustness_ADBM))

rob_all_merged_mean <- rob_all_merged_mean %>%
  pivot_longer(!c(fw_name, robustness_emp, study), names_to = 'value_type', values_to = "values")


plot_MC_R100 <- rob_all_merged %>%
  filter(study == "(a) Most connected") %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.002, outlier.shape = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.0, 1.01)) +
  ylim(c(0.0, 1.01)) +
  geom_point(data = rob_all_merged_mean[rob_all_merged_mean$study == "(a) Most connected",], aes(x = robustness_emp, y = values, color = fw_name, shape = value_type), size = 3) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab("") +
  ylab("") +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Mean", "Median"), values = c(17, 18)) +
  ggtitle("(a) Most connected") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))


