# 25.11.2021

library(cheddar)
sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

dd_rand_raw <- readRDS("results/dd_rand_raw.RDS")

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
  type_fw = "ADBM_ABC"
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
  
  ###### ADBM_2008 #######
  type_fw = "ADBM_2008"
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

# saveRDS(object = rob_all, file = "results/rand_rob_all.RDS")

rob_all_mean <- rob_all %>%
  group_by(fw_name, type, connectance, S) %>%
  summarise(mean_rob = mean(robustness))


rob_all_mean %>%
  # filter(type != "ADBM_2008") %>%
  ggplot() +
  # geom_line() +
  geom_point(aes(x = connectance, y = mean_rob), size = 5) +
  geom_smooth(aes(x = connectance, y = mean_rob), method = "lm") +
  theme_classic() +
  scale_x_log10()


lm_rob_rand <- lm(mean_rob ~ connectance, data = rob_all_mean)
summary(lm_rob_rand)
anova(lm_rob_rand)
