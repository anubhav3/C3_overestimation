# 2022.07.13

library(cheddar)
library(R.utils)
library(ggplot2)
library(HDInterval)
library(readxl)
library(dplyr)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

dd_rand <- readRDS("results/random_raw_uncertainty_july11.RDS")

fw_list <- unique(dd_rand$fw_name)
rob_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                      S = integer(), nsim = integer())
Nsim <- 100

metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")

for(foodweb in fw_list){
  
  for(nsim_local in 1:1000){
    
    #### ADBM ABC predicted food web first ####
    dd_rand_ADBM_ABC <- dd_rand %>%
      filter(fw_name == foodweb, type == "ADBM", nsim == nsim_local) %>%
      mutate(total_ext = (n_ext + acc_sec_ext)/S)
    
    robustness <- 0
    for(k in 1:Nsim){
      dd_rand_ADBM_ABC_local <- dd_rand_ADBM_ABC %>%
        filter(nsim_rand == k)
      
      N_row <- dim(dd_rand_ADBM_ABC_local)[1]
      for(i in 1:N_row){
        ith_row <- dd_rand_ADBM_ABC_local[i,]
        if(ith_row$total_ext >= 1){
          robustness <- robustness + ith_row$n_ext/ith_row$S
          break
        }
      }
    }
    
    robustness <- robustness/Nsim
    
    fw_tol <- metadata_fw_tol$dist_rej[which(metadata_fw_tol$foodweb == foodweb)]
    pred_mat_ADBM_ABC <- ADBM_ABC_fw_all(fw_name = foodweb, fw_tol = fw_tol, nsim = nsim_local)$ADBM_pred_mat
    S <- dim(pred_mat_ADBM_ABC)[1]
    conn_ADBM_ABC <- sum(pred_mat_ADBM_ABC)/(dim(pred_mat_ADBM_ABC)[1]^2)
    rob_all <- rbind(rob_all,
                     data.frame(robustness = robustness, fw_name = foodweb, type = "ADBM_ABC", connectance = conn_ADBM_ABC,
                                S = S, nsim = nsim_local)
    )
    
    
    
    #### Emprirical food web second ####
    dd_rand_ADBM <- dd_rand %>%
      filter(fw_name == foodweb, type == "Empirical", nsim == 1)
    
    dd_rand_ADBM <- dd_rand_ADBM %>%
      mutate(total_ext = (n_ext + acc_sec_ext)/S)
    
    N_row <- dim(dd_rand_ADBM)[1]
    for(i in 1:N_row){
      ith_row <- dd_rand_ADBM[i,]
      if(ith_row$total_ext >= 1){
        robustness <- ith_row$n_ext/ith_row$S
        print(robustness)
        break
      }
    }
    
    pred_mat_emp <- readRDS(paste0("../C1_method_v2/data/", foodweb, ".web.RDS"))$predation.matrix
    conn_emp <- sum(pred_mat_emp)/(dim(pred_mat_emp)[1]^2)
    # intervality_emp <- intervality(pred_mat_emp)
    rob_all <- rbind(rob_all,
                     data.frame(robustness = robustness, fw_name = foodweb, type = "Empirical", connectance = conn_emp,
                                S = S, fnsim = 1)
    )
    print(nsim_local)  
  }
  print(foodweb)
}


# saveRDS(object = rob_all, file = "results/robustness_R100_random_uncertainty_july11.RDS")

