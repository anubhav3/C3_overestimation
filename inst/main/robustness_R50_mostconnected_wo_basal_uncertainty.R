# 2022.07.29

# Compute robustness (R50) of 

library(cheddar)
library(R.utils)
library(ggplot2)
library(HDInterval)
library(readxl)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

dd_mc <- readRDS("results/most_connnected_wo_basal_uncertainty_new.RDS")

fw_list <- unique(dd_mc$fw_name)
rob_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                      S = integer(), S_wo_basal = integer(), nsim = integer())

metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")

for(foodweb in fw_list){
  
  #### Emprirical food web first ####
  dd_mc_emp <- dd_mc %>%
    filter(fw_name == foodweb, type == "Empirical", nsim == 1)
  
  dd_mc_emp <- dd_mc_emp %>%
    mutate(total_ext = (n_ext + acc_sec_ext)/S_wo_basal)
  
  robustness <- NA
  
  N_row <- dim(dd_mc_emp)[1]
  for(i in 1:N_row){
    ith_row <- dd_mc_emp[i,]
    if(ith_row$total_ext >= 0.5){
      robustness <- ith_row$n_ext/ith_row$S_wo_basal
      break
    }
  }
  
  robustness <- min(robustness, 0.5)
  
  pred_mat_emp <- readRDS(paste0("../C1_method_v2/data/", foodweb, ".web.RDS"))$predation.matrix
  conn_emp <- sum(pred_mat_emp)/(dim(pred_mat_emp)[1]^2)
  S <- dim(pred_mat_emp)[1]
  S_wo_basal <- dd_mc_emp$S_wo_basal[1]
  rob_all <- rbind(rob_all,
                   data.frame(robustness = robustness, fw_name = foodweb, type = "Empirical", connectance = conn_emp,
                              S = S, S_wo_basal = S_wo_basal, nsim = 1)
  )
  
  for(nsim_local in 1:1000){
    
    #### ADBM ABC predicted food web second ####
    dd_mc_ADBM_ABC <- dd_mc %>%
      filter(fw_name == foodweb, type == "ADBM", nsim == nsim_local) %>%
      mutate(total_ext = (n_ext + acc_sec_ext)/S_wo_basal)
    
    robustness <- NA
      
    N_row <- dim(dd_mc_ADBM_ABC)[1]
    for(i in 1:N_row){
      ith_row <- dd_mc_ADBM_ABC[i,]
      if(ith_row$total_ext >= 0.5){
        robustness <- ith_row$n_ext/ith_row$S_wo_basal
        break
      }
    }
    robustness <- min(robustness, 0.5)
    
    fw_tol <- metadata_fw_tol$dist_rej[which(metadata_fw_tol$foodweb == foodweb)]
    pred_mat_ADBM_ABC <- ADBM_ABC_fw_all(fw_name = foodweb, fw_tol = fw_tol, nsim = nsim_local)$ADBM_pred_mat
    S <- dim(pred_mat_ADBM_ABC)[1]
    S_wo_basal <- dd_mc_ADBM_ABC$S_wo_basal[1]
    conn_ADBM_ABC <- sum(pred_mat_ADBM_ABC)/(dim(pred_mat_ADBM_ABC)[1]^2)
    # intervality_ADBM_ABC <- intervality(pred_mat_ADBM_ABC)
    rob_all <- rbind(rob_all,
                     data.frame(robustness = robustness, fw_name = foodweb, type = "ADBM_ABC", connectance = conn_ADBM_ABC,
                                S = S, S_wo_basal = S_wo_basal, nsim = nsim_local)
    )
    print(nsim_local)  
  }
  print(foodweb)
}


# saveRDS(object = rob_all, file = "results/robustness_R50_mostconnected_wo_basal_uncertainty.RDS")


