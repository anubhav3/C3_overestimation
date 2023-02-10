## 2022.09.06
## Calculate food web properties: Max trophic level


max_tl_ADBM_fun <- function(foodweb){
  
  fw <- foodweb
  max_tl_ADBM_all <- c()
  
  library(R.utils)
  library(readxl)
  library(cheddar)
  
  sourceDirectory("R", modifiedOnly = FALSE)
  sourceDirectory("../C1_method_v2/R", modifiedOnly = FALSE)

  metadata <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
  fw_tol_loc <- metadata$dist_rej[metadata$foodweb == fw]
  
  for(nsim_loc in 1:1000){
    pred_mat <- ADBM_ABC_fw_all(fw_name = fw, fw_tol = fw_tol_loc, nsim = nsim_loc)
    
    pred_mat_ADBM <- pred_mat$ADBM_pred_mat
    
    comm_ADBM <- mat.to.comm(pred.mat = pred_mat_ADBM, fw_title = fw)
    
    max_tl_ADBM <- max(PreyAveragedTrophicLevel(community = comm_ADBM))
    
    max_tl_ADBM_all <- c(max_tl_ADBM_all, max_tl_ADBM)
  }
  
  return(max_tl_ADBM_all)
}


max_tl_emp_fun <- function(foodweb){
  
  fw <- foodweb
  
  library(R.utils)
  library(readxl)
  library(cheddar)
  
  sourceDirectory("R", modifiedOnly = FALSE)
  sourceDirectory("../C1_method_v2/R", modifiedOnly = FALSE)
  
  metadata <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
  fw_tol_loc <- metadata$dist_rej[metadata$foodweb == fw]
  
  for(nsim_loc in 1:1){
    pred_mat <- ADBM_ABC_fw_all(fw_name = fw, fw_tol = fw_tol_loc, nsim = nsim_loc)
    
    pred_mat_emp <- pred_mat$real_pred_mat
    
    comm_emp <- mat.to.comm(pred.mat = pred_mat_emp, fw_title = fw)
    
    max_tl_emp <- max(PreyAveragedTrophicLevel(community = comm_emp))
    
  }
  
  return(max_tl_emp)
}


library(readxl)


#### ADBM predicted food webs ####

max_tl_ADBM_df <- data.frame(fw_name = character(), max_tl_ADBM = double(), nsim = integer())

fw_all <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")$foodweb

for(fw in fw_all){
  max_tl_ADBM = max_tl_ADBM_fun(foodweb = fw)
  
  max_tl_ADBM_df <- rbind(max_tl_ADBM_df, 
                          data.frame(fw_name = fw, max_tl_ADBM = max_tl_ADBM, nsim = 1:1000)
                          )
  print(fw)
}

# saveRDS(max_tl_ADBM_df, file = "results/OP_2022_08_10/max_tl_ADBM.RDS")




#### Empirical food webs ####

max_tl_emp_df <- data.frame(fw_name = character(), max_tl_emp = double(), nsim = integer())

fw_all <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")$foodweb

for(fw in fw_all){
  max_tl_emp = max_tl_emp_fun(foodweb = fw)
  
  max_tl_emp_df <- rbind(max_tl_emp_df, 
                          data.frame(fw_name = fw, max_tl_emp = max_tl_emp)
  )
  print(fw)
}


# saveRDS(max_tl_emp_df, file = "results/OP_2022_08_10/max_tl_emp.RDS")



max_tl_emp_df <- readRDS("results/OP_2022_08_10/max_tl_emp.RDS")
max_tl_ADBM_df <- readRDS("results/OP_2022_08_10/max_tl_ADBM.RDS")

max_tl_df <- merge(max_tl_emp_df, max_tl_ADBM_df, by = "fw_name")

max_tl_df %>%
  filter(!is.na(max_tl_ADBM)) %>%
  group_by(fw_name, max_tl_emp) 

c
  
