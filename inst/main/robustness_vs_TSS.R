# 2022.02.27
# Robustness and TSS of the predicted food webs

library(dplyr)
library(HDInterval)

dd_rob_TSS = data.frame(foodweb = character(), nsim = integer(), robustness = double(), TSS = double(), study = character())

metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")

for(foodweb in metadata_fw_tol$foodweb){
  
  
  fw_ADBM <- readRDS(paste0("../C1_method_v2/results/rejection/", foodweb, "/rN=1000_tol=", metadata_fw_tol$dist_rej[which(metadata_fw_tol$foodweb == foodweb)], "_TSS_lower_a/", foodweb,".RDS"))
  
  TSS <- 1 - fw_ADBM$dist
  
  fw_rob <- readRDS("results/robustness_mostconnected_uncertainty.RDS")
  
  robustness_ADBM <- fw_rob %>%
    filter(fw_name == foodweb, type == "ADBM_ABC")
  
  robustness_emp <- fw_rob %>%
    filter(fw_name == foodweb, type == "Empirical")
  
  robustness <- robustness_ADBM$robustness - robustness_emp$robustness
  
  dd_rob_TSS <- rbind(dd_rob_TSS, 
                      data.frame(foodweb = foodweb, nsim = 1:1000, robustness = robustness, TSS = TSS, study = "Most connected"))
  
}


dd_rob_TSS <- dd_rob_TSS %>%
  group_by(foodweb, study) %>%
  summarise(mean_robustness = mean(robustness), l_robustness = as.numeric(hdi(robustness)[1]), u_robustness = as.numeric(hdi(robustness)[2]),
            mean_TSS = mean(TSS), l_TSS = as.numeric(hdi(TSS)[1]), u_TSS = as.numeric(hdi(TSS)[2]))


ggplot(dd_rob_TSS) +
  geom_point(aes(x = mean_TSS, y = mean_robustness))
