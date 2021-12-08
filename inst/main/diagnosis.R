# 13.10.2021
# We investigate the reasons for the differences in the secondary extinctions from ADBM predicted food webs and that of 
# empirical food webs

library(dplyr)
library(tidyverse)

dd_mc <- readRDS("results/dd_mc.RDS")
rob_all <- readRDS("results/rob_all.RDS")
dd <- data.frame(diff_rob = double(), conn_ADBM = double(), conn_emp = double(), diff_conn = double(),
                 foodweb = character(), diff_intervality = double(), L_ADBM = integer(),
                 L_emp = integer(), diff_L = integer(), rob_emp = double())

metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method/results/check/check_TSS_a.xlsx")

for(fw_name in unique(dd_mc$fw_name)){
  
  pred_mat_ADBM <- readRDS(paste0("data/ADBM_2008_predicted_foodwebs/", fw_name, ".pred_mat.RDS"))
  pred_mat_emp <- readRDS(paste0("../C1_method/data_new/", fw_name, ".web.Rdata"))$predation.matrix
  
  conn_ADBM <- sum(pred_mat_ADBM)/(dim(pred_mat_ADBM)[1]^2)
  conn_emp <- sum(pred_mat_emp)/(dim(pred_mat_emp)[1]^2)
  
  L_ADBM <- sum(pred_mat_ADBM)
  L_emp <- sum(pred_mat_emp)
  
  fw_name_f <- fw_name
  
  rob_ADBM <- filter(rob_all, type == "ADBM", fw_name == fw_name_f)$robustness
  rob_emp <- filter(rob_all, type == "Empirical", fw_name == fw_name_f)$robustness
  
  dd <- rbind(dd,
              data.frame(diff_rob = rob_ADBM - rob_emp, conn_ADBM = conn_ADBM, 
                         conn_emp = conn_emp, diff_conn = conn_ADBM - conn_emp,
                         foodweb = fw_name, 
                         diff_intervality = intervality(pred_mat_emp) - intervality(pred_mat_ADBM),
                         L_ADBM = L_ADBM, L_emp = L_emp, diff_L = L_ADBM - L_emp, rob_emp = rob_emp)
  )
}

  
dd <- mutate(dd, rel_diff_conn = diff_conn/conn_emp, rel_diff_rob = diff_rob/rob_emp)


ggplot(dd) +
  geom_point(aes(x = rel_diff_conn, y = rel_diff_rob)) +
  # geom_smooth(aes(x = rel_diff_conn, y = diff_ext), method = "lm") +
  theme_classic() +
  scale_x_log10() +
  scale_y_log10()
  

lm_ext <- lm(rel_diff_rob ~ rel_diff_conn, data = dd)
summary(lm_ext)
anova(lm_ext)

##########Diff_ext vs diff in intervality #########

dd %>%
  filter(foodweb != "Caricaie Lakes") %>%
ggplot() +
  geom_point(aes(x = diff_intervality, y = diff_ext)) +
  # geom_smooth(aes(x = rel_diff_conn, y = diff_ext), method = "lm") +
  theme_bw()


lm_ext_inter <- 
  dd %>%
  filter(foodweb != "Caricaie Lakes") %>%
  lm(formula = diff_ext ~ diff_intervality*rel_diff_conn)

summary(lm_ext_inter)
anova(lm_ext_inter)


dd %>%
  filter(foodweb != "Caricaie Lakes") %>%
  ggplot() +
  geom_point(aes(x = diff_intervality, y = conn_emp)) +
  # geom_smooth(aes(x = rel_diff_conn, y = diff_ext), method = "lm") +
  theme_bw()



