# 2022.07.21
# We investigate if the difference in robustness of the ADBM predicted food webs and observed food webs is dependent
# on food web properties such as proportion of basal species

library(R.utils)
library(dplyr)
library(ggplot2)
library(lme4)

sourceDirectory("../C1_method_v2/R", modifiedOnly = FALSE)
sourceDirectory("R", modifiedOnly = FALSE)

fw_par_all <- readxl::read_excel("../C1_method_v2/data/parameter_values.xlsx")

fw_name_loc <- "Broadstone Stream size_agg"
fw_tol <- fw_par_all$dist_rej[fw_par_all$foodweb == fw_name_loc]


ADBM_prop <- readRDS(paste0("../C1_method_v2/results/rejection/", fw_name_loc,"/rN=1000_tol=", fw_tol, "_TSS_lower_a/", fw_name_loc,"_prop.RDS"))$prop

basal_ADBM <- ADBM_prop$prop_basal
conn_ADBM <- ADBM_prop$connectance

obs_fw <- readRDS(paste0("../C1_data/data/", fw_name_loc, ".web.RDS"))$predation.matrix

basal_obs <- prop_basal_inter_top_herb(obs_fw)$prop_basal


dd_rand <- readRDS("results/robustness_R50_random_uncertainty_july14.RDS")

rob_fw <- dd_rand %>%
  filter(type == "ADBM_ABC", fw_name == fw_name_loc) %>%
  cbind(basal_ADBM = basal_ADBM)


ggplot(rob_fw) +
  geom_point(aes(x = basal_ADBM, y = robustness)) +
  geom_smooth(aes(x = basal_ADBM, y = robustness), method = "lm")

lm_rob_basal <- lm(robustness ~ basal_ADBM, data = rob_fw)
summary(lm_rob_basal)



ADBM_pred <- ADBM_ABC_fw_all(fw_name = fw_name_loc, fw_tol = fw_tol, nsim = 1)



rob_all_raw_mc <- readRDS("results/robustness_R50_mostconnected_wo_basal_uncertainty.RDS")

rob_ADBM_mc <- rob_all_raw_mc %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness, S_basal_ADBM = S - S_wo_basal, connectance_ADBM = connectance) %>%
  select(-c(robustness, connectance, type, S_wo_basal))

rob_obs_mc <- rob_all_raw_mc %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness, S_basal_emp = S - S_wo_basal, connectance_emp = connectance) %>%
  select(-c(robustness, connectance, type, S_wo_basal)) %>%
  unique()

rob_all_mc <- merge(x = rob_ADBM_mc, rob_obs_mc, by = c("fw_name")) %>%
  mutate(S = S.x, nsim = nsim.x) %>%
  select(-c(S.x, S.y, nsim.x, nsim.y))


dd <- rob_all_mc %>%
  mutate(diff_rob = robustness_ADBM - robustness_emp, diff_basal = S_basal_ADBM - S_basal_emp,
         diff_connectance = connectance_ADBM - connectance_emp)


lm_dd <- lm(diff_rob ~ diff_connectance + diff_basal, data = dd)

summary(lm_dd)

ggplot(dd) +
  geom_point(aes(x = diff_basal, y = diff_rob)) +
  geom_smooth(aes(x = diff_basal, y = diff_rob), method = "lm") +
  facet_wrap(~fw_name, scale = "free")


ggplot(dd) +
  geom_point(aes(x = diff_connectance, y = diff_rob)) +
  geom_smooth(aes(x = diff_connectance, y = diff_rob), method = "lm") +
  facet_wrap(~fw_name, scale = "free")


lm_mm <- lmer(diff_rob ~ diff_connectance + diff_basal + (1|fw_name), data = dd)

summary(lm_mm)
