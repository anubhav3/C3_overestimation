# 2022.09.05
# Investigate how connectance and proportion of basal species affect robustness

library(dplyr)
library(readxl)
library(R.utils)
library(bipartite)
library(cheddar)
library(DirectedClustering)
library(ggplot2)
library(lme4)

sourceDirectory("../C1_method_v2/R", modifiedOnly = FALSE)

rob_all_raw <- readRDS("results/OP_2022_08_10/robustness_R50_random_uncertainty_sep4.RDS")

fw_all <- as.character(unique(rob_all_raw$fw_name))

dd_all <- data.frame(fw_name = character(), type = character(), nsim_fw = integer(),
                     connectance = double(), robustness = double(), conn_2 = double(), prop_basal = double(),
                     delta_rob = double(), delta_conn  = double(), delta_basal = double(),
                     delta_sd_gen = double())

for(fw in fw_all){
  
  rob_all_raw_ra <- rob_all_raw %>%
    filter(fw_name == fw)
  
  dd <- rob_all_raw_ra %>%
    group_by(fw_name, type, nsim_fw, connectance) %>%
    summarise(robustness = median(robustness))
  
  metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
  fw_all <- metadata_fw_tol$foodweb
  
  fw_tol_all <- read_excel("../C1_method_v2/data/parameter_values.xlsx")
  
  ### Calculating proportion of basal species
  
  fw_tol <- fw_tol_all$dist_rej[fw_tol_all$foodweb == fw]
  
  ### ADBM predicted food webs
  dd_single_ADBM <- dd %>%
    filter(fw_name == fw & type == "ADBM_ABC")
  
  dd_C1_ADBM <- readRDS(paste0("../C1_method_v2/results/rejection/", fw, "/rN=1000_tol=", fw_tol, "_TSS_lower_a/", fw, "_prop.RDS"))
  
  dd_C1_ADBM <- dd_C1_ADBM$prop
  
  dd_ADBM <- cbind(dd_single_ADBM, conn_2 = dd_C1_ADBM$connectance, prop_basal = dd_C1_ADBM$prop_basal,
                   sd_gen = dd_C1_ADBM$sd_gen)
  
  ### Empirical food web
  
  dd_single_emp <- dd %>%
    filter(fw_name == fw & type == "Empirical")
  
  dd_C1_emp <- readRDS(paste0("../C1_method_v2/data/", fw, ".web.RDS"))
  
  dd_C1_emp <- real_prop_v2(dd_C1_emp)
  
  dd_emp <- cbind(dd_single_emp, conn_2 = dd_C1_emp$connectance, prop_basal = dd_C1_emp$prop_basal,
                  sd_gen = dd_C1_emp$sd_gen)
  dd_ADBM_emp <- rbind(dd_ADBM, dd_emp)
  
  
  emp_basal <- unique(dd_ADBM_emp$prop_basal[dd_ADBM_emp$fw_name == fw & dd_ADBM_emp$type == "Empirical"])
  emp_rob <- unique(dd_ADBM_emp$robustness[dd_ADBM_emp$fw_name == fw & dd_ADBM_emp$type == "Empirical"])
  emp_conn <- unique(dd_ADBM_emp$connectance[dd_ADBM_emp$fw_name == fw & dd_ADBM_emp$type == "Empirical"])
  emp_sd_gen <- unique(dd_ADBM_emp$sd_gen[dd_ADBM_emp$fw_name == fw & dd_ADBM_emp$type == "Empirical"])
  
  
  dd_loc <- dd_ADBM_emp %>%
    filter(fw_name == fw, type == "ADBM_ABC") %>%
    mutate(delta_rob = log(robustness) - log(emp_rob),
           delta_conn = connectance - emp_conn, 
           delta_basal = prop_basal - emp_basal,
           delta_sd_gen = sd_gen - emp_sd_gen)
  
  
  dd_all <- rbind(dd_all, dd_loc)
  print(fw)
}
  
#### Investigation ####

ggplot(dd_all) +
  geom_point(aes(x = delta_basal, y = delta_rob)) +
  geom_smooth(aes(x = delta_basal, y = delta_rob), method = "lm")

ggplot(dd_all) +
  geom_point(aes(x = delta_basal, y = delta_rob)) +
  facet_wrap(~fw_name) + 
  geom_smooth(aes(x = delta_basal, y = delta_rob), method = "lm")

ggplot(dd_all) +
  geom_point(aes(x = delta_conn, y = delta_rob), position = position_jitter(height = 0.05)) +
  facet_wrap(~fw_name) +
  geom_smooth(aes(x = delta_conn, y = delta_rob), method = "lm")


ggplot(dd_all) +
  geom_point(aes(x = sd_gen, y = delta_rob)) +
  facet_wrap(~fw_name) + 
  geom_smooth(aes(x = sd_gen, y = delta_rob), method = "lm")

lm_r_b_c <- lm(delta_rob ~ delta_conn*delta_basal, dd_all)

summary(lm_r_b_c)

anova(lm_r_b_c)


dd_all %>%
  group_by(fw_name) %>%
  summarise(cor_var = cor(delta_conn, delta_basal))


mef <- lmer(delta_rob ~ delta_conn*delta_basal*fw_name + (1|fw_name), data = dd_all)
summary(mef)
anova(mef)



max_tl_ADBM_df %>%
  filter(fw_name == "Small Reef")
