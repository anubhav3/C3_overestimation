# 2022.09.05
# Investigate how connectance and proportion of basal species affect robustness

library(dplyr)
library(readxl)
library(R.utils)
library(bipartite)
library(cheddar)
library(DirectedClustering)
library(ggplot2)

sourceDirectory("../C1_method_v2/R", modifiedOnly = FALSE)


rob_all_raw_ra <- readRDS("results/OP_2022_08_10/robustness_R50_random_uncertainty_sep4.RDS")

fw <- "Small Reef"

rob_all_raw_ra <- rob_all_raw_ra %>%
  filter(fw_name == fw)

dd <- rob_all_raw_ra %>%
  group_by(fw_name, type, nsim_fw, connectance) %>%
  summarise(robustness = median(robustness))

metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
fw_all <- metadata_fw_tol$foodweb

dd_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                     S = integer(), nsim = integer(), slope = double(), delta_rob = double(),
                     delta_conn  = double(), delta_basal = double())

fw_tol_all <- read_excel("../C1_method_v2/data/parameter_values.xlsx")

### Calculating proportion of basal species

fw_tol <- fw_tol_all$dist_rej[fw_tol_all$foodweb == fw]

### ADBM predicted food webs
dd_single_ADBM <- dd %>%
  filter(fw_name == fw & type == "ADBM_ABC")

dd_C1_ADBM <- readRDS(paste0("../C1_method_v2/results/rejection/", fw, "/rN=1000_tol=", fw_tol, "_TSS_lower_a/", fw, "_prop.RDS"))

dd_C1_ADBM <- dd_C1_ADBM$prop

dd_ADBM <- cbind(dd_single_ADBM, conn_2 = dd_C1_ADBM$connectance, prop_basal = dd_C1_ADBM$prop_basal)

### Empirical food web

dd_single_emp <- dd %>%
  filter(fw_name == fw & type == "Empirical")

dd_C1_emp <- readRDS(paste0("../C1_method_v2/data/", fw, ".web.RDS"))

dd_C1_emp <- real_prop_v2(dd_C1_emp)

dd_emp <- cbind(dd_single_emp, conn_2 = dd_C1_emp$connectance, prop_basal = dd_C1_emp$prop_basal)
dd_ADBM_emp <- rbind(dd_ADBM, dd_emp)


emp_basal <- unique(dd_ADBM_emp$prop_basal[dd_ADBM_emp$fw_name == fw & dd_ADBM_emp$type == "Empirical"])
emp_rob <- unique(dd_ADBM_emp$robustness[dd_ADBM_emp$fw_name == fw & dd_ADBM_emp$type == "Empirical"])
emp_conn <- unique(dd_ADBM_emp$connectance[dd_ADBM_emp$fw_name == fw & dd_ADBM_emp$type == "Empirical"])

dd_loc <- dd_ADBM_emp %>%
  filter(fw_name == fw, type == "ADBM_ABC") %>%
  mutate(delta_rob = log(robustness) - log(emp_rob),
         delta_conn = connectance - emp_conn, 
         delta_basal = prop_basal - emp_basal)

# dd_loc$delta_basal <- as.factor(dd_loc$delta_basal)

ggplot(dd_loc) +
  geom_point(aes(x = delta_conn, y = delta_rob)) +
  facet_wrap(~delta_basal) +
  geom_smooth(aes(x = delta_conn, y = delta_rob), method = "lm")


ggplot(dd_loc) +
  geom_point(aes(x = delta_basal, y = delta_rob)) +
  geom_smooth(aes(x = delta_basal, y = delta_rob), method = "lm")

ggplot(dd_loc) +
  geom_point(aes(x = delta_conn, y = delta_basal))
  

lm_r_b_c <- lm(delta_rob ~ delta_conn * delta_basal, dd_loc)

summary(lm_r_b_c)

anova(lm_r_b_c)
