# 2022.06.28
# We compute the 95% prediction intervals of the connectance predicted using the ADBM in Gupta et al. (2022)

library(HDInterval)

dd <- readRDS("results/robustness_mostconnected_uncertainty.RDS")

dd %>%
  filter(type == "ADBM_ABC") %>%
  group_by(fw_name) %>%
  summarise(l_conn = as.numeric(hdi(connectance, credMass = 0.95)[1]), u_conn =as.numeric(hdi(connectance, credMass = 0.95)[2]) )

