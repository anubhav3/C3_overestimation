tl_ADBM <- readRDS("../C1_method_v2/results/rejection/Benguela Pelagic/rN=1000_tol=0.6_TSS_lower_a/Benguela Pelagic_prop.RDS")

tl_ADBM_basal <- tl_ADBM$prop$prop_basal
tl_ADBM_conn <- tl_ADBM$prop$connectance

tl_emp <- readRDS("../C1_data/data/Benguela Pelagic.web.RDS")
tl_emp <- tl_emp$predation.matrix

sourceDirectory("../C1_method_v2/R", modifiedOnly = FALSE)

prop_basal_inter_top_herb(tl_emp)

tl_emp_basal <- prop_basal_inter_top_herb(tl_emp)$prop_basal


hist(tl_ADBM_basal)

tl_rob <- dd %>%
  filter(type == "ADBM_ABC", fw_name == "Benguela Pelagic")

tl_rob_emp <- dd %>%
  filter(type == "Empirical", fw_name == "Benguela Pelagic")


tl_rob_basal <- data.frame(rob = tl_rob$robustness, prop_basal = tl_ADBM_basal)


ggplot(tl_rob_basal) +
  geom_point(aes(x = prop_basal, y = rob)) +
  geom_smooth(aes(x = prop_basal, y = rob), method = "lm")

tl_rob_basal %>%
  lm(formula = rob ~ prop_basal) %>%
  summary()

ggplot(tl_rob) +
  geom_point(aes(x = connectance, y = robustness))



delta_df <- data.frame(delta_rob = tl_rob$robustness - tl_rob_emp$robustness,
                       delta_conn = tl_rob$connectance - tl_rob_emp$connectance,
                       delta_basal = tl_rob_basal$prop_basal - tl_emp_basal)


lm_delta <- lm(data = delta_df, formula = delta_rob ~ delta_basal * delta_conn)

summary(lm_delta)


ggplot(delta_df) +
  geom_point(aes(x = delta_conn, y = delta_rob)) +
  geom_smooth(aes(x = delta_conn, y = delta_rob), method = "lm")

ggplot(delta_df) +
  geom_point(aes(x = delta_basal, y = delta_rob)) +
  geom_smooth(aes(x = delta_basal, y = delta_rob), method = "lm")


plot_ly(x = delta_df$delta_conn, y = delta_df$delta_basal, z = delta_df$delta_rob, type = "scatter3d", mode = "markers")



dd <- readRDS("results/robustness_R50_random_uncertainty_july14.RDS")

dd <- dd %>%
  group_by(fw_name, type) %>%
  summarise(connectance = connectance, robustness = robustness, l_conn = as.numeric(hdi(connectance)[1]), u_conn = as.numeric(hdi(connectance)[2]))
  # filter(connectance >= l_conn & connectance <= u_conn) %>%
  # group_by(fw_name, type) %>%
  # select(-c(l_conn, u_conn))

metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
fw_all <- metadata_fw_tol$foodweb

dd_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                     S = integer(), nsim = integer(), slope = double())

for(fw in fw_all){
  
  emp_conn <- unique(dd$connectance[dd$fw_name == fw & dd$type == "Empirical"])
  emp_rob <- mean(dd$robustness[dd$fw_name == fw & dd$type == "Empirical"])
  
  
  dd_loc <- dd %>%
    filter(fw_name == fw, type == "ADBM_ABC") %>%
    # mutate(slope = ((robustness - emp_rob)/emp_rob)/((connectance - emp_conn)/emp_conn))
    mutate(delta_rob = (robustness - emp_rob), delta_conn = (connectance - emp_conn))
  
  dd_all <- rbind(dd_all, dd_loc)
}

dd_all %>%
  filter(delta_rob > 0) %>%
ggplot() +
  geom_point(aes(x = delta_conn, y = delta_rob)) +
  geom_smooth(aes(x = delta_conn, y = delta_rob), method = "lm") +
  facet_wrap(~fw_name)

lm_delta <- dd_all %>%
  filter(delta_rob > 0) %>%
  lm(formula = delta_rob ~ delta_conn)

summary(lm_delta)

