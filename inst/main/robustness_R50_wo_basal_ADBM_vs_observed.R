# 2022.07.29
# Robustness (R50) (without basal) of ADBM predicted food webs VS robustness of observed food webs

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(latex2exp)

##### Most connected scenario #####

rob_all_raw_mc <- readRDS("results/robustness_R50_mostconnected_wo_basal_uncertainty.RDS")

rob_ADBM_mc <- rob_all_raw_mc %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness, S_basal_ADBM = S - S_wo_basal) %>%
  select(-c(robustness, connectance, type))

rob_obs_mc <- rob_all_raw_mc %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness, S_basal_emp = S - S_wo_basal) %>%
  select(-c(robustness, connectance, type)) %>%
  unique()

rob_all_mc <- merge(x = rob_ADBM_mc, rob_obs_mc, by = c("fw_name")) %>%
  mutate(S = S.x, nsim = nsim.x) %>%
  select(-c(S.x, S.y, nsim.x, nsim.y))

rob_all_mc_mean <- rob_all_mc %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

rob_all_mc_mean <- rob_all_mc_mean %>%
  pivot_longer(!c(fw_name, robustness_emp), names_to = 'value_type', values_to = "values")

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")


plot_MC_R50 <- rob_all_mc %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.005, outlier.shape = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.0, 0.51)) +
  ylim(c(0.0, 0.51)) +
  # geom_point(data = rob_all_mc_mean, aes(x = robustness_emp, y = values, color = fw_name, shape = value_type), size = 3) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab("") +
  ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(17)) +
  ggtitle("(a) Most connected") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))



##### Random scenario #####

rob_all_raw_ra <- readRDS("results/robustness_R50_wo_basal_random_uncertainty_120.RDS")

rob_ADBM_ra <- rob_all_raw_ra %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness, S_basal_ADBM = S - S_wo_basal) %>%
  select(-c(robustness, connectance, type))

rob_obs_ra <- rob_all_raw_ra %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness, S_basal_emp = S - S_wo_basal) %>%
  select(-c(robustness, connectance, type)) %>%
  unique()

rob_all_ra <- merge(x = rob_ADBM_ra, rob_obs_ra, by = c("fw_name")) %>%
  mutate(S = S.x, nsim = nsim.x) %>%
  select(-c(S.x, S.y, nsim.x, nsim.y))

rob_all_ra_mean <- rob_all_ra %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

rob_all_ra_mean <- rob_all_ra_mean %>%
  pivot_longer(!c(fw_name, robustness_emp), names_to = 'value_type', values_to = "values")

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")


plot_RA_R50 <- rob_all_ra %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.005, outlier.shape = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.4, 0.51)) +
  ylim(c(0.4, 0.51)) +
  # geom_point(data = rob_all_ra_mean, aes(x = robustness_emp, y = values, color = fw_name, shape = value_type), size = 3) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  ylab("") +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(17)) +
  ggtitle("(b) Random") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))


##### Least connected scenario #####

rob_all_raw_lc <- readRDS("results/robustness_R50_leastconnected_wo_basal_uncertainty.RDS")

rob_ADBM_lc <- rob_all_raw_lc %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness, S_basal_ADBM = S - S_wo_basal) %>%
  select(-c(robustness, connectance, type))

rob_obs_lc <- rob_all_raw_lc %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness, S_basal_emp = S - S_wo_basal) %>%
  select(-c(robustness, connectance, type)) %>%
  unique()

rob_all_lc <- merge(x = rob_ADBM_lc, rob_obs_lc, by = c("fw_name")) %>%
  mutate(S = S.x, nsim = nsim.x) %>%
  select(-c(S.x, S.y, nsim.x, nsim.y))

rob_all_lc_mean <- rob_all_lc %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

rob_all_lc_mean <- rob_all_lc_mean %>%
  pivot_longer(!c(fw_name, robustness_emp), names_to = 'value_type', values_to = "values")

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")


plot_LC_R50 <- rob_all_lc %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.005, outlier.shape = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.0, 0.51)) +
  ylim(c(0.0, 0.51)) +
  # geom_point(data = rob_all_lc_mean, aes(x = robustness_emp, y = values, color = fw_name, shape = value_type), size = 3) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab("") +
  ylab("") +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(17)) +
  ggtitle("(c) Least connected") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))



##### Merging all the three simulations scenarios #####

plot_R50_all_wo_basal <- ggarrange(plot_MC_R50, plot_RA_R50, plot_LC_R50, nrow = 1, ncol = 3, common.legend = TRUE,
                          legend = "bottom")

# ggsave(plot_R50_all_wo_basal, filename = "results/plot_R50_wo_basal_ADBM_vs_obs.png", width = 15, height = 6, dpi = 500)


#####  Investigation #####
rob_all_mc %>%
  group_by(fw_name) %>%
  filter(fw_name != "Broadstone Stream") %>%
  summarise(ttest = t.test(x = robustness_ADBM, mu = unique(robustness_emp), alternative = "greater")$p.value)

asad <- rob_all_mc %>%
  filter(fw_name == "Broadstone Stream")


rob_all_merged %>%
  filter(study == "(b) Random") %>%
  filter(fw_name != "Broadstone Stream") %>%
  group_by(fw_name) %>%
  summarise(ttest = t.test(x = robustness_ADBM, mu = unique(robustness_emp), alternative = "greater")$p.value)




##### Max vulnerability scenario #####

rob_all_raw_mv <- readRDS("results/robustness_R50_maxvulnerability_uncertainty.RDS")

rob_ADBM_mv <- rob_all_raw_mv %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness) %>%
  select(-c(robustness, connectance, type))

rob_obs_mv <- rob_all_raw_mv %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness) %>%
  select(-c(robustness, connectance, type)) %>%
  unique()

rob_all_mv <- merge(x = rob_ADBM_mv, rob_obs_mv, by = c("fw_name")) %>%
  mutate(S = S.x, nsim = nsim.x) %>%
  select(-c(S.x, S.y, nsim.x, nsim.y))

rob_all_mv_mean <- rob_all_mv %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

rob_all_mv_mean <- rob_all_mv_mean %>%
  pivot_longer(!c(fw_name, robustness_emp), names_to = 'value_type', values_to = "values")

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")


plot_mv_R50 <- rob_all_mv %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.005, outlier.shape = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.0, 0.51)) +
  ylim(c(0.0, 0.51)) +
  # geom_point(data = rob_all_mc_mean, aes(x = robustness_emp, y = values, color = fw_name, shape = value_type), size = 3) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(17)) +
  ggtitle("Maximum vulnerability") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

# ggsave(plot_mv_R50, filename = "results/plot_R50_maxvulnerability_ADBM_vs_obs.png", width = 8, height = 6, dpi = 500)


##### Max generality scenario #####

rob_all_raw_mg <- readRDS("results/robustness_R50_maxgenerality_uncertainty.RDS")

rob_ADBM_mg <- rob_all_raw_mg %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness) %>%
  select(-c(robustness, connectance, type))

rob_obs_mg <- rob_all_raw_mg %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness) %>%
  select(-c(robustness, connectance, type)) %>%
  unique()

rob_all_mg <- merge(x = rob_ADBM_mg, rob_obs_mg, by = c("fw_name")) %>%
  mutate(S = S.x, nsim = nsim.x) %>%
  select(-c(S.x, S.y, nsim.x, nsim.y))

rob_all_mg_mean <- rob_all_mg %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

rob_all_mg_mean <- rob_all_mg_mean %>%
  pivot_longer(!c(fw_name, robustness_emp), names_to = 'value_type', values_to = "values")

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")


plot_mg_R50 <- rob_all_mg %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.005, outlier.shape = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.35, 0.51)) +
  ylim(c(0.35, 0.51)) +
  # geom_point(data = rob_all_mc_mean, aes(x = robustness_emp, y = values, color = fw_name, shape = value_type), size = 3) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(17)) +
  ggtitle("Maximum generality") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

# ggsave(plot_mg_R50, filename = "results/plot_R50_maxgenerality_ADBM_vs_obs.png", width = 8, height = 6, dpi = 500)



##### Largest body mass scenario #####

rob_all_raw_la <- readRDS("results/robustness_R50_largestbodymass_uncertainty.RDS")

rob_ADBM_la <- rob_all_raw_la %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness) %>%
  select(-c(robustness, connectance, type))

rob_obs_la <- rob_all_raw_la %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness) %>%
  select(-c(robustness, connectance, type)) %>%
  unique()

rob_all_la <- merge(x = rob_ADBM_la, rob_obs_la, by = c("fw_name")) %>%
  mutate(S = S.x, nsim = nsim.x) %>%
  select(-c(S.x, S.y, nsim.x, nsim.y))

rob_all_la_mean <- rob_all_la %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

rob_all_la_mean <- rob_all_la_mean %>%
  pivot_longer(!c(fw_name, robustness_emp), names_to = 'value_type', values_to = "values")

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")


plot_la_R50 <- rob_all_la %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.005, outlier.shape = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0, 0.51)) +
  ylim(c(0, 0.51)) +
  # geom_point(data = rob_all_mc_mean, aes(x = robustness_emp, y = values, color = fw_name, shape = value_type), size = 3) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(17)) +
  ggtitle("Largest body mass") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

# ggsave(plot_la_R50, filename = "results/plot_R50_largestbodymass_ADBM_vs_obs.png", width = 8, height = 6, dpi = 500)


