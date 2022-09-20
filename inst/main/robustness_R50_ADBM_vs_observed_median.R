# 2022.09.08

library(ggplot2)
library(dplyr)
library(latex2exp)

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

##### Most connected scenario #####

rob_all_raw_mc <- readRDS("results/robustness_R50_mostconnected_uncertainty.RDS")

rob_all_raw_mc <- rob_all_raw_mc %>%
  group_by(fw_name, type, nsim_fw, connectance) %>%
  summarise(robustness = median(robustness))

rob_all_raw_mc <- data.frame(rob_all_raw_mc)

rob_ADBM_mc <- rob_all_raw_mc %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness) %>%
  select(-c(type, robustness))

rob_obs_mc <- rob_all_raw_mc %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness) %>%
  select(-c(type, robustness))

rob_all_mc <- merge(x = rob_ADBM_mc, rob_obs_mc, by = c("fw_name")) 

rob_all_mc_median <- rob_all_mc %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

rob_all_mc %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name), alpha = 1, width = 0.0019, outlier.shape = 1,
               outlier.alpha = 0.5, outlier.size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0, 0.51)) +
  ylim(c(0, 0.51)) +
  geom_point(data = rob_all_mc_median, aes(x = robustness_emp, y = median_robustness_ADBM, color = fw_name, shape = "Median"), 
             size = 4) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  # xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  xlab("") +
  ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(18)) +
  ggtitle("(a) Most connected") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))

##### Random scenario #####

rob_all_raw_ra <- readRDS("results/OP_2022_08_10/robustness_R50_random_uncertainty_sep4.RDS")

rob_all_raw_ra <- rob_all_raw_ra %>%
  group_by(fw_name, type, nsim_fw, connectance) %>%
  summarise(robustness = median(robustness))

rob_all_raw_ra <- data.frame(rob_all_raw_ra)

rob_ADBM_ra <- rob_all_raw_ra %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness) %>%
  select(-c(type, robustness))

rob_obs_ra <- rob_all_raw_ra %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness) %>%
  select(-c(type, robustness))

rob_all_ra_median <- rob_all_ra %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

rob_all_ra <- merge(x = rob_ADBM_ra, rob_obs_ra, by = c("fw_name")) 


plot_R50_ra_ADBM_vs_obs_median <- rob_all_ra %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 1, width = 0.0019, outlier.shape = 1,
               outlier.alpha = 0.5, outlier.size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.1, 0.51)) +
  ylim(c(0.1, 0.51)) +
  geom_point(data = rob_all_ra_median, aes(x = robustness_emp, y = median_robustness_ADBM, color = fw_name, shape = "Median"), 
             size = 4) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(18)) +
  ggtitle("(b) Random") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))


# ggsave(plot_R50_ra_ADBM_vs_obs_median, filename = "results/OP_2022_08_10/plot_R50_ra_ADBM_vs_obs_median.png",
#        width = 7.5, height = 5)

# ggsave(plot_R50_ra_ADBM_vs_obs_median, filename = "results/OP_2022_08_10/plot_R50_ra_ADBM_vs_obs_median_outlier.png",
#        width = 7.5, height = 5)


##### Least connected scenario #####

rob_all_raw_lc <- readRDS("results/robustness_R50_leastconnected_uncertainty.RDS")

rob_all_raw_lc <- rob_all_raw_lc %>%
  group_by(fw_name, type, nsim_fw, connectance) %>%
  summarise(robustness = median(robustness))

rob_all_raw_lc <- data.frame(rob_all_raw_lc)

rob_ADBM_lc <- rob_all_raw_lc %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness) %>%
  select(-c(type, robustness))

rob_obs_lc <- rob_all_raw_lc %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness) %>%
  select(-c(type, robustness))

rob_all_lc <- merge(x = rob_ADBM_lc, rob_obs_lc, by = c("fw_name")) 

rob_all_lc_median <- rob_all_lc %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

rob_all_lc %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name), alpha = 1, width = 0.0019, outlier.shape = 1,
               outlier.alpha = 0.5, outlier.size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0, 0.51)) +
  ylim(c(0, 0.51)) +
  geom_point(data = rob_all_lc_median, aes(x = robustness_emp, y = median_robustness_ADBM, color = fw_name, shape = "Median"), 
             size = 4) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  # xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  xlab("") +
  ylab("") +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(18)) +
  ggtitle("(c) Least connected") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))


