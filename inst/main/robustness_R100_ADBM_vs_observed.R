# 2022.06.27
# Robustness (R100) of ADBM predicted food webs VS robustness of observed food webs

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(latex2exp)

##### Merging all the three simulations scenarios #####

rob_all_raw1 <- readRDS("results/robustness_R100_leastconnected_uncertainty.RDS")
rob_all_raw2 <- readRDS("results/robustness_R100_random_uncertainty_july14.RDS")
rob_all_raw3 <- readRDS("results/robustness_R100_mostconnected_uncertainty.RDS")

rob_all_raw1 <- rob_all_raw1 %>%
  mutate(study = "(c) Least connected")

rob_all_raw2 <- rob_all_raw2 %>%
  mutate(study = "(b) Random")

rob_all_raw3 <- rob_all_raw3 %>%
  mutate(study = "(a) Most connected")

rob_all_raw <- rbind(rob_all_raw1, rob_all_raw2, rob_all_raw3)


rob_ADBM <- rob_all_raw %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness) %>%
  select(-c(robustness, connectance, type))

rob_obs <- rob_all_raw %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness) %>%
  select(-c(robustness, connectance, type)) %>%
  unique()

rob_all_merged <- merge(x = rob_ADBM, rob_obs, by = c("fw_name", "study")) %>%
  mutate(S = S.x, nsim = nsim.x) %>%
  select(-c(S.x, S.y, nsim.x, nsim.y))


rob_all_merged_mean <- rob_all_merged %>%
  group_by(fw_name, robustness_emp, study) %>%
  summarise(mean_robustness_ADBM = mean(robustness_ADBM), median_robustness_ADBM = median(robustness_ADBM))

rob_all_merged_mean <- rob_all_merged_mean %>%
  pivot_longer(!c(fw_name, robustness_emp, study), names_to = 'value_type', values_to = "values")

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")


# dummy <- rbind(data.frame(robustness_emp = c(0.1, 0.5), robustness_ADBM = c(0.1, 0.5), study = "(a) Least connected", stringsAsFactors=FALSE),
#                data.frame(robustness_emp = c(0.3, 0.5), robustness_ADBM = c(0.3, 0.5), study = "(b) Random", stringsAsFactors=FALSE),
#                data.frame(robustness_emp = c(0.0, 0.5), robustness_ADBM = c(0.0, 0.5), study = "(c) Most connected", stringsAsFactors=FALSE))

# ggplot(rob_all_merged) +
#   geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.01) +
#   facet_wrap(~study, scale = "free") +
#   geom_abline(slope = 1, intercept = 0, linetype = 3) +
#   # xlim(c(0.0, 0.55)) +
#   # ylim(c(0.0, 0.55)) +
#   geom_point(data = rob_all_merged_mean, aes(x = robustness_emp, y = mean_robustness_ADBM, color = fw_name), size = 3, shape = 2) +
#   geom_point(data = rob_all_merged_mean, aes(x = robustness_emp, y = median_robustness_ADBM, color = fw_name), size = 3, shape = 1) +
#   geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
#   theme_classic() +
#   xlab("Robustness (Observed food webs)") +
#   ylab("Robustness (ADBM food webs)") +
#   scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
#   scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
#   theme(legend.position = "right")


plot_LC_R100 <- rob_all_merged %>%
  filter(study == "(c) Least connected") %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.002, outlier.shape = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.0, 1)) +
  ylim(c(0.0, 1)) +
  geom_point(data = rob_all_merged_mean[rob_all_merged_mean$study == "(c) Least connected",], aes(x = robustness_emp, y = values, color = fw_name, shape = value_type), size = 3) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab("") +
  ylab("") +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Mean", "Median"), values = c(17, 18)) +
  ggtitle("(c) Least connected") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))


plot_random_R100 <- rob_all_merged %>%
  filter(study == "(b) Random") %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.002, outlier.shape = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0, 1)) +
  ylim(c(0., 1)) +
  geom_point(data = rob_all_merged_mean[rob_all_merged_mean$study == "(b) Random",], aes(x = robustness_emp, y = values, color = fw_name, shape = value_type), size = 3) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  ylab("") +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Mean", "Median"), values = c(17, 18)) +
  ggtitle("(b) Random") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))


plot_MC_R100 <- rob_all_merged %>%
  filter(study == "(a) Most connected") %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.005, outlier.shape = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.0, 1)) +
  ylim(c(0.0, 1)) +
  geom_point(data = rob_all_merged_mean[rob_all_merged_mean$study == "(a) Most connected",], aes(x = robustness_emp, y = values, color = fw_name, shape = value_type), size = 3) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab("") +
  ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Mean", "Median"), values = c(17, 18)) +
  ggtitle("(a) Most connected") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))


plot_R100_all <- ggarrange(plot_MC_R100, plot_random_R100, plot_LC_R100, nrow = 1, ncol = 3, common.legend = TRUE,
                          legend = "bottom")
# ggsave(plot_R100_all, filename = "results/plot_R100_ADBM_vs_obs_new.png", width = 15, height = 6, dpi = 500)


sad <- rob_all_raw2 %>%
  filter(fw_name == "Benguela Pelagic", type == "Empirical")
  

dd_rand <- readRDS("results/random_raw_uncertainty_july11.RDS")


sadd <- dd_rand %>%
  filter(fw_name == "Broadstone Stream size_agg", type == "Empirical")

