# 2022.09.13
# Robustness (R50) of ADBM predicted food webs VS robustness of observed food webs

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(latex2exp)
library(HDInterval)


fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

fw_labs_facet <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream \n (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream \n (size aggregation)")

#### Random extinction scenario ####
rob_all <- readRDS("results/robustness_R50_random_uncertainty_1000.RDS")

# rob_all <- rob_all %>%
#   filter(nsim_ind %in% c(1:500))


#### Observed food webs
rob_all_emp <- rob_all %>%
  filter(type == "Empirical") %>%
  group_by(fw_name, type, nsim_fw, connectance) %>%
  summarise(rob_med_fw = median(robustness)) 

rob_all_emp <- data.frame(rob_all_emp)
  
rob_all_emp <- rob_all_emp %>%
  mutate(robustness_emp = rob_med_fw) %>%
  select(-c(type, rob_med_fw))

#### ADBM predicted food webs
rob_all_ADBM <- rob_all %>%
  filter(type == "ADBM_ABC") %>%
  group_by(fw_name, type, nsim_fw, connectance) %>%
  summarise(rob_med_fw = median(robustness)) 

rob_all_ADBM <- rob_all_ADBM %>%
  group_by(fw_name, type) %>%
  summarise(connectance = connectance, nsim_fw = nsim_fw, rob_med_fw = rob_med_fw,
            l_conn = as.numeric(hdi(connectance)[1]), u_conn = as.numeric(hdi(connectance)[2])) %>%
  filter(connectance >= l_conn & connectance <= u_conn) %>%
  select(-c(l_conn, u_conn))

rob_all_ADBM <- data.frame(rob_all_ADBM)

rob_all_ADBM <- rob_all_ADBM %>%
  mutate(robustness_ADBM = rob_med_fw) %>%
  select(-c(type, rob_med_fw))

rob_both <- merge(x = rob_all_emp, rob_all_ADBM, by = c("fw_name")) %>%
  mutate(nsim_fw = nsim_fw.y) %>%
  select(-c(nsim_fw.x, nsim_fw.y, connectance.x, connectance.y))

rob_both_median <- rob_both %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

#### Graph
plot_R50_ra <- rob_both %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.01, outlier.shape = 1,
               outlier.alpha = 0.5, outlier.size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.0, 0.51)) +
  ylim(c(0.0, 0.51)) +
  geom_point(data = rob_both_median, aes(x = robustness_emp, y = median_robustness_ADBM, color = fw_name, shape = "Median"), 
             size = 4) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  # ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  ylab("") +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(18)) +
  ggtitle("(b) Random") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 15))


##### Most Connected extinction ####

rob_all_raw_mc <- readRDS("results/robustness_R50_mostconnected_uncertainty.RDS")

## Observed food web
rob_all_mc_emp <- rob_all_raw_mc %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness) 

rob_all_mc_emp <- data.frame(rob_all_mc_emp)

rob_all_mc_emp <- rob_all_mc_emp %>%
  select(-c(type, S, robustness))

#### ADBM predicted food webs
rob_all_mc_ADBM <- rob_all_raw_mc %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness) 

rob_all_mc_ADBM <- data.frame(rob_all_mc_ADBM)

rob_all_mc_ADBM <- rob_all_mc_ADBM %>%
  select(-c(type, S, robustness))

rob_all_mc_ADBM <- rob_all_mc_ADBM %>%
  group_by(fw_name) %>%
  summarise(connectance = connectance, nsim_fw = nsim_fw, robustness_ADBM = robustness_ADBM,
            l_conn = as.numeric(hdi(connectance)[1]), u_conn = as.numeric(hdi(connectance)[2])) %>%
  filter(connectance >= l_conn & connectance <= u_conn) %>%
  select(-c(l_conn, u_conn))

rob_all_mc_ADBM <- data.frame(rob_all_mc_ADBM)


rob_both_mc <- merge(x = rob_all_mc_emp, rob_all_mc_ADBM, by = c("fw_name")) %>%
  mutate(nsim_fw = nsim_fw.y) %>%
  select(-c(nsim_fw.x, nsim_fw.y, connectance.x, connectance.y))

rob_both_mc_median <- rob_both_mc %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

#### Graph
plot_R50_mc <- rob_both_mc %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.01, outlier.shape = 1,
               outlier.alpha = 0.5, outlier.size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.0, 0.51)) +
  ylim(c(0.0, 0.51)) +
  geom_point(data = rob_both_mc_median, aes(x = robustness_emp, y = median_robustness_ADBM, color = fw_name, shape = "Median"), 
             size = 4) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  xlab("") +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(18)) +
  ggtitle("(a) Most connected") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 15))





##### Least Connected extinction ####

rob_all_raw_lc <- readRDS("results/robustness_R50_leastconnected_uncertainty.RDS")

## Observed food web
rob_all_lc_emp <- rob_all_raw_lc %>%
  filter(type == "Empirical") %>%
  mutate(robustness_emp = robustness) 

rob_all_lc_emp <- data.frame(rob_all_lc_emp)

rob_all_lc_emp <- rob_all_lc_emp %>%
  select(-c(type, S, robustness))

#### ADBM predicted food webs
rob_all_lc_ADBM <- rob_all_raw_lc %>%
  filter(type == "ADBM_ABC") %>%
  mutate(robustness_ADBM = robustness) 

rob_all_lc_ADBM <- data.frame(rob_all_lc_ADBM)

rob_all_lc_ADBM <- rob_all_lc_ADBM %>%
  select(-c(type, S, robustness))

rob_all_lc_ADBM <- rob_all_lc_ADBM %>%
  group_by(fw_name) %>%
  summarise(connectance = connectance, nsim_fw = nsim_fw, robustness_ADBM = robustness_ADBM,
            l_conn = as.numeric(hdi(connectance)[1]), u_conn = as.numeric(hdi(connectance)[2])) %>%
  filter(connectance >= l_conn & connectance <= u_conn) %>%
  select(-c(l_conn, u_conn))

rob_all_lc_ADBM <- data.frame(rob_all_lc_ADBM)


rob_both_lc <- merge(x = rob_all_lc_emp, rob_all_lc_ADBM, by = c("fw_name")) %>%
  mutate(nsim_fw = nsim_fw.y) %>%
  select(-c(nsim_fw.x, nsim_fw.y, connectance.x, connectance.y))

rob_both_lc_median <- rob_both_lc %>%
  group_by(fw_name, robustness_emp) %>%
  summarise(median_robustness_ADBM = median(robustness_ADBM))

#### Graph
plot_R50_lc <- rob_both_lc %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name, fill = fw_name), alpha = 0.5, width = 0.01, outlier.shape = 1,
               outlier.alpha = 0.5, outlier.size = 0.3) +
  # facet_wrap(~fw_name) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.0, 0.51)) +
  ylim(c(0.0, 0.51)) +
  geom_point(data = rob_both_lc_median, aes(x = robustness_emp, y = median_robustness_ADBM, color = fw_name, shape = "Median"), 
             size = 4) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  # xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  # ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  xlab("") +
  ylab("") +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(18)) +
  ggtitle("(c) Least connected") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 15))


###  Merging all the above plots
plot_R50_all <- ggarrange(plot_R50_mc, plot_R50_ra, plot_R50_lc, nrow = 1, ncol = 3, common.legend = TRUE,
                                   legend = "bottom")

# ggsave(plot_R50_all, filename = "results/plot_R50_ADBM_vs_obs.png", width = 15, height = 6, dpi = 500)






#### Least connected faceted ####
plot_R50_lc_facet <- rob_both_lc %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name), alpha = 1, width = 0.019, outlier.shape = 1,
               outlier.alpha = 0.5, outlier.size = 0.3) +
  facet_wrap(~fw_name, labeller = labeller(fw_name = fw_labs_facet)) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  xlim(c(0.0, 0.51)) +
  ylim(c(0.0, 0.51)) +
  geom_point(data = rob_both_lc_median, aes(x = robustness_emp, y = median_robustness_ADBM, color = fw_name, shape = "Median"), 
             size = 4) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  # ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  # xlab("") +
  ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(18)) +
  ggtitle("Least connected") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 15))


#### Random faceted ####

plot_R50_ra_facet <- rob_both %>%
  ggplot() +
  geom_boxplot(aes(x = robustness_emp, y = robustness_ADBM, color = fw_name), alpha = 1, width = 0.019, outlier.shape = 1,
               outlier.alpha = 0.5, outlier.size = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  facet_wrap(~fw_name, labeller = labeller(fw_name = fw_labs_facet)) +
  xlim(c(0.0, 0.51)) +
  ylim(c(0.0, 0.51)) +
  geom_point(data = rob_both_median, aes(x = robustness_emp, y = median_robustness_ADBM, color = fw_name, shape = "Median"), 
             size = 4) +
  # geom_blank(data = dummy, mapping = aes(x = robustness_emp, y = robustness_ADBM)) +
  theme_classic() +
  xlab(TeX("Robustness ($R_{50}$) of observed food webs")) +
  ylab(TeX("Robustness ($R_{50}$) of ADBM food webs")) +
  # ylab("") +
  scale_fill_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_color_brewer(type = "qual", palette = "Paired", labels = fw_labs, name = "Food web") +
  scale_shape_manual(name  = "Summary statistics", labels = c("Median"), values = c(18)) +
  ggtitle("Random") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 15))



#### Combining faceted random and least connected ###

plot_R50_ra_lc_facet <- ggarrange(plot_R50_ra_facet, plot_R50_lc_facet, nrow = 2, ncol = 1, common.legend = TRUE,
                          legend = "bottom")

# ggsave(plot_R50_ra_lc_facet, filename = "results/plot_R50_ADBM_vs_obs_ra_lc_facet.png", width = 13, height = 15, dpi = 250)
