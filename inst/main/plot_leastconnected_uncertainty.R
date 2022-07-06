# 2022.02.24

# Plot how secondary extinctions vary when number of species removed is increased

library(dplyr)
library(ggplot2)

dd_lc_raw <- readRDS("results/least_connnected_uncertainty.RDS")


dd_lc_mod <- dd_lc_raw %>%
  group_by(fw_name, type, nsim, S) %>%
  mutate(max_pri = max(n_ext)) %>%
  group_by(fw_name, type, S) %>%
  mutate(max_min_pri = min(max_pri)) %>%
  filter(n_ext <= max_min_pri) %>%
  group_by(fw_name, n_ext, type, S) %>%
  summarise(mean_acc_sec_ext = mean(acc_sec_ext), l_acc_sec_ext = min(acc_sec_ext), u_acc_sec_ext = max(acc_sec_ext)) %>%
  mutate(n_ext_by_S = n_ext/S, acc_sec_ext_by_S = mean_acc_sec_ext/S, l_acc_sec_ext_by_S = l_acc_sec_ext/S,
         u_acc_sec_ext_by_S = u_acc_sec_ext/S)

fw_labs <- c("Benguela Pelagic" = "(a) Benguela Pelagic", "Broadstone Stream" = "(b) Broadstone Stream \n(taxonomic aggregation)",
             "Broadstone Stream size_agg" = "(c) Broadstone Stream \n(size aggregation)",
             "Broom" = "(d) Broom", "Capinteria" = "(e) Capinteria", "Caricaie Lakes" = "(f) Caricaie Lakes",
             "Grasslands" = "(g) Grasslands", "Mill Stream" = "(h) Mill Stream",
             "Skipwith Pond" = "(i) Skipwith Pond", "Small Reef" = "(j) Small Reef", "Tuesday Lake" = "(k) Tuesday Lake",
             "Ythan" = "(l) Ythan")


plot_dd_lc_mod <- ggplot(dd_lc_mod) +
  geom_point(aes(x = n_ext_by_S, y = acc_sec_ext_by_S, color = type)) +
  geom_line(aes(x = n_ext_by_S, y = acc_sec_ext_by_S, color = type)) + 
  geom_ribbon(aes(x = n_ext_by_S, ymin = l_acc_sec_ext_by_S, ymax = u_acc_sec_ext_by_S, color = type), alpha = 0.3) +
  facet_wrap(~fw_name, labeller = labeller(fw_name = fw_labs), nrow = 2, scale = "free") +
  theme_classic() +
  xlab("Species removed/S") +
  ylab("Cumulative secondary extinctions/S") +
  scale_color_manual(name = "Food web type", labels = c("ADBM" = "ADBM food web", "Empirical" = "Observed food web"), 
                     values = c("ADBM" = "red", "Empirical" = "blue")) +
  theme(legend.position="bottom", legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))


# ggsave(plot = plot_dd_lc_mod, filename = "results/plot_leastconnected.png", width = 13, height = 6)
