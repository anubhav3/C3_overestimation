# 2022.07.29

# Plot how secondary extinctions vary when number of species removed (without basal) is increased

library(dplyr)
library(ggplot2)

dd_mc <- readRDS("results/most_connected_wo_basal_maxTSS.RDS")

dd_mc_mod <- dd_mc %>%
  mutate(n_ext_by_S = n_ext/S_wo_basal, acc_sec_ext_by_S = acc_sec_ext/S_wo_basal)

fw_labs <- c("Benguela Pelagic" = "(a) Benguela Pelagic", "Broadstone Stream" = "(b) Broadstone Stream \n(taxonomic aggregation)",
             "Broadstone Stream size_agg" = "(c) Broadstone Stream \n(size aggregation)",
             "Broom" = "(d) Broom", "Capinteria" = "(e) Capinteria", "Caricaie Lakes" = "(f) Caricaie Lakes",
             "Grasslands" = "(g) Grasslands", "Mill Stream" = "(h) Mill Stream",
             "Skipwith Pond" = "(i) Skipwith Pond", "Small Reef" = "(j) Small Reef", "Tuesday Lake" = "(k) Tuesday Lake",
             "Ythan" = "(l) Ythan")


plot_dd_mc_mod <- ggplot(dd_mc_mod) +
  geom_point(aes(x = n_ext_by_S, y = acc_sec_ext_by_S, color = type)) +
  geom_line(aes(x = n_ext_by_S, y = acc_sec_ext_by_S, color = type)) +
  facet_wrap(~fw_name, labeller = labeller(fw_name = fw_labs), nrow = 2) +
  theme_classic() +
  xlab("Species removed/S") +
  ylab("Cumulative secondary extinctions/S") +
  scale_color_manual(name = "Food web type", labels = c("ADBM" = "ADBM food web", "Empirical" = "Observed food web"), 
                     values = c("ADBM" = "red", "Empirical" = "blue")) +
  theme(legend.position="bottom", legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))


# ggsave(plot = plot_dd_mc_mod, filename = "results/plot_mostconnected_wo_basal_maxTSS.png", width = 13, height = 6)
