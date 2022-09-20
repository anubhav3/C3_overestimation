# 2022.08.02

# Plot how secondary extinctions vary when number of species removed is increased

library(dplyr)
library(ggplot2)

dd_la <- readRDS("results/largestbodymass_maxTSS.RDS")

dd_la_mod <- dd_la %>%
  mutate(n_ext_by_S = n_ext/S, acc_sec_ext_by_S = acc_sec_ext/S)

fw_labs <- c("Benguela Pelagic" = "(a) Benguela Pelagic", "Broadstone Stream" = "(b) Broadstone Stream \n(taxonomic aggregation)",
             "Broadstone Stream size_agg" = "(c) Broadstone Stream \n(size aggregation)",
             "Broom" = "(d) Broom", "Capinteria" = "(e) Capinteria", "Caricaie Lakes" = "(f) Caricaie Lakes",
             "Grasslands" = "(g) Grasslands", "Mill Stream" = "(h) Mill Stream",
             "Skipwith Pond" = "(i) Skipwith Pond", "Small Reef" = "(j) Small Reef", "Tuesday Lake" = "(k) Tuesday Lake",
             "Ythan" = "(l) Ythan")


plot_dd_la_mod <- ggplot(dd_la_mod) +
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


# ggsave(plot = plot_dd_la_mod, filename = "results/plot_largestbodymass_maxTSS.png", width = 13, height = 6)