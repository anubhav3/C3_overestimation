# 2022.02.13

# Plot how secondary extinctions vary when number of species removed is increased

library(dplyr)
library(ggplot2)

dd_rand_raw <- readRDS("results/random_raw_wo_basal_maxTSS.RDS")

dd_rand_mod <- dd_rand_raw %>%
  group_by(fw_name, type, nsim, S_wo_basal) %>%
  mutate(max_pri = max(acc_pri_ext)) %>%
  group_by(fw_name, type, S_wo_basal) %>%
  mutate(max_min_pri = min(max_pri)) %>%
  filter(acc_pri_ext <= max_min_pri) %>%
  group_by(fw_name, acc_pri_ext, type, S_wo_basal) %>%
  summarise(mean_acc_sec_ext = mean(acc_sec_ext), l_acc_sec_ext = min(acc_sec_ext), u_acc_sec_ext = max(acc_sec_ext)) %>%
  mutate(acc_pri_ext_by_S = acc_pri_ext/S_wo_basal, acc_sec_ext_by_S = mean_acc_sec_ext/S_wo_basal, l_acc_sec_ext_by_S = l_acc_sec_ext/S_wo_basal,
         u_acc_sec_ext_by_S = u_acc_sec_ext/S_wo_basal)




fw_labs <- c("Benguela Pelagic" = "(a) Benguela Pelagic", "Broadstone Stream" = "(b) Broadstone Stream \n(taxonomic aggregation)",
             "Broadstone Stream size_agg" = "(c) Broadstone Stream \n(size aggregation)",
             "Broom" = "(d) Broom", "Capinteria" = "(e) Capinteria", "Caricaie Lakes" = "(f) Caricaie Lakes",
             "Grasslands" = "(g) Grasslands", "Mill Stream" = "(h) Mill Stream",
             "Skipwith Pond" = "(i) Skipwith Pond", "Small Reef" = "(j) Small Reef", "Tuesday Lake" = "(k) Tuesday Lake",
             "Ythan" = "(l) Ythan")


plot_dd_random_mod <- ggplot(dd_rand_mod) +
  geom_point(aes(x = acc_pri_ext_by_S, y = acc_sec_ext_by_S, color = type)) +
  geom_line(aes(x = acc_pri_ext_by_S, y = acc_sec_ext_by_S, color = type)) +
  facet_wrap(~fw_name, labeller = labeller(fw_name = fw_labs), nrow = 2) +
  theme_classic() +
  xlab("Species removed/S") +
  ylab("Cumulative secondary extinctions/S") +
  scale_color_manual(name = "Food web type", labels = c("ADBM" = "ADBM food web", "Empirical" = "Observed food web"), 
                     values = c("ADBM" = "red", "Empirical" = "blue")) +
  theme(legend.position="bottom", legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

# ggsave(plot = plot_dd_random_mod, filename = "results/plot_random_wo_basal_maxTSS.png", width = 13, height = 6)



dd_rand_raw %>%
  filter(fw_name == "Capinteria") %>%
  group_by(type, fw_name, acc_pri_ext) %>%
  summarise(mean_acc_sec_ext = mean(acc_sec_ext)) %>%
  ggplot() +
  geom_point(aes(x = acc_pri_ext, y = mean_acc_sec_ext, color = type)) +
  geom_line(aes(x = acc_pri_ext, y = mean_acc_sec_ext, color = type))


asd <- dd_rand_raw %>%
  filter(fw_name == "Capinteria", type == "Empirical")
