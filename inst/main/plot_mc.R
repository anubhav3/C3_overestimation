# 22.10.2021
# Plot how secondary extinctions vary when number of species removed is increased

dd_mc <- readRDS("results/dd_mc_v2.RDS")

library(ggplot2)
library(dplyr)
library(latex2exp)

dd_mc_mod <- dd_mc %>%
  mutate(n_ext_by_S = n_ext/S, sec_ext_by_S = sec_ext/S)

plot_dd_mc_mod <- ggplot(dd_mc_mod) +
  geom_point(aes(x = n_ext_by_S, y = sec_ext_by_S, color = type), size = 2) +
  facet_wrap(~fw_name, scales = "free") +
  theme_classic() +
  xlab("Species removed/S") +
  ylab("Cumulative secondary extinction/S") +
  scale_color_discrete(name = "Type")

# ggsave(plot = plot_dd_mc_mod, filename = "results/plot_dd_mc.png", width = 10, height = 6)

# ggsave(plot = plot_dd_mc_mod, filename = "results/plot_dd_mc_2008_ABC_emp.png", width = 9, height = 6)

dd_rand_mod <- dd_rand %>%
  mutate(n_ext_by_S = n_ext/S, mean_ext_by_S = mean_ext/S)

plot_dd_rand_mod <- ggplot(dd_rand_mod) +
  geom_point(aes(x = n_ext_by_S, y = mean_ext_by_S, color = type), size = 2) +
  facet_wrap(~fw_name, scales = "free") +
  theme_classic() +
  xlab("Species removed/S") +
  ylab("Cumulative secondary extinction/S") +
  scale_color_discrete(name = "Type")


# ggsave(plot = plot_dd_rand_mod, filename = "results/plot_dd_rand.png", width = 10, height = 6)



dd_rand_mod_1 <- dd_rand %>%
  mutate(n_ext_by_S = n_ext/S, mean_ext_by_S = mean_ext/S, CI_lower_by_S = CI_lower/S, CI_upper_by_S = CI_upper/S)

plot_dd_rand_mod_1 <- ggplot(dd_rand_mod_1) +
  geom_point(aes(x = n_ext_by_S, y = mean_ext_by_S, color = type), size = 2) +
  geom_line(aes(x = n_ext_by_S, y = mean_ext_by_S, color = type)) +
  geom_ribbon(aes(x = n_ext_by_S, ymin = CI_lower_by_S, ymax = CI_upper_by_S, fill = type), alpha = 0.3) +
  facet_wrap(~fw_name, scales = "free") +
  theme_classic() +
  xlab("Species removed/S") +
  ylab("Cumulative secondary extinction/S") +
  scale_color_discrete(name = "Type") +
  scale_fill_discrete(name = "Type")


# ggsave(plot = plot_dd_rand_mod_1, filename = "results/plot_dd_rand_1.png", width = 10, height = 6)
