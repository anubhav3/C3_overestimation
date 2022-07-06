# 2022.02.17

library(ggplot2)
library(ggpubr)
library(latex2exp)

plot_lc <- readRDS("results/ggplot_rob_leastconnected.RDS")
plot_mc <- readRDS("results/ggplot_rob_mostconnected.RDS")
plot_rand <- readRDS("results/ggplot_rob_random.RDS")

plot_all <- ggarrange(plot_lc, plot_mc, plot_rand, ncol = 3, common.legend = TRUE,
                      legend = "bottom")


#### load rob_all files ####

rob_all_lc <- readRDS("results/robustness_leastconnected.RDS")

rob_all_lc <- rob_all_lc %>%
  select(-c("intervality")) %>%
  mutate(study = "Least connected")


rob_all_mc <- readRDS("results/robustness_mostconnected.RDS")

rob_all_mc <- rob_all_mc %>%
  select(-c("intervality")) %>%
  mutate(study = "Most connected")


rob_all_random <- readRDS("results/robustness_random.RDS")

rob_all_random <- rob_all_random %>%
  group_by(fw_name, type, connectance, S) %>%
  summarise(robustness = mean(robustness))

rob_all_random <- rob_all_random %>%
  mutate(study = "Random")

rob_all <- rbind(rob_all_lc, rob_all_mc, rob_all_random)


rob_all$study = factor(rob_all$study, levels = c("Least connected", "Random", "Most connected"))


fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream \n(taxonomic aggregation)",
             "Broadstone Stream size_agg" = "Broadstone Stream \n(size aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan")

study_lab = c("Least connected" = "(a) Least connected",
              "Random" = "(b) Random",
              "Most connected" = "(c) Most connected")
##### Plotting ####

plot_rob_all <- 
  rob_all %>%
  ggplot() +
  geom_line(aes(x = connectance, y = robustness, color = fw_name)) +
  geom_point(aes(x = connectance, y = robustness, shape = type, color = fw_name), size = 2) +
  facet_wrap(~study, labeller = labeller(study = study_lab)) +
  theme_classic() +
  scale_color_brewer(type = "qual", name = "Food web", palette = "Paired", labels = fw_labs) +
  scale_shape_discrete(name = "Type", labels = c("ADBM" = "ADBM food web", "Empirical" = "Observed food web")) +
  scale_x_log10() +
  ylab(TeX(r'(Robustness $(R_{50})$)')) +
  xlab("Connectance") +
  ylim(c(0, 0.53)) +
  theme(legend.position = "bottom")


# ggsave(plot = plot_rob_all, filename = "results/plot_robustness_all.png", width = 10, height = 4)

