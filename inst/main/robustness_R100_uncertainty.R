# 2022.05.10
# We plot the robustness (R100: the proportion of species that have to be removed to achieve a total loss of all species),
# for all the extinction scenarios

library(cheddar)
library(R.utils)
library(ggplot2)
library(HDInterval)
library(readxl)
library(dplyr)

sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

rob_all_raw1 <- readRDS("results/robustness_leastconnected.RDS") %>%
  select(-intervality)

rob_all_raw2 <- readRDS("results/robustness_R100_random.RDS")

rob_all_raw3 <- readRDS("results/robustness_R100_mostconnected.RDS")  %>%
  select(-nsim)



rob_all_raw1 <- rob_all_raw1 %>%
  mutate(study = "(a) Least connected")

rob_all_raw2 <- rob_all_raw2 %>%
  mutate(study = "(b) Random")

rob_all_raw3 <- rob_all_raw3 %>%
  mutate(study = "(c) Most connected")

rob_all_raw <- rbind(rob_all_raw1, rob_all_raw2, rob_all_raw3)

# rob_all_raw <- rob_all_raw %>%
#   mutate(robustness = ifelse(robustness > 0.5, 0.5, robustness))

library(raincloudplots)


rob_all_rain <- rob_all_raw %>%
  group_by(fw_name, type, S, study) %>%
  summarise(mean_connectance = mean(connectance), robustness = robustness, mean_robustness = mean(robustness))

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

plot_rob_all_rain <- ggplot(rob_all_rain) +
  # ggdist::stat_slab(aes(x = mean_connectance, y = robustness, fill = fw_name), width = 1.5, point_colour = NA,
  # alpha = 1.5) +
  geom_boxplot(aes(x = mean_connectance, y = robustness, group = mean_connectance), width = 0.003) +
  geom_point(aes(x = mean_connectance, y = mean_robustness, shape = type, color = fw_name), size = 2) +
  geom_line(aes(x = mean_connectance, y = mean_robustness, color = fw_name)) +
  facet_wrap(~study, scale = "free_y", nrow = 3) +
  # scale_x_log10() +
  ylab("Robustness") +
  xlab("Connectance") +
  theme(legend.position = "bottom") +
  scale_shape_discrete(name = "Type", labels = c("ADBM_ABC" = "ADBM food web", "Empirical" = "Observed food web")) +
  scale_color_brewer(type = "qual", name = "Food web", palette = "Paired", labels = fw_labs) +
  scale_fill_brewer(type = "qual", name = "Food web", palette = "Paired", labels = fw_labs) +
  theme_classic() +
  theme(legend.position = "right")
