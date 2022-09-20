# 2022.09.19
# Plot Max trophic level of the ADBM predicted food webs vs that of the empirical food webs

library(dplyr)
library(ggplot2)

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

max_tl_emp_df <- readRDS("results/max_tl_emp.RDS")
max_tl_ADBM_df <- readRDS("results/max_tl_ADBM.RDS")

max_tl_df <- merge(max_tl_emp_df, max_tl_ADBM_df, by = "fw_name")


plot_max_tl_ADBM_vs_emp <- ggplot(max_tl_df) +
  geom_boxplot(aes(x = max_tl_emp, y = max_tl_ADBM, color = fw_name), width = 0.1, outlier.size = 0.3) +
  xlim(c(0, 10)) +
  ylim(c(0, 10)) +
  scale_color_brewer(palette = "Paired", labels = fw_labs, name = "Food web") +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Maximum trophic level in observed food webs") +
  ylab("Maximum trophic level in ADBM predicted food webs") +
  theme_classic()

# ggsave(plot = plot_max_tl_ADBM_vs_emp, filename = "results/plot_max_tl_ADBM_vs_emp.png", width = 8, height = 5)
