

library(tidyr)
library(HDInterval)
#### Random ####

fw_labs <- c("Benguela Pelagic", "Broadstone Stream", "Broom", "Capinteria", "Caricaie Lakes", 
             "Grasslands", "Mill Stream", "Skipwith Pond", "Small Reef", "Tuesday Lake",
             "Ythan", "Broadstone Stream size_agg")

fw_labs_nice <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
                  "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
                  "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
                  "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
                  "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)",
                  "All food webs" = "All food webs")

rob_all_raw_ra <- readRDS("results/OP_2022_08_10/robustness_R50_random_uncertainty_sep4.RDS")

dd <- rob_all_raw_ra %>%
  group_by(fw_name, type, nsim_fw, connectance) %>%
  summarise(robustness = median(robustness))

dd <- dd %>%
  group_by(fw_name, type) %>%
  summarise(connectance = connectance, robustness = robustness, l_conn = as.numeric(hdi(connectance)[1]), u_conn = as.numeric(hdi(connectance)[2])) %>%
  filter(connectance >= l_conn & connectance <= u_conn) %>%
  group_by(fw_name, type) %>%
  select(-c(l_conn, u_conn))

metadata_fw_tol <- read_excel("~/Google Drive/GitHub/C1_method_v2/data/parameter_values.xlsx")
fw_all <- metadata_fw_tol$foodweb

dd_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                     S = integer(), nsim = integer(), slope = double())

for(fw in fw_all){
  
  emp_conn <- unique(dd$connectance[dd$fw_name == fw & dd$type == "Empirical"])
  emp_rob <- mean(dd$robustness[dd$fw_name == fw & dd$type == "Empirical"])
  
  
  dd_loc <- dd %>%
    filter(fw_name == fw, type == "ADBM_ABC") %>%
    # mutate(slope = ((robustness - emp_rob)/emp_rob)/((connectance - emp_conn)/emp_conn))
    mutate(slope = ((robustness - emp_rob)/0.5)/((connectance - emp_conn)/1.0))
  
  dd_all <- rbind(dd_all, dd_loc)
}

# dd_all_main <- rbind(dd_all_main, cbind(dd_all, data.frame(study = rep("(b) Random", dim(dd_all)[1]))))

dd_all$fw_name <- factor(dd_all$fw_name, levels = fw_labs)

plot_ra_R50_slope_conn <- dd_all %>%
  filter(is.finite(slope)) %>%
  ggplot() +
  geom_boxplot(aes(x = fw_name, y = slope), alpha = 0.5, outlier.shape = NA) +
  # geom_boxplot(aes(y = slope, x = "All food webs"), outlier.shape = NA) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_classic() +
  # ylim(c(-3, 3)) +
  # scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), limits = c(-2, 2)) +
  scale_y_continuous(limits = c(-5, 5)) +
  # geom_point(data = dd_all_mean, mapping = aes(x = fw_name, y = mean_slope), color = "red", size = 3) +
  # geom_point(data = dd_all_finite, mapping = aes(x = "All food webs", y = mean(slope)), color = "red", size = 3) +
  # ylab(expression(paste(frac(paste(Delta, R[50], "/", R[50]) , paste(Delta, connectance, "/", connectance))))) +
  ylab(expression(paste(frac(paste("Normalised difference in robustness ", (R[50])), "Normalised difference in connectance")))) +
  xlab("Food web") +
  scale_x_discrete(labels = fw_labs_nice) +
  ggtitle("(b) Random") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), axis.ticks.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

# ggsave(plot_ra_R50_slope_conn, file = "results/OP_2022_08_10/plot_ra_R50_slope_conn_median.png")

dd_all %>%
  filter(is.finite(slope)) %>%
  group_by(fw_name) %>%
  summarise(median_slope = median(slope))
