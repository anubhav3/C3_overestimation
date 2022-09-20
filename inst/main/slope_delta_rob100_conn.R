# 2022.06.29
# We investigate the difference in robustness (R100) between ADBM predicted food webs and observed food webs wrt the difference in their connectance

library(HDInterval)
library(readxl)
library(latex2exp)
library(facetscales)


dd_all_main <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                          S = integer(), nsim = integer(), slope = double(), study = character())

dd_all_mean_main <- data.frame(fw_name = character(), mean_slope = double(), study = character())

#### Most connected ####
fw_labs <- c("Benguela Pelagic", "Broadstone Stream", "Broom", "Capinteria", "Caricaie Lakes", 
             "Grasslands", "Mill Stream", "Skipwith Pond", "Small Reef", "Tuesday Lake",
             "Ythan", "Broadstone Stream size_agg")

dd <- readRDS("results/robustness_R50_mostconnected_uncertainty_new.RDS")

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
  emp_rob <- mean(unique(dd$robustness[dd$fw_name == fw & dd$type == "Empirical"]))
  
  
  dd_loc <- dd %>%
    filter(fw_name == fw, type == "ADBM_ABC") %>%
    # mutate(slope = ((robustness - emp_rob)/emp_rob)/((connectance - emp_conn)/emp_conn))
    mutate(slope = ((robustness - emp_rob))/((connectance - emp_conn)))
  
  dd_all <- rbind(dd_all, dd_loc)
}

dd_all$fw_name <- factor(dd_all$fw_name, levels = fw_labs)

dd_mc <- dd_all %>%
  filter(is.finite(slope))

# dd_all_main <- rbind(dd_all_main, cbind(dd_all, data.frame(study = rep("(a) Most connected", dim(dd_all)[1]))))

dd_all_mean <- dd_all %>%
  filter(is.finite(slope)) %>%
  group_by(fw_name) %>%
  summarise(mean_slope = mean(slope))

# dd_all_mean_main <- rbind(dd_all_mean_main, cbind(dd_all_mean, data.frame(study = rep("(a) Most connected", dim(dd_all_mean)[1]))))


# fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
#              "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
#              "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
#              "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
#              "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

dd_all_finite <- dd_all %>%
  filter(is.finite(slope))

plot_mc_R50_slope <- dd_all %>%
  filter(is.finite(slope)) %>%
  ggplot() +
  geom_boxplot(aes(x = fw_name, y = slope), alpha = 0.5, outlier.shape = NA) +
  # geom_boxplot(aes(y = slope, x = "All food webs"), outlier.shape = NA) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_classic() +
  # ylim(c(-3, 3)) +
  scale_y_continuous(breaks = c(-3, -1.5, 0, 1.5, 3), limits = c(-3.5, 3.5)) +
  geom_point(data = dd_all_mean, mapping = aes(x = fw_name, y = mean_slope), color = "red", size = 3) +
  # geom_point(data = dd_all_finite, mapping = aes(x = "All food webs", y = mean(slope)), color = "red", size = 3) +
  # ylab(expression(paste(frac(paste(Delta, R[50], "/", R[50]) , paste(Delta, connectance, "/", connectance))))) +
  ylab("") +
  xlab("Food web") +
  scale_x_discrete(labels = fw_labs_nice) +
  ggtitle("(a) Most connected") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))


#### Random ####

dd <- readRDS("results/robustness_R100_random_uncertainty_july14.RDS")

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
    mutate(slope = ((robustness - emp_rob))/((connectance - emp_conn)))
  
  dd_all <- rbind(dd_all, dd_loc)
}

# dd_all_main <- rbind(dd_all_main, cbind(dd_all, data.frame(study = rep("(b) Random", dim(dd_all)[1]))))

dd_all$fw_name <- factor(dd_all$fw_name, levels = fw_labs)

dd_ra <- dd_all %>%
  filter(is.finite(slope))

dd_all_mean <- dd_all %>%
  filter(is.finite(slope)) %>%
  group_by(fw_name) %>%
  summarise(mean_slope = mean(slope))

# dd_all_mean_main <- rbind(dd_all_mean_main, cbind(dd_all_mean, data.frame(study = rep("(b) Random", dim(dd_all_mean)[1]))))


# fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
#              "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)",
#              "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
#              "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
#              "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
#              "Ythan" = "Ythan")

dd_all_finite <- dd_all %>%
  filter(is.finite(slope))

plot_random_R50_slope <- dd_all %>%
  filter(is.finite(slope)) %>%
  ggplot() +
  geom_boxplot(aes(x = fw_name, y = slope), alpha = 0.5, outlier.shape = NA) +
  # geom_boxplot(aes(y = slope, x = "All food webs"), outlier.shape = NA) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_classic() +
  # ylim(c(-3, 3)) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2), limits = c(-2, 2)) +
  geom_point(data = dd_all_mean, mapping = aes(x = fw_name, y = mean_slope), color = "red", size = 3) +
  # geom_point(data = dd_all_finite, mapping = aes(x = "All food webs", y = mean(slope)), color = "red", size = 3) +
  # ylab(expression(paste(frac(paste(Delta, R[50], "/", R[50]) , paste(Delta, connectance, "/", connectance))))) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels = fw_labs_nice) +
  ggtitle("(b) Random") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))


#### Least connected ####

dd <- readRDS("results/robustness_R50_leastconnected_uncertainty_new.RDS")

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
  emp_rob <- mean(unique(dd$robustness[dd$fw_name == fw & dd$type == "Empirical"]))
  
  
  dd_loc <- dd %>%
    filter(fw_name == fw, type == "ADBM_ABC") %>%
    mutate(slope = ((robustness - emp_rob)/emp_rob)/((connectance - emp_conn)/emp_conn))
  # mutate(slope = ((robustness - emp_rob))/((connectance - emp_conn)))
  
  dd_all <- rbind(dd_all, dd_loc)
}

# dd_all_main <- rbind(dd_all_main, cbind(dd_all, data.frame(study = rep("(c) Least connected", dim(dd_all)[1]))))

dd_all$fw_name <- factor(dd_all$fw_name, levels = fw_labs)

dd_lc <- dd_all %>%
  filter(is.finite(slope))

dd_all_mean <- dd_all %>%
  filter(is.finite(slope)) %>%
  group_by(fw_name) %>%
  summarise(mean_slope = mean(slope))

# dd_all_mean_main <- rbind(dd_all_mean_main, cbind(dd_all_mean, data.frame(study = rep("(c) Least connected", dim(dd_all_mean)[1]))))


fw_labs_nice <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
                  "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
                  "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
                  "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
                  "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

dd_all_finite <- dd_all %>%
  filter(is.finite(slope))

plot_lc_R50_slope <- dd_all %>%
  filter(is.finite(slope)) %>%
  ggplot() +
  geom_boxplot(aes(x = fw_name, y = slope), alpha = 0.5, outlier.shape = NA) +
  # geom_boxplot(aes(y = slope, x = "All food webs"), outlier.shape = NA) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_classic() +
  # ylim(c(-1, 1)) +
  scale_y_continuous(breaks = c(-3, -1.5, 0, 1.5, 3), limits = c(-3, 3)) +
  geom_point(data = dd_all_mean, mapping = aes(x = fw_name, y = mean_slope), color = "red", size = 3) +
  # geom_point(data = dd_all_finite, mapping = aes(x = "All food webs", y = mean(slope)), color = "red", size = 3) +
  # ylab(expression(paste(frac(paste(Delta, R[50], "/", R[50]) , paste(Delta, connectance, "/", connectance))))) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels = fw_labs_nice) +
  ggtitle("(c) Least connected") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  coord_flip()



#### Merging all the plots ####

plot_R50_slope <- ggarrange(plot_mc_R50_slope, plot_random_R50_slope, plot_lc_R50_slope, nrow = 1, 
                            widths = c(1, 0.67, 0.67))
plot_R50_slope <- annotate_figure(plot_R50_slope, bottom = 
                                    text_grob(expression(paste(frac(paste("Difference in robustness ", (R[50])), "Difference in connectance"))),
                                              hjust = 0.05))

# ggsave(plot = plot_R50_slope, filename = "results/plot_R50_slope_new.png", width = 15, height = 6)

# dummy <- rbind(data.frame(slope = c(-2, 2), study = "(a) Most connected", stringsAsFactors=FALSE),
#                data.frame(slope = c(-0.3, 0.3), study = "(b) Random", stringsAsFactors=FALSE),
#                data.frame(slope = c(-0.5, 0.5), study = "(c) Least connected", stringsAsFactors=FALSE))
# 
# scales_y <- list(
#   `(a) Most connected` = scale_y_continuous(limits = c(-2, 2), breaks = c(-2, 0, 2)),
#   `(b) Random` = scale_y_continuous(limits = c(-0.3, 0.3), breaks = c(-0.3, 0, 0.3)),
#   `(c) Least connected` = scale_y_continuous(limits = c(-0.5, 0.5), breaks = c(-0.5, 0, 0.5))
# )
# 
# dd_all_main %>%
#   filter(is.finite(slope)) %>%
#   ggplot() +
#   # coord_flip() +
#   geom_boxplot(aes(x = fw_name, y = slope), alpha = 0.5, outlier.shape = NA) +
#   facet_grid_sc(cols = vars(study), scales = list(y = scales_y)) +
#   geom_abline(slope = 0, intercept = 0, linetype = 2) +
#   theme_classic() +
#   # ylim(c(-0.5, 0.5)) +
#   geom_point(data = dd_all_mean_main, mapping = aes(x = fw_name, y = mean_slope), color = "red", size = 3) +
#   # ylab(expression(paste(frac(paste(Delta, R[50], "/", R[50]) , paste(Delta, connectance, "/", connectance))))) +
#   ylab("") +
#   xlab("") +
#   # scale_x_discrete(labels = fw_labs) +
#   # ggtitle("(c) Least connected") +
#   theme(plot.title = element_text(hjust = 0.5)) 
# 
# dd %>%
#   filter(fw_name == "Broadstone Stream") %>%
#   ggplot() +
#   geom_point(aes(x = connectance, y = robustness, color = type))
# 


sadd <- dd_ra %>%
  filter(fw_name == "Caricaie Lakes", type == "ADBM_ABC")
