#  2022.09.13
# delta R50 vs delta connectance

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(latex2exp)
library(HDInterval)


fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

fw_labs_nice <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
                  "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
                  "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
                  "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
                  "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)",
                  "All food webs" = "All food webs")

#### Most connected ####

rob_all_raw_mc <- readRDS("results/robustness_R50_mostconnected_uncertainty.RDS")

fw_all <- unique(rob_all_raw_mc$fw_name)

## Observed food web
rob_all_mc_emp <- rob_all_raw_mc %>%
  filter(type == "Empirical")

rob_all_mc_emp <- data.frame(rob_all_mc_emp)

rob_all_mc_emp <- rob_all_mc_emp %>%
  select(-c(S))

#### ADBM predicted food webs
rob_all_mc_ADBM <- rob_all_raw_mc %>%
  filter(type == "ADBM_ABC")

rob_all_mc_ADBM <- data.frame(rob_all_mc_ADBM)

rob_all_mc_ADBM <- rob_all_mc_ADBM %>%
  select(-c(S))

rob_all_mc_ADBM <- rob_all_mc_ADBM %>%
  group_by(fw_name, type) %>%
  summarise(connectance = connectance, nsim_fw = nsim_fw, robustness = robustness,
            l_conn = as.numeric(hdi(connectance)[1]), u_conn = as.numeric(hdi(connectance)[2])) %>%
  filter(connectance >= l_conn & connectance <= u_conn) %>%
  select(-c(l_conn, u_conn))

rob_all_mc_ADBM <- data.frame(rob_all_mc_ADBM)


dd_all_mc <- data.frame(fw_name = character(), type = character(), connectance = double(), 
                        robustness = double(), slope = double())

for(fw in fw_all){
  
  emp_conn <- rob_all_mc_emp$connectance[rob_all_mc_emp$fw_name == fw]
  emp_rob <- rob_all_mc_emp$robustness[rob_all_mc_emp$fw_name == fw]
  
  dd_loc <- rob_all_mc_ADBM %>%
    filter(fw_name == fw) %>%
    mutate(slope = ((robustness - emp_rob)/0.5)/((connectance - emp_conn)/1))
  
  dd_all_mc <- rbind(dd_all_mc, dd_loc)
}


#### Graph
plot_slope_mc <- dd_all_mc %>%
  filter(is.finite(slope)) %>%
  ggplot() +
  geom_boxplot(aes(x = fw_name, y = slope), alpha = 0.5, outlier.size = 0.3) +
  geom_boxplot(aes(y = slope, x = "All food webs"), outlier.size = 0.3) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_classic() +
  ylim(c(-6, 6)) +
  # scale_y_continuous(breaks = c(-3, -1.5, 0, 1.5, 3), limits = c(-3.5, 3.5)) +
  # geom_point(data = dd_all_mean, mapping = aes(x = fw_name, y = mean_slope), color = "red", size = 3) +
  # geom_point(data = dd_all_finite, mapping = aes(x = "All food webs", y = mean(slope)), color = "red", size = 3) +
  # ylab(expression(paste(frac(paste(Delta, R[50], "/", R[50]) , paste(Delta, connectance, "/", connectance))))) +
  ylab("") +
  xlab("Food web") +
  scale_x_discrete(labels = fw_labs_nice) +
  ggtitle("(a) Most connected") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 15))



#### Random ####

rob_all_raw_ra <- readRDS("results/robustness_R50_random_uncertainty_1000.RDS")

rob_all_raw_ra <- rob_all_raw_ra %>%
  filter(nsim_ind %in% c(1:500))

fw_all <- as.character(unique(rob_all_raw_ra$fw_name))

## Observed food web
rob_all_ra_emp <- rob_all_raw_ra %>%
  filter(type == "Empirical") %>%
  group_by(fw_name, type, nsim_fw, connectance) %>%
  summarise(rob_med_fw = median(robustness)) %>%
  mutate(robustness = rob_med_fw)

rob_all_ra_emp <- data.frame(rob_all_ra_emp)

rob_all_ra_emp <- rob_all_ra_emp %>%
  select(-rob_med_fw)

#### ADBM predicted food webs
rob_all_ra_ADBM <- rob_all_raw_ra %>%
  filter(type == "ADBM_ABC") %>%
  group_by(fw_name, type, nsim_fw, connectance) %>%
  summarise(rob_med_fw = median(robustness)) %>%
  mutate(robustness = rob_med_fw)

rob_all_ra_ADBM <- data.frame(rob_all_ra_ADBM)

rob_all_ra_ADBM <- rob_all_ra_ADBM %>%
  select(-rob_med_fw)

rob_all_ra_ADBM <- rob_all_ra_ADBM %>%
  group_by(fw_name, type) %>%
  summarise(connectance = connectance, nsim_fw = nsim_fw, robustness = robustness,
            l_conn = as.numeric(hdi(connectance)[1]), u_conn = as.numeric(hdi(connectance)[2])) %>%
  filter(connectance >= l_conn & connectance <= u_conn) %>%
  select(-c(l_conn, u_conn))

rob_all_ra_ADBM <- data.frame(rob_all_ra_ADBM)


dd_all_ra <- data.frame(fw_name = character(), type = character(), connectance = double(), 
                        robustness = double(), slope = double())

for(fw in fw_all){
  
  emp_conn <- rob_all_ra_emp$connectance[rob_all_ra_emp$fw_name == fw]
  emp_rob <- rob_all_ra_emp$robustness[rob_all_ra_emp$fw_name == fw]
  
  dd_loc <- rob_all_ra_ADBM %>%
    filter(fw_name == fw) %>%
    mutate(slope = ((robustness - emp_rob)/0.5)/((connectance - emp_conn)/1))
  
  dd_all_ra <- rbind(dd_all_ra, dd_loc)
}


#### Graph
plot_slope_ra <- dd_all_ra %>%
  filter(is.finite(slope)) %>%
  ggplot() +
  geom_boxplot(aes(x = fw_name, y = slope), alpha = 0.5, outlier.size = 0.3) +
  geom_boxplot(aes(y = slope, x = "All food webs"), outlier.size = 0.3) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_classic() +
  ylim(c(-6, 6)) +
  # scale_y_continuous(breaks = c(-3, -1.5, 0, 1.5, 3), limits = c(-3.5, 3.5)) +
  # ylab(expression(paste(frac(paste("Normalised difference in robustness ", (R[50])), "Normalised difference in connectance")))) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels = fw_labs_nice) +
  ggtitle("(b) Random") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  theme(text = element_text(size = 15))



#### Least connected ####

rob_all_raw_lc <- readRDS("results/robustness_R50_leastconnected_uncertainty.RDS")

fw_all <- unique(rob_all_raw_lc$fw_name)

## Observed food web
rob_all_lc_emp <- rob_all_raw_lc %>%
  filter(type == "Empirical")

rob_all_lc_emp <- data.frame(rob_all_lc_emp)

rob_all_lc_emp <- rob_all_lc_emp %>%
  select(-c(S))

#### ADBM predicted food webs
rob_all_lc_ADBM <- rob_all_raw_lc %>%
  filter(type == "ADBM_ABC")

rob_all_lc_ADBM <- data.frame(rob_all_lc_ADBM)

rob_all_lc_ADBM <- rob_all_lc_ADBM %>%
  select(-c(S))

rob_all_lc_ADBM <- rob_all_lc_ADBM %>%
  group_by(fw_name, type) %>%
  summarise(connectance = connectance, nsim_fw = nsim_fw, robustness = robustness,
            l_conn = as.numeric(hdi(connectance)[1]), u_conn = as.numeric(hdi(connectance)[2])) %>%
  filter(connectance >= l_conn & connectance <= u_conn) %>%
  select(-c(l_conn, u_conn))

rob_all_lc_ADBM <- data.frame(rob_all_lc_ADBM)


dd_all_lc <- data.frame(fw_name = character(), type = character(), connectance = double(), 
                        robustness = double(), slope = double())

for(fw in fw_all){
  
  emp_conn <- rob_all_lc_emp$connectance[rob_all_lc_emp$fw_name == fw]
  emp_rob <- rob_all_lc_emp$robustness[rob_all_lc_emp$fw_name == fw]
  
  dd_loc <- rob_all_lc_ADBM %>%
    filter(fw_name == fw) %>%
    mutate(slope = ((robustness - emp_rob)/0.5)/((connectance - emp_conn)/1))
  
  dd_all_lc <- rbind(dd_all_lc, dd_loc)
}


#### Graph
plot_slope_lc <- dd_all_lc %>%
  filter(is.finite(slope)) %>%
  ggplot() +
  geom_boxplot(aes(x = fw_name, y = slope), alpha = 0.5, outlier.size = 0.3) +
  geom_boxplot(aes(y = slope, x = "All food webs"), outlier.size = 0.3) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_classic() +
  ylim(c(-20, 20)) +
  # scale_y_continuous(breaks = c(-3, -1.5, 0, 1.5, 3), limits = c(-3.5, 3.5)) +
  # geom_point(data = dd_all_mean, mapping = aes(x = fw_name, y = mean_slope), color = "red", size = 3) +
  # geom_point(data = dd_all_finite, mapping = aes(x = "All food webs", y = mean(slope)), color = "red", size = 3) +
  # ylab(expression(paste(frac(paste(Delta, R[50], "/", R[50]) , paste(Delta, connectance, "/", connectance))))) +
  ylab("") +
  xlab("") +
  scale_x_discrete(labels = fw_labs_nice) +
  ggtitle("(c) Least connected") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5), axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  theme(text = element_text(size = 15))


#### Merging all the plots ####

plot_R50_slope <- ggarrange(plot_slope_mc, plot_slope_ra, plot_slope_lc, nrow = 1, 
                            widths = c(1, 0.67, 0.67))

plot_R50_slope <- annotate_figure(plot_R50_slope, bottom = 
                                    text_grob(expression(paste(frac(paste("Difference in normalised robustness ", (R[50])), "Difference in normalised connectance"))),
                                              hjust = 0.05, size = 15))

# ggsave(plot = plot_R50_slope, filename = "results/plot_R50_slope.png", width = 15, height = 6)
