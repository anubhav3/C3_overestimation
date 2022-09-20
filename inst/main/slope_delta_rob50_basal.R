# 2022.08.24
# We investigate the difference in robustness (R50) between ADBM predicted food webs and observed food webs wrt the difference in their proportion of basal species

library(HDInterval)
library(readxl)
library(latex2exp)
library(facetscales)
library(R.utils)
library(cheddar)
library(DirectedClustering)
library(bipartite)
library(dplyr)
library(ggplot2)

sourceDirectory("../C1_method_v2/R", modifiedOnly = FALSE)

dd_all_main <- data.frame(robustness = double(), fw_name = character(), type = character(), prop_basal = double(),
                          S = integer(), nsim = integer(), slope = double(), study = character())

dd_all_mean_main <- data.frame(fw_name = character(), mean_slope = double(), study = character())

fw_labs <- c("Benguela Pelagic", "Broadstone Stream", "Broom", "Capinteria", "Caricaie Lakes", 
             "Grasslands", "Mill Stream", "Skipwith Pond", "Small Reef", "Tuesday Lake",
             "Ythan", "Broadstone Stream size_agg")

fw_labs_nice <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
                  "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
                  "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
                  "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
                  "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)",
                  "All food webs" = "All food webs")


#### Random ####

dd <- readRDS("results/robustness_R50_random_uncertainty_july14.RDS")

fw_tol_all <- read_excel("../C1_method_v2/data/parameter_values.xlsx")

fw_all <- as.character(fw_tol_all$foodweb)

dd_ADBM_emp_all <- data.frame(robustness = double(), fw_name = character(), type = character(),
                              connectance = double(), S = integer(), nsim = integer(),
                              conn_2 = double(), prop_basal = double())

for(fw in fw_all){
  fw_tol <- fw_tol_all$dist_rej[fw_tol_all$foodweb == fw]
  
  ### ADBM predicted food webs
  
  dd_single_ADBM <- dd %>%
    filter(fw_name == fw & type == "ADBM_ABC")
  
  dd_C1_ADBM <- readRDS(paste0("../C1_method_v2/results/rejection/", fw, "/rN=1000_tol=", fw_tol, "_TSS_lower_a/", fw, "_prop.RDS"))
  
  dd_C1_ADBM <- dd_C1_ADBM$prop
  
  dd_ADBM <- cbind(dd_single_ADBM, conn_2 = dd_C1_ADBM$connectance, prop_basal = dd_C1_ADBM$prop_basal)
  
  ### Empirical food web
  
  dd_single_emp <- dd %>%
    filter(fw_name == fw & type == "Empirical")
  
  dd_C1_emp <- readRDS(paste0("../C1_method_v2/data/", fw, ".web.RDS"))
  
  dd_C1_emp <- real_prop_v2(dd_C1_emp)
  
  dd_emp <- cbind(dd_single_emp, conn_2 = dd_C1_emp$connectance, prop_basal = dd_C1_emp$prop_basal)
  
  dd_ADBM_emp <- rbind(dd_ADBM, dd_emp)
  
  dd_ADBM_emp_all <- rbind(dd_ADBM_emp_all, dd_ADBM_emp)
}


### Computing the slope

dd_all <- data.frame(robustness = double(), fw_name = character(), type = character(), prop_basal = double(),
                     S = integer(), nsim = integer(), slope = double())

for(fw in fw_all){
  
  emp_basal <- unique(dd_ADBM_emp_all$prop_basal[dd_ADBM_emp_all$fw_name == fw & dd_ADBM_emp_all$type == "Empirical"])
  emp_rob <- unique(dd_ADBM_emp_all$robustness[dd_ADBM_emp_all$fw_name == fw & dd_ADBM_emp_all$type == "Empirical"])
  
  dd_loc <- dd_ADBM_emp_all %>%
    filter(fw_name == fw, type == "ADBM_ABC", prop_basal < emp_basal) %>%
    # mutate(slope = ((robustness - emp_rob)/emp_rob)/((prop_basal - emp_basal)/emp_basal))
    mutate(slope = ((robustness - emp_rob))/((prop_basal - emp_basal)))
  
  dd_all <- rbind(dd_all, dd_loc)
}


dd_all$fw_name <- factor(dd_all$fw_name, levels = fw_labs)

dd_ra <- dd_all %>%
  filter(is.finite(slope))


dd_all_mean <- dd_all %>%
  filter(is.finite(slope)) %>%
  group_by(fw_name) %>%
  summarise(mean_slope = mean(slope))

dd_all_finite <- dd_all %>%
  filter(is.finite(slope))


plot_ra_R50_slope_basal <- dd_all %>%
  filter(is.finite(slope)) %>%
  ggplot() +
  geom_boxplot(aes(x = fw_name, y = slope), alpha = 0.5, outlier.shape = NA) +
  # geom_boxplot(aes(y = slope, x = "All food webs"), outlier.shape = NA) +
  geom_abline(slope = 0, intercept = 0, linetype = 2) +
  theme_classic() +
  # ylim(c(-3, 3)) +
  scale_y_continuous(limits = c(-5, 15)) +
  # geom_point(data = dd_all_mean, mapping = aes(x = fw_name, y = mean_slope), color = "red", size = 3) +
  # geom_point(data = dd_all_finite, mapping = aes(x = "All food webs", y = mean(slope)), color = "red", size = 3) +
  ylab(expression(paste(frac(paste("Difference in robustness ", (R[50])), "Difference in proportion of basal species")))) +
  # ylab("") +
  xlab("Food web") +
  scale_x_discrete(labels = fw_labs_nice) +
  ggtitle("(b) Random") +
  coord_flip() +
  # theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))


# ggsave(plot = plot_ra_R50_slope_basal, filename = "results/plot_ra_R50_slope_basal.png")
