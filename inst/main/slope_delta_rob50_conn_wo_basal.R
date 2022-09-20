# 2022.08.29
# We investigate the difference in robustness (R50) between ADBM predicted food webs and observed food webs wrt the difference in their connectance (without basal species)

library(HDInterval)
library(readxl)
library(latex2exp)
library(facetscales)
library(R.utils)
library(ggplot2)
library(dplyr)

sourceDirectory("../C1_method_v2/R", modifiedOnly = FALSE)
sourceDirectory("../C3_overestimation/R", modifiedOnly = FALSE)

dd_all_main <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
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

dd_all <- data.frame(robustness = double(), fw_name = character(), type = character(),
                     connectance = double(), conn_wo_basal = double(), S = integer(), nsim = integer())

fw_all <- as.character(unique(dd$fw_name))

fw_meta <- read_excel("../C1_method_v2/data/ADBM_2008_Petchey_par.xlsx")

for(fw in fw_all){
  
  ## Empirical food web
  dd_emp <- dd %>%
    filter(fw_name == fw & type == "Empirical")
  
  fw_emp <- readRDS(paste0("../C1_method_v2/data/", fw, ".web.RDS"))
  fw_emp <- fw_emp$predation.matrix
  
  conn_wo_basal_emp <- connectance_wo_basal(pred_mat = fw_emp)

  dd_emp <- dd_emp %>%
    mutate(conn_wo_basal = conn_wo_basal_emp)
    
  dd_all <- rbind(dd_all, dd_emp)
  
  ## ADBM food web
  fw_tol <- fw_meta$dist_rej[fw_meta$foodweb == fw]
  
  for(nsim_loc in 1:1000){
    
    dd_ADBM <- dd %>%
      filter(fw_name == fw & type == "ADBM_ABC" & nsim == nsim_loc)
    
    fw_ADBM <- ADBM_ABC_fw_all(fw_name = fw, fw_tol = fw_tol, nsim = nsim_loc)$ADBM_pred_mat
    
    conn_wo_basal_ADBM <- connectance_wo_basal(pred_mat = fw_ADBM)
    
    dd_ADBM <- dd_ADBM %>%
      mutate(conn_wo_basal = conn_wo_basal_ADBM)
    
    dd_all <- rbind(dd_all, dd_ADBM)
    
    # print(nsim_loc)
  }
  print(fw)
}


# saveRDS(object = dd_all, file = "results/robustness_R50_random_wo_basal_uncertainty.RDS")


dd_all_rob <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                         conn_wo_basal = double(), S = integer(), nsim = integer(), slope = double())

for(fw in fw_all){
  
  emp_conn <- unique(dd_all$connectance[dd_all$fw_name == fw & dd_all$type == "Empirical"])
  emp_conn_wo_basal <- unique(dd_all$conn_wo_basal[dd_all$fw_name == fw & dd_all$type == "Empirical"])
  emp_rob <- dd_all$robustness[dd_all$fw_name == fw & dd_all$type == "Empirical"]
  
  
  dd_loc <- dd_all %>%
    filter(fw_name == fw, type == "ADBM_ABC") %>%
    # mutate(slope = ((robustness - emp_rob)/emp_rob)/((connectance - emp_conn)/emp_conn))
    mutate(slope = ((robustness - emp_rob))/((conn_wo_basal - emp_conn_wo_basal)))
  
  dd_all_rob <- rbind(dd_all_rob, dd_loc)
}


dd_all$fw_name <- factor(dd_all$fw_name, levels = fw_labs)

dd_ra <- dd_all_rob %>%
  filter(is.finite(slope))


plot_ra_conn_wo_basal <- dd_all_rob %>%
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
  ylab(expression(paste(frac(paste("Difference in robustness ", (R[50])), "Difference in connectance without basal species")))) +
  xlab("Food web") +
  scale_x_discrete(labels = fw_labs_nice) +
  ggtitle("(b) Random") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

# ggsave(plot = plot_ra_conn_wo_basal, filename = "results/plot_R50_slope_conn_wo_basal.png")



dd_all %>%
  filter(type == "Empirical") %>%
  ggplot() +
  geom_point(aes(x = connectance, y = conn_wo_basal, color = type)) +
  geom_abline(slope = 1, intercept = 0) +
  xlim(c(0, 1)) +
  ylim(c(0, 1))


dd_all %>%
  filter(type == "ADBM_ABC" & fw_name == "Broadstone Stream size_agg") %>%
  ggplot() +
  geom_point(aes(x = connectance, y = conn_wo_basal, color = fw_name)) +
  geom_abline(slope = 1, intercept = 0) +
  xlim(c(0, 1)) +
  ylim(c(0, 1))



View(dd_all %>%
       filter(fw_name == "Broadstone Stream size_agg"))
