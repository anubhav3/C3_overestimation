# 2022.08.30
# Plot multiple cumulative secondary extinctions for random extinction scenario for independent simulations

library(dplyr)
library(ggplot2)

dd_rand_raw <- readRDS("results/random_raw_uncertainty_maxTSS.RDS")

dd_rand_mod <- dd_rand_raw %>%
  mutate(acc_pri_ext_by_S = n_ext/S, acc_sec_ext_by_S = acc_sec_ext/S)

fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream \n (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream \n (size aggregation)")

plot_ra_ext_lines <- dd_rand_mod %>%
  filter(nsim_rand %in% c(36:40)) %>%
  ggplot() +
    geom_point(aes(x = acc_pri_ext_by_S, y = acc_sec_ext_by_S, color = type, color = as.factor(nsim_rand))) +
    geom_line(aes(x = acc_pri_ext_by_S, y = acc_sec_ext_by_S, color = type, color = as.factor(nsim_rand))) +
    facet_wrap(~fw_name, labeller = labeller(fw_name = fw_labs), nrow = 2) +
    theme_classic() +
    xlab("Species removed/S") +
    ylab("Cumulative secondary extinctions/S") +
    # scale_color_manual(name = "Food web type", labels = c("ADBM" = "ADBM food web", "Empirical" = "Observed food web"), 
    #                    values = c("ADBM" = "red", "Empirical" = "blue")) +
    scale_shape_discrete(name = "Type") +
    scale_linetype_discrete(name = "Type") +
    scale_color_manual(name = "Food web type", labels = c("ADBM_ABC" = "ADBM food web with maximum TSS", "Empirical" = "Observed food web"), 
                     values = c("ADBM_ABC" = "red", "Empirical" = "blue")) +
    theme(legend.position="bottom", legend.text = element_text(size = 15),
          legend.title = element_text(size = 15))

# ggsave(plot = plot_ra_ext_lines, filename = "results//plot_ra_extlines_maxTSS.png", width = 13, height = 6)
