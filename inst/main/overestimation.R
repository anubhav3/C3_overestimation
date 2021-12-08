# 07.09.2021

# We investigate the effect of overestimation of connectance in the stability
library(R.utils)
sourceDirectory("../C1_method/R", modifiedOnly=FALSE)

######### Benguela Pelagic food web

fw_name <- "Benguela Pelagic"
# fw_name <- "Broadstone Stream size_agg"
# fw_name <- "Coachella"

real_fw <- readRDS(paste("../C1_method/data_new/", fw_name, ".web.Rdata", sep = ""))
real_pred_mat <- real_fw$predation.matrix

ADBM_fw_all <- readRDS(paste0("../C1_method/results/rejection/", fw_name, "/rN=1000_tol=0.6_TSS_lower_a/", fw_name, ".Rdata"))

sel_ind <- which.min(ADBM_fw_all$dist)

ADBM_fw_par <- ADBM_fw_all$post_dists[sel_ind,]

## Generating the ADBM predicted food web

fw_data <- real_fw

n_species <- length(fw_data$species.sizes)

M <- fw_data$species.sizes
M <- sort(M)

sim_a <- 10^ADBM_fw_par$a
sim_ai <- ADBM_fw_par$ai
sim_aj <- ADBM_fw_par$aj
sim_r.b <- 10^ADBM_fw_par$r.b

local_par <- data.frame(a = sim_a, ai = sim_ai, aj = sim_aj, r.b = sim_r.b)
sim_model_core_par <- list(e = 1, n = 1, ni = -3/4, r.a = 1, M  = M)
pred_mat <- ratio.power(opt=local_par, x = sim_model_core_par)
rownames(pred_mat) <- as.character(seq(1, n_species))
colnames(pred_mat) <- as.character(seq(1, n_species))

ADBM_pred_mat <- pred_mat

real_conn <- sum(real_pred_mat)/(dim(real_pred_mat)[1]^2)
ADBM_conn <- sum(ADBM_pred_mat)/(dim(ADBM_pred_mat)[1]^2)

### Using the package NetworkExtinction to simulate extinction

library(NetworkExtinction)
library(network)

real_net <- as.network(real_pred_mat, loops = TRUE)
ADBM_net <- as.network(ADBM_pred_mat, loops = TRUE)

real_ext <- Mostconnected(Network = real_net)
ADBM_ext <- Mostconnected(Network = ADBM_net)


real_ext_rand <- RandomExtinctions(Network = real_net, nsim = 1000)
ADBM_ext_rand <- RandomExtinctions(Network = ADBM_net, nsim = 1000)

ExtinctionPlot(History = real_ext)
ExtinctionPlot(History = ADBM_ext)

dd <- data.frame(sec_ext = real_ext$AccSecondaryExtinction,
                 n_ext = real_ext$NumExt,
                 type = "Real")
dd <- rbind(dd,
             data.frame(sec_ext = ADBM_ext$AccSecondaryExtinction,
                        n_ext = ADBM_ext$NumExt,
                        type = "ADBM"))

plot_dd <- ggplot(dd) +
  geom_point(aes(x = n_ext, y = sec_ext, color = type)) +
  geom_line(aes(x = n_ext, y = sec_ext, color = type)) +
  theme_bw() +
  xlab("Number of Extinctions") +
  ylab("Secondary extinctions")

# ggsave(plot = plot_dd, filename = "results/Benguela_Pelagic_ext_most_connected.png")

dd_rand <- data.frame(mean_ext = real_ext_rand$sims$AccSecondaryExtinction, 
                      SD = real_ext_rand$sims$SdAccSecondaryExtinction, 
                      n_ext = real_ext_rand$sims$NumExt,
                      type = "Real")

dd_rand <- rbind(dd_rand, data.frame(mean_ext = ADBM_ext_rand$sims$AccSecondaryExtinction, 
                                     SD = ADBM_ext_rand$sims$SdAccSecondaryExtinction, 
                                     n_ext = ADBM_ext_rand$sims$NumExt,
                                     type = "ADBM"))

plot_dd_rand <- ggplot(dd_rand) +
  geom_line(aes(x = n_ext, y = mean_ext, color = type)) +
  geom_ribbon(aes(x = n_ext, ymin = mean_ext - SD, ymax = mean_ext + SD, fill = type), alpha = 0.5) +
  theme_bw() +
  xlab("Number of Extinctions") +
  ylab("Secondary extinctions")

# ggsave(plot = plot_dd_rand, filename = "results/Benguela_Pelagic_ext_random.png")

