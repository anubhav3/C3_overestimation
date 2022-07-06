# 09.02.2022

library(HDInterval)
dd_mc <- readRDS("results/dd_mc_v3.RDS")

dd_mc_mod <- dd_mc %>%
  group_by(n_ext, type, fw_name) %>%
  summarise(mean_acc_sec_ext = mean(acc_sec_ext), l_acc_sec_ext = as.numeric(hdi(acc_sec_ext, credMass = 0.95))[1],
            u_acc_sec_ext = as.numeric(hdi(acc_sec_ext, credMass = 0.95))[2]) %>%
  mutate(n_ext_by_S = n_ext/S, mean_acc_sec_ext_by_S = mean_acc_sec_ext/S, l_acc_sec_ext_by_S = l_acc_sec_ext/S,
         u_acc_sec_ext_by_S = u_acc_sec_ext/S)


dd_mc_mod %>%
  # filter(fw_name == "Benguela Pelagic", type == "ADBM_ABC") %>%
ggplot() +
  geom_point(aes(x = n_ext_by_S, y = mean_acc_sec_ext_by_S, color = type)) +
  geom_ribbon(aes(x = n_ext_by_S, ymin = l_acc_sec_ext_by_S, ymax = u_acc_sec_ext_by_S, fill = type), alpha = 0.5) +
  facet_wrap(~fw_name, scale = "free") +
  theme_classic() +
  xlab("Species removed/S") +
  ylab("Cumulative secondary extinction/S") +
  scale_color_discrete(name = "Type") +
  scale_fill_discrete(name = "Type")

