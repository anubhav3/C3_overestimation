# 25.10.2021
# We investigate how the robustness vary across food webs

library(cheddar)
sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

dd_mc <- readRDS("results/dd_mc_v2.RDS")

fw_list <- unique(dd_mc$fw_name)
rob_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                      S = double(), intervality = double())

for(foodweb in fw_list){

  #### ADBM ABC predicted food web first ####
  dd_mc_ADBM_ABC <- dd_mc %>%
    filter(fw_name == foodweb, type == "ADBM_ABC") %>%
    mutate(total_ext = (n_ext + acc_sec_ext)/S)
  
  N_row <- dim(dd_mc_ADBM_ABC)[1]
  for(i in 1:N_row){
    ith_row <- dd_mc_ADBM_ABC[i,]
    if(ith_row$total_ext >= 0.5){
      robustness <- ith_row$n_ext/ith_row$S
      break
    }
  }
  
  pred_mat_ADBM_ABC <- readRDS(paste0("data/ADBM_ABC_predicted_foodwebs/", foodweb, ".pred_mat.RDS"))
  S <- dim(pred_mat_ADBM_ABC)[1]
  conn_ADBM_ABC <- sum(pred_mat_ADBM_ABC)/(dim(pred_mat_ADBM_ABC)[1]^2)
  intervality_ADBM_ABC <- intervality(pred_mat_ADBM_ABC)
  rob_all <- rbind(rob_all,
                   data.frame(robustness = robustness, fw_name = foodweb, type = "ADBM_ABC", connectance = conn_ADBM_ABC,
                              S = S, intervality = intervality_ADBM_ABC)
                   )
  
  #### ADBM 2008 predicted food web second ####
  dd_mc_ADBM_2008 <- dd_mc %>%
    filter(fw_name == foodweb, type == "ADBM_2008") %>%
    mutate(total_ext = (n_ext + acc_sec_ext)/S)
  
  N_row <- dim(dd_mc_ADBM_2008)[1]
  for(i in 1:N_row){
    ith_row <- dd_mc_ADBM_2008[i,]
    if(ith_row$total_ext >= 0.5){
      robustness <- ith_row$n_ext/ith_row$S
      break
    }
  }
  
  pred_mat_ADBM_2008 <- readRDS(paste0("data/ADBM_2008_predicted_foodwebs/", foodweb, ".pred_mat.RDS"))
  conn_ADBM_2008 <- sum(pred_mat_ADBM_2008)/(dim(pred_mat_ADBM_2008)[1]^2)
  intervality_ADBM_2008 <- intervality(pred_mat_ADBM_2008)
  rob_all <- rbind(rob_all,
                   data.frame(robustness = robustness, fw_name = foodweb, type = "ADBM_2008", connectance = conn_ADBM_2008,
                              S = S, intervality = intervality_ADBM_2008)
  )
  
  #### Emprirical food web third ####
  dd_mc_ADBM <- dd_mc %>%
    filter(fw_name == foodweb, type == "Empirical")
  
  dd_mc_ADBM <- dd_mc_ADBM %>%
    mutate(total_ext = (n_ext + acc_sec_ext)/S)
  
  N_row <- dim(dd_mc_ADBM)[1]
  for(i in 1:N_row){
    ith_row <- dd_mc_ADBM[i,]
    if(ith_row$total_ext >= 0.5){
      robustness <- ith_row$n_ext/ith_row$S
      break
    }
  }
  
  pred_mat_emp <- readRDS(paste0("../C1_method_v2/data/", foodweb, ".web.RDS"))$predation.matrix
  conn_emp <- sum(pred_mat_emp)/(dim(pred_mat_emp)[1]^2)
  intervality_emp <- intervality(pred_mat_emp)
  rob_all <- rbind(rob_all,
                   data.frame(robustness = robustness, fw_name = foodweb, type = "Empirical", connectance = conn_emp,
                              S = S, intervality = intervality_emp)
  )
}

# saveRDS(object = rob_all, file = "results/rob_mc_all.RDS")


########## Plotting robustness ##########


fw_labs <- c("Benguela Pelagic" = "Benguela Pelagic", "Broadstone Stream" = "Broadstone Stream (taxonomic aggregation)",
             "Broom" = "Broom", "Capinteria" = "Capinteria", "Caricaie Lakes" = "Caricaie Lakes",
             "Grasslands" = "Grasslands", "Mill Stream" = "Mill Stream",
             "Skipwith Pond" = "Skipwith Pond", "Small Reef" = "Small Reef", "Tuesday Lake" = "Tuesday Lake",
             "Ythan" = "Ythan", "Broadstone Stream size_agg" = "Broadstone Stream (size aggregation)")

plot_rob_all <- 
  rob_all %>%
  filter(type != "ADBM_2008") %>%
  ggplot() +
    geom_line(aes(x = connectance, y = robustness, color = fw_name)) +
    geom_point(aes(x = connectance, y = robustness, shape = type, color = fw_name), size = 5) +
    theme_classic() +
    scale_color_brewer(type = "qual", name = "Food web", palette = "Paired", labels = fw_labs) +
    scale_shape_discrete(name = "Type", labels = c("ADBM_ABC" = "ADBM food web", "Empirical" = "Observed food web")) +
    scale_x_log10() +
    ylab("Robustness") +
    xlab("Connectance") +
    theme(legend.position = "bottom")
  
# ggsave(plot = plot_rob_all, filename = "results/plot_rob_all.png", width = 11, height = 6)
    
plot_rob_fw <-
  rob_all %>%
    filter(type != "ADBM_2008") %>%
    ggplot() +
    # geom_line(aes(x = connectance, y = robustness, color = fw_name)) +
    geom_point(aes(x = fw_name, y = robustness, color = type), size = 5, position = position_dodge(width = 0.1)) +
    theme_classic() +
    scale_color_manual(name = "Type", labels = c("ADBM_ABC" = "ADBM food web", "Empirical" = "Observed food web"), 
                         values = c("ADBM_ABC" = "red", "Empirical" = "blue")) +
    scale_x_discrete(labels = fw_labs) +
    ylab("Robustness") +
    xlab("Food web") +
    theme(legend.position = "bottom")

# ggsave(plot = plot_rob_fw, filename = "results/plot_rob_fw.png", width = 16, height = 6)



plot_rob_all <- ggplot(rob_all) +
  geom_point(aes(x = connectance, y = robustness, shape = type, color = fw_name), size = 5) +
  geom_smooth(aes(x = connectance, y = robustness), method = "lm") +
  theme_classic() +
  scale_x_log10()


plot_rob_all <- rob_all %>%
  filter(type != "ADBM_2008") %>%
  ggplot(aes(x = connectance, y = robustness, color = fw_name)) +
  geom_line() +
  geom_point(aes(x = connectance, y = robustness, shape = type), size = 5) +
  theme_classic() +
  scale_x_log10()

# ggsave(plot = plot_rob_all, filename = "results/plot_rob_all.png")

lm_rob <- lm(robustness ~ type+connectance, data = rob_all)
summary(lm_rob)
anova(lm_rob)

mix_rob <- lmer(robustness ~ connectance + (1 + connectance|fw_name), data = rob_all)
summary(mix_rob)
anova(mix_rob)


lm_wo_emp <- rob_all %>%
  filter(type != "Empirical") %>%
  lm(formula = robustness ~ connectance)
summary(lm_wo_emp)
anova(lm_wo_emp)


lm_wo_2008 <- rob_all %>%
  filter(type != "ADBM_2008") %>%
  lm(formula = robustness ~ connectance)
summary(lm_wo_2008)
anova(lm_wo_2008)
