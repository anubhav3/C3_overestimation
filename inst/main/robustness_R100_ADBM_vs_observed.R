# 2022.07.04
# Robustness (R100) of ADBM predicted food webs VS robustness of observed food webs

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(latex2exp)


rob_all_raw1 <- readRDS("results/robustness_R100_leastconnected.RDS")
rob_all_raw2 <- readRDS("results/robustness_R100_random.RDS")
rob_all_raw3 <- readRDS("results/robustness_R100_mostconnected_uncertainty.RDS")
