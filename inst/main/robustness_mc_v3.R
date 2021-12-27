# 14.12.2021
# We investigate how the robustness vary across food webs 
# We use the output produced by using the script comp_mc_v3.R

library(cheddar)
sourceDirectory("../C1_method/R", modifiedOnly=FALSE)
sourceDirectory("R", modifiedOnly=FALSE)

dd_mc <- readRDS("results/dd_mc_v3.RDS")

fw_list <- unique(dd_mc$fw_name)
rob_all <- data.frame(robustness = double(), fw_name = character(), type = character(), connectance = double(),
                      S = double(), intervality = double())

