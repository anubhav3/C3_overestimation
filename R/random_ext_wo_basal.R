# 2022.07.25
# Simulating extinctions for random nodes (without basal species)

random_ext_wo_basal <- function(net, nsim){
  
  dd <- data.frame(node = character(), acc_pri_ext = integer(), n_sec_ext = integer(), acc_sec_ext = integer(), nsim = integer())
  basal_species <- basal_species_fun(net)
  
  ## Removing isolated nodes
  isolated_nodes <- isolated_nodes_fun(net = net)
  not_isolated_nodes <- setdiff(x = colnames(net), y = isolated_nodes)
  net <- net[not_isolated_nodes, not_isolated_nodes]
  
  for(sim_ind in 1:nsim){
    dd_temp <- data.frame(node = character(), acc_pri_ext = integer(), n_sec_ext = integer(), acc_sec_ext = integer(), nsim = integer())
    new_net <- net
    k <- 1
    ext_list <- list()
    
    wo_basal <- setdiff(rownames(new_net), basal_species)
    
    while(length(wo_basal) != 0){
      pool_node <- wo_basal
      
      node <- sample(x = pool_node, size = 1)
      
      ext_list[[k]] <- sim_single_ext_v2(net = new_net, node = node, basal_species = basal_species)
      new_net <- ext_list[[k]]$net
      
      wo_basal <- setdiff(rownames(new_net), basal_species)
      
      if(k != 1){
        dd_temp <- rbind(dd_temp, 
                         data.frame(node = node, acc_pri_ext = k, n_sec_ext = ext_list[[k]][[2]],
                                    acc_sec_ext = ext_list[[k]][[2]] + dd_temp$acc_sec_ext[k-1], nsim = sim_ind)
        )
      } else {
        dd_temp <- rbind(dd_temp, 
                         data.frame(node = node, acc_pri_ext = k, n_sec_ext = ext_list[[k]][[2]],
                                    acc_sec_ext = ext_list[[k]][[2]], nsim = sim_ind)
        )
      }
      
      k <- k + 1
      # print(k)
    }
    dd <- rbind(dd, dd_temp)
  }
  
  return(dd)
}