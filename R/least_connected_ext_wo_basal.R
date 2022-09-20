# 2022.07.22
# Simulating extinction for least connected nodes
# Do not remove basal nodes

least_connected_ext_wo_basal <- function(net){
  
  dd <- data.frame(node = character(), acc_pri_ext = integer(), n_sec_ext = integer(), acc_sec_ext = integer())
  basal_species <- basal_species_fun(net)
  
  ## Removing isolated nodes
  isolated_nodes <- isolated_nodes_fun(net = net)
  not_isolated_nodes <- setdiff(x = colnames(net), y = isolated_nodes)
  net <- net[not_isolated_nodes, not_isolated_nodes]
  
  new_net <- net
  k <- 1
  ext_list <- list()
  
  wo_basal <- setdiff(rownames(new_net), basal_species)
  net_wo_basal <- new_net[wo_basal, wo_basal]
  
  # Stop when all the species is extinct
  while(all(is.na(net_wo_basal)) == FALSE){
    indeg <- rowSums(new_net)
    outdeg <- colSums(new_net)
    totdeg <- indeg + outdeg
    
    not_basal_species <- setdiff(names(totdeg), basal_species)
    
    min_totdeg <- which.min(totdeg[not_basal_species])
    node <- names(min_totdeg)
    
    ext_list[[k]] <- sim_single_ext_v2(net = new_net, node = node, basal_species = basal_species)
    new_net <- ext_list[[k]]$net
    
    wo_basal <- setdiff(rownames(new_net), basal_species)
    net_wo_basal <- new_net[wo_basal, wo_basal]
    
    
    if(k != 1){
      dd <- rbind(dd, 
                  data.frame(node = node, acc_pri_ext = k, n_sec_ext = ext_list[[k]][[2]],
                             acc_sec_ext = ext_list[[k]][[2]] + dd$acc_sec_ext[k-1])
      )
    }else{
      dd <- rbind(dd, 
                  data.frame(node = node, acc_pri_ext = k, n_sec_ext = ext_list[[k]][[2]],
                             acc_sec_ext = ext_list[[k]][[2]])
      )
    }
    
    k <- k + 1
    # print(k)
  }
  
  return(dd)
  
}
