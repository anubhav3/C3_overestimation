# 24.11.2021
# Simulating extinctions for random nodes

random_ext <- function(net, nsim){
  
  dd <- data.frame(node = character(), acc_pri_ext = integer(), n_sec_ext = integer(), acc_sec_ext = integer(), nsim = integer())
  
  for(sim_ind in 1:nsim){
      dd_temp <- data.frame(node = character(), acc_pri_ext = integer(), n_sec_ext = integer(), acc_sec_ext = integer(), nsim = integer())
      new_net <- net
      k <- 1
      ext_list <- list()
    
    while(all(is.na(new_net)) == FALSE | sum(new_net) != 0){
      pool_node <- rownames(new_net)
      
      node <- sample(x = pool_node, size = 1)
      
      ext_list[[k]] <- sim_single_ext(net = new_net, node = node)
      new_net <- ext_list[[k]]$net
      
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
      print(k)
    }
    dd <- rbind(dd, dd_temp)
  }
  
  
  sim_ind <- sim_ind + 1
  
  return(dd)
}