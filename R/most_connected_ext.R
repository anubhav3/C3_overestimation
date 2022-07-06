# 23.11.2021
# Simulating extinction for most connected nodes

most_connected_ext <- function(net){
  
  dd <- data.frame(node = character(), acc_pri_ext = integer(), n_sec_ext = integer(), acc_sec_ext = integer())
  new_net <- net
  k <- 1
  ext_list <- list()
  
  # Stop when all the species is extinct
  while(all(is.na(new_net)) == FALSE){
    indeg <- rowSums(new_net)
    outdeg <- colSums(new_net)
    totdeg <- indeg + outdeg
    
    max_totdeg <- which.max(totdeg)
    node <- names(max_totdeg)
    
    ext_list[[k]] <- sim_single_ext(net = new_net, node = node)
    new_net <- ext_list[[k]]$net
    
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
