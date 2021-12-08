# 22.11.2021
# Simulating single extinction in a network

sim_single_ext <- function(net, node){
  
  indeg <- rowSums(net)
  print(length((indeg)))
  outdeg <- colSums(net)
  print(length((outdeg)))
  totdeg <- indeg + outdeg
  

  new_net <- net[!rownames(net) %in% node, !colnames(net) %in% node]
  
  if(length(new_net) == 1){
    new_net <- as.matrix(new_net)
    lost_node <- setdiff(rownames(net), node)
    rownames(new_net) <- lost_node
    colnames(new_net) <- lost_node
  }
  
  ## Predators of node "node"
  node_pred <- net[node,]
  
  node_to_sec_ext <- which(outdeg[as.logical(node_pred)] == 1)
  
  if(length(node_to_sec_ext) != 0){
    new_net_2 <- new_net[!rownames(new_net) %in% names(node_to_sec_ext), !colnames(new_net) %in% names(node_to_sec_ext)]
    if(length(new_net_2) == 1){
      new_net_2 <- as.matrix(new_net_2)
      lost_node <- setdiff(rownames(new_net), names(node_to_sec_ext))
      rownames(new_net_2) <- lost_node
      colnames(new_net_2) <- lost_node
    }
  } else {
    new_net_2 <- new_net
  }
  
  ## Setdiff to ensure cannibalistic extinction are not included again in secondary ext
  n_sec_ext <- length(setdiff(names(node_to_sec_ext), node))
  
  list_ret <- list(net = new_net_2, n_sec_ext = n_sec_ext)
  
  return(list_ret)
  
  }
  
  
  
  