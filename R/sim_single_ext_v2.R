# 2022.07.06
# Simulating single primary extinction in a food web i.e removing given node from a given food web
# Calculating secondary extinction 
# Ensure that the input network "net" does not have any isolated nodes


sim_single_ext_v2 <- function(net, node, basal_species){
  
  indeg <- rowSums(net)
  outdeg <- colSums(net)
  
  totdeg <- indeg + outdeg
  
  new_net <- net[!rownames(net) %in% node, !colnames(net) %in% node]
  
  if(length(new_net) == 1){
    new_net <- as.matrix(new_net)
    lost_node <- setdiff(rownames(net), node)
    rownames(new_net) <- lost_node
    colnames(new_net) <- lost_node
  }
  
  sp_outdeg_zero <- names(which(colSums(new_net) == 0))
  sp_outdeg_zero_wo_basal <- setdiff(sp_outdeg_zero, basal_species)
  
  count <- 1
  n_sec_ext <- length(sp_outdeg_zero_wo_basal)
  while(length(sp_outdeg_zero_wo_basal) != 0){
    
    node_to_sec_ext <- sp_outdeg_zero_wo_basal
    new_net_2 <- new_net[!rownames(new_net) %in% node_to_sec_ext, !colnames(new_net) %in% node_to_sec_ext]
    
    if(length(new_net_2) == 1){
      new_net_2 <- as.matrix(new_net_2)
      lost_node <- setdiff(rownames(new_net), node_to_sec_ext)
      rownames(new_net_2) <- lost_node
      colnames(new_net_2) <- lost_node
    }
    
    sp_outdeg_zero <- names(which(colSums(new_net_2) == 0))
    sp_outdeg_zero_wo_basal <- setdiff(sp_outdeg_zero, basal_species)
    
    n_sec_ext <- n_sec_ext + length(sp_outdeg_zero_wo_basal)
    # print(count)
    count <- count + 1
    new_net <- new_net_2
  }
  
  if(count > 1){
    list_ret <- list(net = new_net_2, n_sec_ext = n_sec_ext)
  } else {
    new_net_2 <- new_net
    list_ret <- list(net = new_net_2, n_sec_ext = n_sec_ext)
  }
  
  return(list_ret)
}

