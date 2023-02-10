# 2022.07.07
# Obtain isolated nodes if any (isolated nodes are the one that do not feed on any other nodes)

isolated_nodes_fun <- function(net){
  
  indeg <- rowSums(net)
  outdeg <- colSums(net)
  
  totdeg <- indeg + outdeg
  
  return(names(which(totdeg == 0)))
  
}

