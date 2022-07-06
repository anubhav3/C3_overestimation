# 2022.07.06
# Simulating single primary extinction in a food web i.e removing given node from a given food web
# Calculating secondary extinction 


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
