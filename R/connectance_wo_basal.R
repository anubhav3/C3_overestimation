# 2022.08.29
# We compute the connectance of a food web without basal species

connectance_wo_basal <- function(pred_mat){
  
  nspecies <- dim(pred_mat)[1]
  var_colsum <- colSums(pred_mat)
  var_rowsum <- rowSums(pred_mat)
  
  zero_colsum <- which(var_colsum == 0)
  zero_rowsum <- which(var_rowsum == 0)
  
  basal_species <- setdiff(zero_colsum, zero_rowsum)
  
  if(length(basal_species) != 0){
    without_basal_species <- pred_mat[-basal_species, -basal_species]
    
    S_wo_basal <- dim(without_basal_species)[1]
    L_wo_basal <- sum(without_basal_species)
    
    conn_wo_basal <- sum(without_basal_species)/(dim(without_basal_species)[1]^2)
  }
  else{
    conn_wo_basal <- sum(pred_mat)/(dim(pred_mat)[1]^2)
  }
  
  
  return(conn_wo_basal)
}
