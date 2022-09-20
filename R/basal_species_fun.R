# 2022.07.06
# Returns an array of basal species

basal_species_fun <- function(net){
  
  indeg <- rowSums(net)
  outdeg <- colSums(net)
  
  basal_outdeg <- names(which(outdeg == 0))
  indeg_zero <- names(which(indeg == 0))
  
  return(setdiff(basal_outdeg, indeg_zero))
}