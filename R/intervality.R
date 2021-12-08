# 14.10.2021
# We compute the intervality of food webs using the approach defined in Stouffer et al. (2006) and written in Cheddar package

library(cheddar)

## Converts predation matrix into an object which cheddar can use
mat.to.comm <- function(pred.mat, fw_title){
  append_n <- function(n){
    append_n <- paste(c('n',n), collapse='')
  }
  
  dm <- dim(pred.mat)[1]
  nod <- data.frame(node = sapply(1:dm, append_n), stringsAsFactors = FALSE)
  prop <- list(title = fw_title)
  trop <- data.frame(resource = character(), consumer = character())
  
  for(i in 1:dm){
    for(j in 1:dm){
      if(pred.mat[i,j] == 1){
        temp <- data.frame(resource = append_n(i), consumer = append_n(j), stringsAsFactors = FALSE)
        trop = rbind(trop, temp)
      }
    }
  }
  mat.to.comm <- Community(nod, prop, trop)
}


intervality <- function(pred_mat){
  title <- "foodweb"
  community <- mat.to.comm(pred_mat, title)
 
  sdg <- SumDietGaps(community = community)
   
  return(sdg)
}
