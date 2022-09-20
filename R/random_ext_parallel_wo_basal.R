# 25.11.2021


random_ext_parallel_wo_basal <- function(net, nsim, n_cores){
  
  nsim_pcore <- nsim/n_cores
  registerDoParallel(cores = n_cores)
  
  res_pcores <- foreach(i = 1:n_cores, .combine = rbind) %dopar% 
    {
      set.seed(i)
      result <- random_ext_wo_basal(net = net, nsim = nsim_pcore)
      list(result)
    }
  
  result_all <- res_pcores[[1]]
  
  for(i in 2:n_cores){
    result_all <- rbind(result_all, 
                        res_pcores[[i]] %>%
                          mutate(nsim = (i-1)*nsim_pcore + nsim)
    ) 
  }
  
  return(result_all)  
}




