# 25.11.2021

random_ext_summary <- function(ext_seq){
  
  ext_seq_smry <- ext_seq %>%
    group_by(acc_pri_ext) %>%
    summarise(mean_acc_sec_ext = mean(acc_sec_ext), l_acc_sec_ext = min(acc_sec_ext), u_acc_sec_ext = max(acc_sec_ext),
              count_n = n())
  
  return(ext_seq_smry)
}