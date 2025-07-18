fn_target_RT <- function(all_data, vars) {
  
  mean_RT    <- all_data %>% 
    filter(RT > vars$odd_rt_min) %>%
    filter(RT < vars$odd_rt_max) %>%
    filter(category=="target") %>% 
    filter(target_resp==1) %>%   
    summarize(mean_RT = mean(RT))
  
  summary_target_RT <- all_data %>% 
    filter(category=="target") %>% filter(target_resp==1)
  
  return(mean_RT, summary_target_RT)
  
}