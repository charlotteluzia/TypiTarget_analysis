fn_summarize_mem_accuracy <- function(all_data, vars){
  
  summary_mem_accuracy <- all_data %>%
    filter(task == "memory") %>%
    filter(category %in% vars$categories) %>%
    group_by(participant, typicality, category, ImgDur) %>%
    summarize(
      mem_hitr      = (sum(mem_perform == "hit" , na.rm = TRUE) + 0.5) / (sum(cond == "old", na.rm = TRUE) + 1),
      mem_falr      = (sum(mem_perform == "fa" , na.rm = TRUE) + 0.5)  / (sum(cond == "new", na.rm = TRUE) + 1),
      .groups = 'drop' # Drop the automatic grouping to avoid issues later
    ) %>%
    mutate(
      mem_dprime      = qnorm(mem_hitr) - qnorm(mem_falr),
      mem_crit        = -0.5 * (qnorm(mem_hitr) + qnorm(mem_falr))
    )
  
  return(summary_mem_accuracy) 
}  
