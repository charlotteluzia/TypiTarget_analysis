fn_isolation_spread <- function(all_data, vars){
  
  summary_iso_spread <- all_data %>%
    filter(task == "memory") %>%
    filter(category %in% vars$categories) %>%
    filter(iso_spread == 1) %>%
    group_by(participant, typicality, category, iso_spread) %>%
    summarize(
      mem_hitr      = (sum(mem_perform == "hit" , na.rm = TRUE) + 0.5) / (sum(cond == "old", na.rm = TRUE) + 1),
      mem_falr      = (sum(mem_perform == "fa" , na.rm = TRUE) + 0.5)  / (sum(cond == "new", na.rm = TRUE) + 1),
      .groups = 'drop' # Drop the automatic grouping to avoid issues later
    ) %>%
    mutate(
      mem_dprime      = qnorm(mem_hitr) - qnorm(mem_falr),
      mem_crit        = -0.5 * (qnorm(mem_hitr) + qnorm(mem_falr)),
    )
  
  summary_no_iso_spread <- all_data %>%
    filter(task == "memory") %>%
    filter(iso_spread == 0) %>%
    filter(category %in% vars$categories) %>%
    group_by(participant, typicality, category, iso_spread) %>%
    summarize(
      mem_hitr      = (sum(mem_perform == "hit" , na.rm = TRUE) + 0.5) / (sum(cond == "old", na.rm = TRUE) + 1),
      mem_falr      = (sum(mem_perform == "fa" , na.rm = TRUE) + 0.5)  / (sum(cond == "new", na.rm = TRUE) + 1),
      .groups = 'drop' # Drop the automatic grouping to avoid issues later
    ) %>%
    mutate(
      mem_dprime      = qnorm(mem_hitr) - qnorm(mem_falr),
      mem_crit        = -0.5 * (qnorm(mem_hitr) + qnorm(mem_falr)),
    )
  
  summary_iso_spread_complete <- summary_iso_spread %>%
    full_join(summary_no_iso_spread, by=NULL)
  
  return(summary_iso_spread_complete)
  
  
}