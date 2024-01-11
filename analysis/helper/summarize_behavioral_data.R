
summarize_behavioral_data <- function(raw_d){
  
  bd_fit_summary <- raw_d %>% 
    mutate(trial_type = case_when(
      trial_type == "background" ~ "background", 
      block_type == "deviant_block" & trial_type == "deviant" ~ violation_type
    )) %>% 
    group_by(trial_number, trial_type) %>% 
    summarise(mean_lt = mean(total_rt)) 
  
  return (bd_fit_summary)
}


summarize_behavioral_data_with_block <- function(raw_d){
  
  bd_fit_summary <- raw_d %>% 
    mutate(trial_type = case_when(
      trial_type == "background" ~ "background", 
      block_type == "deviant_block" & trial_type == "deviant" ~ violation_type
    )) %>% 
    group_by(trial_number, trial_type, block_number) %>% 
    summarise(mean_lt = mean(total_rt)) 
  
  return (bd_fit_summary)
}


summarize_hab_dishab <- function(raw_d){
  
  bd_fit_summary <- raw_d %>% 
    group_by(total_trial_number, trial_number, trial_type) %>% 
    summarise(mean_lt = mean(total_rt)) 
  
  return (bd_fit_summary)
  
}

summarize_dev_only <- function(raw_d){
  
  bd_fit_summary <- raw_d %>% 
    mutate(trial_type = case_when(
      trial_type == "background" ~ "background", 
      block_type == "deviant_block" & trial_type == "deviant" ~ violation_type
    )) %>% 
    group_by(trial_type)%>% 
    summarise(mean_lt = mean(total_rt)) 
  
  return (bd_fit_summary)
  
}