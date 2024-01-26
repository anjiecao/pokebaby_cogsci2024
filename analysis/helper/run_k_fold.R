

run_no_fold <- function(p_id, raw_bd, sd){
  
  
    # separate the data in to train & test
    data = summarize_behavioral_data(raw_bd) %>% ungroup() %>% left_join(sd %>% filter(param_id == p_id) %>% ungroup(), by = c("trial_number", "trial_type"))

    # fit the training set
    
    
    fitted_stats = colf_nlxb(mean_lt ~ mean_sample, data = data, lower = c(-Inf, 0.0000001))
    sample_slope = fitted_stats$coefficients["param_mean_sample"]
    sample_intercept = fitted_stats$coefficients["param_X.Intercept."]
    
    test_scaled <- data %>% 
      mutate(scaled_samples = mean_sample * sample_slope  + sample_intercept)
    
    # save results for each fold 
    rsquared <- cor(test_scaled$mean_lt, test_scaled$scaled_samples)^2
    rmse <- rmse(test_scaled$mean_lt, test_scaled$scaled_samples)
    
  
  
  return(
    tibble(
      "param_id" = p_id, 
      "rsquared" = rsquared, 
      "rmse" = rmse, 

    )
  )
  
}



run_k_fold <- function(p_id, raw_bd, sd){
  
  # get all the subjects 
  all_sbj <- unique(raw_d$prolific_id)
  n_folds = 10
  
  # currently testing on one set of parameter 
  rsquared <- vector(mode = "list", length = n_folds)
  rmse <- vector(mode = "list", length = n_folds)
  
  for (k in 1:n_folds){
    
    # separate the data in to train & test
    train_subject = sample(all_sbj, floor(length(all_sbj) * .9))
    train_data = summarize_behavioral_data(raw_d %>% filter(prolific_id %in% train_subject)) %>% ungroup() %>% left_join(sd %>% filter(param_id == p_id) %>% ungroup(), by = c("trial_number", "trial_type"))
    test_data = summarize_behavioral_data(raw_d %>% filter(!prolific_id %in% train_subject)) %>% ungroup() %>% left_join(sd %>% filter(param_id == p_id) %>% ungroup(), by = c("trial_number", "trial_type"))
    
    # fit the training set
    
    
    fitted_stats = colf_nlxb(mean_lt ~ mean_sample, data = train_data, lower = c(-Inf, 0.0000001))
    sample_slope = fitted_stats$coefficients["param_mean_sample"]
    sample_intercept = fitted_stats$coefficients["param_X.Intercept."]
    
    test_scaled <- test_data %>% 
      mutate(scaled_samples = mean_sample * sample_slope  + sample_intercept)
    
    # save results for each fold 
    rsquared[[k]] <- cor(test_scaled$mean_lt, test_scaled$scaled_samples)^2
    rmse[[k]] <- rmse(test_scaled$mean_lt, test_scaled$scaled_samples)
    
  }
  
  rsquared_average <- mean(unlist(rsquared))
  rmse_average <- mean(unlist(rmse))
  rsquared_sd = sd(unlist(rsquared))
  rmse_sd <- sd(unlist(rmse))
  
  
  return(
    tibble(
      "param_id" = p_id, 
      "mean_rsquared" = rsquared_average, 
      "mean_rmse" = rmse_average, 
      "ub_rsquared" = rsquared_average + 1.96 * (rsquared_sd / sqrt(n_folds)), 
      "lb_rsquared" = rsquared_average - 1.96 * (rsquared_sd / sqrt(n_folds)), 
      "ub_rmse" = rmse_average + 1.96 * (rmse_sd / sqrt(n_folds)), 
      "lb_rmse" = rmse_average - 1.96 * (rmse_sd / sqrt(n_folds))
      
    )
  )
  
}


## raw material to run other types of fitting 






run_k_fold_with_block_id <- function(p_id, raw_bd, sd){
  
  # get all the subjects 
  all_sbj <- unique(raw_d$prolific_id)
  n_folds = 10
  
  # currently testing on one set of parameter 
  rsquared <- vector(mode = "list", length = n_folds)
  rmse <- vector(mode = "list", length = n_folds)
  
  for (k in 1:n_folds){
    
    # separate the data in to train & test
    train_subject = sample(all_sbj, floor(length(all_sbj) * .9))
    train_data = summarize_behavioral_data_with_block(raw_d %>% filter(prolific_id %in% train_subject)) %>% ungroup() %>% left_join(sd %>% filter(param_id == p_id) %>% ungroup())
    test_data = summarize_behavioral_data_with_block(raw_d %>% filter(!prolific_id %in% train_subject)) %>% ungroup() %>% left_join(sd %>% filter(param_id == p_id) %>% ungroup())
    
    # fit the training set, currently excluding the block_number 
    fitted_stats = colf_nlxb(mean_lt ~ mean_sample + log(block_number), data = train_data, lower = c(-Inf, 0, -Inf))
    #fitted_stats = colf_nlxb(mean_lt ~ mean_sample + block_number, data = train_data, lower = c(-Inf, 0, -Inf))
    sample_slope = fitted_stats$coefficients["param_mean_sample"]
    sample_intercept = fitted_stats$coefficients["param_X.Intercept."]
    block_slope = fitted_stats$coefficients["param_log.block_number."]
    #block_slope = fitted_stats$coefficients["param_block_number"]
    
    test_scaled <- test_data %>% 
      mutate(scaled_samples = mean_sample * sample_slope + log(block_number) * block_slope + sample_intercept)
      #mutate(scaled_samples = mean_sample * sample_slope + block_number * block_slope + sample_intercept)
    
    # save results for each fold 
    rsquared[[k]] <- cor(test_scaled$mean_lt, test_scaled$scaled_samples)^2
    rmse[[k]] <- rmse(test_scaled$mean_lt, test_scaled$scaled_samples)
    
  }
  
  rsquared_average <- mean(unlist(rsquared))
  rmse_average <- mean(unlist(rmse))
  
  return(
    tibble(
      "param_id" = p_id, 
      "mean_rsquared" = rsquared_average, 
      "mean_rmse" = rmse_average
    )
  )
  
}





