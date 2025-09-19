

is_binary <- function(x) {
  unique_vals <- unique(na.omit(x))  # Exclude NAs before checking
  all(unique_vals %in% c(0, 1)) && length(unique_vals) <= 2
}

# Define the choice estimation function
estimate_choice_model <- function(market, var1, var2, pfx.vars, pfx.inc, data) {
  
  # Step 1: Data Preparation ------------------------------------------------
    
  # Filter by the market
  choice.reg.m <- data %>% filter(mkt == market)
  
  # Step 2: Model Specification ---------------------------------------------
  
  # Create the main effects formula using var1
  main_effects <- paste(var1, collapse = " + ")
  
  # Create the interaction terms between var1 and var2
  interaction_terms <- outer(var1, var2, FUN = function(x, y) paste(x, "*", y))
  interaction_formula <- paste(interaction_terms, collapse = " + ")
  
  # Combine the main effects and interaction terms into one formula
  full_formula <- paste(main_effects, interaction_formula, sep = " + ")
  
  # Create the full formula for mclogit
  model_formula <- as.formula(paste("cbind(choice, id) ~", full_formula))
  
  
  
  # Step 3: Model Estimation ------------------------------------------------
  
  logit.reg <- mclogit(model_formula, data = choice.reg.m)
  coef.logit <- logit.reg$coeff
  
  coef_df <- as.data.frame(coef.logit) %>%
    rownames_to_column(var = "term") %>%
    rename(!!paste0("market_", market) := coef.logit)


  # Step 4: Baseline Predictions --------------------------------------------
  
  # Initialize the linear predictor
  choice.reg.m$linear_pred <- 0
  
  # Compute the linear predictor for both main effects and interaction terms in one step
  for (v in var1) {
    if (v %in% names(coef.logit)) {
      # Add the main effect for each variable in var1
      choice.reg.m$linear_pred <- choice.reg.m$linear_pred + choice.reg.m[[v]] * coef.logit[v]
    }
  }
  
  # Add interaction effects
  for (v1 in var1) {
    for (v2 in var2) {
      interaction_term <- paste(v1, v2, sep = ":")
      if (interaction_term %in% names(coef.logit)) {
        # Add interaction effect for each combination of var1 and var2
        choice.reg.m$linear_pred <- choice.reg.m$linear_pred + 
          choice.reg.m[[v1]] * choice.reg.m[[v2]] * coef.logit[interaction_term]
      }
    }
  }
  
  # Compute predicted probabilities
  choice.dat.pred <- choice.reg.m %>%
    mutate(num = exp(linear_pred)) %>%
    group_by(id) %>%
    mutate(denom = sum(num, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pred_prob = num / denom)
  
  
  
  # Step 5: Partial Effect Calculations -------------------------------------
  
  # Loop over each variable in pfx.vars to calculate partial effects
  for (i in seq_along(pfx.vars)) {
    var <- pfx.vars[i]
    inc <- pfx.inc[i]
    
    # Initialize a modified linear predictor for each variable of interest
    choice.dat.pred$linear_pred_mod1 <- 0
    choice.dat.pred$linear_pred_mod0 <- 0    

    
    # Recalculate the linear predictor for both main effects and interaction terms
    for (v in var1) {
      if (v %in% names(coef.logit)) {
        # If the current variable is the one being modified, apply the increment
        if (v == var) {
          if (is_binary(choice.dat.pred[[v]])) {
            choice.dat.pred$linear_pred_mod1 <- choice.dat.pred$linear_pred_mod1 + coef.logit[v]
            choice.dat.pred$linear_pred_mod0 <- choice.dat.pred$linear_pred_mod0
          } else {
            choice.dat.pred$linear_pred_mod1 <- choice.dat.pred$linear_pred_mod1 + (choice.dat.pred[[v]] + inc) * coef.logit[v]
            choice.dat.pred$linear_pred_mod0 <- choice.dat.pred$linear_pred_mod0 + choice.dat.pred[[v]] * coef.logit[v]            
          }
        } else {
          # Otherwise, use the original value
          choice.dat.pred$linear_pred_mod1 <- choice.dat.pred$linear_pred_mod1 + choice.dat.pred[[v]] * coef.logit[v]          
          choice.dat.pred$linear_pred_mod0 <- choice.dat.pred$linear_pred_mod0 + choice.dat.pred[[v]] * coef.logit[v]
        }
      }
    }
    
    # Add interaction effects
    for (v1 in var1) {
      for (v2 in var2) {
        interaction_term <- paste(v1, v2, sep = ":")
        if (interaction_term %in% names(coef.logit)) {
          # Adjust interaction terms involving the variable of interest
          if (v1 == var) {
            if (is_binary(choice.dat.pred[[v1]])) {
              choice.dat.pred$linear_pred_mod1 <- choice.dat.pred$linear_pred_mod1 + choice.dat.pred[[v2]] * coef.logit[interaction_term]
              choice.dat.pred$linear_pred_mod0 <- choice.dat.pred$linear_pred_mod0
            } else {
              choice.dat.pred$linear_pred_mod1 <- choice.dat.pred$linear_pred_mod1 + (choice.dat.pred[[v1]] + inc) * choice.dat.pred[[v2]] * coef.logit[interaction_term]
              choice.dat.pred$linear_pred_mod0 <- choice.dat.pred$linear_pred_mod0 + choice.dat.pred[[v1]] * choice.dat.pred[[v2]] * coef.logit[interaction_term]
            }
          } else {
            choice.dat.pred$linear_pred_mod1 <- choice.dat.pred$linear_pred_mod1 + choice.dat.pred[[v1]] * choice.dat.pred[[v2]] * coef.logit[interaction_term]            
            choice.dat.pred$linear_pred_mod0 <- choice.dat.pred$linear_pred_mod0 + choice.dat.pred[[v1]] * choice.dat.pred[[v2]] * coef.logit[interaction_term]            
          }
        }
      }
    }
    
    # Compute the modified predicted probabilities
    choice.dat.pred <- choice.dat.pred %>%
      mutate(num_mod0 = if_else(choice==1, exp(linear_pred_mod0), exp(linear_pred)),
             num_mod1 = if_else(choice==1, exp(linear_pred_mod1), exp(linear_pred))) %>%
      group_by(id) %>%
      mutate(denom0=sum(num_mod0, na.rm = TRUE),
             denom1=sum(num_mod1, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(!!paste0("pred_", var,"0") := num_mod0 / denom0,
             !!paste0("pred_", var,"1") := num_mod1 / denom1) %>%
      select(-c(num_mod0, num_mod1, linear_pred_mod0, linear_pred_mod1, denom0, denom1))  # Remove temporary columns
  }
  
  
  # Step 6: Return the Tidy Dataset -----------------------------------------
  
  # Return only relevant columnes for pred_ and matching back to original data
  final_data <- choice.dat.pred %>%
    select(choice, id, patid, facility, year, mkt, date_delivery, starts_with("pred_"))

  # Set predictions to NA for any variables that were dropped in mclogit
  for (var in pfx.vars) {
    if (!(var %in% names(coef.logit))) {
      final_data <- final_data %>%
        mutate(!!paste0("pred_", var,"0") := NA, !!paste0("pred_", var,"1") := NA)
    }
  }   
  
  list(predictions = final_data, coefficients = coef_df, model = logit.reg)
}




bootstrap_choice_model <- function(markets, var1, var2, pfx.vars, pfx.inc, data, n_bootstrap) {
  
  # Initialize an empty list to store results for all markets
  boot_results <- list()
  
  # Loop over each market
  for (market in markets) {
    cat("Processing market:", market, "\n")
    
    # Store bootstrap predictions for the current market
    boot_preds <- vector("list", n_bootstrap)
    
    # Perform bootstrapping
    for (b in 1:n_bootstrap) {

      # Step 1: Resample the data for the market with replacement
      market_data <- data %>% filter(mkt == market)
      sampled_ids <- sample(unique(market_data$id), size = length(unique(market_data$id)), replace = TRUE)
      # resampled_data <- market_data %>% filter(id %in% sampled_ids)

      # Create a data frame with a row for each sampled ID, preserving duplicates
      boot_ids <- tibble(sampled_id = sampled_ids, boot_id = seq_along(sampled_ids))

      # Join back to market_data
      resampled_data <- boot_ids %>%
        left_join(market_data, by = c("sampled_id" = "id")) %>%
        mutate(orig_id=sampled_id,
               id = boot_id) %>%
        select(-sampled_id)

      # Step 2: Estimate the choice model on the resampled data
      boot_model <- estimate_choice_model(market, var1, var2, pfx.vars, pfx.inc, resampled_data)
      
      # Store predictions from the current bootstrap iteration
      boot_preds[[b]] <- boot_model$predictions %>%
                      left_join(resampled_data %>% select(id, orig_id, patid, facility, year, mkt, date_delivery),
                          by = c("id","patid","facility","year","mkt","date_delivery")) %>%
                      mutate(id = orig_id) %>%
                      filter(choice==1) %>%
                      mutate(ch_dist = pred_diff_dist1 - pred_prob,
                             ch_peri = pred_perilevel_plus1 - pred_perilevel_plus0,
                             ch_teach = pred_any_teach1 - pred_any_teach0,
                             ch_csection = pred_c_section_elect1 - pred_prob,
                             boot_rep = b) %>%
                      group_by(id, patid, facility, year, mkt, date_delivery, boot_rep) %>%
                      slice(1) %>% 
                      ungroup() %>%
                      select(id, patid, facility, year, mkt, date_delivery, boot_rep, starts_with("ch_"))
      
    }
    
    all_boot_pred <- bind_rows(boot_preds)
        
    # Store the summarized results for the market
    boot_results[[paste0("market_", market)]] <- pivot_wider(all_boot_pred, 
                                                    names_from=boot_rep, 
                                                    values_from=starts_with("ch_"))
  }
  
  return(boot_results)
}
