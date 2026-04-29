

is_binary <- function(x) {
  unique_vals <- unique(na.omit(x))  # Exclude NAs before checking
  all(unique_vals %in% c(0, 1)) && length(unique_vals) <= 2
}

# Binning helpers (used by both main estimates and the bootstrap so they match) ----
apply_age_bin <- function(x, breaks) {
  cut(x, breaks = breaks, include.lowest = TRUE)
}

apply_qntl_bin <- function(x, q10, q25, q50, q75, q90) {
  q10 <- unname(q10); q25 <- unname(q25); q50 <- unname(q50)
  q75 <- unname(q75); q90 <- unname(q90)
  out <- case_when(
    x <= q10 ~ "10",
    x <= q25 ~ "25",
    x <= q50 ~ "50",
    x <= q75 ~ "75",
    x <= q90 ~ "90",
    TRUE ~ "100"
  )
  factor(out, levels = c("10","25","50","75","90","100"))
}

summarize_boot_rep <- function(per_rep, bin_spec, b, market_id) {
  hist.list <- c("ch_dist","ch_peri","ch_teach","ch_csection")
  qb_ci <- bin_spec$ci_scorent$qbreaks_by_mkt %>% filter(mkt == market_id)
  qb_ob <- bin_spec$obgyn_10kwra$qbreaks_by_mkt %>% filter(mkt == market_id)

  out <- list()
  for (var in hist.list) {
    rep <- per_rep %>% filter(!is.na(.data[[var]]), .data[[var]] < 1, .data[[var]] > -1)

    overall <- rep %>% summarize(mean = mean(.data[[var]], na.rm=TRUE)) %>%
      mutate(group_var="overall", bin=NA_character_)
    age      <- rep %>% mutate(bin = as.character(apply_age_bin(age, bin_spec$age$breaks))) %>%
      filter(!is.na(bin)) %>% group_by(bin) %>%
      summarize(mean = mean(.data[[var]], na.rm=TRUE), .groups="drop") %>%
      mutate(group_var="age")
    ci       <- rep %>% mutate(bin = as.character(apply_qntl_bin(ci_scorent,
                                qb_ci$q10, qb_ci$q25, qb_ci$q50, qb_ci$q75, qb_ci$q90))) %>%
      filter(!is.na(bin)) %>% group_by(bin) %>%
      summarize(mean = mean(.data[[var]], na.rm=TRUE), .groups="drop") %>%
      mutate(group_var="ci_scorent")
    ob       <- rep %>% mutate(bin = as.character(apply_qntl_bin(obgyn_10kwra,
                                qb_ob$q10, qb_ob$q25, qb_ob$q50, qb_ob$q75, qb_ob$q90))) %>%
      filter(!is.na(bin)) %>% group_by(bin) %>%
      summarize(mean = mean(.data[[var]], na.rm=TRUE), .groups="drop") %>%
      mutate(group_var="obgyn_10kwra")
    blk      <- rep %>% filter(nhwhite == 1 | nhblack == 1) %>%
      group_by(bin = as.character(nhblack)) %>%
      summarize(mean = mean(.data[[var]], na.rm=TRUE), .groups="drop") %>%
      mutate(group_var="nhblack")
    mcd      <- rep %>% group_by(bin = as.character(mcaid_unins)) %>%
      summarize(mean = mean(.data[[var]], na.rm=TRUE), .groups="drop") %>%
      mutate(group_var="mcaid_unins")
    hsp      <- rep %>% group_by(bin = as.character(hispanic)) %>%
      summarize(mean = mean(.data[[var]], na.rm=TRUE), .groups="drop") %>%
      mutate(group_var="hispanic")

    out[[var]] <- bind_rows(overall, age, ci, ob, blk, mcd, hsp) %>%
      mutate(outcome = var, mkt = market_id, boot_rep = b)
  }
  bind_rows(out)
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




bootstrap_choice_model <- function(markets, var1, var2, pfx.vars, pfx.inc, data,
                                   n_bootstrap, bin_spec) {

  boot_results <- list()

  for (market in markets) {
    cat("Processing market:", market, "\n")
    boot_summaries <- vector("list", n_bootstrap)

    for (b in 1:n_bootstrap) {
      market_data <- data %>% filter(mkt == market)
      sampled_ids <- sample(unique(market_data$id),
                            size = length(unique(market_data$id)), replace = TRUE)
      boot_ids <- tibble(sampled_id = sampled_ids, boot_id = seq_along(sampled_ids))

      resampled_data <- boot_ids %>%
        left_join(market_data, by = c("sampled_id" = "id"),
                  relationship = "many-to-many") %>%
        mutate(orig_id = sampled_id, id = boot_id) %>%
        select(-sampled_id)

      boot_model <- estimate_choice_model(market, var1, var2, pfx.vars, pfx.inc,
                                          resampled_data)

      per_rep <- boot_model$predictions %>%
        left_join(resampled_data %>%
                    select(id, patid, facility, year, mkt, date_delivery,
                           age, ci_scorent, obgyn_10kwra,
                           nhblack, nhwhite, mcaid_unins, hispanic),
                  by = c("id","patid","facility","year","mkt","date_delivery")) %>%
        filter(choice == 1) %>%
        mutate(ch_dist     = pred_diff_dist1       - pred_prob,
               ch_peri     = pred_perilevel_plus1  - pred_perilevel_plus0,
               ch_teach    = pred_any_teach1       - pred_any_teach0,
               ch_csection = pred_c_section_elect1 - pred_prob)

      boot_summaries[[b]] <- summarize_boot_rep(per_rep, bin_spec, b, market)

      rm(per_rep, boot_model, resampled_data, market_data, boot_ids, sampled_ids)
      gc(verbose = FALSE)
    }
    boot_results[[paste0("market_", market)]] <- bind_rows(boot_summaries)
  }
  bind_rows(boot_results)
}
