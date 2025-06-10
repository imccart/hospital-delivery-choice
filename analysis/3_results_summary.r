# Coefficients ---------------------------------------------------------

summaries <- list()

## Loop over each saved model to extract coefficients and standard errors
for (market in names(models)) {
  model <- models[[market]]
  
  # Extract the summary of coefficients and standard errors
  model.summary <- tidy(model)
  
  # Add a column to indicate the market
  model.summary <- model.summary %>%
    mutate(market = market)
  
  # Save the summary
  summaries[[market]] <- model.summary
}

processed.summaries <- lapply(seq_along(summaries), function(i) {

  market.name <- unique(summaries[[i]]$market)

  summaries[[i]] %>%
    select(term, estimate, std.error) %>%
    mutate(
      estimate = sprintf("%.3f", estimate),
      std.error = sprintf("(%.3f)", std.error)
    ) %>%
    pivot_longer(
      cols = c(estimate, std.error),
      names_to = "stat",
      values_to = market.name  # Create unique value column for each summary
    ) %>%
    select(term, stat, all_of(market.name))
})

# Combine all processed summaries by joining them on "term" and "stat"
summary.table <- reduce(processed.summaries, full_join, by = c("term", "stat"))


# Partial Effects -------------------------------------------------------

final.predictions <- bind_rows(all.predictions)
hist.list <- c("ch_dist", "ch_peri", "ch_teach", "ch_csection")
boot.list <- unlist(
  lapply(hist.list, function(var) {
    paste0(var, "_", 1:n_boot)
  })
)

logit.final <- choice.reg %>%
       inner_join(final.predictions, by=c("id","choice","year","patid","facility","mkt")) %>%
       filter(choice==1) %>%
       mutate(ch_dist=pred_diff_dist1 - pred_prob,
              ch_peri=pred_perilevel_plus1 - pred_perilevel_plus0,
              ch_teach=pred_any_teach1 - pred_any_teach0,
              ch_csection=pred_c_section_elect1 - pred_prob) %>%
       mutate(across(hist.list, ~ as.numeric(.)))
       
## histograms of partial effects
for (var in hist.list) {

  hist.data <- logit.final %>% 
    filter(!is.na(.data[[var]]), .data[[var]] < 1, .data[[var]] > -1)

  hist.plot <- ggplot(hist.data, aes(x = .data[[var]])) +
    geom_histogram(binwidth = 0.01, fill = "gray", color = "black", alpha = 0.7) +
    labs(x = "Change in Predicted Probability",
        y = "Frequency") +
    scale_y_continuous(labels = comma) +        
    theme_bw()

  ggsave(plot=hist.plot, filename=paste0("results/figures/", mkt.path, "/", var, "_hist.png"), width = 8, height = 6, dpi = 300)
}


## table of mean predictions
pfx.wide <- logit.final %>% left_join(market.stats2 %>% select(mkt, n_deliveries), by="mkt") %>%
    ungroup() %>%
    summarize(
      across(
        all_of(hist.list),
        list(
          mean = ~ weighted.mean(.x, w = n_deliveries, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )) 
      

pfx.long <- pfx.wide %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )


boot.wide <- final.boot %>% left_join(market.stats2 %>% select(mkt, n_deliveries), by="mkt") %>%
    ungroup() %>%
    summarize(
      across(
        all_of(boot.list),
        list(
          mean = ~ weighted.mean(.x, w = n_deliveries, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )) 

boot.long <- boot.wide %>%
  pivot_longer(
    cols = matches("_[0-9]+_mean$"),
    names_to = c(".value", "bootstrap"),
    names_pattern = "(.*)_([0-9]+)_mean$"
  )

boot.se <- boot.long %>%
  summarize(
    across(
      c(hist.list),
      ~ sd(.x, na.rm = TRUE),
      .names = "{.col}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "se"
  )  

pfx.data <- pfx.long %>%
  left_join(boot.se, by="variable") %>%
  mutate(l_95=mean-1.96*se,
         u_95=mean+1.96*se)


# Partial Effects by Patient Characteristics ----------------------------

# Loop through each variable in hist.list and pat.list (continuous variables only)
pat.list <- c("age")
pat.labels <- c("Age")

for (var in hist.list) {
  for (i in seq_along(pat.list)) {
  
    boot.vars <- unlist(
      lapply(var, function(v) {
        paste0(v, "_", 1:n_boot)
      })
    )

    pat.var <- pat.list[i]
    pat.label <- pat.labels[i]

    graph.final <- logit.final %>% 
      filter(!is.na(.data[[var]]), .data[[var]] < 1, .data[[var]] > -1)

    # Compute dynamic bin width and bin ranges
    min_val <- min(graph.final[[pat.var]], na.rm = TRUE)
    max_val <- max(graph.final[[pat.var]], na.rm = TRUE)
    bin_width <- (max_val - min_val) / 5  # 5 bins
    bin_breaks <- seq(min_val, max_val, by = bin_width)


    # Compute effects by bins for the current continuous variable
    graph.final <- graph.final %>%
      mutate(bin = cut(.data[[pat.var]], 
                 breaks = bin_breaks, 
                 include.lowest = TRUE)) %>%
      filter(!is.na(bin))

    effects <- graph.final %>%
      left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
      group_by(bin) %>%
      summarize(
        mean = weighted.mean(.data[[var]], w = n_deliveries, na.rm = TRUE)) %>%
      ungroup()

    boot.wide <- graph.final %>%
      left_join(final.boot, by=c("id","year","patid","facility","mkt")) %>%
      left_join(market.stats2 %>% select(mkt, n_deliveries), by="mkt") %>%
      group_by(bin) %>%
      summarize(
        across(
          all_of(boot.vars),
          list(
            boot_mean = ~ weighted.mean(.x, w = n_deliveries, na.rm = TRUE)
          ),
          .names = "{.col}_mean"
        ))

    boot.long <- boot.wide %>%
      pivot_longer(
        cols = matches("_[0-9]+_mean$"),
        names_to = c(".value", "bootstrap"),
        names_pattern = "(.*)_([0-9]+)_mean$"
      )

    boot.se <- boot.long %>%
      group_by(bin) %>%
      summarize(sd = sd(.data[[var]], na.rm = TRUE))
      
    effects <- effects %>% 
      left_join(boot.se, by="bin") %>%
      mutate(l_95=mean-1.96*sd,
             u_95=mean+1.96*sd)
  
    # Compute market-level effects by bin
    effects_mkt <- graph.final %>%
      group_by(bin, mkt) %>%
      summarize(
        mean = mean(.data[[var]], na.rm = TRUE)
      ) %>%
      ungroup()

    boot_mkt.wide <- graph.final %>%
      left_join(final.boot, by=c("id","year","patid","facility","mkt")) %>%
      group_by(bin, mkt) %>%
      summarize(
        across(
          all_of(boot.vars),
          list(
            boot_mean = ~ mean(.x, na.rm = TRUE)
          ),
          .names = "{.col}_mean"
        ))

    boot_mkt.long <- boot_mkt.wide %>%
      pivot_longer(
        cols = matches("_[0-9]+_mean$"),
        names_to = c(".value", "bootstrap"),
        names_pattern = "(.*)_([0-9]+)_mean$"
      )

    boot_mkt.se <- boot_mkt.long %>%
      group_by(bin, mkt) %>%
      summarize(sd = sd(.data[[var]], na.rm = TRUE))      

    effects_mkt <- effects_mkt %>%
      left_join(boot_mkt.se, by=c("bin","mkt")) %>%
      mutate(l_95=mean-1.96*sd,
             u_95=mean+1.96*sd)

    # Calculate y-axis limits
    y_max <- max(effects_mkt$u_95, na.rm = TRUE)
    y_min <- min(effects_mkt$l_95, na.rm = TRUE)


    # Generate the plot
    plot <- ggplot() +
      geom_line(data = effects_mkt, aes(x = bin, y = mean, group = mkt), 
                color = "gray80", linewidth = 0.6) +
      geom_line(data = effects, aes(x = bin, y = mean, group = 1), color = "black") +
      geom_errorbar(data = effects, aes(x = bin, ymin = l_95, ymax = u_95), 
                    width = 0.2, color = "gray40") +
      geom_point(data = effects, aes(x = bin, y = mean), size = 2, color = "black") +
      labs(x = paste0("Bin of ", pat.label), y = "Mean Change in Predicted Probability") +
      coord_cartesian(ylim = c(y_min, y_max)) +
      theme_bw()

    # Save the plot
    ggsave(
      filename = paste0("results/figures/", mkt.path, "/", var, "_", pat.var, ".png"),
      plot = plot, width = 8, height = 6, dpi = 300
    )
  }
}

# Loop through each variable in hist.list and pat.list (quantiles)
pat.list <- c("ci_scorent", "obgyn_10kwra")
pat.labels <- c("CI Score", "OB/GYN per 10,000")

for (var in hist.list) {
  for (i in seq_along(pat.list)) {
  
    boot.vars <- unlist(
      lapply(var, function(v) {
        paste0(v, "_", 1:n_boot)
      })
    )

    pat.var <- pat.list[i]
    pat.label <- pat.labels[i]

  graph.final <- logit.final %>%
    filter(!is.na(.data[[var]]), .data[[var]] < 1, .data[[var]] > -1) %>%
    group_by(mkt) %>%
    mutate(
      bin = case_when(
        .data[[pat.var]] <= quantile(.data[[pat.var]], 0.2, na.rm = TRUE) ~ "20",
        .data[[pat.var]] <= quantile(.data[[pat.var]], 0.4, na.rm = TRUE) ~ "40",
        .data[[pat.var]] <= quantile(.data[[pat.var]], 0.6, na.rm = TRUE) ~ "60",
        .data[[pat.var]] <= quantile(.data[[pat.var]], 0.8, na.rm = TRUE) ~ "80",
        TRUE ~ "100"
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(bin)) %>%
    mutate(bin = factor(bin, levels = c("20", "40", "60", "80", "100")))

    # Compute effects by bins for the current continuous variable
    effects <- graph.final %>%
      left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
      group_by(bin) %>%
      summarize(
        mean = weighted.mean(.data[[var]], w = n_deliveries, na.rm = TRUE)) %>%
      ungroup()

    boot.wide <- graph.final %>%
      left_join(final.boot, by=c("id","year","patid","facility","mkt")) %>%
      left_join(market.stats2 %>% select(mkt, n_deliveries), by="mkt") %>%
      group_by(bin) %>%
      summarize(
        across(
          all_of(boot.vars),
          list(
            boot_mean = ~ weighted.mean(.x, w = n_deliveries, na.rm = TRUE)
          ),
          .names = "{.col}_mean"
        ))

    boot.long <- boot.wide %>%
      pivot_longer(
        cols = matches("_[0-9]+_mean$"),
        names_to = c(".value", "bootstrap"),
        names_pattern = "(.*)_([0-9]+)_mean$"
      )

    boot.se <- boot.long %>%
      group_by(bin) %>%
      summarize(sd = sd(.data[[var]], na.rm = TRUE))
      
    effects <- effects %>% 
      left_join(boot.se, by="bin") %>%
      mutate(l_95=mean-1.96*sd,
             u_95=mean+1.96*sd)
  
    # Compute market-level effects by bin
    effects_mkt <- graph.final %>%
      group_by(bin, mkt) %>%
      summarize(
        mean = mean(.data[[var]], na.rm = TRUE)
      ) %>%
      ungroup()

    boot_mkt.wide <- graph.final %>%
      left_join(final.boot, by=c("id","year","patid","facility","mkt")) %>%
      group_by(bin, mkt) %>%
      summarize(
        across(
          all_of(boot.vars),
          list(
            boot_mean = ~ mean(.x, na.rm = TRUE)
          ),
          .names = "{.col}_mean"
        ))

    boot_mkt.long <- boot_mkt.wide %>%
      pivot_longer(
        cols = matches("_[0-9]+_mean$"),
        names_to = c(".value", "bootstrap"),
        names_pattern = "(.*)_([0-9]+)_mean$"
      )

    boot_mkt.se <- boot_mkt.long %>%
      group_by(bin, mkt) %>%
      summarize(sd = sd(.data[[var]], na.rm = TRUE))      

    effects_mkt <- effects_mkt %>%
      left_join(boot_mkt.se, by=c("bin","mkt")) %>%
      mutate(l_95=mean-1.96*sd,
             u_95=mean+1.96*sd)

    # Calculate y-axis limits
    y_max <- max(effects_mkt$u_95, na.rm = TRUE)
    y_min <- min(effects_mkt$l_95, na.rm = TRUE)


    # Generate the plot
    plot <- ggplot() +
      geom_line(data = effects_mkt, aes(x = bin, y = mean, group = mkt), 
                color = "gray80", linewidth = 0.6) +
      geom_line(data = effects, aes(x = bin, y = mean, group = 1), color = "black") +
      geom_errorbar(data = effects, aes(x = bin, ymin = l_95, ymax = u_95), 
                    width = 0.2, color = "gray40") +
      geom_point(data = effects, aes(x = bin, y = mean), size = 2, color = "black") +
      labs(x = paste0("Bin of ", pat.label), y = "Mean Change in Predicted Probability") +
      coord_cartesian(ylim = c(y_min, y_max)) +
      theme_bw()

    # Save the plot
    ggsave(
      filename = paste0("results/figures/", mkt.path, "/", var, "_", pat.var, ".png"),
      plot = plot, width = 8, height = 6, dpi = 300
    )
  }
}



# Loop through each variable in hist.list and pat.list (binary variables only)
pat.list <- c("nhwhite","mcaid_unins","hispanic")
pat.labels <- c("White", "Medicaid","Hispanic")

for (var in hist.list) {
  for (i in seq_along(pat.list)) {
    
    pat.var <- pat.list[i]
    pat.label <- pat.labels[i]

    boot.vars <- unlist(
      lapply(var, function(v) {
        paste0(v, "_", 1:n_boot)
      })
    )

    graph.final <- logit.final %>%
      filter(!is.na(.data[[var]]), .data[[var]] < 1, .data[[var]] > -1)
  
    # Compute overall weighted effects by binary variable
    if (pat.var=="nhwhite") {
      effects <- graph.final %>%
        left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
        filter(nhwhite==1 | nhblack==1) %>%
        group_by(.data[[pat.var]]) %>%
        summarize(
          mean = weighted.mean(.data[[var]], w = n_deliveries, na.rm = TRUE)
        ) %>%
        ungroup()
    } else {
      effects <- graph.final %>%
        left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
        group_by(.data[[pat.var]]) %>%
        summarize(
          mean = weighted.mean(.data[[var]], w = n_deliveries, na.rm = TRUE)
        ) %>%
        ungroup()
    }

    boot.wide <- graph.final %>%
      left_join(final.boot, by=c("id","year","patid","facility","mkt")) %>%
      left_join(market.stats2 %>% select(mkt, n_deliveries), by="mkt") %>%
      group_by(.data[[pat.var]]) %>%
      summarize(
        across(
          all_of(boot.vars),
          list(
            boot_mean = ~ weighted.mean(.x, w = n_deliveries, na.rm = TRUE)
          ),
          .names = "{.col}_mean"
        ))

    boot.long <- boot.wide %>%
      pivot_longer(
        cols = matches("_[0-9]+_mean$"),
        names_to = c(".value", "bootstrap"),
        names_pattern = "(.*)_([0-9]+)_mean$"
      )

    boot.se <- boot.long %>%
      group_by(.data[[pat.var]]) %>%
      summarize(sd = sd(.data[[var]], na.rm = TRUE))
      
    effects <- effects %>% 
      left_join(boot.se, by=pat.var) %>%
      mutate(l_95=mean-1.96*sd,
             u_95=mean+1.96*sd)


    # Compute market-level effects by binary variable
    effects_mkt <- graph.final %>%
      left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
      group_by(.data[[pat.var]], mkt) %>%
      summarize(
        mean = mean(.data[[var]], na.rm = TRUE)
      ) %>%
      ungroup()

    boot_mkt.wide <- graph.final %>%
      left_join(final.boot, by=c("id","year","patid","facility","mkt")) %>%
      group_by(.data[[pat.var]], mkt) %>%
      summarize(
        across(
          all_of(boot.vars),
          list(
            boot_mean = ~ mean(.x, na.rm = TRUE)
          ),
          .names = "{.col}_mean"
        ))

    boot_mkt.long <- boot_mkt.wide %>%
      pivot_longer(
        cols = matches("_[0-9]+_mean$"),
        names_to = c(".value", "bootstrap"),
        names_pattern = "(.*)_([0-9]+)_mean$"
      )

    boot_mkt.se <- boot_mkt.long %>%
      group_by(.data[[pat.var]], mkt) %>%
      summarize(sd = sd(.data[[var]], na.rm = TRUE))      

    effects_mkt <- effects_mkt %>%
      left_join(boot_mkt.se, by=c(pat.var,"mkt")) %>%
      mutate(l_95=mean-1.96*sd,
             u_95=mean+1.96*sd)

    # Calculate y-axis limits
    y_max <- max(effects_mkt$u_95, na.rm = TRUE)
    y_min <- min(effects_mkt$l_95, na.rm = TRUE)


    # Generate the plot
    plot <- ggplot() +
      # Light, transparent lines connecting gray dots for each market
      geom_line(data = effects_mkt, 
                aes(x = as.factor(.data[[pat.var]]), y = mean, group = mkt), 
                color = "gray90", alpha = 0.5) +    
      # Gray dots for each market
      geom_point(data = effects_mkt, 
                 aes(x = as.factor(.data[[pat.var]]), y = mean, group = mkt), 
                 size = 2, color = "gray80", alpha = 0.7) +
      # Black dot for the weighted average
      geom_point(data = effects, 
                 aes(x = as.factor(.data[[pat.var]]), y = mean), 
                 size = 3, color = "black") +
      # Error bars for the weighted average
      geom_linerange(data = effects, 
                     aes(x = as.factor(.data[[pat.var]]), ymin = l_95, ymax = u_95), 
                     color = "gray40") +
      labs(x = pat.label, y = "Mean Change in Predicted Probability") +
      coord_cartesian(ylim = c(y_min, y_max)) +
      theme_bw()

    # Save the plot
    ggsave(
      filename = paste0("results/figures/", mkt.path, "/", var, "_", pat.var, ".png"),
      plot = plot, width = 8, height = 6, dpi = 300
    )
  }
}
