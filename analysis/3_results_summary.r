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
coefficient.table <- reduce(processed.summaries, full_join, by = c("term", "stat"))


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
pfx.cont <- list()
pfx.cont.mkt <- list()


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
    
    pfx.cont[[paste0(var, "_", pat.var)]] <- effects
    pfx.cont.mkt[[paste0(var, "_", pat.var)]] <- effects_mkt
  }
}

# Loop through each variable in hist.list and pat.list (quantiles)
pat.list <- c("ci_scorent", "obgyn_10kwra")
pat.labels <- c("CI Score", "OB/GYN per 10,000")
pfx.qntl <- list()
pfx.qntl.mkt <- list()

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
        .data[[pat.var]] <= quantile(.data[[pat.var]], 0.1, na.rm = TRUE) ~ "10",
        .data[[pat.var]] <= quantile(.data[[pat.var]], 0.25, na.rm = TRUE) ~ "25",
        .data[[pat.var]] <= quantile(.data[[pat.var]], 0.5, na.rm = TRUE) ~ "50",
        .data[[pat.var]] <= quantile(.data[[pat.var]], 0.75, na.rm = TRUE) ~ "75",
        .data[[pat.var]] <= quantile(.data[[pat.var]], 0.90, na.rm = TRUE) ~ "90",
        TRUE ~ "100"
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(bin)) %>%
    mutate(bin = factor(bin, levels = c("10", "25", "50", "75", "90","100")))

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

    pfx.qntl[[paste0(var, "_", pat.var)]] <- effects
    pfx.qntl.mkt[[paste0(var, "_", pat.var)]] <- effects_mkt

  }
}



# Loop through each variable in hist.list and pat.list (binary variables only)
pat.list <- c("nhblack","mcaid_unins","hispanic")
pat.labels <- c("Black", "Medicaid","Hispanic")
pfx.bin <- list()
pfx.bin.mkt <- list()

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
    if (pat.var=="nhblack") {
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

    boot.diff <- boot.long %>%
      group_by(bootstrap) %>%
      summarize(
        diff = .data[[var]][.data[[pat.var]] == 1] - 
              .data[[var]][.data[[pat.var]] == 0],
        .groups = "drop"
      )

    boot.diff.se <- boot.diff %>%
      summarize(diff_sd = sd(diff, na.rm = TRUE))

    effects <- effects %>% 
      left_join(boot.se, by=pat.var) %>%
      bind_cols(boot.diff.se) %>%
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

    pfx.bin[[paste0(var, "_", pat.var)]] <- effects
    pfx.bin.mkt[[paste0(var, "_", pat.var)]] <- effects_mkt

  }
}


## Combine into table format
summary_means1 <- purrr::map_dfr(
  .x = names(pfx.cont),
  .f = function(nm) {
    tbl <- pfx.cont[[nm]] %>% filter(bin == target_bin_label)
    if (nrow(tbl) == 0) return(NULL)

    tibble(
      group = str_remove(nm, "^ch_(dist|peri|teach|csection)_"),
      outcome = str_extract(nm, "^ch_(dist|peri|teach|csection)"),
      mean = tbl$mean,
      se = tbl$sd,
      l_95 = tbl$l_95,
      u_95 = tbl$u_95      
    )
  }
) %>%
  pivot_wider(names_from = outcome, values_from = c(mean, se, l_95, u_95)) %>%
  relocate(group)


# Choose the bin you want
target_bin <- 50
summary_means2 <- map_dfr(
  .x = names(pfx.qntl),
  .f = function(nm) {
    tbl <- pfx.qntl[[nm]] %>% filter(bin == target_bin)
    if (nrow(tbl) == 0) return(NULL) 

    tibble(
      group = str_remove(nm, "^ch_(dist|peri|teach|csection)_"),
      outcome = str_extract(nm, "^ch_(dist|peri|teach|csection)"),
      mean = tbl$mean,
      se = tbl$sd,
      l_95 = tbl$l_95,
      u_95 = tbl$u_95      
    )
  }
) %>%
  pivot_wider(names_from = outcome, values_from = c(mean, se, l_95, u_95)) %>%
  relocate(group)

summary_means3 <- map_dfr(
  .x = names(pfx.bin),
  .f = function(nm) {
    tbl <- pfx.bin[[nm]]
    if (!all(c(0, 1) %in% tbl[[1]])) return(NULL)

    diff <- tbl$mean[which(tbl[[1]] == 1)] - tbl$mean[which(tbl[[1]] == 0)]
    row1 <- tbl %>% filter(.[[1]] == 1)
    row0 <- tbl %>% filter(.[[1]] == 0)

    tibble(
      group = str_remove(nm, "^ch_(dist|peri|teach|csection)_"),
      outcome = str_extract(nm, "^ch_(dist|peri|teach|csection)"),
      mean = diff,
      se = row1$diff_sd,
      l_95 = diff - 1.96 * se,
      u_95 = diff + 1.96 * se      
    )
  }
) %>%
  pivot_wider(names_from = outcome, values_from = c(mean, se, l_95, u_95)) %>%
  relocate(group)


summary_means <- bind_rows(summary_means1, summary_means2, summary_means3) %>%
  mutate(Row = c(
    "Age",
    "Leonard Obstetric Comorbidity Score",
    "Ob/Gyns per 10,000 WRA in County",
    "Medicaid vs Private Insured",
    "Non-Hispanic White vs Non-Hispanic Black",
    "Hispanic vs Non-Hispanic"
  )) %>%
  mutate(
    ch_dist = str_glue("{sprintf('%.4f', mean_ch_dist)}\n({sprintf('%.4f', se_ch_dist)})\n[{sprintf('%.4f', l_95_ch_dist)}, {sprintf('%.4f', u_95_ch_dist)}]"),
    ch_peri = str_glue("{sprintf('%.4f', mean_ch_peri)}\n({sprintf('%.4f', se_ch_peri)})\n[{sprintf('%.4f', l_95_ch_peri)}, {sprintf('%.4f', u_95_ch_peri)}]"),
    ch_teach = str_glue("{sprintf('%.4f', mean_ch_teach)}\n({sprintf('%.4f', se_ch_teach)})\n[{sprintf('%.4f', l_95_ch_teach)}, {sprintf('%.4f', u_95_ch_teach)}]"),
    ch_csection = str_glue("{sprintf('%.4f', mean_ch_csection)}\n({sprintf('%.4f', se_ch_csection)})\n[{sprintf('%.4f', l_95_ch_csection)}, {sprintf('%.4f', u_95_ch_csection)}]")
  ) %>%
  select(Row, ch_dist, ch_peri, ch_teach, ch_csection)

ft <- flextable(summary_means) %>%
  set_header_labels(
    Row = "",
    ch_dist = "Distance",
    ch_peri = "Perinatal\nLevel of Care 2+",
    ch_teach = "Minor or Major\nTeaching Hospital",
    ch_csection = "Elective\nC-Section Rate"
  ) %>%
  theme_booktabs() %>%
  width(j = 1, width = 2.8) %>%
  width(j = 2:5, width = 1.4) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  valign(valign = "top", part = "all") %>%
  set_table_properties(layout = "fixed")

# Export to word
summary_table <- read_docx() %>%
  body_add_par("Summary of Marginal Effects by Patient and Area Characteristics", style = "heading 1") %>%
  body_add_flextable(ft)