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


## SE per outcome from per-rep, per-market overall means
boot.se <- final.boot %>%
  filter(group_var == "overall") %>%
  left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
  group_by(boot_rep, outcome) %>%
  summarize(rep_mean = weighted.mean(mean, w = n_deliveries, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(outcome) %>%
  summarize(se = sd(rep_mean, na.rm = TRUE), .groups = "drop") %>%
  rename(variable = outcome)

pfx.data <- pfx.long %>%
  left_join(boot.se, by="variable") %>%
  mutate(l_95=mean-1.96*se,
         u_95=mean+1.96*se)


# Partial Effects by Patient Characteristics ----------------------------

# Partial effects by age (continuous)
pfx.cont <- list()
pfx.cont.mkt <- list()
age_levels <- levels(cut(logit.final$age, bin_spec$age$breaks, include.lowest = TRUE))

for (var in hist.list) {

  graph.final <- logit.final %>%
    filter(!is.na(.data[[var]]), abs(.data[[var]]) < 1) %>%
    mutate(bin = as.character(cut(age, bin_spec$age$breaks, include.lowest = TRUE))) %>%
    filter(!is.na(bin))

  effects <- graph.final %>%
    left_join(market.stats2, by = "mkt") %>%
    group_by(bin) %>%
    summarize(mean = weighted.mean(.data[[var]], w = n_deliveries, na.rm = TRUE),
              .groups = "drop")

  boot.se <- final.boot %>%
    filter(group_var == "age", outcome == var) %>%
    left_join(market.stats2, by = "mkt") %>%
    group_by(boot_rep, bin) %>%
    summarize(rep_mean = weighted.mean(mean, w = n_deliveries, na.rm = TRUE),
              .groups = "drop") %>%
    group_by(bin) %>%
    summarize(sd = sd(rep_mean, na.rm = TRUE), .groups = "drop")

  effects <- effects %>%
    left_join(boot.se, by = "bin") %>%
    mutate(l_95 = mean - 1.96 * sd, u_95 = mean + 1.96 * sd)

  effects_mkt <- graph.final %>%
    group_by(bin, mkt) %>%
    summarize(mean = mean(.data[[var]], na.rm = TRUE), .groups = "drop")

  boot_mkt.se <- final.boot %>%
    filter(group_var == "age", outcome == var) %>%
    group_by(bin, mkt) %>%
    summarize(sd = sd(mean, na.rm = TRUE), .groups = "drop")

  effects_mkt <- effects_mkt %>%
    left_join(boot_mkt.se, by = c("bin", "mkt")) %>%
    mutate(l_95 = mean - 1.96 * sd, u_95 = mean + 1.96 * sd)

  effects     <- effects     %>% mutate(bin = factor(bin, levels = age_levels))
  effects_mkt <- effects_mkt %>% mutate(bin = factor(bin, levels = age_levels))

  y_max <- max(effects_mkt$u_95, na.rm = TRUE)
  y_min <- min(effects_mkt$l_95, na.rm = TRUE)

  plot <- ggplot() +
    geom_line(data = effects_mkt, aes(x = bin, y = mean, group = mkt),
              color = "gray80", linewidth = 0.6) +
    geom_line(data = effects, aes(x = bin, y = mean, group = 1), color = "black") +
    geom_errorbar(data = effects, aes(x = bin, ymin = l_95, ymax = u_95),
                  width = 0.2, color = "gray40") +
    geom_point(data = effects, aes(x = bin, y = mean), size = 2, color = "black") +
    labs(x = "Bin of Age", y = "Mean Change in Predicted Probability") +
    coord_cartesian(ylim = c(y_min, y_max)) +
    theme_bw()

  ggsave(filename = paste0("results/figures/", mkt.path, "/", var, "_age.png"),
         plot = plot, width = 8, height = 6, dpi = 300)

  pfx.cont[[paste0(var, "_age")]] <- effects
  pfx.cont.mkt[[paste0(var, "_age")]] <- effects_mkt
}

# Loop through each variable in hist.list and pat.list (quantiles)
pat.list <- c("ci_scorent", "obgyn_10kwra")
pat.labels <- c("CI Score", "OB/GYN per 10,000")
pfx.qntl <- list()
pfx.qntl.mkt <- list()

for (var in hist.list) {
  for (i in seq_along(pat.list)) {

    pat.var <- pat.list[i]
    pat.label <- pat.labels[i]

    qbreaks_lookup <- bin_spec[[pat.var]]$qbreaks_by_mkt

    graph.final <- logit.final %>%
      filter(!is.na(.data[[var]]), .data[[var]] < 1, .data[[var]] > -1) %>%
      left_join(qbreaks_lookup, by = "mkt") %>%
      mutate(bin = case_when(
        .data[[pat.var]] <= q10 ~ "10",
        .data[[pat.var]] <= q25 ~ "25",
        .data[[pat.var]] <= q50 ~ "50",
        .data[[pat.var]] <= q75 ~ "75",
        .data[[pat.var]] <= q90 ~ "90",
        TRUE ~ "100"
      )) %>%
      filter(!is.na(bin)) %>%
      select(-q10, -q25, -q50, -q75, -q90)

    bin_lvls <- c("10","25","50","75","90","100")

    effects <- graph.final %>%
      left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
      group_by(bin) %>%
      summarize(mean = weighted.mean(.data[[var]], w = n_deliveries, na.rm = TRUE),
                .groups = "drop")

    boot.se <- final.boot %>%
      filter(group_var == pat.var, outcome == var) %>%
      left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
      group_by(boot_rep, bin) %>%
      summarize(rep_mean = weighted.mean(mean, w = n_deliveries, na.rm = TRUE),
                .groups = "drop") %>%
      group_by(bin) %>%
      summarize(sd = sd(rep_mean, na.rm = TRUE), .groups = "drop")

    effects <- effects %>%
      left_join(boot.se, by="bin") %>%
      mutate(l_95=mean-1.96*sd,
             u_95=mean+1.96*sd,
             bin = factor(bin, levels = bin_lvls)) %>%
      arrange(bin)

    effects_mkt <- graph.final %>%
      group_by(bin, mkt) %>%
      summarize(mean = mean(.data[[var]], na.rm = TRUE), .groups = "drop")

    boot_mkt.se <- final.boot %>%
      filter(group_var == pat.var, outcome == var) %>%
      group_by(bin, mkt) %>%
      summarize(sd = sd(mean, na.rm = TRUE), .groups = "drop")

    effects_mkt <- effects_mkt %>%
      left_join(boot_mkt.se, by=c("bin","mkt")) %>%
      mutate(l_95=mean-1.96*sd,
             u_95=mean+1.96*sd) %>%
      left_join(qbreaks_lookup, by = "mkt") %>%
      duplicate_for_tied_bins() %>%
      select(-q10, -q25, -q50, -q75, -q90) %>%
      mutate(bin = factor(bin, levels = bin_lvls)) %>%
      arrange(bin, mkt)

    # Figure-only fill: augment across-market effects with bin 25 = bin 10 when
    # q10 == q25 in every market (universal tie). Saved pfx_qntl.csv is untouched.
    effects_plot <- effects
    if (all(qbreaks_lookup$q10 == qbreaks_lookup$q25) && !"25" %in% as.character(effects_plot$bin)) {
      effects_plot <- bind_rows(
        effects_plot,
        effects_plot %>% filter(as.character(bin) == "10") %>%
          mutate(bin = factor("25", levels = bin_lvls))
      ) %>% arrange(bin)
    }

    # Calculate y-axis limits
    y_max <- max(effects_mkt$u_95, na.rm = TRUE)
    y_min <- min(effects_mkt$l_95, na.rm = TRUE)


    # Generate the plot
    plot <- ggplot() +
      geom_line(data = effects_mkt, aes(x = bin, y = mean, group = mkt),
                color = "gray80", linewidth = 0.6) +
      geom_line(data = effects_plot, aes(x = bin, y = mean, group = 1), color = "black") +
      geom_errorbar(data = effects_plot, aes(x = bin, ymin = l_95, ymax = u_95),
                    width = 0.2, color = "gray40") +
      geom_point(data = effects_plot, aes(x = bin, y = mean), size = 2, color = "black") +
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

binary_boot <- final.boot %>%
  filter(group_var %in% pat.list) %>%
  mutate(bin = as.integer(bin))

for (var in hist.list) {
  for (i in seq_along(pat.list)) {

    pat.var <- pat.list[i]
    pat.label <- pat.labels[i]

    graph.final <- logit.final %>%
      filter(!is.na(.data[[var]]), .data[[var]] < 1, .data[[var]] > -1)

    if (pat.var=="nhblack") {
      temp <- graph.final %>%
        left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
        filter(nhwhite==1 | nhblack==1) %>%
        group_by(bin = .data[[pat.var]])
    } else {
      temp <- graph.final %>%
        left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
        group_by(bin = .data[[pat.var]])
    }

    if (mkt.path == "atl-only") {
      effects <- temp %>%
        summarize(mean = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      effects <- temp %>%
        summarize(mean = weighted.mean(.data[[var]], w = n_deliveries, na.rm = TRUE),
                  .groups = "drop")
    }

    # Bootstrap SE per group (rep means → sd across reps)
    rep_means <- binary_boot %>%
      filter(group_var == pat.var, outcome == var) %>%
      left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
      group_by(boot_rep, bin) %>%
      summarize(
        rep_mean = weighted.mean(mean, w = n_deliveries, na.rm = TRUE),
        .groups = "drop")

    boot.se <- rep_means %>%
      group_by(bin) %>%
      summarize(sd = sd(rep_mean, na.rm = TRUE), .groups = "drop")

    boot.diff.se <- rep_means %>%
      group_by(boot_rep) %>%
      summarize(diff = rep_mean[bin == 1] - rep_mean[bin == 0], .groups = "drop") %>%
      summarize(diff_sd = sd(diff, na.rm = TRUE))

    effects <- effects %>%
      left_join(boot.se, by = "bin") %>%
      bind_cols(boot.diff.se) %>%
      mutate(l_95=mean-1.96*sd,
             u_95=mean+1.96*sd)

    # Market-level effects by binary variable
    if (pat.var=="nhblack") {
      effects_mkt <- graph.final %>%
        left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
        filter(nhwhite==1 | nhblack==1) %>%
        group_by(bin = .data[[pat.var]], mkt) %>%
        summarize(mean = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    } else {
      effects_mkt <- graph.final %>%
        left_join(market.stats2 %>% select(mkt, n_deliveries), by = "mkt") %>%
        group_by(bin = .data[[pat.var]], mkt) %>%
        summarize(mean = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
    }

    boot_mkt.se <- binary_boot %>%
      filter(group_var == pat.var, outcome == var) %>%
      group_by(bin, mkt) %>%
      summarize(sd = sd(mean, na.rm = TRUE), .groups = "drop")

    effects_mkt <- effects_mkt %>%
      left_join(boot_mkt.se, by = c("bin","mkt")) %>%
      mutate(l_95=mean-1.96*sd,
             u_95=mean+1.96*sd)

    # Calculate y-axis limits
    y_max <- max(effects_mkt$u_95, na.rm = TRUE)
    y_min <- min(effects_mkt$l_95, na.rm = TRUE)


    # Generate the plot
    plot <- ggplot() +
      # Light, transparent lines connecting gray dots for each market
      geom_line(data = effects_mkt,
                aes(x = as.factor(bin), y = mean, group = mkt),
                color = "gray90", alpha = 0.5) +
      # Gray dots for each market
      geom_point(data = effects_mkt,
                 aes(x = as.factor(bin), y = mean, group = mkt),
                 size = 2, color = "gray80", alpha = 0.7) +
      # Black dot for the weighted average
      geom_point(data = effects,
                 aes(x = as.factor(bin), y = mean),
                 size = 3, color = "black") +
      # Error bars for the weighted average
      geom_linerange(data = effects,
                     aes(x = as.factor(bin), ymin = l_95, ymax = u_95),
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
target_bin <- "50"
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
    "Non-Hispanic Black vs Non-Hispanic White",
    "Medicaid vs Private Insured",
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


# Save aggregated partial-effects for HSR figure iteration ---------------
# Flatten each list-of-data-frames into a single long-form CSV with
# outcome + patient_char columns. Loaded by analysis/4_hsr_figures.R.

flatten_pfx <- function(lst) {
  map2_dfr(names(lst), lst, function(nm, df) {
    df %>%
      mutate(outcome = str_extract(nm, "^ch_(dist|peri|teach|csection)"),
             patient_char = str_remove(nm, "^ch_(dist|peri|teach|csection)_")) %>%
      select(outcome, patient_char, everything())
  })
}

write.csv(flatten_pfx(pfx.cont),
          paste0("results/tables/", mkt.path, "/pfx_cont.csv"), row.names = FALSE)
write.csv(flatten_pfx(pfx.cont.mkt),
          paste0("results/tables/", mkt.path, "/pfx_cont_mkt.csv"), row.names = FALSE)
write.csv(flatten_pfx(pfx.qntl),
          paste0("results/tables/", mkt.path, "/pfx_qntl.csv"), row.names = FALSE)
write.csv(flatten_pfx(pfx.qntl.mkt),
          paste0("results/tables/", mkt.path, "/pfx_qntl_mkt.csv"), row.names = FALSE)
write.csv(flatten_pfx(pfx.bin),
          paste0("results/tables/", mkt.path, "/pfx_bin.csv"), row.names = FALSE)
write.csv(flatten_pfx(pfx.bin.mkt),
          paste0("results/tables/", mkt.path, "/pfx_bin_mkt.csv"), row.names = FALSE)