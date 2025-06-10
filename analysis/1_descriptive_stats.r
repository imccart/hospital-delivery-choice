# Market summary statistics -----------------------------------------------

## most common city per market
mkt.city <- delivery.dat %>% filter(facility_city!="_UNIN", !is.na(facility_city)) %>%
    group_by(mkt) %>%
    count(facility_city, sort = TRUE) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(mkt, city=facility_city)

## raw stats (allows for out of market facility choice)
market.stats1 <- delivery.dat %>%
    group_by(mkt) %>%
    summarize(n_deliveries=n(), 
              n_facilities=n_distinct(facility_d),
              n_patients=n_distinct(alias_to_mother_longid)) %>%
    left_join(mkt.city, by="mkt")


## choice stats (only in-market facility choice)
market.stats2 <- choice.reg %>% filter(choice==TRUE) %>%
    group_by(mkt) %>%
    summarize(n_deliveries=n(), 
              n_facilities=n_distinct(facility_d),
              n_patients=n_distinct(patid),
              mean_dist=mean(diff_dist, na.rm=TRUE)) %>%
    left_join(mkt.city, by="mkt")              

## Atlanta versus other
market.table <- choice.reg %>% filter(choice==TRUE) %>%
    group_by(atlanta) %>%
    summarize(n_deliveries=n(), 
              n_patients=n_distinct(patid),    
              mean_dist=mean(nearest, na.rm=TRUE),
              mean_diff_dist=mean(diff_dist, na.rm=TRUE),
              n_facilities=n_distinct(facility_d))

hospital.table <- choice.reg %>% filter(choice==TRUE) %>%
    arrange(facility_d, date_delivery) %>%
    group_by(facility_d) %>%
    slice_tail(n=1) %>%
    ungroup() %>%
    group_by(atlanta) %>%
    summarize(perilevel_plus=mean(perilevel_plus, na.rm=TRUE),
              any_teach=mean(any_teach, na.rm=TRUE),
              c_section_elect=mean(c_section_elect, na.rm=TRUE))

patient.table <- choice.reg %>% filter(choice==TRUE) %>%
    mutate(ins_other=ins_other+ins_mcare) %>%
    arrange(patid, date_delivery) %>%
    group_by(patid) %>%
    slice_tail(n=1) %>%
    ungroup() %>%
    group_by(atlanta) %>%
    summarize(age=mean(age, na.rm=TRUE),
              ci_scorent=mean(ci_scorent, na.rm=TRUE),
              ins_mcaid=mean(ins_mcaid, na.rm=TRUE),
              ins_priv=mean(ins_priv, na.rm=TRUE),
              ins_self=mean(ins_self, na.rm=TRUE),
              ins_other=mean(ins_other, na.rm=TRUE),
              hispanic=mean(hispanic, na.rm=TRUE),
              nhblack=mean(nhblack, na.rm=TRUE),
              nhwhite=mean(nhwhite, na.rm=TRUE),
              nhother=mean(nhother, na.rm=TRUE),
              obgyn_10kwra=mean(obgyn_10kwra, na.rm=TRUE))



# --- Prepare Market Table ---
market.panel <- market.table %>%
  pivot_longer(cols = -atlanta, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = recode(Variable,
    n_deliveries = "All Deliveries",
    n_patients = "Patients",
    mean_dist = "Distance to Closest Hospital",
    mean_diff_dist = "Distance Traveled Past Closest",
    n_facilities = "Delivery Hospitals"
  )) %>%
  pivot_wider(names_from = atlanta, values_from = Value) %>%
  rename(`In Atlanta` = `1`, `Out of Atlanta` = `0`) %>%
  mutate(
    `In Atlanta` = case_when(
      Variable %in% c("All Deliveries", "Patients", "Delivery Hospitals") ~ formatC(`In Atlanta`, format = "d", big.mark = ","),
      TRUE ~ formatC(`In Atlanta`, digits = 3, format = "f")
    ),
    `Out of Atlanta` = case_when(
      Variable %in% c("All Deliveries", "Patients", "Delivery Hospitals") ~ formatC(`Out of Atlanta`, format = "d", big.mark = ","),
      TRUE ~ formatC(`Out of Atlanta`, digits = 3, format = "f")
    )
  )

# --- Prepare Hospital Table ---
hospital.panel <- hospital.table %>%
  pivot_longer(cols = -atlanta, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = recode(Variable,
    perilevel_plus = "Perinatal Level 3 or more",
    any_teach = "Minor or Major Teaching",
    c_section_elect = "Elective C-Section Rate"
  )) %>%
  pivot_wider(names_from = atlanta, values_from = Value) %>%
  rename(`In Atlanta` = `1`, `Out of Atlanta` = `0`) %>%
  mutate(
    `In Atlanta` = formatC(`In Atlanta`, digits = 3, format = "f"),
    `Out of Atlanta` = formatC(`Out of Atlanta`, digits = 3, format = "f")
  )

# --- Prepare Patient Table ---
patient.panel <- patient.table %>%
  pivot_longer(cols = -atlanta, names_to = "var", values_to = "value") %>%
  mutate(
    Variable = recode(var,
      age = "Age",
      ci_scorent = "Leonard Obstetric Comorbidity Score",
      ins_mcaid = "Medicaid",
      ins_priv = "Private",
      ins_self = "Self-Pay",
      ins_other = "Other Payer",
      hispanic = "Hispanic",
      nhblack = "Black Non-Hispanic",
      nhwhite = "White Non-Hispanic",
      nhother = "Other/Unknown",
      obgyn_10kwra = "OB/GYNs per 10,000 WRA in County"
    ),
    Format = ifelse(var == "age", "int", "dec")
  ) %>%
  pivot_wider(names_from = atlanta, values_from = value) %>%
  rename(`In Atlanta` = `1`, `Out of Atlanta` = `0`) %>%
  mutate(
    `In Atlanta` = ifelse(Format == "int",
                          formatC(`In Atlanta`, format = "d"),
                          formatC(`In Atlanta`, digits = 3, format = "f")),
    `Out of Atlanta` = ifelse(Format == "int",
                              formatC(`Out of Atlanta`, format = "d"),
                              formatC(`Out of Atlanta`, digits = 3, format = "f"))
  ) %>%
  select(Variable, `In Atlanta`, `Out of Atlanta`)


# --- Build combined table with panel break rows ---
panel.break <- function(label) {
  tibble(
    Variable = label,
    `In Atlanta` = "",
    `Out of Atlanta` = ""
  )
}

final.table <- bind_rows(
  panel.break("Panel A. Market Characteristics"),
  market.panel,
  panel.break("Panel B. Hospital Characteristics"),
  hospital.panel,
panel.break("Panel C. Patient Characteristics"),
  patient.panel %>%
    slice(1:2),  # Age + Comorbidity
  patient.panel %>%
    filter(Variable == "OB/GYNs per 10,000 WRA in County"),
  panel.break("Insurance"),
  patient.panel %>%
    filter(Variable %in% c("Medicaid", "Private", "Self-Pay", "Other Payer")),
  panel.break("Race/Ethnicity"),
  patient.panel %>%
    filter(Variable %in% c("Hispanic", "Black Non-Hispanic", "White Non-Hispanic", "Other/Unknown"))
)

# --- Create formatted kable ---
final.table %>%
  kbl(
    booktabs = TRUE,
    align = "lrr",
    col.names = c(" ", "In Atlanta", "Out of Atlanta"),
    escape = FALSE
  ) %>%
  row_spec(
    which(final.table$`In Atlanta` == "" & final.table$`Out of Atlanta` == ""),
    bold = TRUE, align = "left", background = "#F0F0F0", extra_latex_after = "\\addlinespace"
  ) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position", "striped"))


# Identify panel rows (they have blank In/Out values)
panel_rows <- which(final.table$`In Atlanta` == "" & final.table$`Out of Atlanta` == "")

# Create base flextable
ft <- flextable(final.table) %>%
  set_header_labels(
    Variable = " ",
    `In Atlanta` = "In Atlanta",
    `Out of Atlanta` = "Out of Atlanta"
  ) %>%
  autofit() %>%
  align(align = "left", part = "all")

# Loop to apply formatting to each panel row
for (r in panel_rows) {
  ft <- ft %>%
    bold(i = r, bold = TRUE, part = "body") %>%
    merge_at(i = r, j = 1:3, part = "body") %>%
    fontsize(i = r, size = 11, part = "body") %>%
    padding(i = r, padding.top = 4, padding.bottom = 4, part = "body")
}

# Export to Word
doc <- read_docx() %>%
  body_add_flextable(value = ft) %>%
  body_add_par("", style = "Normal")


print(doc, target = "results/tables/sum_stats.docx")




# Linear regressions ---------------------------------------------------

reg.dat <- delivery.dat %>%
    group_by(mkt, year) %>%
    mutate(mkt_deliveries=n()) %>%
    ungroup() %>%
    group_by(facility_d, year) %>%
    mutate(hosp_deliveries=n(),
           hosp_count=row_number()) %>%
    ungroup() %>%
    mutate(mkt_share=pmin(hosp_deliveries/mkt_deliveries,1)) %>%
    filter(hosp_count==1) %>%
    select(facility_d, perilevel2, perilevel34, teach_major, teach_minor, c_section_elect, hosp_deliveries, mkt_share, year, mkt) %>%
    mutate(ln_q=log(hosp_deliveries), teach_any=if_else(teach_major==1 | teach_minor==1, 1, 0)) %>%
    left_join(market.stats2 %>% select(mkt, city), by="mkt")


q.all <- feols(ln_q ~ c_section_elect | facility_d + year, cluster=~facility_d, data=reg.dat)
share.all <- feols(mkt_share ~  c_section_elect | facility_d + year, cluster=~facility_d, data=reg.dat)

q.models <- list("Overall" = q.all)
share.models <- list("Overall" = share.all)
for (m in 2:11) {
    city_name <- reg.dat %>% filter(mkt == m) %>% pull(city) %>% unique()
    q.m <- feols(ln_q ~ c_section_elect | facility_d + year, cluster=~facility_d, data=reg.dat %>% filter(mkt==m))
    share.m <- feols(mkt_share ~  c_section_elect | facility_d + year, cluster=~facility_d, data=reg.dat %>% filter(mkt==m))
    q.models <- c(q.models, setNames(list(q.m), city_name))
    share.models <- c(share.models, setNames(list(share.m), city_name))
}

modelsummary(q.models,
             coef_rename=c("c_section_elect" = "Elective C-Section Rate"),
             gof_omit='DF|F|Lik|AIC|BIC|Adj',
             output="results/tables/req_ln_q.csv")

modelsummary(share.models,
             coef_rename=c("c_section_elect" = "Elective C-Section Rate"),
             gof_omit='DF|F|Lik|AIC|BIC|Adj',
             output="results/tables/req_share.csv")             


# Choice summary statistics ---------------------------------------------

## Atlanta market
datasummary_balance(~choice, data= choice.reg %>%
                             select(choice, diff_dist, perilevel_plus, perilevel2, perilevel34,
                                    teach_major, teach_minor, any_teach, c_section_elect,
                                    ci_score, ci_scorent),
                             fmt=fmt_decimal(digits=4),
                             output="results/tables/choice_stats.csv")

datasummary_skim(data= choice.reg %>%
                             select(choice, diff_dist, perilevel_plus, perilevel2, perilevel34,
                                    teach_major, teach_minor, any_teach, c_section_elect,
                                    ci_score, ci_scorent),
                            fmt=fmt_decimal(digits=4),
                            output="results/tables/choice_stats_detail.csv")