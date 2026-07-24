# Meta --------------------------------------------------------------------
## Title:        Reviewer 1, Comment 2 — maternal-transfer sensitivity
## Description:  Transfer agreements and perinatal regionalization can determine
##               the delivery hospital independent of choice. Re-estimate the
##               baseline marginal effects excluding deliveries flagged as a
##               maternal transfer prior to delivery, and compare to the full
##               sample. Point estimates only, no re-bootstrap, split Atlanta /
##               outside. The full sample reproduces the baseline effects and
##               dropping the transfer occasions (~0.2% of deliveries) leaves
##               them unchanged.
## Inputs:       data/output/delivery_data.rds, data/output/choice_data_mkt.rds,
##               data/input/obgyn20162022.sas7bdat
## Output:       results/tables/revision/transfer_sensitivity.csv
## Note:         Packages and analysis/functions.r are prepared by _revision.R.

dir.create("results/tables/revision", showWarnings = FALSE, recursive = TRUE)

delivery.dat <- read_rds("data/output/delivery_data.rds") %>% mutate(year = as.numeric(year))
obgyn.data   <- read_sas("data/input/obgyn20162022.sas7bdat") %>% mutate(year = as.numeric(year))

hosp.attrs <- delivery.dat %>%
  filter(!is.na(facility_d)) %>%
  group_by(facility = facility_d, year) %>%
  summarize(perilevel34 = first(perilevel34), teach_major = first(teach_major),
            teach_minor = first(teach_minor), c_section_elect = first(c_section_elect),
            .groups = "drop")

pat.attrs <- delivery.dat %>%
  transmute(patid = alias_to_mother_longid, date_delivery, fcounty_d,
            age = mother_age_in_years, ci_scorent,
            hispanic, nhblack, nhwhite, ins_mcaid, ins_self)

var1    <- c("diff_dist", "perilevel_plus", "any_teach", "c_section_elect")
var2    <- c("ci_scorent", "age", "nhwhite", "nhblack", "hispanic", "mcaid_unins", "obgyn_10kwra")
pfx.inc <- c(1, 1, 1, 0.01)
markets <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

choice.reg <- read_rds("data/output/choice_data_mkt.rds") %>%
  mutate(year = as.numeric(year)) %>%
  left_join(hosp.attrs, by = c("facility", "year")) %>%
  left_join(pat.attrs,  by = c("patid", "date_delivery")) %>%
  left_join(obgyn.data, by = c("fcounty_d" = "fipscounty", "year")) %>%
  mutate(any_teach      = if_else(teach_major == 1 | teach_minor == 1, 1, 0),
         perilevel_plus = if_else(perilevel34 == 1, 1, 0),
         mcaid_unins    = if_else(ins_mcaid == 1 | ins_self == 1, 1, 0))

## choice occasions whose delivery is flagged as a maternal transfer
transfer_keys <- delivery.dat %>%
  filter(mother_transfer_prior_to_deliver == 1) %>%
  distinct(patid = alias_to_mother_longid, date_delivery)

samples <- list(
  full        = choice.reg,
  no_transfer = choice.reg %>% anti_join(transfer_keys, by = c("patid", "date_delivery"))
)

# Fit each market for each sample -----------------------------------------

fit_one_market <- function(cr, m) {
  res <- tryCatch(estimate_choice_model(m, var1, var2, var1, pfx.inc, cr)$predictions,
                  error = function(e) { cat("  market", m, "skipped:",
                                            conditionMessage(e), "\n"); NULL })
  if (is.null(res)) return(NULL)
  res %>% filter(choice == 1) %>%
    summarize(n_deliveries = n(),
              ch_dist     = mean(pred_diff_dist1      - pred_prob,             na.rm = TRUE),
              ch_peri     = mean(pred_perilevel_plus1 - pred_perilevel_plus0,  na.rm = TRUE),
              ch_teach    = mean(pred_any_teach1      - pred_any_teach0,       na.rm = TRUE),
              ch_csection = mean(pred_c_section_elect1 - pred_prob,            na.rm = TRUE)) %>%
    mutate(mkt = m)
}

by_market <- bind_rows(lapply(names(samples), function(s)
  bind_rows(lapply(markets, function(m) fit_one_market(samples[[s]], m))) %>%
    mutate(sample = s)))

atl <- tibble(mkt = markets, atl_area = if_else(markets == 9, "Atlanta area", "Outside Atlanta"))
by_market <- by_market %>% left_join(atl, by = "mkt") %>%
  relocate(sample, mkt, atl_area, n_deliveries)

wm <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
transfer_sensitivity <- by_market %>%
  group_by(sample, atl_area) %>%
  summarize(n_markets = n(),
            ch_dist = wm(ch_dist, n_deliveries), ch_peri = wm(ch_peri, n_deliveries),
            ch_teach = wm(ch_teach, n_deliveries), ch_csection = wm(ch_csection, n_deliveries),
            .groups = "drop") %>%
  arrange(atl_area, sample)

write_csv(transfer_sensitivity, "results/tables/revision/transfer_sensitivity.csv")
print(as.data.frame(transfer_sensitivity %>% mutate(across(starts_with("ch_"), ~round(.x, 4)))))
