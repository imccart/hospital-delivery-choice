# Meta --------------------------------------------------------------------
## Title:        Reviewer 2, Comment 3 — system-membership sensitivity
## Description:  Add a hospital system-membership indicator to the choice model
##               and re-fit each baseline (0.05) market, to show the hospital-
##               attribute effects are robust to including it. System membership
##               comes from step 2 (facility matched to AHA by location;
##               facility addresses are public). Point
##               estimates only, no re-bootstrap, split Atlanta / outside.
## Usage:        Sourced by analysis/revision/_revision.R (after step 2 crosswalk).
## Output:       results/tables/revision/system_membership_{by_market,grouped}.csv

## Packages, setDTthreads, and analysis/functions.r are prepared by _revision.R.
dir.create("results/tables/revision", showWarnings = FALSE, recursive = TRUE)

# Inputs ------------------------------------------------------------------

delivery.dat <- read_rds("data/output/delivery_data.rds") %>% mutate(year = as.numeric(year))
obgyn.data   <- read_sas("data/input/obgyn20162022.sas7bdat") %>% mutate(year = as.numeric(year))
sys.xwalk    <- read_csv("data/output/facility_system_xwalk.csv", show_col_types = FALSE) %>%
  distinct(facility, year, system)

hosp.attrs <- delivery.dat %>%
  filter(!is.na(facility_d)) %>%
  group_by(facility = facility_d, year) %>%
  summarize(perilevel34 = first(perilevel34), teach_major = first(teach_major),
            teach_minor = first(teach_minor), c_section_elect = first(c_section_elect),
            .groups = "drop")

pat.attrs <- delivery.dat %>%
  transmute(patid = alias_to_mother_longid, date_delivery, censustract_d, fcounty_d,
            age = mother_age_in_years, ci_scorent,
            hispanic, nhblack, nhwhite, ins_mcaid, ins_self)

var2    <- c("ci_scorent", "age", "nhwhite", "nhblack", "hispanic", "mcaid_unins", "obgyn_10kwra")
markets <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

# Baseline choice frame with covariates and system indicator --------------

choice.reg <- read_rds("data/output/choice_data_mkt.rds") %>%
  mutate(year = as.numeric(year)) %>%
  left_join(hosp.attrs, by = c("facility", "year")) %>%
  left_join(pat.attrs,  by = c("patid", "date_delivery")) %>%
  left_join(obgyn.data, by = c("fcounty_d" = "fipscounty", "year")) %>%
  left_join(sys.xwalk,  by = c("facility", "year")) %>%
  group_by(facility) %>% mutate(system = if_else(is.na(system),
                                as.integer(median(system, na.rm = TRUE) >= 0.5), system)) %>%
  ungroup() %>%
  mutate(any_teach      = if_else(teach_major == 1 | teach_minor == 1, 1, 0),
         perilevel_plus = if_else(perilevel34 == 1, 1, 0),
         mcaid_unins    = if_else(ins_mcaid == 1 | ins_self == 1, 1, 0))

cat("rows missing system after fill:", sum(is.na(choice.reg$system)), "\n")

# Fit each market under both specifications -------------------------------

## one market, one specification; returns the four attribute effects (and the
## system effect when system is in the spec) averaged over chosen deliveries
fit_market <- function(cr, m, var1, pfx.inc, label) {
  res <- tryCatch(estimate_choice_model(m, var1, var2, var1, pfx.inc, cr)$predictions,
                  error = function(e) { cat("  market", m, label, "skipped:",
                                            conditionMessage(e), "\n"); NULL })
  if (is.null(res)) return(NULL)
  has_sys <- "system" %in% var1
  res %>% filter(choice == 1) %>%
    summarize(n_deliveries = n(),
              ch_dist     = mean(pred_diff_dist1      - pred_prob,            na.rm = TRUE),
              ch_peri     = mean(pred_perilevel_plus1 - pred_perilevel_plus0, na.rm = TRUE),
              ch_teach    = mean(pred_any_teach1      - pred_any_teach0,      na.rm = TRUE),
              ch_csection = mean(pred_c_section_elect1 - pred_prob,           na.rm = TRUE),
              ch_system   = if (has_sys) mean(pred_system1 - pred_system0, na.rm = TRUE) else NA_real_) %>%
    mutate(mkt = m, spec = label)
}

specs <- list(
  base   = list(var1 = c("diff_dist","perilevel_plus","any_teach","c_section_elect"),
                inc  = c(1,1,1,0.01)),
  system = list(var1 = c("diff_dist","perilevel_plus","any_teach","c_section_elect","system"),
                inc  = c(1,1,1,0.01,1))
)

by_market <- bind_rows(lapply(names(specs), function(s)
  bind_rows(lapply(markets, function(m)
    fit_market(choice.reg, m, specs[[s]]$var1, specs[[s]]$inc, s)))))

atl <- tibble(mkt = markets, atl_area = if_else(markets == 9, "Atlanta area", "Outside Atlanta"))
by_market <- by_market %>% left_join(atl, by = "mkt") %>%
  relocate(spec, mkt, atl_area, n_deliveries)
write_csv(by_market, "results/tables/revision/system_membership_by_market.csv")

# Deliveries-weighted group means -----------------------------------------

wm <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
grouped <- by_market %>%
  group_by(spec, atl_area) %>%
  summarize(n_markets = n(),
            ch_dist = wm(ch_dist, n_deliveries), ch_peri = wm(ch_peri, n_deliveries),
            ch_teach = wm(ch_teach, n_deliveries), ch_csection = wm(ch_csection, n_deliveries),
            ch_system = wm(ch_system, n_deliveries), .groups = "drop") %>%
  arrange(atl_area, spec)
write_csv(grouped, "results/tables/revision/system_membership_grouped.csv")
print(as.data.frame(grouped %>% mutate(across(starts_with("ch_"), ~round(.x, 4)))))
