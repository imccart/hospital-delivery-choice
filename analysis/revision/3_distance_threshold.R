# Meta --------------------------------------------------------------------
## Title:        Reviewer 2 sensitivity sweeps (Medical Care R&R)
## Description:  Two sensitivity analyses reported point-estimates-only (no
##               re-bootstrap), both producing deliveries-weighted marginal
##               effects split into the Atlanta metro area versus the rest of
##               the state.
##
##   R2.1  Driving distance. On the baseline 0.05 markets, refit each market
##         with differential distance measured in driving miles over the
##         Georgia road network (drive_distance.csv) rather than straight-line,
##         on the shared drive-complete, valid-geocode sample.
##
##   R2.2  Market-definition threshold. Rebuild choice sets at strong-link
##         thresholds 0.10, 0.15, 0.20 and 0.25 (0.05 is the baseline) and
##         refit every multi-hospital market. As the threshold rises the
##         Atlanta metro subdivides; each resulting market is labeled Atlanta
##         area when the majority of its deliveries originate in baseline
##         (0.05) Atlanta tracts.
##
## Usage:       Run from project root, or sourced by _revision.R. Needs
##              data/output/delivery_data.rds, choice_data_mkt.rds,
##              drive_distance.csv (step 1), the market-defs CSVs, and the obgyn
##              file.
## Outputs:     results/tables/revision/drive_sensitivity_by_market.csv
##              results/tables/revision/drive_sensitivity_grouped.csv
##              results/tables/revision/threshold_sweep_by_market.csv
##              results/tables/revision/threshold_sweep_grouped.csv

## Packages, setDTthreads, and analysis/functions.r are prepared by
## analysis/revision/_revision.R, which sources this script.
dir.create("results/tables/revision", showWarnings = FALSE, recursive = TRUE)

# Shared inputs and specification -----------------------------------------

delivery.dat <- read_rds("data/output/delivery_data.rds") %>%
  mutate(year = as.numeric(year))

obgyn.data <- read_sas("data/input/obgyn20162022.sas7bdat") %>%
  mutate(year = as.numeric(year))

## baseline Atlanta tracts, for labeling markets under every definition
atl.tracts <- read_csv("data/output/market-defs/market_assignment_005_steps10.csv",
                       show_col_types = FALSE) %>%
  filter(mkt == 9) %>% pull(GEOID)

## each facility-year's attributes, taken as the alternative-specific covariates
hosp.attrs <- delivery.dat %>%
  filter(!is.na(facility_d)) %>%
  group_by(facility = facility_d, year) %>%
  summarize(perilevel34    = first(perilevel34),
            teach_major     = first(teach_major),
            teach_minor     = first(teach_minor),
            c_section_elect = first(c_section_elect),
            .groups = "drop")

## each delivery's patient attributes, constant across its choice set
pat.attrs <- delivery.dat %>%
  transmute(patid = alias_to_mother_longid, date_delivery, censustract_d, fcounty_d,
            age = mother_age_in_years, ci_scorent,
            hispanic, nhblack, nhwhite, ins_mcaid, ins_self)

var1 <- c("diff_dist", "perilevel_plus", "any_teach", "c_section_elect")
var2 <- c("ci_scorent", "age", "nhwhite", "nhblack", "hispanic", "mcaid_unins", "obgyn_10kwra")
pfx.vars <- var1
pfx.inc  <- c(1, 1, 1, 0.01)

# Helpers -----------------------------------------------------------------

## attach the model covariates to a choice frame keyed by
## (patid, date_delivery, facility, year, mkt, id, choice, diff_dist)
add_covariates <- function(choice_df) {
  choice_df %>%
    left_join(hosp.attrs, by = c("facility", "year")) %>%
    left_join(pat.attrs, by = c("patid", "date_delivery")) %>%
    left_join(obgyn.data, by = c("fcounty_d" = "fipscounty", "year")) %>%
    mutate(any_teach      = if_else(teach_major == 1 | teach_minor == 1, 1, 0),
           perilevel_plus = if_else(perilevel34 == 1, 1, 0),
           mcaid_unins    = if_else(ins_mcaid == 1 | ins_self == 1, 1, 0))
}

## deliveries-weighted marginal effects for one market, or NULL if the fit fails
fit_one_market <- function(cr, m) {
  res <- tryCatch(
    estimate_choice_model(m, var1, var2, pfx.vars, pfx.inc, cr)$predictions,
    error = function(e) { cat("  market", m, "skipped:", conditionMessage(e), "\n"); NULL })
  if (is.null(res)) return(NULL)
  res %>%
    filter(choice == 1) %>%
    summarize(n_deliveries = n(),
              ch_dist     = mean(pred_diff_dist1      - pred_prob,             na.rm = TRUE),
              ch_peri     = mean(pred_perilevel_plus1 - pred_perilevel_plus0,  na.rm = TRUE),
              ch_teach    = mean(pred_any_teach1      - pred_any_teach0,       na.rm = TRUE),
              ch_csection = mean(pred_c_section_elect1 - pred_prob,            na.rm = TRUE)) %>%
    mutate(mkt = m)
}

## per-market Atlanta-area label from the share of chosen deliveries whose
## residential tract falls in the baseline Atlanta market
atl_labels <- function(cr) {
  cr %>% filter(choice == 1) %>%
    group_by(mkt) %>%
    summarize(atl_share = mean(censustract_d %in% atl.tracts, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(atl_area = if_else(atl_share > 0.5, "Atlanta area", "Outside Atlanta"))
}

## fit every multi-hospital market in a choice frame, attach labels
fit_all_markets <- function(cr) {
  nhosp <- cr %>% distinct(mkt, facility) %>% count(mkt, name = "n_hosp")
  mkts  <- nhosp %>% filter(n_hosp > 1) %>% pull(mkt) %>% sort()
  out <- bind_rows(lapply(mkts, function(m) fit_one_market(cr, m)))
  out %>%
    left_join(nhosp, by = "mkt") %>%
    left_join(atl_labels(cr), by = "mkt")
}

## collapse per-market effects to deliveries-weighted group means. Each mean is
## taken over the markets that identify that effect (na.rm), since perinatal
## level and teaching status are collinear within markets that hold only one
## such hospital and are dropped by mclogit there. n_id_* reports how many
## markets identify each effect, so "stable where estimable" is visible.
group_effects <- function(by_market, ...) {
  wm <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
  by_market %>%
    group_by(...) %>%
    summarize(n_markets      = n(),
              tot_deliveries = sum(n_deliveries),
              n_id_dist      = sum(!is.na(ch_dist)),
              n_id_peri      = sum(!is.na(ch_peri)),
              n_id_teach     = sum(!is.na(ch_teach)),
              n_id_csection  = sum(!is.na(ch_csection)),
              ch_dist        = wm(ch_dist,     n_deliveries),
              ch_peri        = wm(ch_peri,     n_deliveries),
              ch_teach       = wm(ch_teach,    n_deliveries),
              ch_csection    = wm(ch_csection, n_deliveries),
              .groups = "drop") %>%
    rename(n_deliveries = tot_deliveries)
}

# R2.1  Driving-distance sensitivity (baseline 0.05 markets) --------------
cat("\n=== R2.1 driving distance ===\n")

ga_box <- function(lat, lon) lat > 30.0 & lat < 35.2 & lon > -85.9 & lon < -80.5

drive.dist <- read_csv("data/output/drive_distance.csv", show_col_types = FALSE)

base <- read_rds("data/output/choice_data_mkt.rds") %>%
  mutate(year = as.numeric(year),
         origin_key = sprintf("%.6f_%.6f", latitude_d, longitude_d)) %>%
  left_join(drive.dist %>% select(mkt, origin_key, facility, drive_mi),
            by = c("mkt", "origin_key", "facility"), relationship = "many-to-one") %>%
  group_by(id) %>%
  mutate(drop_id = any(is.na(drive_mi)) | any(!ga_box(latitude_d, longitude_d)),
         nearest_drive = min(drive_mi)) %>%
  ungroup() %>%
  filter(!drop_id) %>%
  mutate(diff_dist_straight = distance_mi - nearest,
         diff_dist_drive    = drive_mi - nearest_drive)

cor_diff <- cor(base$diff_dist_straight, base$diff_dist_drive)
cat("differential-distance correlation (straight vs drive):", round(cor_diff, 4), "\n")

base.cov <- add_covariates(
  base %>% mutate(diff_dist = diff_dist_straight) %>%
    select(patid, date_delivery, facility, year, mkt, id, choice,
           diff_dist, diff_dist_straight, diff_dist_drive))

drive_by_market <- bind_rows(
  fit_all_markets(base.cov %>% mutate(diff_dist = diff_dist_straight)) %>% mutate(measure = "straight"),
  fit_all_markets(base.cov %>% mutate(diff_dist = diff_dist_drive))    %>% mutate(measure = "drive")
) %>%
  mutate(cor_diff_dist = cor_diff) %>%
  relocate(measure, mkt, atl_area, n_hosp, n_deliveries)

write_csv(drive_by_market, "results/tables/revision/drive_sensitivity_by_market.csv")

drive_grouped <- group_effects(drive_by_market, measure, atl_area) %>%
  arrange(atl_area, measure)
write_csv(drive_grouped, "results/tables/revision/drive_sensitivity_grouped.csv")
print(drive_grouped)

rm(base, base.cov, drive.dist); gc(verbose = FALSE)

# R2.2  Market-definition threshold sweep ---------------------------------
cat("\n=== R2.2 threshold sweep ===\n")

thresholds <- tibble(tag = c("005", "010", "015", "020", "025"),
                     threshold = c(0.05, 0.10, 0.15, 0.20, 0.25))

sweep_by_market <- list()
for (i in seq_len(nrow(thresholds))) {
  tag <- thresholds$tag[i]; thr <- thresholds$threshold[i]
  cat("\n-- threshold", thr, "--\n")

  mkt.def <- read_csv(paste0("data/output/market-defs/market_assignment_", tag, "_steps10.csv"),
                      show_col_types = FALSE)

  delivery.t <- delivery.dat %>%
    select(-mkt) %>%
    left_join(mkt.def %>% select(GEOID, mkt), by = c("censustract_d" = "GEOID"))

  admits <- delivery.t %>%
    transmute(patid = alias_to_mother_longid, date_delivery, year, facility_d, mkt,
              latitude_d, longitude_d)

  hospitals <- delivery.t %>%
    filter(!is.na(mkt), !is.na(facility_d)) %>%
    group_by(facility_d, mkt, year) %>%
    summarize(latitude_f = first(facility_latitude),
              longitude_f = first(facility_longitude),
              delivery_count = n(), .groups = "drop") %>%
    group_by(facility_d, year) %>%
    mutate(mkt_count = max(delivery_count)) %>%
    filter(delivery_count > 10, mkt_count == delivery_count) %>%
    ungroup() %>%
    select(facility = facility_d, mkt, year, latitude_f, longitude_f)

  cr <- admits %>%
    full_join(hospitals, by = c("mkt", "year"), relationship = "many-to-many") %>%
    filter(!is.na(facility), !is.na(patid)) %>%
    arrange(patid, date_delivery, year, mkt, facility) %>%
    group_by(patid, date_delivery) %>%
    mutate(id = cur_group_id(), choice = (facility == facility_d)) %>%
    ungroup() %>%
    mutate(distance_mi = distGeo(cbind(longitude_d, latitude_d),
                                 cbind(longitude_f, latitude_f)) / 1609.34) %>%
    filter(!is.na(distance_mi)) %>%
    group_by(id) %>%
    mutate(any_choice = max(choice), nearest = min(distance_mi)) %>%
    ungroup() %>%
    mutate(diff_dist = distance_mi - nearest) %>%
    filter(any_choice == 1) %>%
    add_covariates()

  res <- fit_all_markets(cr) %>%
    mutate(tag = tag, threshold = thr) %>%
    relocate(threshold, mkt, atl_area, n_hosp, n_deliveries)
  sweep_by_market[[tag]] <- res
  cat("  markets fit:", nrow(res), " Atlanta-area:",
      sum(res$atl_area == "Atlanta area"), "\n")

  rm(delivery.t, admits, hospitals, cr); gc(verbose = FALSE)
}

threshold_by_market <- bind_rows(sweep_by_market)
write_csv(threshold_by_market, "results/tables/revision/threshold_sweep_by_market.csv")

threshold_grouped <- group_effects(threshold_by_market, threshold, atl_area) %>%
  arrange(atl_area, threshold)
write_csv(threshold_grouped, "results/tables/revision/threshold_sweep_grouped.csv")
print(threshold_grouped, n = Inf)

cat("\ndone\n")
