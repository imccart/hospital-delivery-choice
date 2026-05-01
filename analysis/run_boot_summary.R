# Post-bootstrap summary runner.
#
# Assumes run_boot_loop.sh has finished and the per-rep CSVs exist under
#   results/tables/<mkt.path>/boot_reps/rep_NNNN.csv
#
# This script re-builds everything _analysis.r needs after the bootstrap
# (data, choice.reg, bin_spec, per-market mclogit fits) and then loads
# final.boot from the saved CSVs instead of re-running the bootstrap.
#
# Run from the project root:
#   Rscript analysis/run_boot_summary.R
# or interactively after setting the config block below.

# ---- Config (toggle for the market you just finished) ------------------------
# NOTE: mkt values are walktrap output. If 1_community_detection.R is re-run,
# the IDs may change; see ReadMe.md "Walktrap markets and regeneration".
##mkt.path         <- "atl-only"
##markets          <- c(9)
##target_bin_label <- "(28.6,37.4]"

mkt.path         <- "excluding-atl"
markets          <- c(2,3,4,5,6,7,8,10,11)
target_bin_label <- "(28.2,36.8]"

var1     <- c("diff_dist","perilevel_plus","any_teach","c_section_elect")
var2     <- c("ci_scorent","age","nhwhite","nhblack","hispanic","mcaid_unins","obgyn_10kwra")
pfx.vars <- c("diff_dist","perilevel_plus","any_teach","c_section_elect")
pfx.inc  <- c(1,1,1,0.01)
# ------------------------------------------------------------------------------

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, ggplot2, haven, janitor, mclogit, broom, modelr, fixest,
               modelsummary, prediction, effects, marginaleffects, purrr, kableExtra, scales,
               estimatr, flextable, officer)

source("data-code/functions.R")
source("analysis/functions.R")


# Import data ------------------------------------------------------------------

delivery.dat <- read_rds("data/output/delivery_data.rds")

hosp.summaries <- delivery.dat %>% ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  group_by(mkt, year) %>%
  mutate(mkt_patients = n_distinct(alias_to_mother_longid)) %>%
  ungroup() %>%
  group_by(facility_d, year) %>%
  mutate(hosp_patients = n_distinct(alias_to_mother_longid),
         hosp_ciscore  = mean(ci_scorent, na.rm = TRUE),
         hosp_count    = row_number()) %>%
  ungroup() %>%
  mutate(mkt_share = hosp_patients / mkt_patients) %>%
  filter(hosp_count == 1) %>%
  select(facility = facility_d, mkt_share, hosp_ciscore, year) %>%
  mutate(year = year + 1)

choice.dat.mkt <- read_rds("data/output/choice_data_mkt.rds")
hosp.dat       <- read_dta("data/input/Hospitallevel_V2.dta")
patient.dat    <- read_dta("data/input/Mom_delivery_V2.dta")
obgyn.data     <- read_sas("data/input/obgyn20162022.sas7bdat")

choice.reg <- choice.dat.mkt %>%
  mutate(year = as.numeric(year)) %>%
  left_join(hosp.dat %>%
              select(facility = facility_d, psi_90, perilevel01, perilevel2, perilevel34,
                     teach_major, teach_minor, c_section_elect, year),
            by = c("facility","year")) %>%
  left_join(patient.dat %>%
              select(patid = alias_to_mother_longid, age = mother_age_in_years,
                     ci_score, ci_scorent, fcounty_d, ndi_scale, noncore, micropolitan,
                     largemetro, largefringe, mediummetro, smallmetro,
                     hispanic, nhblack, nhwhite, nhother, nulliparous, primiparous,
                     ins_priv, ins_mcaid, ins_mcare, ins_self, ins_other, date_delivery),
            by = c("patid","date_delivery")) %>%
  left_join(obgyn.data, by = c("fcounty_d" = "fipscounty", "year")) %>%
  left_join(hosp.summaries %>% select(facility, mkt_share, hosp_ciscore, year),
            by = c("facility","year")) %>%
  mutate(any_teach      = if_else(teach_major == 1 | teach_minor == 1, 1, 0),
         rural          = (noncore == 1 | micropolitan == 1),
         perilevel_plus = if_else(perilevel34 == 1, 1, 0),
         mcaid_unins    = if_else(ins_mcaid == 1 | ins_self == 1, 1, 0),
         atlanta        = if_else(mkt == 9, 1, 0))


# Bin spec ---------------------------------------------------------------------

chosen.dat <- choice.reg %>% filter(choice == TRUE, mkt %in% markets)

bin_spec <- list(
  age = list(
    breaks = seq(min(chosen.dat$age, na.rm = TRUE),
                 max(chosen.dat$age, na.rm = TRUE), length.out = 6)
  ),
  ci_scorent = list(
    qbreaks_by_mkt = chosen.dat %>% group_by(mkt) %>%
      summarize(q10 = quantile(ci_scorent, 0.10, na.rm = TRUE),
                q25 = quantile(ci_scorent, 0.25, na.rm = TRUE),
                q50 = quantile(ci_scorent, 0.50, na.rm = TRUE),
                q75 = quantile(ci_scorent, 0.75, na.rm = TRUE),
                q90 = quantile(ci_scorent, 0.90, na.rm = TRUE),
                .groups = "drop")
  ),
  obgyn_10kwra = list(
    qbreaks_by_mkt = chosen.dat %>% group_by(mkt) %>%
      summarize(q10 = quantile(obgyn_10kwra, 0.10, na.rm = TRUE),
                q25 = quantile(obgyn_10kwra, 0.25, na.rm = TRUE),
                q50 = quantile(obgyn_10kwra, 0.50, na.rm = TRUE),
                q75 = quantile(obgyn_10kwra, 0.75, na.rm = TRUE),
                q90 = quantile(obgyn_10kwra, 0.90, na.rm = TRUE),
                .groups = "drop")
  )
)


# Per-market mclogit fits (gives models, all.predictions, all.coefficients) ----

all.predictions  <- list()
all.coefficients <- list()
models           <- list()

for (market in markets) {
  result <- estimate_choice_model(market, var1, var2, pfx.vars, pfx.inc, choice.reg)
  all.predictions[[market]]  <- result$predictions
  all.coefficients[[market]] <- result$coefficients
  models[[paste0("market_", market)]] <- result$model
}


# market.stats2 — only n_deliveries is used by 3_results_summary.r ------------

market.stats2 <- choice.reg %>%
  filter(choice == TRUE) %>%
  group_by(mkt) %>%
  summarize(n_deliveries = n(), .groups = "drop")


# Load final.boot from saved per-rep CSVs --------------------------------------

boot_dir <- file.path("results/tables", mkt.path, "boot_reps")
boot_files <- list.files(boot_dir, pattern = "^rep_.*\\.csv$", full.names = TRUE, recursive = TRUE)
stopifnot(length(boot_files) > 0)
final.boot <- rbindlist(lapply(boot_files, fread))
cat("Loaded final.boot:", length(boot_files), "reps,", nrow(final.boot), "rows\n")


# Summarize results ------------------------------------------------------------

source("analysis/3_results_summary.R")

write.csv(coefficient.table, paste0("results/tables/", mkt.path, "/coefficients.csv"), row.names = FALSE)
write.csv(pfx.data,          paste0("results/tables/", mkt.path, "/partial_effects.csv"), row.names = FALSE)
print(summary_table, target = paste0("results/tables/pfx_means_", mkt.path, ".docx"))
