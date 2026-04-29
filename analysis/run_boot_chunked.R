# Resumable per-rep bootstrap runner.
#
# Run from the project root (a fresh R session, RStudio, or `Rscript`):
#   Rscript analysis/run_boot_chunked.R
# After a BSOD: reboot, re-run the same command. Already-completed reps are
# skipped, so it picks up where it left off.
#
# When all reps are saved, build final.boot in your interactive session via:
#   library(data.table)
#   files <- list.files("results/tables/atl-only/boot_reps",
#                       pattern = "^rep_.*\\.csv$", full.names = TRUE)
#   final.boot <- rbindlist(lapply(files, fread))
# Then source analysis/3_results_summary.R as usual.

library(tidyverse)
library(data.table)
library(haven)
library(mclogit)
library(broom)

source("analysis/functions.R")
source("data-code/functions.R")

# ---- Knobs you might tweak ---------------------------------------------------
mkt.path         <- "excluding-atl"   # "atl-only" or "excluding-atl"
markets          <- c(2:8,10,11)      # c(9) for Atlanta; for excluding-atl use c(2:8,10,11)
n_target         <- 150          # total reps to accumulate
reps_per_session <- 25L          # exit after this many reps (1 = max isolation,
                                 # higher = lower data-load overhead per rep)

var1      <- c("diff_dist","perilevel_plus","any_teach","c_section_elect")
var2      <- c("ci_scorent","age","nhwhite","nhblack","hispanic","mcaid_unins","obgyn_10kwra")
pfx.vars  <- var1
pfx.inc   <- c(1,1,1,0.01)
# ------------------------------------------------------------------------------

# Per-market output dirs and missing-rep lists
out_dirs <- setNames(
  file.path("results/tables", mkt.path, "boot_reps", paste0("mkt_", markets)),
  as.character(markets)
)
for (d in out_dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

missing_by_market <- lapply(markets, function(m) {
  d <- out_dirs[[as.character(m)]]
  existing <- list.files(d, pattern = "^rep_.*\\.csv$")
  done_ids <- as.integer(sub("rep_(\\d+)\\.csv$", "\\1", existing))
  setdiff(seq_len(n_target), done_ids)
})
names(missing_by_market) <- as.character(markets)

total_missing <- sum(lengths(missing_by_market))
if (total_missing == 0) {
  cat("All", n_target, "reps already saved for all markets.\n")
  quit(save = "no")
}
cat("Need", total_missing, "more reps across", length(markets), "markets.\n")

# ---- Build choice.reg --------------------------------------------------------
cat("[", format(Sys.time(), "%H:%M:%S"), "] loading data\n", sep = "")

delivery.dat <- read_rds("data/output/delivery_data.rds")

hosp.summaries <- delivery.dat %>%
  ungroup() %>%
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

rm(delivery.dat, choice.dat.mkt, hosp.dat, patient.dat, obgyn.data); gc(verbose = FALSE)

# ---- Bin spec ----------------------------------------------------------------
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
rm(chosen.dat); gc(verbose = FALSE)

# ---- One-rep loop with checkpoint logging ------------------------------------
# Each step writes to debug.log via open/close-per-call so output survives a
# hard segfault (no buffered lines lost). When the next run starts, the tail
# of debug.log shows the last successful checkpoint before the crash.

log_step <- function(rep_id, step, log_path) {
  mem_mb <- round(sum(gc(verbose = FALSE)[, 2]), 1)   # R-side resident MB
  msg <- sprintf("[%s] rep=%4d  mem=%6.1fMB  %s\n",
                 format(Sys.time(), "%H:%M:%S"), rep_id, mem_mb, step)
  cat(msg)
  cat(msg, file = log_path, append = TRUE)
}

reps_done_this_session <- 0L

for (market in markets) {
  missing  <- missing_by_market[[as.character(market)]]
  if (length(missing) == 0) next

  out_dir  <- out_dirs[[as.character(market)]]
  log_path <- file.path(out_dir, "debug.log")

  log_step(0, paste("=== runner start, market =", market,
                    ", reps_per_session =", reps_per_session, "==="), log_path)

  for (b in missing) {
    out_path <- file.path(out_dir, sprintf("rep_%04d.csv", b))
    if (file.exists(out_path)) next

    t0 <- Sys.time()
    set.seed(b)
    log_step(b, "rep start", log_path)

    log_step(b, "filter market_data", log_path)
    market_data <- choice.reg %>% filter(mkt == market)

    log_step(b, "sample ids", log_path)
    sampled_ids <- sample(unique(market_data$id),
                          size = length(unique(market_data$id)), replace = TRUE)
    boot_ids <- tibble(sampled_id = sampled_ids, boot_id = seq_along(sampled_ids))

    log_step(b, "left_join (many-to-many)", log_path)
    resampled_data <- boot_ids %>%
      left_join(market_data, by = c("sampled_id" = "id"),
                relationship = "many-to-many") %>%
      mutate(id = boot_id) %>%
      select(-sampled_id)

    log_step(b, "estimate_choice_model: start", log_path)
    boot_model <- estimate_choice_model(market, var1, var2, pfx.vars, pfx.inc,
                                        resampled_data)
    log_step(b, "estimate_choice_model: done", log_path)

    log_step(b, "compute per_rep + ch_*", log_path)
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

    log_step(b, "free big intermediates", log_path)
    rm(boot_model, resampled_data, market_data, boot_ids, sampled_ids)
    gc(verbose = FALSE)

    log_step(b, "summarize_boot_rep", log_path)
    one <- summarize_boot_rep(per_rep, bin_spec, b, market)
    one$boot_rep <- b

    log_step(b, "fwrite csv", log_path)
    fwrite(one, out_path)

    log_step(b, sprintf("rep done in %.1fs", as.numeric(Sys.time() - t0, units = "secs")), log_path)

    rm(per_rep, one)
    gc(verbose = FALSE)

    reps_done_this_session <- reps_done_this_session + 1L
    if (reps_done_this_session >= reps_per_session) {
      log_step(0, sprintf("=== session quota of %d reps reached, exiting ===",
                          reps_per_session), log_path)
      quit(save = "no")
    }
  }
}

cat("=== all reps complete across all markets ===\n")
