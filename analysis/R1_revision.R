# Meta --------------------------------------------------------------------
## Title:         Revision analyses, Medical Care R&R (MDC-D-26-00255)
## Author:        Ian McCarthy
## Date Created:  7/21/2026
##
## One script, one delineated chunk per editor/reviewer comment that requires
## computation. Some chunks feed more than one comment, which is why they live
## together rather than in per-comment files. Outputs feed both the response
## letter and the revised manuscript.
##
## Sourced by _analysis.r after 1_descriptive_stats.R through 3_results_summary.R,
## so packages, parameters, helpers, and the loaded data are already in scope.
## Chunks that need only delivery.dat and choice.dat.mkt are marked as such and
## can be run on their own in a fresh session.


# Reviewer 1, Comment 2 — Maternal transfer sensitivity -------------------
## Transfer agreements and perinatal regionalization can determine the delivery
## hospital independent of choice. We re-estimate the baseline overall marginal
## effects excluding deliveries flagged as a maternal transfer prior to delivery
## (mother_transfer_prior_to_deliver == 1) and compare to the full-sample
## baseline. Point estimates only, no re-bootstrap. Atlanta run 2026-07-22: the
## full sample reproduces partial_effects.csv exactly and dropping the 1,672
## transfer occasions leaves the four effects unchanged (C-section 0.0252 ->
## 0.0253, the rest identical).
##
## Uses the in-scope choice.reg, markets, var1/var2/pfx.vars/pfx.inc and
## delivery.dat, so it runs for whichever config _analysis.r has active. Writes
## results/tables/<mkt.path>/transfer_sensitivity.csv.

## Choice occasions whose delivery is flagged as a maternal transfer
transfer_keys <- delivery.dat %>%
  filter(mother_transfer_prior_to_deliver == 1) %>%
  distinct(patid = alias_to_mother_longid, date_delivery)

## Full sample, and the sample with transfer occasions removed
samples <- list(
  full        = choice.reg,
  no_transfer = choice.reg %>% anti_join(transfer_keys, by = c("patid","date_delivery"))
)

transfer_pfx <- list()
for (s in names(samples)) {
  cr <- samples[[s]]
  n_del <- cr %>% filter(choice == TRUE) %>% count(mkt, name = "n_deliveries")

  preds <- list()
  for (m in markets) {
    preds[[as.character(m)]] <-
      estimate_choice_model(m, var1, var2, pfx.vars, pfx.inc, cr)$predictions
  }

  transfer_pfx[[s]] <- cr %>%
    inner_join(bind_rows(preds), by = c("id","choice","year","patid","facility","mkt")) %>%
    filter(choice == 1) %>%
    mutate(ch_dist     = pred_diff_dist1       - pred_prob,
           ch_peri     = pred_perilevel_plus1  - pred_perilevel_plus0,
           ch_teach    = pred_any_teach1       - pred_any_teach0,
           ch_csection = pred_c_section_elect1 - pred_prob) %>%
    left_join(n_del, by = "mkt") %>%
    ungroup() %>%
    summarize(across(c(ch_dist, ch_peri, ch_teach, ch_csection),
                     ~ weighted.mean(.x, w = n_deliveries, na.rm = TRUE))) %>%
    mutate(sample = s, n_occasions = n_distinct(cr$id))
}

transfer_sensitivity <- bind_rows(transfer_pfx) %>%
  relocate(sample, n_occasions)

write_csv(transfer_sensitivity,
          paste0("results/tables/", mkt.path, "/transfer_sensitivity.csv"))
print(transfer_sensitivity)


# Reviewer 3, Comments 1-2 — Participant flow and missingness -------------
## R3 asks for (1) a flow diagram giving numbers eligible, linked, excluded with
## reasons, and the final analytic sample, and (2) per-variable missingness with
## a statement of how it was handled. This chunk produces the counts behind both.
## The flow figure itself is drawn from sample_flow.csv.
##
## The exclusions mirror data-code/3_choice_data.R exactly. Needs only
## delivery.dat and choice.dat.mkt.

## Step 1. All linked delivery records
n_all <- nrow(delivery.dat)

## Step 2. Drop deliveries whose census tract was not assigned to a market by
## the walktrap clustering. This is where the (0,0) sentinel geocodes leave the
## sample; they carry no census tract and so never receive a market.
flow.dat <- delivery.dat %>%
    mutate(zero_geo=(latitude_d==0 & longitude_d==0))

n_zero_geo <- sum(flow.dat$zero_geo, na.rm=TRUE)
n_no_mkt <- sum(is.na(flow.dat$mkt))
n_no_mkt_nonzero <- sum(is.na(flow.dat$mkt) & !flow.dat$zero_geo, na.rm=TRUE)

## Step 3. Drop deliveries in the LaGrange market (mkt 12), which has a
## single-hospital choice set and is excluded from the choice model.
n_lagrange <- sum(flow.dat$mkt==12, na.rm=TRUE)

## Step 4. Hospital-side exclusions applied in 3_choice_data.R: a facility must
## contribute more than 10 deliveries to a market-year, and is assigned to the
## single market where it sees the most deliveries.
hosp.flow <- delivery.dat %>%
    filter(!is.na(mkt), !is.na(facility_d)) %>%
    group_by(facility_d, mkt, year) %>%
    summarize(delivery_count=n(), .groups="drop") %>%
    group_by(facility_d, year) %>%
    mutate(mkt_count=max(delivery_count)) %>%
    ungroup()

n_fac_all <- n_distinct(hosp.flow$facility_d)
n_fac_kept <- hosp.flow %>%
    filter(delivery_count>10, mkt_count==delivery_count) %>%
    pull(facility_d) %>%
    n_distinct()

## Step 5. Final analytic sample, taken from the choice data itself so the
## counts are the ones the model actually used.
n_final_deliveries <- n_distinct(choice.dat.mkt$id)
n_final_patients <- n_distinct(choice.dat.mkt$patid)
n_final_facilities <- n_distinct(choice.dat.mkt$facility)

sample_flow <- tribble(
    ~step, ~label,                                              ~n,
    1,     "Linked delivery records, 2016-2020",                n_all,
    2,     "Excluded: no market assigned (unmapped tract)",     n_no_mkt,
    2.1,   "   of which missing geocode, coded (0,0)",          n_zero_geo,
    2.2,   "   of which tract present but market unassigned",   n_no_mkt_nonzero,
    3,     "Excluded: LaGrange market, single-hospital choice set", n_lagrange,
    4,     "Delivery facilities before hospital-side exclusions", n_fac_all,
    4.1,   "Delivery facilities retained",                      n_fac_kept,
    5,     "Analytic sample: deliveries",                       n_final_deliveries,
    5.1,   "Analytic sample: individuals",                      n_final_patients,
    5.2,   "Analytic sample: hospitals",                        n_final_facilities
)

write_csv(sample_flow, "results/tables/sample_flow.csv")
print(sample_flow, n=Inf)

## Reconcile the individual count against the Methods section, which reports
## 710,870 deliveries to 577,846 individuals. The delivery count matches the
## analytic sample exactly; the individual count does not, so identify which
## point in the pipeline the published figure was taken at.
individual_counts <- tribble(
    ~definition, ~n_individuals,
    "All linked delivery records",
        n_distinct(delivery.dat$alias_to_mother_longid),
    "Market assigned (includes LaGrange)",
        n_distinct(delivery.dat$alias_to_mother_longid[!is.na(delivery.dat$mkt)]),
    "Market assigned, excluding LaGrange",
        n_distinct(delivery.dat$alias_to_mother_longid[!is.na(delivery.dat$mkt) &
                                                        delivery.dat$mkt!=12]),
    "Analytic sample (choice data)",
        n_distinct(choice.dat.mkt$patid)
) %>%
    mutate(published=577846,
           difference=n_individuals-published)

write_csv(individual_counts, "results/tables/individual_count_reconciliation.csv")
print(individual_counts, n=Inf)

## Missingness on the variables entering the choice model, computed on the
## analytic sample of deliveries rather than on all linked records.
## Match on the delivery, not the mother. Matching on patid alone pulls in other
## deliveries by the same individual that are not themselves in the choice data.
analytic.keys <- choice.dat.mkt %>%
    distinct(patid, date_delivery)

analytic.deliveries <- delivery.dat %>%
    semi_join(analytic.keys, by=c("alias_to_mother_longid"="patid", "date_delivery"))

model_vars <- c("mother_age_in_years", "ci_scorent", "hispanic", "nhblack",
                "nhwhite", "nhother", "ins_priv", "ins_mcaid", "ins_self",
                "latitude_d", "longitude_d", "censustract_d", "fcounty_d",
                "perilevel34", "teach_major", "teach_minor", "c_section_elect",
                "facility_latitude", "facility_longitude")

missingness <- analytic.deliveries %>%
    select(all_of(model_vars)) %>%
    summarize(across(everything(), ~sum(is.na(.x)))) %>%
    pivot_longer(everything(), names_to="variable", values_to="n_missing") %>%
    mutate(n_total=nrow(analytic.deliveries),
           pct_missing=round(100*n_missing/n_total, 3)) %>%
    arrange(desc(pct_missing))

write_csv(missingness, "results/tables/missingness.csv")
print(missingness, n=Inf)
