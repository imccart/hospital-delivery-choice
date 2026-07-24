# Meta --------------------------------------------------------------------
## Title:        Reviewer 3, Comments 1-2 — participant flow and missingness
## Description:  Counts behind (1) the participant flow diagram (eligible, linked,
##               excluded with reasons, final analytic sample) and (2) per-variable
##               missingness with the complete-case handling statement. Also
##               reconciles the distinct-individual count against the submitted
##               Methods figure. The exclusions mirror data-code/3_choice_data.R.
## Inputs:       data/output/delivery_data.rds, data/output/choice_data_mkt.rds
## Output:       results/tables/revision/{sample_flow, missingness,
##               individual_count_reconciliation}.csv
## Note:         Packages are prepared by analysis/revision/_revision.R.

delivery.dat  <- read_rds("data/output/delivery_data.rds")
choice.dat.mkt <- read_rds("data/output/choice_data_mkt.rds")
dir.create("results/tables/revision", showWarnings = FALSE, recursive = TRUE)

# Participant flow --------------------------------------------------------

## Step 1. All linked delivery records
n_all <- nrow(delivery.dat)

## Step 2. Drop deliveries whose census tract was not assigned to a market by
## the walktrap clustering. This is where the (0,0) sentinel geocodes leave the
## sample; they carry no census tract and so never receive a market.
flow.dat <- delivery.dat %>%
    mutate(zero_geo = (latitude_d == 0 & longitude_d == 0))

n_zero_geo       <- sum(flow.dat$zero_geo, na.rm = TRUE)
n_no_mkt         <- sum(is.na(flow.dat$mkt))
n_no_mkt_nonzero <- sum(is.na(flow.dat$mkt) & !flow.dat$zero_geo, na.rm = TRUE)

## Step 3. Drop deliveries in the LaGrange market (mkt 12), which has a
## single-hospital choice set and is excluded from the choice model.
n_lagrange <- sum(flow.dat$mkt == 12, na.rm = TRUE)

## Step 4. Hospital-side exclusions applied in 3_choice_data.R: a facility must
## contribute more than 10 deliveries to a market-year, and is assigned to the
## single market where it sees the most deliveries.
hosp.flow <- delivery.dat %>%
    filter(!is.na(mkt), !is.na(facility_d)) %>%
    group_by(facility_d, mkt, year) %>%
    summarize(delivery_count = n(), .groups = "drop") %>%
    group_by(facility_d, year) %>%
    mutate(mkt_count = max(delivery_count)) %>%
    ungroup()

n_fac_all  <- n_distinct(hosp.flow$facility_d)
n_fac_kept <- hosp.flow %>%
    filter(delivery_count > 10, mkt_count == delivery_count) %>%
    pull(facility_d) %>%
    n_distinct()

## Step 5. Final analytic sample, taken from the choice data itself so the
## counts are the ones the model actually used.
n_final_deliveries <- n_distinct(choice.dat.mkt$id)
n_final_patients   <- n_distinct(choice.dat.mkt$patid)
n_final_facilities <- n_distinct(choice.dat.mkt$facility)

sample_flow <- tribble(
    ~step, ~label,                                                  ~n,
    1,     "Linked delivery records, 2016-2020",                    n_all,
    2,     "Excluded: no market assigned (unmapped tract)",         n_no_mkt,
    2.1,   "   of which missing geocode, coded (0,0)",              n_zero_geo,
    2.2,   "   of which tract present but market unassigned",       n_no_mkt_nonzero,
    3,     "Excluded: LaGrange market, single-hospital choice set", n_lagrange,
    4,     "Delivery facilities before hospital-side exclusions",   n_fac_all,
    4.1,   "Delivery facilities retained",                          n_fac_kept,
    5,     "Analytic sample: deliveries",                           n_final_deliveries,
    5.1,   "Analytic sample: individuals",                          n_final_patients,
    5.2,   "Analytic sample: hospitals",                            n_final_facilities
)

write_csv(sample_flow, "results/tables/revision/sample_flow.csv")
print(sample_flow, n = Inf)

# Individual-count reconciliation -----------------------------------------
## The submitted Methods reports 710,870 deliveries to 577,846 individuals. The
## delivery count matches the analytic sample exactly; the individual count does
## not. Identify where the published figure was taken.
individual_counts <- tribble(
    ~definition, ~n_individuals,
    "All linked delivery records",
        n_distinct(delivery.dat$alias_to_mother_longid),
    "Market assigned (includes LaGrange)",
        n_distinct(delivery.dat$alias_to_mother_longid[!is.na(delivery.dat$mkt)]),
    "Market assigned, excluding LaGrange",
        n_distinct(delivery.dat$alias_to_mother_longid[!is.na(delivery.dat$mkt) &
                                                        delivery.dat$mkt != 12]),
    "Analytic sample (choice data)",
        n_distinct(choice.dat.mkt$patid)
) %>%
    mutate(published = 577846, difference = n_individuals - published)

write_csv(individual_counts, "results/tables/revision/individual_count_reconciliation.csv")
print(individual_counts, n = Inf)

# Missingness -------------------------------------------------------------
## Variables entering the choice model, computed on the analytic sample of
## deliveries. Match on the delivery, not the mother. Matching on patid alone
## pulls in other deliveries by the same individual not in the choice data.
analytic.keys <- choice.dat.mkt %>% distinct(patid, date_delivery)

analytic.deliveries <- delivery.dat %>%
    semi_join(analytic.keys, by = c("alias_to_mother_longid" = "patid", "date_delivery"))

model_vars <- c("mother_age_in_years", "ci_scorent", "hispanic", "nhblack",
                "nhwhite", "nhother", "ins_priv", "ins_mcaid", "ins_self",
                "latitude_d", "longitude_d", "censustract_d", "fcounty_d",
                "perilevel34", "teach_major", "teach_minor", "c_section_elect",
                "facility_latitude", "facility_longitude")

missingness <- analytic.deliveries %>%
    select(all_of(model_vars)) %>%
    summarize(across(everything(), ~ sum(is.na(.x)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
    mutate(n_total = nrow(analytic.deliveries),
           pct_missing = round(100 * n_missing / n_total, 3)) %>%
    arrange(desc(pct_missing))

write_csv(missingness, "results/tables/revision/missingness.csv")
print(missingness, n = Inf)
