# Meta --------------------------------------------------------------------
## Title:        Facility-to-AHA system-membership crosswalk (R2.3)
## Description:  Match each Georgia delivery facility in the choice data to the
##               nearest AHA hospital in the same year, by straight-line
##               distance between facility coordinates, and carry over the AHA
##               system identifier. Facility locations are public hospital
##               addresses, not patient data. The match is verified by the
##               nearest-neighbor distance and by ZIP agreement.
## Usage:        Run from project root, or sourced by analysis/revision/_revision.R.
## Output:       data/output/facility_system_xwalk.csv  (facility, year, system)

library(tidyverse)
library(data.table)
library(geosphere)

## our facilities, one row per facility-year, with coordinates and ZIP
choice.fac <- read_rds("data/output/choice_data_mkt.rds") %>%
  distinct(facility, year) %>% mutate(year = as.numeric(year))

our.hosp <- read_rds("data/output/delivery_data.rds") %>%
  mutate(year = as.numeric(year)) %>%
  distinct(facility = facility_d, year,
           lat = facility_latitude, lon = facility_longitude,
           zip = facility_zipcode) %>%
  semi_join(choice.fac, by = c("facility", "year")) %>%
  filter(!is.na(lat), !is.na(lon))

## AHA Georgia hospitals with system id and coordinates, plus name for checking
aha.geo <- fread("C:/Users/immccar/SynologyDrive/work/research-data-repo/aha-data/data/output/aha_geo_1980-2024.csv") %>%
  filter(MSTATE == "GA", !is.na(LAT), !is.na(LONG)) %>%
  select(aha_id = ID, SYSID, LAT, LONG, MLOCZIP, year)
aha.name <- fread("C:/Users/immccar/SynologyDrive/work/research-data-repo/aha-data/data/output/aha_data_1980-2024.csv") %>%
  distinct(aha_id = ID, year, MNAME)
aha.geo <- aha.geo %>%
  left_join(aha.name, by = c("aha_id", "year")) %>%
  mutate(aha_zip5 = str_sub(as.character(MLOCZIP), 1, 5))

## Match each facility-year to an AHA hospital in the same year. Prefer the
## nearest AHA hospital sharing the facility's ZIP, which resolves both rural
## coordinate imprecision and the case where the raw nearest neighbor is a
## psychiatric or rehab hospital rather than the delivery hospital. Fall back to
## the nearest overall when no AHA hospital shares the ZIP.
match_year <- function(y) {
  oh <- our.hosp %>% filter(year == y) %>%
    mutate(zip5 = str_sub(as.character(zip), 1, 5))
  ah <- aha.geo %>% filter(year == y)
  if (nrow(oh) == 0 || nrow(ah) == 0) return(NULL)
  d <- distm(oh %>% select(lon, lat), ah %>% select(LONG, LAT), fun = distHaversine)

  pick <- integer(nrow(oh))
  for (i in seq_len(nrow(oh))) {
    same_zip <- which(ah$aha_zip5 == oh$zip5[i])
    cand <- if (length(same_zip) > 0) same_zip else seq_len(nrow(ah))
    pick[i] <- cand[which.min(d[i, cand])]
  }
  oh %>%
    mutate(aha_id   = ah$aha_id[pick],
           aha_name = ah$MNAME[pick],
           aha_zip  = ah$MLOCZIP[pick],
           SYSID    = ah$SYSID[pick],
           match_km = d[cbind(seq_len(nrow(oh)), pick)] / 1000)
}

xwalk <- bind_rows(lapply(sort(unique(our.hosp$year)), match_year)) %>%
  mutate(zip5      = str_sub(as.character(zip), 1, 5),
         aha_zip5  = str_sub(as.character(aha_zip), 1, 5),
         zip_ok    = zip5 == aha_zip5,
         system    = as.integer(!is.na(SYSID) & SYSID != 0 & SYSID != ""))

## match quality
cat("facility-years matched:", nrow(xwalk), "\n")
cat("match distance (km):\n"); print(round(quantile(xwalk$match_km, c(0,.5,.9,.95,1)), 3))
cat("ZIP agreement:", sprintf("%d of %d (%.0f%%)\n",
    sum(xwalk$zip_ok, na.rm = TRUE), nrow(xwalk), 100*mean(xwalk$zip_ok, na.rm = TRUE)))
cat("\nsuspect matches (>1 km or ZIP mismatch):\n")
xwalk %>% filter(match_km > 1 | !zip_ok) %>%
  select(facility, year, aha_name, match_km, zip5, aha_zip5, system) %>%
  arrange(desc(match_km)) %>% as.data.frame() %>% head(40) %>% print()

## share system-affiliated among matched facilities (facility-year level)
cat("\nsystem-affiliated share (facility-years):",
    sprintf("%.0f%%\n", 100*mean(xwalk$system)))

write_csv(xwalk %>% select(facility, year, system, aha_id, match_km, zip_ok),
          "data/output/facility_system_xwalk.csv")
