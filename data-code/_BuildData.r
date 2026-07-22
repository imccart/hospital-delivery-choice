# Meta --------------------------------------------------------------------
## Title:         Georgia Hospital Delivery Markets
## Author:        Ian McCarthy
## Date Created:  10/12/2020 (from prior repo)
## Date Edited:   8/2/2024


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, ggplot2, haven, sf, janitor, sp, igraph, plotly, geosphere)

## call functions
source("data-code/functions.R")


# Import data -------------------------------------------------------------

## patient/delivery data
patient.dat <- read_dta("data/input/Mom_delivery_V2.dta")

## hospital data
hosp.dat <- read_dta("data/input/Hospitallevel_V2.dta")

## census tract data
## 2000 census tract
## tract.dat <- read_sf("data/input/shapefiles-2010/tl_2010_13_tract00.shp") %>%
##     mutate(GEOID = as.double(CTIDFP00))

## 2010 census tract
tract.dat <- read_sf("data/input/shapefiles/tl_2020_13_tract10.shp") %>%
    mutate(GEOID = as.double(GEOID10))

## count missing census tracts in delivery data
patient.dat %>% filter(is.na(censustract_d)) %>% nrow()

## census tracts in the state without any patients (or with <10 patients)
tract.dat %>% select(GEOID) %>% st_set_geometry(NULL) %>%
    left_join(patient.dat %>% group_by(censustract_d) %>% summarize(n=n()), 
              by=c("GEOID"="censustract_d")) %>%
    filter(is.na(n) | n<10) %>% nrow()



# Patient and Facility Data ------------------------------------------------
full.dat <- patient.dat %>% left_join(hosp.dat, by=c("facility_d","year")) 

market.dat <- full.dat %>%
    mutate(patient_GEOID=censustract_d,
           facility_GEOID=facility_censustract) %>%
    group_by(facility_d, facility_GEOID, patient_GEOID) %>% 
    filter(!is.na(patient_GEOID), !is.na(facility_GEOID)) %>%
    summarize(total_cases=n())


# Check census tract data -------------------------------------------------

## unique patient census tracts
patient.tracts <- full.dat %>% 
    group_by(censustract_d) %>% summarise(n_patient=n()) %>%
    arrange(desc(n_patient)) %>% filter(!is.na(censustract_d)) %>%
    rename(census_tract=censustract_d)

## unique facility census tracts
facility.tracts <- full.dat %>%
    group_by(facility_censustract) %>% summarise(n_facility=n()) %>%
    arrange(desc(n_facility)) %>% filter(!is.na(facility_censustract)) %>%
    rename(census_tract=facility_censustract)

## merge patient.tracts and facility.tracts to see overlap
tract.overlap <- bind_rows(patient.tracts, facility.tracts) %>% distinct(census_tract) %>%
    left_join(patient.tracts, by="census_tract") %>%
    left_join(facility.tracts, by="census_tract") %>%
    left_join(tract.dat, by=c("census_tract"="GEOID")) %>%
    mutate(geo_match=ifelse(is.na(GEOID10),0,1))

## plot the census tracts
ggplot(data = tract.dat) +
  geom_sf() + 
  theme_minimal() 

ggplot(data = tract.dat) +
  geom_sf() +  # Plot all tracts
  geom_sf(data = tract.dat %>% filter(GEOID==13083040200), fill = "red", color = "red") +  # Highlight specific tract
  theme_minimal()

# Form markets ------------------------------------------------------------

## Market definitions are saved inputs. The suffix gives the settings that
## produced them: 005_steps10 is the 0.05 strong-link threshold with 10
## random-walk steps, which is the definition behind every result in the paper.
## To build against another, run 1_community_detection.R at those settings and
## edit the filename here. Cluster labels differ between definitions, so
## switching also means updating the hardcoded `mkt` vectors in the analysis
## scripts. See ReadMe.md.
walktrap.dat <- read_csv("data/output/market-defs/market_assignment_005_steps10.csv", show_col_types=FALSE)


# Final delivery data ------------------------------------------------------

delivery.dat <- full.dat %>% ungroup() %>%
  left_join(walktrap.dat %>% select(GEOID, mkt), by=c("censustract_d"="GEOID")) %>%
  mutate(year=format(date_delivery, "%Y"))

write_rds(delivery.dat,"data/output/delivery_data.rds")


# Choice data --------------------------------------------------------------

## Builds choice_data_mkt.rds (~2 GB) and choice_data_dist.rds (~4 GB). Slow, so
## it is usually run on its own after a rebuild rather than as part of the driver.
## source("data-code/3_choice_data.R")
