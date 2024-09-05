# Meta --------------------------------------------------------------------
## Title:         Estimate Georgia Delivery Choice
## Author:        Ian McCarthy
## Date Created:  10/12/2020 (from prior repo)
## Date Edited:   8/9/2024


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, ggplot2, haven, janitor, mclogit, broom, modelr, 
                modelsummary, prediction, effects, marginaleffects)

## call functions
source("data-code/functions.R")


# Import data -------------------------------------------------------------

## delivery data
delivery.dat <- read_rds("data/output/delivery_data.rds")

## choice data
choice.dat <- read_rds("data/output/choice_data.rds")

## facility data
facility.dat <- read_dta("data/raw/Hospitallevel_V2.dta")

## patient data
patient.dat <- read_dta("data/raw/mom_delivery_v2.dta")

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
    summarize(n=n(), 
              n_facilities=n_distinct(facility_d),
              n_patients=n_distinct(alias_to_mother_longid)) %>%
    left_join(mkt.city, by="mkt")


## choice stats (only in-market facility choice)
market.stats2 <- choice.dat %>% filter(choice==TRUE) %>%
    group_by(mkt) %>%
    summarize(n=n(), 
              n_facilities=n_distinct(facility_d),
              n_patients=n_distinct(patid)) %>%
    left_join(mkt.city, by="mkt")              
