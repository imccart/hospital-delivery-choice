# Meta --------------------------------------------------------------------
## Title:         Estimate Georgia Delivery Choice
## Author:        Ian McCarthy
## Date Created:  10/12/2020 (from prior repo)
## Date Edited:   10/17/2024


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, ggplot2, haven, janitor, mclogit, broom, modelr, 
                modelsummary, prediction, effects, marginaleffects, purrr, kableExtra, scales)

## call functions
source("data-code/functions.R")
source("analysis/functions.R")


# Import data -------------------------------------------------------------

## delivery data
delivery.dat <- read_rds("data/output/delivery_data.rds")

## choice data
choice.dat <- read_rds("data/output/choice_data.rds")

## facility data
hosp.dat <- read_dta("data/input/Hospitallevel_V2.dta")

## patient data
patient.dat <- read_dta("data/input/Mom_delivery_V2.dta")

## patient data
obgyn.data <- read_sas("data/input/obgyn20162022.sas7bdat")

## combine obgyn and hosp.dat
facility.dat <- hosp.dat %>%
    left_join(obgyn.data, by=c("facility_fcounty"="fipscounty","year"))


# Descriptive statistics -----------------------------------------------
source("analysis/1_descriptive_stats.R")
write.csv(market.stats2, "results/tables/market_stats.csv", row.names=FALSE)


# Estimate Choice Model ------------------------------------------------

## Parameters for estimation
n_boot=100
mkt.path="excluding-atl"
##markets <- c(2,3,4,5,6,7,8,9,10,11)
markets <- c(2,3,4,5,6,7,8,10,11)
var1 <- c("diff_dist","perilevel_plus","teach_major","c_section_elect", "psi_90")
var2 <- c("ci_scorent","age","nhwhite","ins_mcaid","obgyn_10kwra")
pfx.vars <- c("diff_dist","perilevel_plus", "teach_major", "c_section_elect", "psi_90")
pfx.inc <- c(1,1,1,0.01,0.02)

source("analysis/2_estimation.R")

# Summarize Results ----------------------------------------------------
source("analysis/3_results_summary.R")
write.csv(summary.table, paste0("results/tables/",mkt.path,"/coefficients.csv"), row.names=FALSE)
write.csv(pfx.data, paste0("results/tables/",mkt.path,"/partial_effects.csv"), row.names=FALSE)

plot_slopes(models[[1]], variables="diff_dist", condition="ins_mcaid")