# Meta --------------------------------------------------------------------
## Title:         Estimate Georgia Delivery Choice
## Author:        Ian McCarthy
## Date Created:  10/12/2020 (from prior repo)
## Date Edited:   12/20/2024


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, ggplot2, haven, janitor, mclogit, broom, modelr, fixest,
                modelsummary, prediction, effects, marginaleffects, purrr, kableExtra, scales, estimatr, flextable, officer)

## call functions
source("data-code/functions.R")
source("analysis/functions.R")


# Import data -------------------------------------------------------------

## delivery data
delivery.dat <- read_rds("data/output/delivery_data.rds")

## hospital summaries
hosp.summaries <- delivery.dat %>% ungroup() %>%     
    mutate(year=as.numeric(year)) %>%
    group_by(mkt, year) %>%
    mutate(mkt_patients=n_distinct(alias_to_mother_longid)) %>%
    ungroup() %>%
    group_by(facility_d, year) %>%
    mutate(hosp_patients=n_distinct(alias_to_mother_longid),
           hosp_ciscore=mean(ci_scorent, na.rm=TRUE),
           hosp_count=row_number()) %>%
    ungroup() %>%
    mutate(mkt_share=hosp_patients/mkt_patients) %>%
    filter(hosp_count==1) %>%
    select(facility=facility_d, mkt_share, hosp_ciscore, year) %>%
    mutate(year=year+1)


## choice data
choice.dat.mkt <- read_rds("data/output/choice_data_mkt.rds")
choice.dat.dist <- read_rds("data/output/choice_data_dist.rds")

## facility data
hosp.dat <- read_dta("data/input/Hospitallevel_V2.dta")
    
## patient data
patient.dat <- read_dta("data/input/Mom_delivery_V2.dta")

## patient data
obgyn.data <- read_sas("data/input/obgyn20162022.sas7bdat")

## combine obgyn and hosp.dat
facility.dat <- hosp.dat %>%
    left_join(obgyn.data, by=c("facility_fcounty"="fipscounty","year"))

# Finalize data for estimation
choice.dat <- choice.dat.mkt

choice.reg <- choice.dat %>% mutate(year=as.numeric(year)) %>%
    left_join(hosp.dat %>% 
              select(facility=facility_d, psi_90, perilevel01, perilevel2, perilevel34,
                     teach_major, teach_minor, c_section_elect, year), by=c("facility","year")) %>%
    left_join(patient.dat %>% 
              select(patid=alias_to_mother_longid, age=mother_age_in_years,
              ci_score, ci_scorent, fcounty_d, ndi_scale, noncore, micropolitan, largemetro, largefringe, mediummetro, smallmetro,
              hispanic, nhblack, nhwhite, nhother, nulliparous, primiparous,
              ins_priv, ins_mcaid, ins_mcare, ins_self, ins_other, date_delivery), by=c("patid","date_delivery")) %>%
    left_join(obgyn.data, by=c("fcounty_d"="fipscounty","year")) %>%
    left_join(hosp.summaries %>% select(facility, mkt_share, hosp_ciscore, year), by=c("facility","year")) %>%
    mutate(any_teach=if_else(teach_major==1 | teach_minor==1, 1, 0),
           rural=(noncore==1 | micropolitan==1),
           perilevel_plus=if_else(perilevel34==1, 1, 0),
           mcaid_unins=if_else(ins_mcaid==1 | ins_self==1, 1, 0),
           atlanta=if_else(mkt==9, 1, 0))
    


# Descriptive statistics -----------------------------------------------

source("analysis/1_descriptive_stats.R")
write.csv(market.stats2, "results/tables/market_stats.csv", row.names=FALSE)


# Estimate Choice Model ------------------------------------------------

## Parameters for estimation
n_boot=500
mkt.path="atl-only"
##mkt.path="excluding-atl"
##markets <- c(2,3,4,5,6,7,8,9,10,11)
##markets <- c(2,3,4,5,6,7,8,10,11)
markets <- c(9)
var1 <- c("diff_dist","perilevel_plus","any_teach","c_section_elect")
var2 <- c("ci_scorent","age","nhwhite","nhblack","hispanic","mcaid_unins","obgyn_10kwra")
pfx.vars <- c("diff_dist","perilevel_plus", "any_teach", "c_section_elect")
pfx.inc <- c(1,1,1,0.01)

source("analysis/2_estimation.R")

# Summarize Results ----------------------------------------------------

source("analysis/3_results_summary.R")
write.csv(summary.table, paste0("results/tables/",mkt.path,"/coefficients.csv"), row.names=FALSE)
write.csv(pfx.data, paste0("results/tables/",mkt.path,"/partial_effects.csv"), row.names=FALSE)










# Test code ------------------------------------------------------------

choice.reg %>% group_by(id) %>% summarize(min_mkt=min(mkt.y), max_mkt=max(mkt.y)) %>% filter(min_mkt!=max_mkt)

plot_slopes(models[[1]], variables="c_section_elect", condition="ins_mcaid")
avg.effects <- tibble()
for (i in 1:9) {
    avg.effects[i,1]=avg_slopes(models[[i]], variables="psi_90")$estimate
}


test.est <- estimate_choice_model(
    market = 5,
    var1 <- c("diff_dist","perilevel_plus","any_teach","c_section_elect", "psi_90"),
    var2 <- c("ci_scorent","age","nhwhite","mcaid_unins","obgyn_10kwra"),
    pfx.vars <- c("diff_dist","perilevel_plus", "any_teach", "c_section_elect", "psi_90"),
    pfx.inc <- c(1,1,1,0.01,0.1),
    data = choice.reg %>% filter(year>=2020)
  )

##avg_slopes(test.est$model, variables="psi_90")
##avg_predictions(test.est$model, variables="psi_90")


test.est$predictions %>%
      mutate(ch_dist=pred_diff_dist1 - pred_prob,
              ch_psi=pred_psi_901 - pred_prob) %>%
      group_by(mkt, choice) %>%
      summarize(mean_psi=mean(ch_psi, na.rm=TRUE))

datasummary_skim(data= choice.reg %>% filter(mkt==2, year==2016) %>%
                             select(choice, diff_dist, perilevel_plus, perilevel2, perilevel34,
                                    psi_90, teach_major, teach_minor, any_teach, c_section_elect,
                                    ci_score, ci_scorent),
                            fmt=fmt_decimal(digits=4))

datasummary_crosstab(data= choice.reg %>% filter(mkt==2, year==2016) %>%
                             select(choice, diff_dist, perilevel_plus, perilevel2, perilevel34,
                                    psi_90, teach_major, teach_minor, any_teach, c_section_elect,
                                    ci_score, ci_scorent),
                     psi_90 ~ choice,
                     statistic = 1 ~ Percent("col"),
                     fmt=fmt_decimal(digits=4))
