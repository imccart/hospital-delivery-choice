# Estimate Choice Model ---------------------------------------------------

## Finalize data for estimation
choice.reg <- choice.dat %>% mutate(year=as.numeric(year)) %>%
    left_join(hosp.dat %>% 
              select(facility=facility_d, psi_90, perilevel01, perilevel2, perilevel34,
                     teach_major, teach_minor, c_section_elect,year), by=c("facility","year")) %>%
    left_join(patient.dat %>% 
              select(patid=alias_to_mother_longid, age=mother_age_in_years,
              ci_score, ci_scorent, fcounty_d, ndi_scale, noncore, micropolitan, largemetro, largefringe, mediummetro, smallmetro,
              hispanic, nhblack, nhwhite, nhother, nulliparous, primiparous,
              ins_priv, ins_mcaid, ins_mcare, ins_self, ins_other, date_delivery), by=c("patid","date_delivery")) %>%
    left_join(obgyn.data, by=c("fcounty_d"="fipscounty","year")) %>%              
    mutate(any_teach=(teach_major==1 | teach_minor==1),
           rural=(noncore==1 | micropolitan==1),
           perilevel_plus=if_else(perilevel2==1 | perilevel34==1, 1, 0))

## Estimate and store results
all.predictions <- list()
all.coefficients <- list()
models <- list()

for (market in markets) {
  result <- estimate_choice_model(
    market = market,
    var1 = var1,
    var2 = var2,
    pfx.vars = pfx.vars,
    pfx.inc = pfx.inc,
    data = choice.reg
  )
  
  all.predictions[[market]] <- result$predictions
  all.coefficients[[market]] <- result$coefficients
  models[[paste0("market_", market)]] <- result$model
  
}

bootstraps <- bootstrap_choice_model(markets, var1, var2, pfx.vars, pfx.inc, choice.reg, n_bootstrap = n_boot)
final.boot <- bind_rows(bootstraps)  