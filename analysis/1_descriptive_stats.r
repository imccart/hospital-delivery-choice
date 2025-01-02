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
    summarize(n_deliveries=n(), 
              n_facilities=n_distinct(facility_d),
              n_patients=n_distinct(alias_to_mother_longid)) %>%
    left_join(mkt.city, by="mkt")


## choice stats (only in-market facility choice)
market.stats2 <- choice.dat %>% filter(choice==TRUE) %>%
    group_by(mkt) %>%
    summarize(n_deliveries=n(), 
              n_facilities=n_distinct(facility_d),
              n_patients=n_distinct(patid),
              mean_dist=mean(diff_dist, na.rm=TRUE)) %>%
    left_join(mkt.city, by="mkt")              


# Linear regressions ---------------------------------------------------

reg.dat <- delivery.dat %>%
    group_by(mkt, year) %>%
    mutate(mkt_deliveries=n()) %>%
    ungroup() %>%
    group_by(facility_d, year) %>%
    mutate(hosp_deliveries=n(),
           hosp_count=row_number()) %>%
    ungroup() %>%
    mutate(mkt_share=pmin(hosp_deliveries/mkt_deliveries,1)) %>%
    filter(hosp_count==1) %>%
    select(facility_d, perilevel2, perilevel34, teach_major, teach_minor, c_section_elect, hosp_deliveries, mkt_share, year, mkt) %>%
    mutate(ln_q=log(hosp_deliveries), teach_any=if_else(teach_major==1 | teach_minor==1, 1, 0)) %>%
    left_join(market.stats2 %>% select(mkt, city), by="mkt")


q.all <- feols(ln_q ~ c_section_elect | facility_d + year, cluster=~facility_d, data=reg.dat)
share.all <- feols(mkt_share ~  c_section_elect | facility_d + year, cluster=~facility_d, data=reg.dat)

q.models <- list("Overall" = q.all)
share.models <- list("Overall" = share.all)
for (m in 2:11) {
    city_name <- reg.dat %>% filter(mkt == m) %>% pull(city) %>% unique()
    q.m <- feols(ln_q ~ c_section_elect | facility_d + year, cluster=~facility_d, data=reg.dat %>% filter(mkt==m))
    share.m <- feols(mkt_share ~  c_section_elect | facility_d + year, cluster=~facility_d, data=reg.dat %>% filter(mkt==m))
    q.models <- c(q.models, setNames(list(q.m), city_name))
    share.models <- c(share.models, setNames(list(share.m), city_name))
}

modelsummary(q.models,
             coef_rename=c("c_section_elect" = "Elective C-Section Rate"),
             gof_omit='DF|F|Lik|AIC|BIC|Adj',
             output="results/tables/req_ln_q.csv")

modelsummary(share.models,
             coef_rename=c("c_section_elect" = "Elective C-Section Rate"),
             gof_omit='DF|F|Lik|AIC|BIC|Adj',
             output="results/tables/req_share.csv")             


# Choice summary statistics ---------------------------------------------

## Atlanta market
datasummary_balance(~choice, data= choice.reg %>%
                             select(choice, diff_dist, perilevel_plus, perilevel2, perilevel34,
                                    teach_major, teach_minor, any_teach, c_section_elect,
                                    ci_score, ci_scorent),
                             fmt=fmt_decimal(digits=4),
                             output="results/tables/choice_stats.csv")

datasummary_skim(data= choice.reg %>%
                             select(choice, diff_dist, perilevel_plus, perilevel2, perilevel34,
                                    teach_major, teach_minor, any_teach, c_section_elect,
                                    ci_score, ci_scorent),
                            fmt=fmt_decimal(digits=4),
                            output="results/tables/choice_stats_detail.csv")