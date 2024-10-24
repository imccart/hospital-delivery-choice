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
              n_patients=n_distinct(patid)) %>%
    left_join(mkt.city, by="mkt")              
