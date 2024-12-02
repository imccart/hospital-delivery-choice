# Build choice data -------------------------------------------------------

## admits only
admits <- delivery.dat %>% 
    select(patid=alias_to_mother_longid, date_delivery, year, facility_d, mkt, latitude_d, longitude_d)

## hospitals only
hospitals <- delivery.dat %>% 
    filter(!is.na(mkt), !is.na(facility_d)) %>% 
    group_by(facility_d, mkt, year) %>%
    summarize(latitude_f=first(facility_latitude, na.rm=TRUE),
              longitude_f=first(facility_longitude, na.rm=TRUE),
              delivery_count=n()) %>%
    group_by(facility_d, year) %>%
    mutate(mkt_count=max(delivery_count)) %>%
    filter(delivery_count>10, mkt_count==delivery_count) %>%
    ungroup() %>%
    select(facility=facility_d, mkt, year, latitude_f, longitude_f, delivery_count)

## merge admits and hospitals
choice.dat.mkt <- admits %>% full_join(hospitals, by=c("mkt","year"), relationship="many-to-many") %>%
    arrange(patid, date_delivery, year, mkt, facility) %>%
    group_by(patid, date_delivery) %>%
    mutate(id=cur_group_id(),
           choice=(facility==facility_d)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(distance=distGeo(c(longitude_d, latitude_d), c(longitude_f, latitude_f)),
            distance_mi=distance/1609.34)

choice.dat.dist <- admits %>% full_join(hospitals, by=c("year"), relationship="many-to-many") %>%
    arrange(patid, date_delivery, year, facility) %>%
    group_by(patid, date_delivery) %>%
    mutate(id=cur_group_id(),
           choice=(facility==facility_d)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(distance=distGeo(c(longitude_d, latitude_d), c(longitude_f, latitude_f)),
            distance_mi=distance/1609.34)


choice.reg <- choice.dat.mkt %>%
    filter(!is.na(distance_mi)) %>%
    group_by(id) %>%
    mutate(any_choice=max(choice),
           nearest=min(distance_mi)) %>%
    ungroup() %>%
    mutate(diff_dist=distance_mi-nearest) %>%
    filter(any_choice==1)

write_rds(choice.reg,"data/output/choice_data_mkt.rds")


choice.reg <- choice.dat.dist %>%
    filter(!is.na(distance_mi), distance_mi<100) %>%
    group_by(id) %>%
    mutate(any_choice=max(choice),
           nearest=min(distance_mi)) %>%
    ungroup() %>%
    mutate(diff_dist=distance_mi-nearest) %>%
    filter(any_choice==1)

write_rds(choice.reg,"data/output/choice_data_dist.rds")