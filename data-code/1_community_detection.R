# Form markets ------------------------------------------------------------

## consturct market (census tract) data using census tract shapefiles
market.dat %>% left_join(tract.dat, by=c("facility_censustract"="GEOID")) %>%
    mutate(geo_match=ifelse(is.na(GEOID10),0,1)) %>%
    filter(geo_match==1) %>%
    select(GEOID=facility_censustract, n_patient, geometry)

## identify contiguous census tracts
tract.info <- tract.dat %>% get_contig()

## restrict to contiguous census tracts
bp.contig <- tract.info %>% st_set_geometry(NULL) %>%
  pivot_longer(cols=starts_with("contig_"), names_to="contig_num", values_to="GEOID_contig") %>%
  filter(!is.na(GEOID_contig) & !is.na(GEOID)) %>% 
  select(GEOID,GEOID_contig) %>% 
  mutate(contig = 1) %>%
  pivot_wider(names_from="GEOID_contig", values_from="contig", values_fill = 0)

## set parameter values
minimum_share=0.05
minimum_number=10

## create bipartite matrix
bp.hosp.geoid <- market.dat %>% 
  group_by(patient_GEOID) %>% 
  mutate(patient_share=total_cases/sum(total_cases,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(connected = as.integer(patient_share >= minimum_share))  %>%
  mutate(share = ifelse(connected==1,patient_share,0))   %>% 
  select(GEOID=patient_GEOID, facility_d, connected) %>%
  inner_join(bp.contig %>% select(GEOID),"GEOID") %>% 
  pivot_wider(names_from = "facility_d", values_from = "connected", values_fill = 0) %>%
  convert_bp(id = GEOID) 

## create adjacency matrix
up.final <- bp.hosp.geoid %*% t(bp.hosp.geoid)

## graph structure
graph.dat <- graph_from_adjacency_matrix(up.final, weighted = TRUE) %>%
  simplify(., remove.loops = TRUE)

## Run cluster_walktrap on this network
initial.communities <- walktrap.community(graph.dat,
                        steps = 10,
                        merges = TRUE,
                        modularity = TRUE,
                        membership = TRUE) 

market <- membership(initial.communities)
walktrap.dat <- bind_cols(GEOID = names(market), mkt = as.double(market)) %>%
    mutate(GEOID=as.double(GEOID))

## save and plot resulting markets
merged.dat <- tract.dat %>% left_join(walktrap.dat, by="GEOID") %>% filter(!is.na(mkt))
cluster.boundaries <- merged.dat %>%
    group_by(mkt) %>%
    left_join(market.dat %>% mutate(GEOID=patient_GEOID) %>% 
                group_by(GEOID) %>% summarize(patients=sum(total_cases, na.rm=TRUE)),
              by="GEOID") %>%
    summarize(geometry = st_union(geometry),
              total_patients=sum(patients),
              total_tracts=n()) %>%
    ungroup()

market.map <- ggplot(data = merged.dat) + 
  geom_sf(color="grey", size=0.5) + 
  geom_sf(data = cluster.boundaries, fill = NA, color = "black", size = 1.5) +  # Bold black lines for cluster boundaries
  theme_minimal()

write_rds(walktrap.dat,"data/output/hospital_markets.rds")
ggsave("results/figures/market_map.png", market.map, width = 6, height = 10, dpi = 300)
