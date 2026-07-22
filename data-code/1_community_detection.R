# Form markets ------------------------------------------------------------

## Run this directly, not through _BuildData.r, which reads a saved market
## definition instead. Set minimum_share and walk_steps below and run the script.
## It writes data/output/market-defs/market_assignment_<threshold>_steps<n>.csv;
## edit the filename in _BuildData.r's "Form markets" section to build against it.
##
## Requires full.dat, market.dat and tract.dat, so run _BuildData.r down to the
## "Check census tract data" section first.

## consturct market (census tract) data using census tract shapefiles
market.dat %>% left_join(tract.dat, by=c("facility_GEOID"="GEOID")) %>%
    mutate(geo_match=ifelse(is.na(GEOID10),0,1)) %>%
    filter(geo_match==1) %>%
    select(GEOID=facility_GEOID, total_cases, geometry)

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
## The paper uses minimum_share=0.05 with walk_steps=10. The reviewer sensitivity
## runs the same code at 0.10, 0.15, 0.20 and 0.25, one at a time.
minimum_share=0.05
walk_steps=10
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
initial.communities <- cluster_walktrap(graph.dat,
                        steps = walk_steps,
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

## Stamp outputs with both settings so runs at different values sit side by side.
settings_tag <- sprintf("%03d_steps%d", round(minimum_share*100), walk_steps)

write_csv(walktrap.dat, paste0("data/output/market-defs/market_assignment_", settings_tag, ".csv"))
ggsave(paste0("results/figures/market_map_", settings_tag, ".png"),
       market.map, width = 6, height = 10, dpi = 300)
