# Meta --------------------------------------------------------------------
## Title:         Georgia Hospital Delivery Markets
## Author:        Ian McCarthy
## Date Created:  10/12/2020 (from prior repo)
## Date Edited:   1/4/2024


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, ggplot2, haven, sf, janitor, sp, igraph, plotly)

## call functions
source("data-code/functions.R")


# Import data -------------------------------------------------------------

## patient/delivery data
patient.dat <- read_dta("data/input/hospital-market-data.dta")

## census tract data
##tract.dat <- read_sf("data/input/shapefiles-2010/tl_2010_13_tract00.shp") %>%
##    mutate(GEOID = as.double(CTIDFP00))

tract.dat <- read_sf("data/input/shapefiles/tl_2020_13_tract10.shp") %>%
    mutate(GEOID = as.double(GEOID10))

## count missing census tracts in delivery data
patient.dat %>% filter(is.na(censustract_d)) %>% nrow()

## census tracts in the state without any patients (or with <10 patients)
tract.dat %>% select(GEOID) %>% st_set_geometry(NULL) %>%
    left_join(patient.dat %>% group_by(censustract_d) %>% summarize(n=n()), 
              by=c("GEOID"="censustract_d")) %>%
    filter(is.na(n) | n<10) %>% nrow()

# Hospital data -----------------------------------------------------------
hosp.dat <- patient.dat %>% 
    mutate(patient_GEOID=censustract_d,
           facility_GEOID=facility_censustract) %>%
    group_by(facility_d, facility_GEOID, patient_GEOID) %>% 
    summarize(total_cases=n())

# Check census tract data -------------------------------------------------

## unique censustract_d values from patient.dat
patient.tracts <- patient.dat %>% group_by(censustract_d) %>% summarise(n_patient=n()) %>%
    arrange(desc(n_patient)) %>% filter(!is.na(censustract_d)) %>%
    rename(census_tract=censustract_d)

## unique facility_censustract values from patient.dat
facility.tracts <- patient.dat %>% group_by(facility_censustract) %>% summarise(n_facility=n()) %>%
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

## consturct market (census tract) data using the 2000 census tract shapefiles
market.dat <- patient.dat %>% group_by(facility_censustract) %>% summarise(n_patient=n()) %>%
    left_join(tract.dat, by=c("facility_censustract"="GEOID")) %>%
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
bp.hosp.geoid <- hosp.dat %>% 
  group_by(patient_GEOID) %>% 
  mutate(patient_share=total_cases/sum(total_cases,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(connected = as.integer(patient_share >= minimum_share))  %>%
  mutate(share = ifelse(connected==1,patient_share,0)) %>% 
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

## plot resulting markets
merged.dat <- tract.dat %>% left_join(walktrap.dat, by="GEOID") %>% filter(!is.na(mkt))
cluster.boundaries <- merged.dat %>%
    group_by(mkt) %>%
    left_join(hosp.dat %>% mutate(GEOID=patient_GEOID) %>% 
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

ggsave("results/figures/market_map.png", market.map, width = 6, height = 10, dpi = 300)

## Interactive map --------------------------------------------------------

# Convert the cluster boundary polygons to line geometries
cluster_boundaries_lines <- st_cast(cluster.boundaries, "LINESTRING")

# Create an interactive plotly map
interactive_map <- plot_ly() %>%
  # Add the base map of census tracts (as polygons)
  add_sf(data = merged.dat, color = I("grey"), sizes = I(0.5)) %>%
  # Add cluster boundaries as lines explicitly setting the trace type
  add_trace(data = cluster_boundaries_lines, type = 'scatter', mode = 'lines',
            line = list(color = 'black', width = 1.5),
            hoverinfo = 'text',
            text = ~paste('Total Patients: ', total_patients,
                          '<br>Total Tracts: ', total_tracts))

# Customize layout
interactive_map <- interactive_map %>%
  layout(title = "Interactive Map of Census Tracts and Market Clusters",
         hovermode = "closest")

# Print the interactive map
interactive_map

# Save final market data --------------------------------------------------

write_rds(walktrap.dat,"data/output/hospital_markets.rds")

  