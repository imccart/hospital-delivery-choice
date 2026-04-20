# HSR Figure 1 (Market Map) ----------------------------------------------
# Standalone script: loads saved market assignments and shapefiles, produces
# a labeled market map as a PDF at HSR spec. Replaces the manual-overlay
# Word-doc approach used for earlier drafts.
#
# Does not re-run community detection. Requires:
#   data/input/shapefiles/tl_2020_13_tract10.shp  (census tracts)
#   data/output/hospital_markets.rds              (GEOID -> mkt mapping)
#   results/tables/market_detail.csv              (mkt -> city, n_facilities, n_deliveries)

library(tidyverse)
library(sf)
library(ggrepel)

dir.create("results/figures-hsr", showWarnings = FALSE, recursive = TRUE)

# Load inputs
tract.dat <- read_sf("data/input/shapefiles/tl_2020_13_tract10.shp") %>%
  mutate(GEOID = as.double(GEOID10))
walktrap.dat <- read_rds("data/output/hospital_markets.rds")
market.detail <- read.csv("results/tables/market_detail.csv", stringsAsFactors = FALSE)

# Merge tracts to market assignments; dissolve to market polygons
merged.dat <- tract.dat %>%
  left_join(walktrap.dat, by = "GEOID") %>%
  filter(!is.na(mkt))

cluster.boundaries <- merged.dat %>%
  group_by(mkt) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

# Label text at market centroids
label.dat <- cluster.boundaries %>%
  mutate(centroid = st_centroid(geometry)) %>%
  st_drop_geometry() %>%
  left_join(market.detail %>% select(mkt, city, n_facilities, n_deliveries), by = "mkt") %>%
  mutate(label = sprintf("%s\n(%d hosp., %s del.)",
                         city, n_facilities,
                         formatC(n_deliveries, big.mark = ",", format = "d")))

# Extract centroid coords for ggrepel
label.coords <- st_coordinates(label.dat$centroid) %>%
  as.data.frame() %>%
  bind_cols(label.dat %>% select(label))

# Build map
market.map <- ggplot() +
  geom_sf(data = merged.dat, fill = "gray95", color = "gray80", linewidth = 0.1) +
  geom_sf(data = cluster.boundaries, fill = NA, color = "black", linewidth = 0.6) +
  geom_label_repel(
    data = label.coords,
    aes(x = X, y = Y, label = label),
    size = 2.8, family = "sans",
    box.padding = 0.4, label.padding = 0.2,
    min.segment.length = 0, segment.color = "gray50"
  ) +
  coord_sf(datum = NA) +
  theme_void(base_family = "sans") +
  theme(plot.margin = margin(5, 5, 5, 5))

ggsave("results/figures-hsr/figure_1_market_map.pdf",
       market.map, device = cairo_pdf, width = 7, height = 9)

cat("HSR Figure 1 written to results/figures-hsr/figure_1_market_map.pdf\n")
