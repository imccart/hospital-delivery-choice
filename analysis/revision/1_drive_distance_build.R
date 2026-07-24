# Meta --------------------------------------------------------------------
## Title:        Driving-distance lookup (Medical Care R&R, Reviewer 2)
## Description:  For each unique residence geocode in the choice data, compute
##               driving distance (miles) and free-flow driving time (minutes)
##               to every hospital in that residence's market, over the Georgia
##               OpenStreetMap road network (dodgr). Only driving miles are used
##               in the sensitivity analysis; minutes are stored for reference.
## Usage:        Run once from project root (~1.5 hr). Resumable: one CSV per
##               market in data/output/drive-distance/; a market is skipped if
##               its CSV already exists. Combined lookup written to
##               data/output/drive_distance.csv at the end. Output persists, so
##               this build is normally left commented out in _revision.R.

library(tidyverse)
library(sf)
library(osmextract)
library(dodgr)

t0 <- Sys.time()

# Origins and hospitals ----------------------------------------------------

choice.dat <- read_rds("data/output/choice_data_mkt.rds")

## one representative coordinate per rounded geocode (~0.1m precision)
origins <- choice.dat %>%
  mutate(origin_key = sprintf("%.6f_%.6f", latitude_d, longitude_d)) %>%
  distinct(mkt, origin_key, .keep_all = TRUE) %>%
  select(mkt, origin_key, latitude_d, longitude_d)

hospitals <- choice.dat %>%
  group_by(mkt, facility) %>%
  summarize(latitude_f = first(latitude_f),
            longitude_f = first(longitude_f), .groups = "drop")

## straight-line distances, one row per origin-hospital pair (validation only)
straight <- choice.dat %>%
  mutate(origin_key = sprintf("%.6f_%.6f", latitude_d, longitude_d)) %>%
  distinct(mkt, origin_key, facility, .keep_all = TRUE) %>%
  select(mkt, origin_key, facility, distance_mi)

## smallest markets first; Atlanta (mkt 9, by far the largest) runs last
mkt.order <- origins %>%
  count(mkt, name = "n_origins") %>%
  left_join(hospitals %>% count(mkt, name = "n_hosp"), by = "mkt") %>%
  mutate(od_pairs = n_origins * n_hosp) %>%
  arrange(od_pairs) %>%
  pull(mkt)

# Road network --------------------------------------------------------------

ga.net <- oe_read("D:/research-data/geography/osm/georgia-latest.osm.pbf",
                  layer = "lines",
                  extra_tags = c("oneway", "maxspeed", "junction"),
                  quiet = TRUE) %>%
  filter(!is.na(highway))
cat("Georgia drivable lines:", nrow(ga.net), ";",
    round(difftime(Sys.time(), t0, units = "mins"), 1), "min elapsed\n")

# Route each market ----------------------------------------------------------

dir.create("data/output/drive-distance", showWarnings = FALSE)

for (m in mkt.order) {
  out.file <- paste0("data/output/drive-distance/mkt_", m, ".csv")
  if (file.exists(out.file)) {
    cat("market", m, "already done, skipping\n")
    next
  }

  o <- origins %>% filter(mkt == m)
  h <- hospitals %>% filter(mkt == m)
  cat("\nmarket", m, ":", nrow(o), "origins x", nrow(h), "hospitals\n")

  ## market road graph: bounding box plus ~25-mile buffer
  bb <- st_bbox(c(xmin = min(o$longitude_d, h$longitude_f) - 0.4,
                  ymin = min(o$latitude_d,  h$latitude_f)  - 0.4,
                  xmax = max(o$longitude_d, h$longitude_f) + 0.4,
                  ymax = max(o$latitude_d,  h$latitude_f)  + 0.4),
                crs = st_crs(ga.net))
  net.m <- st_crop(ga.net, bb)
  graph <- weight_streetnet(net.m, wt_profile = "motorcar")
  graph <- graph[graph$component == 1, ]
  cat("  graph edges:", nrow(graph), ";",
      round(difftime(Sys.time(), t0, units = "mins"), 1), "min elapsed\n")

  from <- as.matrix(o %>% select(lon = longitude_d, lat = latitude_d))
  to   <- as.matrix(h %>% select(lon = longitude_f, lat = latitude_f))

  t1 <- Sys.time()
  dist.mat <- dodgr_dists(graph, from = from, to = to)
  time.mat <- dodgr_times(graph, from = from, to = to)
  cat("  routing:", round(difftime(Sys.time(), t1, units = "mins"), 1), "min\n")

  drive <- tibble(
    mkt        = m,
    origin_key = rep(o$origin_key, times = nrow(h)),
    facility   = rep(h$facility, each = nrow(o)),
    drive_mi   = as.vector(dist.mat) / 1609.34,
    drive_min  = as.vector(time.mat) / 60
  )

  ## validation against straight-line
  check <- drive %>%
    inner_join(straight %>% filter(mkt == m) %>% select(-mkt),
               by = c("origin_key", "facility"))
  cat("  missing drive distance:", sum(is.na(check$drive_mi)),
      "of", nrow(check), "matched pairs\n")
  cat("  drive < straight-line:",
      sum(check$drive_mi < check$distance_mi - 0.1, na.rm = TRUE), "\n")
  cat("  median ratio drive/straight:",
      round(median(check$drive_mi / check$distance_mi, na.rm = TRUE), 3), "\n")
  cat("  correlation:",
      round(cor(check$drive_mi, check$distance_mi, use = "complete.obs"), 4), "\n")

  write_csv(drive, out.file)
  rm(net.m, graph, dist.mat, time.mat, drive, check)
  gc(verbose = FALSE)
}

# Combine --------------------------------------------------------------------

files <- list.files("data/output/drive-distance", pattern = "^mkt_.*\\.csv$",
                    full.names = TRUE)
drive.dist <- bind_rows(lapply(files, read_csv, show_col_types = FALSE))
cat("\ncombined lookup:", nrow(drive.dist), "rows;",
    sum(is.na(drive.dist$drive_mi)), "missing drive_mi\n")
write_csv(drive.dist, "data/output/drive_distance.csv")
cat("total time:", round(difftime(Sys.time(), t0, units = "hours"), 2), "hours\n")
