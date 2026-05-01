# HSR Main Figures 1-7 ---------------------------------------------------
# Standalone script: produces all seven main-text figures for the HSR paper
# as PDFs (sans-serif/Arial, percentage-point y-axis where applicable, zero
# reference line). Figure 1 is the labeled market map; Figures 2-7 are the
# marginal-effects panels.
#
# Required inputs:
#   results/tables/atl-only/pfx_*.csv         (from 3_results_summary.R, Atlanta)
#   results/tables/excluding-atl/pfx_*.csv    (from 3_results_summary.R, outside ATL)
#   data/input/shapefiles/tl_2020_13_tract10.shp
#   data/output/delivery_data.rds              (tract→market mapping derived inline)
#   results/tables/market_detail.csv

library(tidyverse)
library(sf)
library(ggrepel)

dir.create("results/figures-hsr", showWarnings = FALSE, recursive = TRUE)


# ========================================================================
# Figure 1: Market map
# ========================================================================

tract.dat <- read_sf("data/input/shapefiles/tl_2020_13_tract10.shp") %>%
  mutate(GEOID = as.double(GEOID10))
market.detail <- read.csv("results/tables/market_detail.csv", stringsAsFactors = FALSE)

# Derive tract → market mapping from delivery.dat (authoritative; matches
# market.detail). Each tract gets its most-common market from patient flows.
delivery.dat <- read_rds("data/output/delivery_data.rds")
walktrap.dat <- delivery.dat %>%
  group_by(censustract_d, mkt) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(censustract_d) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(GEOID = as.double(censustract_d), mkt = as.double(mkt))
rm(delivery.dat); gc(verbose = FALSE)

# Reassign every tract not in a named market (orphans + NAs) to its nearest
# named-market polygon so the named-market boundaries close cleanly with no
# interior holes or stray outlines.
named_mkts <- market.detail$mkt
mapped.tracts <- tract.dat %>% left_join(walktrap.dat, by = "GEOID")

named_only <- mapped.tracts %>% filter(mkt %in% named_mkts)
named_unions <- named_only %>%
  group_by(mkt) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

orphans <- mapped.tracts %>% filter(is.na(mkt) | !(mkt %in% named_mkts))
if (nrow(orphans) > 0) {
  nearest_idx <- st_nearest_feature(orphans, named_unions)
  orphans$mkt <- named_unions$mkt[nearest_idx]
  mapped.tracts <- bind_rows(named_only, orphans)
}

cluster.boundaries <- mapped.tracts %>%
  group_by(mkt) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

# Keep only the largest contiguous polygon per market — drops tiny fragments
# (disjoint walktrap pieces) that would otherwise draw stray outlines.
largest_polygon <- function(g) {
  parts <- st_cast(st_geometry(g), "POLYGON", warn = FALSE)
  parts[which.max(st_area(parts))]
}
cluster.boundaries <- cluster.boundaries %>%
  rowwise() %>%
  mutate(geometry = st_sfc(largest_polygon(geometry), crs = st_crs(geometry))) %>%
  ungroup() %>%
  st_as_sf()

label.points <- cluster.boundaries %>%
  mutate(label_pt = st_point_on_surface(geometry)) %>%
  st_drop_geometry() %>%
  inner_join(market.detail %>% select(mkt, city, n_facilities, n_deliveries), by = "mkt") %>%
  mutate(city = if_else(city == "Sandy Springs", "Atlanta", city),
         label = sprintf("%s\n(%d hosp., %s del.)",
                         city, n_facilities,
                         formatC(n_deliveries, big.mark = ",", format = "d")))

label.coords <- st_coordinates(label.points$label_pt) %>%
  as.data.frame() %>%
  bind_cols(label.points %>% select(label))

market.map <- ggplot() +
  geom_sf(data = mapped.tracts, fill = "gray95", color = "gray80", linewidth = 0.1) +
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
       market.map, width = 7, height = 9)


# ========================================================================
# Figures 2-7: Marginal effects
# ========================================================================


# Load aggregated partial effects ----------------------------------------

load_pfx <- function(mkt_path) {
  list(
    cont     = read.csv(paste0("results/tables/", mkt_path, "/pfx_cont.csv"),     stringsAsFactors = FALSE),
    cont_mkt = read.csv(paste0("results/tables/", mkt_path, "/pfx_cont_mkt.csv"), stringsAsFactors = FALSE),
    qntl     = read.csv(paste0("results/tables/", mkt_path, "/pfx_qntl.csv"),     stringsAsFactors = FALSE),
    qntl_mkt = read.csv(paste0("results/tables/", mkt_path, "/pfx_qntl_mkt.csv"), stringsAsFactors = FALSE)
  )
}

atl <- load_pfx("atl-only")
oth <- load_pfx("excluding-atl")


# Helpers ----------------------------------------------------------------

# Scale marginal effects (probability) to percentage points
pp <- function(df) {
  df %>% mutate(across(any_of(c("mean", "l_95", "u_95")), ~ .x * 100))
}

# Relabel age equal-width bins: "(28.6,37.4]" -> "29-37"
relabel_age <- function(bin_values) {
  unique_bins <- unique(bin_values)
  mapping <- sapply(unique_bins, function(b) {
    nums <- as.numeric(regmatches(b, gregexpr("[0-9]+\\.?[0-9]*", b))[[1]])
    paste0(round(nums[1]), "-", round(nums[2]))
  })
  # Preserve original ordering by taking first-appearance order of original factor
  original_order <- unique_bins[order(sapply(unique_bins, function(b)
    as.numeric(regmatches(b, gregexpr("[0-9]+\\.?[0-9]*", b))[[1]])[1]))]
  factor(mapping[bin_values], levels = mapping[original_order])
}

# Relabel quantile bins: "10" -> "10th", etc.
relabel_qntl <- function(bin_values) {
  ordinal <- c("10" = "10th", "25" = "25th", "50" = "50th",
               "75" = "75th", "90" = "90th", "100" = "100th")
  present <- intersect(names(ordinal), as.character(unique(bin_values)))
  factor(ordinal[as.character(bin_values)], levels = ordinal[present])
}

# HSR figure theme: sans-serif (maps to Arial/Helvetica), clean
hsr_theme <- function() {
  theme_bw(base_family = "sans", base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.margin = margin(t = -5)
    )
}

# Compute shared y-limits across supplied data frames (in pp units)
shared_ylim <- function(..., pad = 0.08) {
  dfs <- list(...)
  vals <- unlist(lapply(dfs, function(d) {
    if (nrow(d) == 0) return(NULL)
    c(d$mean, d$l_95, d$u_95) * 100
  }))
  rng <- range(vals, na.rm = TRUE)
  rng + c(-1, 1) * pad * diff(rng)
}

# Subset helper
slice_pfx <- function(df, outcome_val, patient_char_val) {
  df %>% filter(outcome == outcome_val, patient_char == patient_char_val)
}


# Plot functions ---------------------------------------------------------

plot_outside <- function(eff, eff_mkt, x_label, x_relabel, y_limits = NULL) {
  eff     <- eff     %>% mutate(bin = x_relabel(bin)) %>% pp()
  eff_mkt <- eff_mkt %>% mutate(bin = x_relabel(bin)) %>% pp()

  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(data = eff_mkt,
              aes(x = bin, y = mean, group = mkt, color = "Individual market"),
              linewidth = 0.5, alpha = 0.7) +
    geom_errorbar(data = eff,
                  aes(x = bin, ymin = l_95, ymax = u_95),
                  width = 0.15, color = "black") +
    geom_line(data = eff,
              aes(x = bin, y = mean, group = 1, color = "Weighted average"),
              linewidth = 1) +
    geom_point(data = eff,
               aes(x = bin, y = mean, color = "Weighted average"),
               size = 2.5) +
    scale_color_manual(values = c("Individual market" = "gray70",
                                  "Weighted average"  = "black")) +
    labs(x = x_label, y = "Change in Predicted Probability (percentage points)") +
    hsr_theme()

  if (!is.null(y_limits)) p <- p + coord_cartesian(ylim = y_limits)
  p
}

plot_atlanta <- function(eff, x_label, x_relabel, y_limits = NULL) {
  eff <- eff %>% mutate(bin = x_relabel(bin)) %>% pp()

  p <- ggplot(eff, aes(x = bin, y = mean, group = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = l_95, ymax = u_95), width = 0.15, color = "black") +
    geom_line(linewidth = 1, color = "black") +
    geom_point(size = 2.5, color = "black") +
    labs(x = x_label, y = "Change in Predicted Probability (percentage points)") +
    hsr_theme()

  if (!is.null(y_limits)) p <- p + coord_cartesian(ylim = y_limits)
  p
}

save_hsr <- function(plot, name, w = 6, h = 4.5) {
  ggsave(paste0("results/figures-hsr/", name, ".pdf"), plot,
         width = w, height = h)
}


# Subset inputs for each figure ------------------------------------------

# Figures 2 & 4: distance x age (paired y-axis across markets)
oth_dist_age     <- slice_pfx(oth$cont,     "ch_dist", "age")
oth_dist_age_mkt <- slice_pfx(oth$cont_mkt, "ch_dist", "age")
atl_dist_age     <- slice_pfx(atl$cont,     "ch_dist", "age")
ylim_dist_age    <- shared_ylim(oth_dist_age, oth_dist_age_mkt, atl_dist_age)

# Figures 3 & 6: c-section x OCI (paired y-axis)
oth_cs_oci     <- slice_pfx(oth$qntl,     "ch_csection", "ci_scorent")
oth_cs_oci_mkt <- slice_pfx(oth$qntl_mkt, "ch_csection", "ci_scorent")
atl_cs_oci     <- slice_pfx(atl$qntl,     "ch_csection", "ci_scorent")

# Figure-only fill for visual continuity at bin 25 (q10 == q25 in every market
# for OCI; saved pfx_qntl.csv table values are unchanged).
fill_oci_bin25 <- function(df) {
  if (!25 %in% df$bin && 10 %in% df$bin) {
    df <- bind_rows(df, df %>% filter(bin == 10) %>% mutate(bin = 25))
  }
  df
}
oth_cs_oci <- fill_oci_bin25(oth_cs_oci)
atl_cs_oci <- fill_oci_bin25(atl_cs_oci)

ylim_cs_oci    <- shared_ylim(oth_cs_oci, oth_cs_oci_mkt, atl_cs_oci)

# Figure 5: c-section x age (Atlanta). Pair y-axis with outside supplemental counterpart.
atl_cs_age   <- slice_pfx(atl$cont,     "ch_csection", "age")
oth_cs_age   <- slice_pfx(oth$cont,     "ch_csection", "age")
ylim_cs_age  <- shared_ylim(atl_cs_age, oth_cs_age)

# Figure 7: c-section x obgyn (Atlanta). Pair y-axis with outside supplemental counterpart.
atl_cs_ob    <- slice_pfx(atl$qntl,     "ch_csection", "obgyn_10kwra")
oth_cs_ob    <- slice_pfx(oth$qntl,     "ch_csection", "obgyn_10kwra")
ylim_cs_ob   <- shared_ylim(atl_cs_ob, oth_cs_ob)


# Build figures ----------------------------------------------------------

fig2 <- plot_outside(
  oth_dist_age, oth_dist_age_mkt,
  x_label   = "Maternal Age (years)",
  x_relabel = relabel_age,
  y_limits  = ylim_dist_age
)

fig3 <- plot_outside(
  oth_cs_oci, oth_cs_oci_mkt,
  x_label   = "Obstetric Comorbidity Index (percentile)",
  x_relabel = relabel_qntl,
  y_limits  = ylim_cs_oci
)

fig4 <- plot_atlanta(
  atl_dist_age,
  x_label   = "Maternal Age (years)",
  x_relabel = relabel_age,
  y_limits  = ylim_dist_age
)

fig5 <- plot_atlanta(
  atl_cs_age,
  x_label   = "Maternal Age (years)",
  x_relabel = relabel_age,
  y_limits  = ylim_cs_age
)

fig6 <- plot_atlanta(
  atl_cs_oci,
  x_label   = "Obstetric Comorbidity Index (percentile)",
  x_relabel = relabel_qntl,
  y_limits  = ylim_cs_oci
)

fig7 <- plot_atlanta(
  atl_cs_ob,
  x_label   = "OB/GYN Supply (percentile of per-10,000 WRA distribution)",
  x_relabel = relabel_qntl,
  y_limits  = ylim_cs_ob
)


# Save -------------------------------------------------------------------

save_hsr(fig2, "figure_2_outside-atl_age_distance")
save_hsr(fig3, "figure_3_outside-atl_oci_csection")
save_hsr(fig4, "figure_4_atlanta_age_distance")
save_hsr(fig5, "figure_5_atlanta_age_csection")
save_hsr(fig6, "figure_6_atlanta_oci_csection")
save_hsr(fig7, "figure_7_atlanta_obgyn_csection")

cat("HSR Figures 1-7 written to results/figures-hsr/\n")
