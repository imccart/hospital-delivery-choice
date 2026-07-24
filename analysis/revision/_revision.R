# Meta --------------------------------------------------------------------
## Title:        Medical Care R&R revision-analysis driver (MDC-D-26-00255)
## Author:       Ian McCarthy
## Description:  Runs the R&R sensitivity analyses once, across all markets, on
##               the frozen 0.05 market definition. Separate from _analysis.r,
##               which runs the main estimation per configuration (atl-only /
##               excluding-atl). Each step is self-contained and writes to
##               results/tables/revision/. Run from the project root:
##                   Rscript analysis/revision/_revision.R
##               To run a subset, comment out the source() lines below.

## Packages and shared helpers for the estimation steps (3-7).
data.table::setDTthreads(1)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, haven, geosphere, mclogit)
source("analysis/functions.r")

# Build steps (run once; outputs persist, so normally left commented) ------
## 1. Driving-distance lookup over the Georgia road network (~1.5 hr).
##    Loads sf/osmextract/dodgr itself. Writes data/output/drive_distance.csv.
# source("analysis/revision/1_drive_distance_build.R")
## 2. Facility-to-AHA system crosswalk. Writes data/output/facility_system_xwalk.csv.
# source("analysis/revision/2_aha_system_crosswalk.R")

# Estimation steps (produce the response tables) --------------------------
source("analysis/revision/3_distance_threshold.R")   # R2.1 driving distance, R2.2 threshold
source("analysis/revision/4_system.R")               # R2.3 system membership
source("analysis/revision/5_first_birth.R")           # R2.8 first-time vs experienced
source("analysis/revision/6_transfer.R")             # R1.2 maternal transfer
source("analysis/revision/7_flow_missingness.R")     # R3.1/R3.2 flow and missingness
