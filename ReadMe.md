# Hospital Choice in Childbirth

This repository contains the data-build and analysis pipeline for a study of delivery hospital choice in Georgia, 2016--2020. Target journal: Health Services Research.

## Repository layout

```
data-code/                  # Data build pipeline (raw -> analysis-ready)
  _BuildData.r              # Master build script
  1_community_detection.R   # Walktrap market definition
  3_choice_data.R           # Choice set construction (market-based)
  functions.r               # Helpers (get_contig, convert_bp)

analysis/                   # Estimation, bootstrap, results, figures
  _analysis.r               # Driver: data load, params, bin spec, sources sub-scripts
  1_descriptive_stats.r     # Tables 1-2 + market_detail.csv
  2_estimation.r            # Per-market mclogit fits + (legacy) bootstrap
  3_results_summary.r       # Coefficients, partial effects, summary tables
  4_hsr_figures.r           # HSR Figures 1-7 (PDF, sans-serif)
  functions.r               # estimate_choice_model, summarize_boot_rep, bin helpers
  run_boot_chunked.R        # Resumable per-rep bootstrap runner
  run_boot_loop.sh          # Bash wrapper that relaunches run_boot_chunked.R until done
  run_boot_summary.R        # Post-bootstrap: rebuild final.boot from saved CSVs and run 3_results_summary.r

data/
  input/                    # Raw data (Stata + SAS, gitignored)
  output/                   # Intermediate datasets (gitignored)

results/
  figures/                  # Per-market PNGs from 3_results_summary.r
  figures-hsr/              # HSR-spec PDFs from 4_hsr_figures.r
  tables/                   # CSV and docx outputs
```

## Pipeline order

1. `data-code/_BuildData.r` -- builds `delivery_data.rds` (sources `1_community_detection.R` to define markets via walktrap, joins covariates).
2. `data-code/3_choice_data.R` -- builds `choice_data_mkt.rds` (per-patient choice sets within market).
3. `analysis/_analysis.r` -- runs descriptives and per-market mclogit estimation.
4. `analysis/run_boot_loop.sh` -- runs the resumable bootstrap. Edit `mkt.path` in `run_boot_chunked.R` to switch between Atlanta-only and outside-Atlanta. Run separately per configuration.
5. `analysis/run_boot_summary.R` -- assembles bootstrap reps from disk into `final.boot`, sources `3_results_summary.r`, writes per-config `partial_effects.csv` and `pfx_means_<mkt.path>.docx`.
6. `analysis/4_hsr_figures.r` -- writes Figure 1 (market map) and Figures 2--7 (marginal effects panels) as PDFs.

## Walktrap markets and regeneration

Markets are defined empirically by walktrap community detection on patient-flow networks (`data-code/1_community_detection.R`). Each tract's `mkt` value is a numeric cluster label assigned by the algorithm; the labels are arbitrary and **non-deterministic across runs** (re-running walktrap can produce a different numbering). The mapping from `mkt` to a city name (e.g., `mkt == 9` -> "Sandy Springs"/Atlanta) is recovered after the fact in `1_descriptive_stats.r` and stored in `results/tables/market_detail.csv`.

The choice model is estimated per market. Several scripts hardcode specific `mkt` values matching the current `delivery_data.rds`:

- `analysis/_analysis.r`: `markets <- c(9)` (Atlanta-only) and `markets <- c(2,3,4,5,6,7,8,10,11)` (outside-Atlanta).
- `analysis/run_boot_chunked.R`: same vectors.
- `analysis/run_boot_summary.R`: same vectors.
- `analysis/3_results_summary.r`: `target_bin_label` (the median age bin string) depends on the markets included.

If `1_community_detection.R` is re-run, the new walktrap may produce different cluster IDs. Procedure to recover:

1. Rebuild data: run `_BuildData.r` (which now sources `1_community_detection.R`), then `3_choice_data.R`.
2. Open the regenerated `results/tables/market_detail.csv`. The row with `city == "Sandy Springs"` is the new Atlanta `mkt`; the row with `n_facilities == 1` is LaGrange (excluded from the choice model). Update the hardcoded `markets` vectors in the four analysis scripts above.
3. Rerun the analysis: bootstraps, `run_boot_summary.R` per configuration, `4_hsr_figures.r`.

`4_hsr_figures.r` derives its tract-to-market mapping from `delivery_data.rds` inline, so the map will pick up the new IDs automatically.

## Estimation details

- Conditional logit (mclogit) estimated separately per market.
- Hospital attributes interacted with patient characteristics. Marginal effects computed via numeric difference in predicted choice probabilities at increment values defined in `pfx.inc` (1 mile for distance; 0/1 switch for binary attributes; 1 percentage point for the elective C-section rate).
- 95% CIs from 150 nonparametric bootstrap replications resampling patients with replacement, run separately within each market.
- LaGrange (single-hospital market) is included in descriptive Tables 1--2 but excluded from the choice model.

## Outputs used in the manuscript

- Tables 1--2: `results/tables/sum_stats.docx`.
- Table 3 (overall marginal effects): `results/tables/<mkt.path>/partial_effects.csv` for each configuration.
- Tables 4--5 (marginal effects by patient characteristic): `results/tables/pfx_means_atl-only.docx` and `results/tables/pfx_means_excluding-atl.docx`.
- Figures 1--7: `results/figures-hsr/figure_*.pdf`.
- Supplemental Figures S1--S48 (HSR Supporting Information): rendered from `drafts/<round>/appendix.qmd`.
