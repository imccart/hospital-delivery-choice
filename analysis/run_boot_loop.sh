#!/usr/bin/env bash
# Auto-relaunching bootstrap loop. Run from project root:
#   bash analysis/run_boot_loop.sh
#
# Edit MKT_PATH below to switch between atl-only and excluding-atl runs.
# Must match the mkt.path setting in analysis/run_boot_chunked.R.

set -u

MKT_PATH="atl-only"        # "atl-only" or "excluding-atl"
N_TARGET=150
BOOT_DIR="results/tables/${MKT_PATH}/boot_reps"
DONE_FILE="${BOOT_DIR}/rep_$(printf '%04d' "$N_TARGET").csv"
MAX_CONSEC_FAILS=10

fails=0
while [ ! -f "$DONE_FILE" ]; do
  before=$(ls "$BOOT_DIR"/rep_*.csv 2>/dev/null | wc -l)
  Rscript analysis/run_boot_chunked.R
  after=$(ls "$BOOT_DIR"/rep_*.csv 2>/dev/null | wc -l)

  if [ "$after" -gt "$before" ]; then
    fails=0
  else
    fails=$((fails+1))
    echo ">>> no progress this run (fail $fails / $MAX_CONSEC_FAILS)"
    if [ "$fails" -ge "$MAX_CONSEC_FAILS" ]; then
      echo ">>> stopping after $MAX_CONSEC_FAILS consecutive no-progress runs"
      exit 1
    fi
  fi
done

echo ">>> all $N_TARGET reps complete"
