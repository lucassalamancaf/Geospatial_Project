#!/usr/bin/env bash
set -euo pipefail

TS=$(date +%Y%m%d_%H%M%S)
ARCHIVE_BASE="analysis/output/_archive/${TS}"
mkdir -p "${ARCHIVE_BASE}/analysis/maps_intermediate" "${ARCHIVE_BASE}/analysis_2/maps_intermediate"

# Archive intermediate analysis_1 maps if present.
if [ -d analysis/output/maps ]; then
  find analysis/output/maps -type f \( -name '*.png' -o -name '*.csv' -o -name '*.txt' -o -name '*.md' \) -print0 | while IFS= read -r -d '' f; do
    mv "$f" "${ARCHIVE_BASE}/analysis/maps_intermediate/"
  done
  rm -rf analysis/output/maps
fi

# Archive intermediate analysis_2 map helper files if present.
for f in \
  analysis/output/analysis_2/map_region_average_changes.csv \
  analysis/output/analysis_2/map_coverage_summary.csv \
  analysis/output/analysis_2/descriptive_data/map_region_average_changes.csv \
  analysis/output/analysis_2/descriptive_data/map_coverage_summary.csv; do
  if [ -f "$f" ]; then
    mv "$f" "${ARCHIVE_BASE}/analysis_2/maps_intermediate/"
  fi
done

echo "Archived intermediate map files to: ${ARCHIVE_BASE}"
