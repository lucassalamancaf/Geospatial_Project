# analysis_2

Scripts in this folder:

1. `00_dataset_inventory.R`
- Scans CSV datasets under `ETS_ARDECO_merged` and exports variable inventories to `analysis/output/analysis_2/`.

2. `01_industry_employment_emissions_fe.R`
- Builds the requested model:
  - `Y`: change in employment by industry (`employmentbyindustry_sec_*`).
  - `X`: change in emissions in the matched industry.
  - Fixed effects: `country + year + wave`.
  - Includes correlation analysis before FE regression.

3. `02_serial_correlation_diagnostics.R`
- Runs serial-correlation diagnostics on the same model sample:
  - lag correlations for `d_employment`, `d_emissions`, and FE residuals,
  - AR(1)-style panel regressions with `panel_id` fixed effects,
  - exports summary tables and model outputs.

4. `03_maps_and_trends.R`
- Builds report-ready descriptives from the matched analysis panel:
  - NUTS3 maps of average changes (`d_emissions`, `d_employment`),
  - EU trend lines over time (mean, median, and 3-year moving average).
- Figure outputs are written to `analysis/output/analysis_2/figures/`.
- Descriptive supporting tables are written to `analysis/output/analysis_2/descriptive_data/`.

Current data note:
- ARDECO has multiple employment sectors.
- ETS emissions in this repository map to NACE sections `B/C/D/E`, which are mapped to ARDECO sector `B.E`.
- The script writes an explicit sector match table (`industry_sector_match.csv`) so you can verify which sectors are estimable.
