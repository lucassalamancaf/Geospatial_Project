# Analysis Layout

This folder is organized into:

- `analysis/scripts/analysis_1/`
- `analysis/scripts/analysis_2/`
- `analysis/output/analysis_1/`
- `analysis/output/analysis_2/`
- `analysis/output/_archive/`

Compatibility wrappers remain in:

- `analysis/correlation_fe_regression.R`
- `analysis/nuts3_employment_maps.R`
- `analysis/analysis_2.R`

These wrappers call scripts in `analysis/scripts/...`.

## Map policy (active outputs)

For analysis 1 maps, active outputs keep only:

- `averages/map_employment_average_2005_2023.png`
- `periods/map_employment_period_2005_2009.png`
- `periods/map_employment_period_2010_2014.png`
- `periods/map_employment_period_2015_2018.png`
- `periods/map_employment_period_2019_2023.png`

Intermediate maps (yearly/series/log variants) are moved into `analysis/output/_archive/<timestamp>/`.
