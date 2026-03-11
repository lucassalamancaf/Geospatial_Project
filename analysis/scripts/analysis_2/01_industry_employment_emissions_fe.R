# analysis_2 / step 01
# Y: change in employment by industry
# X: change in emissions in the matched industry
# FE: country + year (time) + wave
# Plus correlation analysis

rm(list = ls())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

out_dir <- "analysis/output/analysis_2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ardeco_candidates <- c(
  "ETS_ARDECO_merged/data_eurostat_zip/data_eurostat/out_dir/ardeco_panel_2005_2023_nuts3.csv",
  "ETS_ARDECO_merged/data_eurostat_zip/data_eurostat/processed_data/ardeco_panel_2005_2023_nuts3.csv",
  "ETS_ARDECO_merged/outdir_ardeco_ets_merge/ardeco_panel_premerge.csv"
)
ets_file <- "ETS_ARDECO_merged/outdir_ardeco_ets_merge/ets_panel_final_raw.csv"

ardeco_file <- ardeco_candidates[file.exists(ardeco_candidates)][1]
if (is.na(ardeco_file) || !file.exists(ardeco_file)) {
  stop("Could not find ARDECO NUTS3 panel file in expected locations.")
}
if (!file.exists(ets_file)) {
  stop("ETS file not found: ", ets_file)
}

# ----------------------------
# Helpers
# ----------------------------
safe_cor <- function(x, y, method = "pearson") {
  x <- as.numeric(x)
  y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 3L) return(NA_real_)
  if (sd(x) == 0 || sd(y) == 0) return(NA_real_)
  suppressWarnings(cor(x, y, method = method))
}

sum_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

to_wave <- function(year_vec) {
  fifelse(year_vec <= 2007, 1L,
    fifelse(year_vec <= 2012, 2L,
      fifelse(year_vec <= 2020, 3L, 4L)
    )
  )
}

# ----------------------------
# 1) Load and reshape ARDECO employment-by-industry
# ----------------------------
ardeco <- fread(ardeco_file, na.strings = c("", "NA", "-"))
setnames(ardeco, names(ardeco), make.names(names(ardeco)))

if (!("NUTS_ID" %in% names(ardeco)) && ("region_id" %in% names(ardeco))) {
  setnames(ardeco, "region_id", "NUTS_ID")
}

required_ardeco <- c("NUTS_ID", "year")
missing_ardeco <- setdiff(required_ardeco, names(ardeco))
if (length(missing_ardeco) > 0) {
  stop("Missing ARDECO columns: ", paste(missing_ardeco, collapse = ", "))
}

employment_cols <- grep("^employmentbyindustry_sec_", names(ardeco), value = TRUE)
if (length(employment_cols) == 0) {
  stop("No employment-by-industry columns found in ARDECO file.")
}

ardeco_long <- melt(
  ardeco,
  id.vars = c("NUTS_ID", "year"),
  measure.vars = employment_cols,
  variable.name = "employment_var",
  value.name = "employment_level"
)
ardeco_long[, year := as.integer(year)]
ardeco_long[, sector := sub("^employmentbyindustry_sec_", "", employment_var)]
ardeco_long[, employment_level := suppressWarnings(as.numeric(employment_level))]
setorder(ardeco_long, NUTS_ID, sector, year)
ardeco_long[, d_employment := employment_level - shift(employment_level, n = 1L, type = "lag"),
  by = .(NUTS_ID, sector)
]

# ----------------------------
# 2) Build industry-mapped ETS emissions changes
# ----------------------------
ets <- fread(ets_file, na.strings = c("", "NA", "-"))
setnames(ets, names(ets), make.names(names(ets)))

required_ets <- c("NUTS_ID", "year", "verified", "nace_section")
missing_ets <- setdiff(required_ets, names(ets))
if (length(missing_ets) > 0) {
  stop("Missing ETS columns: ", paste(missing_ets, collapse = ", "))
}

ets <- ets[!is.na(NUTS_ID) & NUTS_ID != ""]
ets[, year := as.integer(year)]
ets[, verified := suppressWarnings(as.numeric(verified))]
ets[, nace_section := as.character(nace_section)]

# Map ETS sections to ARDECO sector labels.
# With current data, ETS emissions are available for B/C/D/E and map to ARDECO B.E.
ets[, sector := fifelse(nace_section %chin% c("B", "C", "D", "E"), "B.E", NA_character_)]
ets <- ets[!is.na(sector)]

ets_agg <- ets[, .(emissions_level = sum_na(verified)), by = .(NUTS_ID, year, sector)]
setorder(ets_agg, NUTS_ID, sector, year)
ets_agg[, d_emissions := emissions_level - shift(emissions_level, n = 1L, type = "lag"),
  by = .(NUTS_ID, sector)
]

# ----------------------------
# 3) Sector matching diagnostics
# ----------------------------
ardeco_sectors <- sort(unique(ardeco_long$sector))
ets_sectors <- sort(unique(ets_agg$sector))
sector_match <- merge(
  data.table(sector = ardeco_sectors, in_ardeco = TRUE),
  data.table(sector = ets_sectors, in_ets = TRUE),
  by = "sector",
  all = TRUE
)
sector_match[is.na(in_ardeco), in_ardeco := FALSE]
sector_match[is.na(in_ets), in_ets := FALSE]
sector_match[, matched := in_ardeco & in_ets]

fwrite(sector_match, file.path(out_dir, "industry_sector_match.csv"))

# ----------------------------
# 4) Merge panel and compute analysis sample
# ----------------------------
panel <- merge(
  ardeco_long[, .(NUTS_ID, year, sector, employment_level, d_employment)],
  ets_agg[, .(NUTS_ID, year, sector, emissions_level, d_emissions)],
  by = c("NUTS_ID", "year", "sector"),
  all = FALSE
)

panel[, country := substr(NUTS_ID, 1, 2)]
panel[, wave := to_wave(year)]

wd <- panel[is.finite(d_employment) & is.finite(d_emissions) & nzchar(country)]
if (nrow(wd) == 0) {
  stop("No usable observations after matching sectors and differencing.")
}

fwrite(
  wd[order(NUTS_ID, sector, year)],
  file.path(out_dir, "industry_model_analysis_panel.csv")
)

fwrite(
  data.table(
    metric = c("ardeco_file", "ets_file", "n_obs", "n_regions", "n_countries", "n_sectors", "year_min", "year_max"),
    value = c(
      ardeco_file,
      ets_file,
      as.character(nrow(wd)),
      as.character(uniqueN(wd$NUTS_ID)),
      as.character(uniqueN(wd$country)),
      as.character(uniqueN(wd$sector)),
      as.character(min(wd$year, na.rm = TRUE)),
      as.character(max(wd$year, na.rm = TRUE))
    )
  ),
  file.path(out_dir, "industry_model_sample_info.csv")
)

fwrite(
  wd[, .N, by = sector][order(-N)],
  file.path(out_dir, "industry_model_sector_counts.csv")
)

# ----------------------------
# 5) Correlation analysis
# ----------------------------
corr_main <- data.table(
  statistic = c("pearson_raw", "spearman_raw"),
  correlation = c(
    safe_cor(wd$d_employment, wd$d_emissions, method = "pearson"),
    safe_cor(wd$d_employment, wd$d_emissions, method = "spearman")
  )
)

resid_y <- resid(feols(d_employment ~ 1 | country + wave + year, data = wd))
resid_x <- resid(feols(d_emissions ~ 1 | country + wave + year, data = wd))

corr_main <- rbind(
  corr_main,
  data.table(
    statistic = "pearson_partial_country_wave_year",
    correlation = safe_cor(resid_y, resid_x, method = "pearson")
  ),
  fill = TRUE
)
fwrite(corr_main, file.path(out_dir, "industry_model_correlations.csv"))

corr_by_sector <- wd[, .(
  n_obs = .N,
  pearson_raw = safe_cor(d_employment, d_emissions, method = "pearson"),
  spearman_raw = safe_cor(d_employment, d_emissions, method = "spearman")
), by = sector][order(-n_obs)]
fwrite(corr_by_sector, file.path(out_dir, "industry_model_correlations_by_sector.csv"))

# ----------------------------
# 6) FE regression
# ----------------------------
m_fe <- feols(
  d_employment ~ d_emissions | country + wave + year,
  data = wd,
  cluster = ~ NUTS_ID + year
)

etable(
  m_fe,
  file = file.path(out_dir, "industry_model_regression_table.md"),
  replace = TRUE,
  tex = FALSE
)

sink(file.path(out_dir, "industry_model_regression_summary.txt"))
cat("=== CORRELATIONS ===\n")
print(corr_main)
cat("\n=== CORRELATIONS BY SECTOR ===\n")
print(corr_by_sector)
cat("\n=== FE REGRESSION ===\n")
print(summary(m_fe))
cat("\nNOTE: wave is a deterministic mapping of year, so fixest may drop one FE due to collinearity.\n")
sink()

cat("Industry-model analysis complete. Outputs written to: ", out_dir, "\n", sep = "")
