# Correlation + Fixed-Effects Regression for ETS-ARDECO merged panel
#
# Goal:
# Y = employmentbyindustry_sec_B.E
# X = verified_delta (change in emissions)
# Controls + fixed effects by time, country, and wave (with collinearity note)

rm(list = ls())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

# ----------------------------
# 1) Paths and load data
# ----------------------------
in_file <- "ETS_ARDECO_merged/outdir_ardeco_ets_merge/ardeco_ets_merged.csv"
out_dir <- "analysis/output/analysis_1"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(in_file)) {
  stop("Input file not found: ", in_file)
}

df <- fread(in_file, na.strings = c("", "NA", "-"))
setnames(df, names(df), make.names(names(df)))

# ----------------------------
# 2) Build analysis variables
# ----------------------------
required <- c("NUTS_ID", "year", "employmentbyindustry_sec_B.E", "verified_delta")
missing_required <- setdiff(required, names(df))
if (length(missing_required) > 0) {
  stop("Missing required columns: ", paste(missing_required, collapse = ", "))
}

# Country from NUTS_ID prefix
df[, country := substr(NUTS_ID, 1, 2)]

# ETS waves derived from year
# Wave 1: 2005-2007, Wave 2: 2008-2012, Wave 3: 2013-2020, Wave 4: 2021+
df[, wave := fifelse(year <= 2007, 1L,
              fifelse(year <= 2012, 2L,
              fifelse(year <= 2020, 3L, 4L)))]

# Y and X
df[, y_emp := as.numeric(employmentbyindustry_sec_B.E)]
df[, x_demis := as.numeric(verified_delta)]
df[, ln_y_emp := log1p(y_emp)]

# Candidate controls (use those available)
candidate_controls <- c(
  "gdppercapitaconstantprices",
  "capitalstock",
  "populationchangeper1000p",
  "valueaddedperindustry_sec_B.E",
  "grossfixedcapitalformationbyindustry_sec_B.E"
)
controls <- intersect(candidate_controls, names(df))

# Keep complete rows for core vars + selected controls
keep_vars <- c("NUTS_ID", "country", "year", "wave", "ln_y_emp", "x_demis", controls)
wd <- copy(df)[, ..keep_vars]
wd <- wd[is.finite(ln_y_emp) & is.finite(x_demis)]
for (cc in controls) {
  wd <- wd[is.finite(get(cc))]
}

if (nrow(wd) == 0) {
  stop("No usable observations after filtering.")
}

# ----------------------------
# 3) Descriptive and sample info
# ----------------------------
sample_info <- data.table(
  metric = c(
    "n_obs",
    "n_regions_nuts3",
    "n_countries",
    "year_min",
    "year_max"
  ),
  value = c(
    nrow(wd),
    uniqueN(wd$NUTS_ID),
    uniqueN(wd$country),
    min(wd$year, na.rm = TRUE),
    max(wd$year, na.rm = TRUE)
  )
)

fwrite(sample_info, file.path(out_dir, "sample_info.csv"))

# ----------------------------
# 4) Correlation analysis
# ----------------------------
raw_corr <- data.table(
  statistic = c("pearson_raw", "spearman_raw"),
  correlation = c(
    cor(wd$ln_y_emp, wd$x_demis, method = "pearson", use = "complete.obs"),
    cor(wd$ln_y_emp, wd$x_demis, method = "spearman", use = "complete.obs")
  )
)

# Partial correlation net of controls + FE (country + year)
ctrl_rhs <- if (length(controls) > 0) paste(controls, collapse = " + ") else "1"
form_y <- as.formula(paste0("ln_y_emp ~ ", ctrl_rhs, " | country + year"))
form_x <- as.formula(paste0("x_demis ~ ", ctrl_rhs, " | country + year"))

resid_y <- resid(feols(form_y, data = wd))
resid_x <- resid(feols(form_x, data = wd))

partial_corr <- data.table(
  statistic = "pearson_partial_country_year",
  correlation = cor(resid_y, resid_x, method = "pearson", use = "complete.obs")
)

corr_table <- rbind(raw_corr, partial_corr, fill = TRUE)
fwrite(corr_table, file.path(out_dir, "correlations.csv"))

# ----------------------------
# 5) Fixed-effects regressions
# ----------------------------
# Collinearity note:
# year FE and wave FE should not be used together (wave is a deterministic mapping of year).

rhs <- paste(c("x_demis", controls), collapse = " + ")

# Baseline: country + year FE
m_country_year <- feols(
  as.formula(paste0("ln_y_emp ~ ", rhs, " | country + year")),
  data = wd,
  cluster = ~ NUTS_ID + year
)

# Alternative: country + wave FE
m_country_wave <- feols(
  as.formula(paste0("ln_y_emp ~ ", rhs, " | country + wave")),
  data = wd,
  cluster = ~ NUTS_ID + year
)

# Alternative tighter spatial control: NUTS3 + year FE
m_nuts3_year <- feols(
  as.formula(paste0("ln_y_emp ~ ", rhs, " | NUTS_ID + year")),
  data = wd,
  cluster = ~ NUTS_ID + year
)

# ----------------------------
# 6) Robustness examples
# ----------------------------
# (a) One-year lag of emission change
setorder(wd, NUTS_ID, year)
wd[, x_demis_l1 := shift(x_demis, n = 1L, type = "lag"), by = NUTS_ID]
wd_lag <- wd[is.finite(x_demis_l1)]

m_lag <- feols(
  as.formula(paste0("ln_y_emp ~ x_demis_l1", if (length(controls) > 0) paste0(" + ", paste(controls, collapse = " + ")) else "", " | country + year")),
  data = wd_lag,
  cluster = ~ NUTS_ID + year
)

# (b) Winsorize X at 1st/99th percentiles
q01 <- quantile(wd$x_demis, 0.01, na.rm = TRUE)
q99 <- quantile(wd$x_demis, 0.99, na.rm = TRUE)
wd[, x_demis_w := pmin(pmax(x_demis, q01), q99)]

m_winsor <- feols(
  as.formula(paste0("ln_y_emp ~ x_demis_w", if (length(controls) > 0) paste0(" + ", paste(controls, collapse = " + ")) else "", " | country + year")),
  data = wd,
  cluster = ~ NUTS_ID + year
)

# ----------------------------
# 7) Export regression tables
# ----------------------------
etable(
  m_country_year,
  m_country_wave,
  m_nuts3_year,
  m_lag,
  m_winsor,
  file = file.path(out_dir, "regression_table.md"),
  replace = TRUE,
  tex = FALSE
)

# Also write a plain text console-friendly summary
sink(file.path(out_dir, "regression_summary.txt"))
cat("=== CORRELATIONS ===\n")
print(corr_table)
cat("\n=== MODELS ===\n")
print(summary(m_country_year))
print(summary(m_country_wave))
print(summary(m_nuts3_year))
print(summary(m_lag))
print(summary(m_winsor))
sink()

cat("Done. Outputs written to: ", out_dir, "\n", sep = "")
cat("Files:\n")
cat("- sample_info.csv\n")
cat("- correlations.csv\n")
cat("- regression_table.md\n")
cat("- regression_summary.txt\n")
