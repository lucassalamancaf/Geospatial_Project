# analysis_2 / step 02
# Serial-correlation diagnostics for the industry FE setup.

rm(list = ls())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

out_dir <- "analysis/output/analysis_2"
panel_file <- file.path(out_dir, "industry_model_analysis_panel.csv")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(panel_file)) {
  stop("Missing analysis panel: ", panel_file, ". Run 01_industry_employment_emissions_fe.R first.")
}

wd <- fread(panel_file, na.strings = c("", "NA", "-"))
setnames(wd, names(wd), make.names(names(wd)))

required <- c("NUTS_ID", "year", "country", "wave", "sector", "d_employment", "d_emissions")
missing_required <- setdiff(required, names(wd))
if (length(missing_required) > 0) {
  stop("Missing columns in analysis panel: ", paste(missing_required, collapse = ", "))
}

wd[, year := as.integer(year)]
wd[, d_employment := as.numeric(d_employment)]
wd[, d_emissions := as.numeric(d_emissions)]
setorder(wd, NUTS_ID, sector, year)

wd[, panel_id := paste(NUTS_ID, sector, sep = "__")]
wd[, d_employment_l1 := shift(d_employment, n = 1L, type = "lag"), by = panel_id]
wd[, d_emissions_l1 := shift(d_emissions, n = 1L, type = "lag"), by = panel_id]

# Re-estimate core FE model and compute serial correlation in residuals.
m_core <- feols(
  d_employment ~ d_emissions | country + wave + year,
  data = wd,
  cluster = ~ NUTS_ID + year
)

wd[, resid_core := resid(m_core)]
wd[, resid_core_l1 := shift(resid_core, n = 1L, type = "lag"), by = panel_id]

safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L) return(NA_real_)
  x2 <- x[ok]
  y2 <- y[ok]
  if (sd(x2) == 0 || sd(y2) == 0) return(NA_real_)
  suppressWarnings(cor(x2, y2))
}

# Pairwise lag correlations.
corr_dt <- data.table(
  metric = c(
    "corr_d_employment_t_tminus1",
    "corr_d_emissions_t_tminus1",
    "corr_core_residual_t_tminus1"
  ),
  value = c(
    safe_cor(wd$d_employment, wd$d_employment_l1),
    safe_cor(wd$d_emissions, wd$d_emissions_l1),
    safe_cor(wd$resid_core, wd$resid_core_l1)
  )
)

# AR(1)-style regressions with panel FE.
wd_emp <- wd[is.finite(d_employment) & is.finite(d_employment_l1)]
wd_emi <- wd[is.finite(d_emissions) & is.finite(d_emissions_l1)]
wd_res <- wd[is.finite(resid_core) & is.finite(resid_core_l1)]

m_emp_ar1 <- feols(
  d_employment ~ d_employment_l1 | panel_id,
  data = wd_emp,
  cluster = ~ NUTS_ID
)
m_emi_ar1 <- feols(
  d_emissions ~ d_emissions_l1 | panel_id,
  data = wd_emi,
  cluster = ~ NUTS_ID
)
m_res_ar1 <- feols(
  resid_core ~ resid_core_l1 | panel_id,
  data = wd_res,
  cluster = ~ NUTS_ID
)

summary_dt <- rbindlist(list(
  data.table(
    model = "AR1_d_employment",
    term = "d_employment_l1",
    estimate = coef(m_emp_ar1)[["d_employment_l1"]],
    se = se(m_emp_ar1)[["d_employment_l1"]],
    p_value = coeftable(m_emp_ar1)["d_employment_l1", "Pr(>|t|)"],
    n_obs = nobs(m_emp_ar1)
  ),
  data.table(
    model = "AR1_d_emissions",
    term = "d_emissions_l1",
    estimate = coef(m_emi_ar1)[["d_emissions_l1"]],
    se = se(m_emi_ar1)[["d_emissions_l1"]],
    p_value = coeftable(m_emi_ar1)["d_emissions_l1", "Pr(>|t|)"],
    n_obs = nobs(m_emi_ar1)
  ),
  data.table(
    model = "AR1_core_residual",
    term = "resid_core_l1",
    estimate = coef(m_res_ar1)[["resid_core_l1"]],
    se = se(m_res_ar1)[["resid_core_l1"]],
    p_value = coeftable(m_res_ar1)["resid_core_l1", "Pr(>|t|)"],
    n_obs = nobs(m_res_ar1)
  )
), fill = TRUE)

fwrite(corr_dt, file.path(out_dir, "serial_corr_correlations.csv"))
fwrite(summary_dt, file.path(out_dir, "serial_corr_summary.csv"))
fwrite(
  wd[, .(
    NUTS_ID, sector, year, d_employment, d_employment_l1,
    d_emissions, d_emissions_l1, resid_core, resid_core_l1
  )],
  file.path(out_dir, "serial_corr_panel_with_lags.csv")
)

etable(
  m_emp_ar1, m_emi_ar1, m_res_ar1,
  file = file.path(out_dir, "serial_corr_models.md"),
  replace = TRUE,
  tex = FALSE
)

sink(file.path(out_dir, "serial_corr_diagnostics.txt"))
cat("=== CORE FE MODEL ===\n")
print(summary(m_core))
cat("\n=== LAG CORRELATIONS ===\n")
print(corr_dt)
cat("\n=== AR(1)-STYLE MODELS ===\n")
print(summary(m_emp_ar1))
print(summary(m_emi_ar1))
print(summary(m_res_ar1))
sink()

cat("Serial-correlation diagnostics complete. Outputs written to: ", out_dir, "\n", sep = "")
