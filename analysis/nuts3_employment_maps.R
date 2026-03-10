# NUTS3 employment choropleth maps (static PNGs for LaTeX)
#
# Input: ETS_ARDECO_merged/outdir_ardeco_ets_merge/ardeco_ets_merged.csv
# Value mapped: employmentbyindustry_sec_B.E
# Output: analysis/output/maps/*.png

rm(list = ls())
options(stringsAsFactors = FALSE)

req_pkgs <- c("data.table", "sf", "giscoR", "ggplot2", "viridis")
missing_pkgs <- req_pkgs[!vapply(req_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before running, e.g. install.packages(c(",
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))"
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(giscoR)
  library(ggplot2)
  library(viridis)
})

sf::sf_use_s2(TRUE)

# ----------------------------
# 1) Paths and load data
# ----------------------------
in_file <- "ETS_ARDECO_merged/outdir_ardeco_ets_merge/ardeco_ets_merged.csv"
out_dir <- "analysis/output/maps"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "series"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "yearly"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "averages"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "periods"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "meta"), showWarnings = FALSE, recursive = TRUE)

if (!file.exists(in_file)) stop("Input file not found: ", in_file)

dt <- fread(in_file, na.strings = c("", "NA", "-", "—", "–"))
setnames(dt, names(dt), make.names(names(dt)))

needed <- c("NUTS_ID", "year", "employmentbyindustry_sec_B.E")
miss <- setdiff(needed, names(dt))
if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))

# Keep one value per NUTS3-year (mean if duplicates)
emp <- dt[, .(employment = mean(as.numeric(employmentbyindustry_sec_B.E), na.rm = TRUE)), by = .(NUTS_ID, year)]
emp[!is.finite(employment), employment := NA_real_]
emp[, ln_employment := log1p(employment)]

# ----------------------------
# 2) Get NUTS3 geometry
# ----------------------------
nuts3 <- gisco_get_nuts(
  year = "2021",
  nuts_level = "3",
  epsg = "4326",
  resolution = "10",
  spatialtype = "RG"
)[, c("NUTS_ID", "NAME_LATN", "CNTR_CODE", "geometry")]

# ----------------------------
# 3) QA: coverage
# ----------------------------
nuts3_ids <- as.data.table(st_drop_geometry(nuts3))[, .(NUTS_ID)]

match_table <- merge(
  unique(emp[, .(NUTS_ID)]),
  nuts3_ids,
  by = "NUTS_ID",
  all.x = TRUE,
  all.y = FALSE
)

unmatched_ids <- setdiff(unique(emp$NUTS_ID), unique(nuts3_ids$NUTS_ID))
unmatched_out <- data.table(NUTS_ID = unmatched_ids)
fwrite(unmatched_out, file.path(out_dir, "meta", "unmatched_nuts_ids.csv"))

sample_info <- data.table(
  metric = c("n_rows_emp", "n_unique_nuts_emp", "n_unique_years", "n_unmatched_nuts"),
  value = c(nrow(emp), uniqueN(emp$NUTS_ID), uniqueN(emp$year), length(unmatched_ids))
)
fwrite(sample_info, file.path(out_dir, "meta", "map_sample_info.csv"))

# ----------------------------
# 4) Helper to draw one map
# ----------------------------
plot_one_year <- function(year_i, value_col = c("employment", "ln_employment"),
                          fill_limits = NULL, out_file = NULL, title_prefix = "") {
  value_col <- match.arg(value_col)

  yr <- emp[year == year_i, .(NUTS_ID, value = get(value_col))]
  m <- merge(nuts3, yr, by = "NUTS_ID", all.x = TRUE)

  p <- ggplot(m) +
    geom_sf(aes(fill = value), color = NA) +
    scale_fill_viridis_c(
      option = "C",
      na.value = "grey92",
      limits = fill_limits,
      oob = scales::squish,
      name = ifelse(value_col == "employment", "Employment\n(B-E)", "log(1+Employment)\n(B-E)")
    ) +
    labs(
      title = sprintf("%s %s", title_prefix, year_i),
      subtitle = "NUTS3 regions, ARDECO employment by industry (B-E)",
      caption = "Source: ARDECO-ETS merged panel; geometry: GISCO NUTS 2021"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )

  if (!is.null(out_file)) {
    ggsave(out_file, p, width = 12, height = 8, dpi = 320)
  }

  invisible(p)
}

# ----------------------------
# 5) Produce maps by year
# ----------------------------
years_all <- sort(unique(emp$year))
years_key <- intersect(c(2005, 2008, 2013, 2020, 2023), years_all)

# Global limits so colors are comparable over time
lims_emp <- range(emp$employment, na.rm = TRUE)
lims_lne <- range(emp$ln_employment, na.rm = TRUE)

# (A) Key years only (report-ready)
for (yy in years_key) {
  plot_one_year(
    year_i = yy,
    value_col = "employment",
    fill_limits = lims_emp,
    out_file = file.path(out_dir, "yearly", sprintf("map_employment_%s.png", yy)),
    title_prefix = "Employment Heatmap"
  )

  plot_one_year(
    year_i = yy,
    value_col = "ln_employment",
    fill_limits = lims_lne,
    out_file = file.path(out_dir, "yearly", sprintf("map_ln_employment_%s.png", yy)),
    title_prefix = "Log Employment Heatmap"
  )
}

# (B) Full yearly series (optional but useful for appendix)
for (yy in years_all) {
  plot_one_year(
    year_i = yy,
    value_col = "employment",
    fill_limits = lims_emp,
    out_file = file.path(out_dir, "series", sprintf("series_employment_%s.png", yy)),
    title_prefix = "Employment Heatmap"
  )
}

# ----------------------------
# 6) Aggregated-period maps + full-period average
# ----------------------------
# Non-overlapping periods (cleaner for report comparison)
emp[, period := fifelse(year >= 2005 & year <= 2009, "2005-2009",
                 fifelse(year >= 2010 & year <= 2014, "2010-2014",
                 fifelse(year >= 2015 & year <= 2018, "2015-2018",
                 fifelse(year >= 2019 & year <= 2023, "2019-2023", NA_character_))))]

emp_period <- emp[!is.na(period), .(
  employment = mean(employment, na.rm = TRUE),
  ln_employment = mean(ln_employment, na.rm = TRUE)
), by = .(NUTS_ID, period)]

# Full-period average map (single panel)
emp_full <- emp[, .(
  employment = mean(employment, na.rm = TRUE),
  ln_employment = mean(ln_employment, na.rm = TRUE)
), by = NUTS_ID]

m_full <- merge(nuts3, emp_full[, .(NUTS_ID, value = employment)], by = "NUTS_ID", all.x = TRUE)
p_full <- ggplot(m_full) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "grey92",
    limits = lims_emp,
    oob = scales::squish,
    name = "Mean Employment\n(B-E)"
  ) +
  labs(
    title = "Average Employment Heatmap (2005-2023)",
    subtitle = "NUTS3 regions, ARDECO employment by industry (B-E)",
    caption = "Source: ARDECO-ETS merged panel; geometry: GISCO NUTS 2021"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )
ggsave(file.path(out_dir, "averages", "map_employment_average_2005_2023.png"), p_full, width = 12, height = 8, dpi = 320)

m_full_ln <- merge(nuts3, emp_full[, .(NUTS_ID, value = ln_employment)], by = "NUTS_ID", all.x = TRUE)
p_full_ln <- ggplot(m_full_ln) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c(
    option = "C",
    na.value = "grey92",
    limits = lims_lne,
    oob = scales::squish,
    name = "Mean log(1+Employment)\n(B-E)"
  ) +
  labs(
    title = "Average Log Employment Heatmap (2005-2023)",
    subtitle = "NUTS3 regions, ARDECO employment by industry (B-E)",
    caption = "Source: ARDECO-ETS merged panel; geometry: GISCO NUTS 2021"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )
ggsave(file.path(out_dir, "averages", "map_ln_employment_average_2005_2023.png"), p_full_ln, width = 12, height = 8, dpi = 320)

# Period-aggregated maps (one PNG per period)
periods <- c("2005-2009", "2010-2014", "2015-2018", "2019-2023")
for (pp in periods) {
  m_p <- merge(
    nuts3,
    emp_period[period == pp, .(NUTS_ID, value = employment)],
    by = "NUTS_ID",
    all.x = TRUE
  )
  p_p <- ggplot(m_p) +
    geom_sf(aes(fill = value), color = NA) +
    scale_fill_viridis_c(
      option = "C",
      na.value = "grey92",
      limits = lims_emp,
      oob = scales::squish,
      name = "Mean Employment\n(B-E)"
    ) +
    labs(
      title = sprintf("Employment Heatmap (%s)", pp),
      subtitle = "NUTS3 period mean, ARDECO employment by industry (B-E)",
      caption = "Source: ARDECO-ETS merged panel; geometry: GISCO NUTS 2021"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
  ggsave(file.path(out_dir, "periods", sprintf("map_employment_period_%s.png", gsub("-", "_", pp))), p_p, width = 12, height = 8, dpi = 320)
}

cat("Done. Maps written to: ", out_dir, "\n", sep = "")
cat("Key years: ", paste(years_key, collapse = ", "), "\n", sep = "")
cat("Total annual maps (series): ", length(years_all), "\n", sep = "")
