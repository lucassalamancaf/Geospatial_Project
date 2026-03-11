# NUTS3 employment choropleth maps (tidy report set)
#
# Input: ETS_ARDECO_merged/outdir_ardeco_ets_merge/ardeco_ets_merged.csv
# Value mapped: employmentbyindustry_sec_B.E
# Output: analysis/output/analysis_1/maps/
# Keeps only:
# - 1 full-period average map (2005-2023)
# - 4 period maps (2005-2009, 2010-2014, 2015-2018, 2019-2023)

rm(list = ls())
options(stringsAsFactors = FALSE)

req_pkgs <- c("data.table", "sf", "ggplot2", "viridis", "scales")
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
  library(ggplot2)
  library(viridis)
  library(scales)
})

sf::sf_use_s2(TRUE)

in_file <- "ETS_ARDECO_merged/outdir_ardeco_ets_merge/ardeco_ets_merged.csv"
out_dir <- "analysis/output/analysis_1/maps"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "periods"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "averages"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "meta"), showWarnings = FALSE, recursive = TRUE)

if (!file.exists(in_file)) stop("Input file not found: ", in_file)

dt <- fread(in_file, na.strings = c("", "NA", "-"))
setnames(dt, names(dt), make.names(names(dt)))

needed <- c("NUTS_ID", "year", "employmentbyindustry_sec_B.E")
miss <- setdiff(needed, names(dt))
if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse = ", "))

emp <- dt[, .(employment = mean(as.numeric(employmentbyindustry_sec_B.E), na.rm = TRUE)), by = .(NUTS_ID, year)]
emp[!is.finite(employment), employment := NA_real_]

emp[, period := fifelse(year >= 2005 & year <= 2009, "2005-2009",
                 fifelse(year >= 2010 & year <= 2014, "2010-2014",
                 fifelse(year >= 2015 & year <= 2018, "2015-2018",
                 fifelse(year >= 2019 & year <= 2023, "2019-2023", NA_character_))))]

emp_period <- emp[!is.na(period), .(employment = mean(employment, na.rm = TRUE)), by = .(NUTS_ID, period)]
emp_full <- emp[, .(employment = mean(employment, na.rm = TRUE)), by = NUTS_ID]

get_nuts3_geometry <- function() {
  url_candidates <- c(
    "https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_10M_2021_4326.geojson",
    "https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_10M_2021_4326_LEVL_3.geojson"
  )

  last_err <- NULL
  for (u in url_candidates) {
    obj <- tryCatch(sf::read_sf(u, quiet = TRUE), error = function(e) {
      last_err <<- e$message
      NULL
    })
    if (!is.null(obj) && nrow(obj) > 0) {
      if ("LEVL_CODE" %in% names(obj)) obj <- obj[obj$LEVL_CODE == 3, ]
      keep <- intersect(c("NUTS_ID", "NAME_LATN", "CNTR_CODE", "geometry"), names(obj))
      if (!("NUTS_ID" %in% keep)) next
      return(obj[, keep])
    }
  }

  stop("Failed to download NUTS3 geometry from GISCO. Last error: ", ifelse(is.null(last_err), "unknown", last_err))
}

nuts3 <- get_nuts3_geometry()

nuts3_ids <- as.data.table(st_drop_geometry(nuts3))[, .(NUTS_ID)]
unmatched_ids <- setdiff(unique(emp$NUTS_ID), unique(nuts3_ids$NUTS_ID))
fwrite(data.table(NUTS_ID = unmatched_ids), file.path(out_dir, "meta", "unmatched_nuts_ids.csv"))

sample_info <- data.table(
  metric = c("n_rows_emp", "n_unique_nuts_emp", "n_unique_years", "n_unmatched_nuts"),
  value = c(nrow(emp), uniqueN(emp$NUTS_ID), uniqueN(emp$year), length(unmatched_ids))
)
fwrite(sample_info, file.path(out_dir, "meta", "map_sample_info.csv"))

lims_emp <- range(emp$employment, na.rm = TRUE)

plot_emp_map <- function(sf_obj, out_file, title_txt, subtitle_txt) {
  p <- ggplot(sf_obj) +
    geom_sf(aes(fill = employment), color = NA) +
    scale_fill_viridis_c(
      option = "C",
      na.value = "grey92",
      limits = lims_emp,
      oob = scales::squish,
      name = "Employment\n(B-E)"
    ) +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
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

  ggsave(out_file, p, width = 12, height = 8, dpi = 320)
}

m_full <- merge(nuts3, emp_full, by = "NUTS_ID", all.x = TRUE)
plot_emp_map(
  sf_obj = m_full,
  out_file = file.path(out_dir, "averages", "map_employment_average_2005_2023.png"),
  title_txt = "Average Employment Heatmap (2005-2023)",
  subtitle_txt = "NUTS3 regions, ARDECO employment by industry (B-E)"
)

periods <- c("2005-2009", "2010-2014", "2015-2018", "2019-2023")
for (pp in periods) {
  m_p <- merge(
    nuts3,
    emp_period[period == pp, .(NUTS_ID, employment)],
    by = "NUTS_ID",
    all.x = TRUE
  )
  plot_emp_map(
    sf_obj = m_p,
    out_file = file.path(out_dir, "periods", sprintf("map_employment_period_%s.png", gsub("-", "_", pp))),
    title_txt = sprintf("Employment Heatmap (%s)", pp),
    subtitle_txt = "NUTS3 period mean, ARDECO employment by industry (B-E)"
  )
}

cat("Done. Map outputs written to: ", out_dir, "\n", sep = "")
cat("Kept maps:\n")
cat("- averages/map_employment_average_2005_2023.png\n")
cat("- periods/map_employment_period_2005_2009.png\n")
cat("- periods/map_employment_period_2010_2014.png\n")
cat("- periods/map_employment_period_2015_2018.png\n")
cat("- periods/map_employment_period_2019_2023.png\n")
