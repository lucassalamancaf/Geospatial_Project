# analysis_2 / step 03
# Descriptive outputs:
# 1) NUTS3 maps of average changes (d_emissions and d_employment)
# 2) EU trend lines over time

rm(list = ls())
options(stringsAsFactors = FALSE)

req_pkgs <- c("data.table", "ggplot2", "sf", "scales")
missing_pkgs <- req_pkgs[!vapply(req_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before running, e.g. install.packages(c(",
    paste(sprintf('\"%s\"', missing_pkgs), collapse = ", "), "))"
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(sf)
  library(scales)
})

sf::sf_use_s2(TRUE)

out_dir <- "analysis/output/analysis_2"
fig_dir <- file.path(out_dir, "figures")
desc_dir <- file.path(out_dir, "descriptive_data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(desc_dir, showWarnings = FALSE, recursive = TRUE)

panel_file <- file.path(out_dir, "industry_model_analysis_panel.csv")
if (!file.exists(panel_file)) {
  stop("Missing analysis panel: ", panel_file, ". Run 01_industry_employment_emissions_fe.R first.")
}

wd <- fread(panel_file, na.strings = c("", "NA", "-"))
setnames(wd, names(wd), make.names(names(wd)))

required <- c("NUTS_ID", "year", "sector", "d_employment", "d_emissions")
missing_required <- setdiff(required, names(wd))
if (length(missing_required) > 0) {
  stop("Missing columns in analysis panel: ", paste(missing_required, collapse = ", "))
}

wd[, year := as.integer(year)]
wd[, d_employment := as.numeric(d_employment)]
wd[, d_emissions := as.numeric(d_emissions)]
wd <- wd[is.finite(d_employment) & is.finite(d_emissions) & !is.na(NUTS_ID) & NUTS_ID != ""]

if (nrow(wd) == 0) stop("No usable rows in analysis panel for maps/trends.")

# If multiple sectors exist in the future, keep all and produce pooled outputs.
# Current setup contains only B.E.
if (uniqueN(wd$sector) == 1L) {
  sector_label <- unique(wd$sector)
} else {
  sector_label <- "all_matched_sectors"
}

# ----------------------------
# 1) NUTS3 average-change maps
# ----------------------------
region_avg <- wd[, .(
  avg_d_emissions = mean(d_emissions, na.rm = TRUE),
  avg_d_employment = mean(d_employment, na.rm = TRUE),
  n_obs = .N
), by = .(NUTS_ID)]

fwrite(region_avg, file.path(desc_dir, "map_region_average_changes.csv"))

get_nuts3_geometry <- function() {
  # Direct GISCO download avoids giscoR/httr2 namespace issues.
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
      nms <- names(obj)
      if ("LEVL_CODE" %in% nms) {
        obj <- obj[obj$LEVL_CODE == 3, ]
      }
      needed <- intersect(c("NUTS_ID", "NAME_LATN", "CNTR_CODE", "geometry"), names(obj))
      if (!("NUTS_ID" %in% needed)) next
      return(obj[, needed])
    }
  }

  stop(
    "Failed to download NUTS3 geometry from GISCO URLs. Last error: ",
    ifelse(is.null(last_err), "unknown", last_err)
  )
}

nuts3 <- get_nuts3_geometry()

map_dt <- merge(nuts3, region_avg, by = "NUTS_ID", all.x = TRUE)

# Keep canonical names even if merge adds suffixes.
if (!("avg_d_emissions" %in% names(map_dt))) {
  em_cols <- grep("^avg_d_emissions", names(map_dt), value = TRUE)
  if (length(em_cols) == 1L) {
    names(map_dt)[names(map_dt) == em_cols] <- "avg_d_emissions"
  }
}
if (!("avg_d_employment" %in% names(map_dt))) {
  ep_cols <- grep("^avg_d_employment", names(map_dt), value = TRUE)
  if (length(ep_cols) == 1L) {
    names(map_dt)[names(map_dt) == ep_cols] <- "avg_d_employment"
  }
}

if (!("avg_d_emissions" %in% names(map_dt)) || !("avg_d_employment" %in% names(map_dt))) {
  stop("Merged map object is missing average-change columns after merge.")
}

ok_map <- is.finite(map_dt$avg_d_emissions) & is.finite(map_dt$avg_d_employment)
ok_map[is.na(ok_map)] <- FALSE

coverage <- data.table(
  metric = c("n_regions_geometry", "n_regions_with_data", "n_regions_without_data"),
  value = c(
    nrow(nuts3),
    sum(ok_map),
    sum(!ok_map)
  )
)
fwrite(coverage, file.path(desc_dir, "map_coverage_summary.csv"))

plot_change_map <- function(sf_dt, value_col, title_txt, legend_txt, out_file) {
  vals <- sf_dt[[value_col]]
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0L) {
    warning("No finite values for ", value_col, ". Skipping map.")
    return(invisible(NULL))
  }

  # Robust symmetric scale around zero for better visual comparability.
  lim_q <- quantile(vals, probs = c(0.02, 0.98), na.rm = TRUE)
  lim_abs <- max(abs(lim_q))
  if (!is.finite(lim_abs) || lim_abs == 0) lim_abs <- max(abs(vals), na.rm = TRUE)
  if (!is.finite(lim_abs) || lim_abs == 0) lim_abs <- 1

  p <- ggplot(sf_dt) +
    geom_sf(aes(fill = .data[[value_col]]), color = NA) +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "#f7f7f7",
      high = "#b2182b",
      midpoint = 0,
      limits = c(-lim_abs, lim_abs),
      oob = scales::squish,
      na.value = "grey92",
      labels = label_number(big.mark = ","),
      name = legend_txt
    ) +
    labs(
      title = title_txt,
      subtitle = paste0("NUTS3 average annual changes, sector ", sector_label),
      caption = "Source: ARDECO + EU ETS matched panel; geometry: GISCO NUTS 2021"
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
  invisible(p)
}

plot_change_map(
  sf_dt = map_dt,
  value_col = "avg_d_emissions",
  title_txt = "NUTS3 Map: Average Change in Emissions",
  legend_txt = "Average Delta Emissions",
  out_file = file.path(fig_dir, "map_nuts3_avg_d_emissions.png")
)

plot_change_map(
  sf_dt = map_dt,
  value_col = "avg_d_employment",
  title_txt = "NUTS3 Map: Average Change in Employment (Industry)",
  legend_txt = "Average Delta Employment",
  out_file = file.path(fig_dir, "map_nuts3_avg_d_employment.png")
)

# ----------------------------
# 2) EU trend lines over time
# ----------------------------
trend_year <- wd[, .(
  mean_d_emissions = mean(d_emissions, na.rm = TRUE),
  median_d_emissions = median(d_emissions, na.rm = TRUE),
  mean_d_employment = mean(d_employment, na.rm = TRUE),
  median_d_employment = median(d_employment, na.rm = TRUE),
  n_obs = .N
), by = year][order(year)]

trend_year[, mean_d_emissions_ma3 := frollmean(mean_d_emissions, n = 3, align = "right", na.rm = TRUE)]
trend_year[, mean_d_employment_ma3 := frollmean(mean_d_employment, n = 3, align = "right", na.rm = TRUE)]

fwrite(trend_year, file.path(desc_dir, "eu_trends_by_year.csv"))

trend_long <- rbindlist(list(
  trend_year[, .(
    year,
    series = "d_emissions",
    mean_value = mean_d_emissions,
    median_value = median_d_emissions,
    mean_ma3 = mean_d_emissions_ma3
  )],
  trend_year[, .(
    year,
    series = "d_employment",
    mean_value = mean_d_employment,
    median_value = median_d_employment,
    mean_ma3 = mean_d_employment_ma3
  )]
))

p_trend <- ggplot(trend_long, aes(x = year)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_line(aes(y = mean_value, color = "Mean"), linewidth = 0.9) +
  geom_line(aes(y = median_value, color = "Median"), linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = mean_ma3, color = "Mean (3y MA)"), linewidth = 1.0, alpha = 0.9) +
  facet_wrap(~ series, scales = "free_y", ncol = 1) +
  scale_color_manual(
    values = c("Mean" = "#1b9e77", "Median" = "#d95f02", "Mean (3y MA)" = "#7570b3"),
    name = NULL
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  labs(
    title = "EU Trend Lines Over Time",
    subtitle = paste0("Annual changes by year, sector ", sector_label),
    x = "Year",
    y = "Change",
    caption = "Source: ARDECO + EU ETS matched panel"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave(file.path(fig_dir, "eu_trend_lines_changes.png"), p_trend, width = 11, height = 9, dpi = 320)

cat("Maps and trend lines complete. Outputs written to: ", out_dir, "\n", sep = "")
