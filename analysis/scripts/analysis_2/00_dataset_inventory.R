# analysis_2 / step 00
# Build an inventory of available CSV datasets and variables in ETS_ARDECO_merged.

rm(list = ls())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(data.table)
})

root_dir <- "ETS_ARDECO_merged"
out_dir <- "analysis/output/analysis_2"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

candidate_dirs <- c(
  file.path(root_dir, "outdir_ardeco_ets_merge"),
  file.path(root_dir, "data_eurostat_zip", "data_eurostat", "out_dir"),
  file.path(root_dir, "data_eurostat_zip", "data_eurostat", "processed_data")
)

csv_files <- unique(unlist(
  lapply(candidate_dirs, function(d) {
    if (!dir.exists(d)) return(character(0))
    list.files(d, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  })
))

if (length(csv_files) == 0) {
  stop("No CSV files found in configured ETS_ARDECO_merged directories.")
}

to_rel <- function(path, base = getwd()) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  b <- normalizePath(base, winslash = "/", mustWork = FALSE)
  prefix <- paste0(b, "/")
  if (startsWith(p, prefix)) substr(p, nchar(prefix) + 1L, nchar(p)) else p
}

classify_variable <- function(v) {
  if (grepl("^employmentbyindustry", v)) return("employment_industry")
  if (grepl("verified|emission|surrender|alloc", v, ignore.case = TRUE)) return("emissions_or_ets")
  if (grepl("nace|sector|activity", v, ignore.case = TRUE)) return("industry_code")
  if (grepl("NUTS|region|country|city", v, ignore.case = TRUE)) return("geography")
  if (grepl("^year$|time|phase|wave", v, ignore.case = TRUE)) return("time")
  if (grepl("gdp|population|employmentrate|capital|valueadded|hoursworked|unemployment", v, ignore.case = TRUE)) return("macro_or_labor")
  "other"
}

file_summary <- rbindlist(lapply(csv_files, function(f) {
  cols <- tryCatch(names(fread(f, nrows = 0)), error = function(e) character(0))
  info <- file.info(f)
  data.table(
    file = to_rel(f),
    n_cols = length(cols),
    file_size_mb = round(as.numeric(info$size) / 1024^2, 3)
  )
}), fill = TRUE)

inventory <- rbindlist(lapply(csv_files, function(f) {
  cols <- tryCatch(names(fread(f, nrows = 0)), error = function(e) character(0))
  if (length(cols) == 0) return(NULL)
  data.table(
    file = to_rel(f),
    variable = cols,
    var_class = vapply(cols, classify_variable, character(1))
  )
}), fill = TRUE)

if (nrow(inventory) == 0) {
  stop("Could not read headers from candidate CSV files.")
}

class_summary <- inventory[, .N, by = .(file, var_class)][order(file, -N)]

# Explicit variable sets for quick reference in downstream analysis.
employment_vars <- inventory[grepl("^employmentbyindustry", variable)]
emissions_vars <- inventory[grepl("verified|emission", variable, ignore.case = TRUE)]
industry_id_vars <- inventory[grepl("nace|sector|activity", variable, ignore.case = TRUE)]

fwrite(file_summary, file.path(out_dir, "dataset_file_summary.csv"))
fwrite(inventory, file.path(out_dir, "dataset_inventory.csv"))
fwrite(class_summary, file.path(out_dir, "dataset_inventory_by_class.csv"))
fwrite(unique(employment_vars), file.path(out_dir, "dataset_inventory_employment_vars.csv"))
fwrite(unique(emissions_vars), file.path(out_dir, "dataset_inventory_emissions_vars.csv"))
fwrite(unique(industry_id_vars), file.path(out_dir, "dataset_inventory_industry_id_vars.csv"))

cat("Inventory complete. Outputs written to: ", out_dir, "\n", sep = "")
