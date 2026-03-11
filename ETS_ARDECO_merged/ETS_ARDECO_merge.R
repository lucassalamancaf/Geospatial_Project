rm(list = ls())
graphics.off()
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(giscoR)
  library(httr)
  library(jsonlite)
  library(tidyverse)
  library(statcodelists)
  
  sf::sf_use_s2(TRUE)
})

### Load data from Jakob
{
## Load ETS panel
# ---- User input: data directory ----
data_dir_ets <- file.path(getwd(), "data_euets_zip", "data_euets")  # <-- CHANGE if needed
# ---- Folder structure  ----
#  inputs must be located in:  <data_dir>/output_euets_installations_panel/
ets_input_dir  <- file.path(data_dir_ets, "output_euets_installations_panel")
# ---- Load ETS panel ----
ets_panel <- read.csv(file.path(ets_input_dir, "panel_final_euets_stationary_nuts3.csv"), stringsAsFactors = FALSE)

## Load ARDECO panel
# ---- User input: data directory ----
data_dir_ardeco <- file.path(getwd(), "data_eurostat_zip", "data_eurostat")  # <-- CHANGE if needed
# ---- Folder structure  ----
#  inputs must be located in:  <data_dir>/output_euets_installations_panel/
ardeco_input_dir    <- file.path(data_dir_ardeco, "out_dir")
# ---- Load ARDECO panel ----
ardeco_panel <- read.csv(file.path(ardeco_input_dir, "ardeco_panel_2005_2023.csv"), stringsAsFactors = FALSE)

## Output directory
# ---- Define output directory directory as parent directory of data_eurostat_zip/data_euets_zip ----
output_dir <- file.path(dirname(dirname(data_dir_ardeco)), "outdir_ardeco_ets_merge") 
# Create output folder in directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
}

### Preparing the ETS data
{
## Constraining the data
wd_ets_panel <- ets_panel %>% 
  group_by(installation_id) %>% 
  ## 1. Restricting to firms entering in first wave (first year = 2005)
  filter(min(first_year_reported) == 2005) %>% #
  # 2. Drop firms which never emit (keeping firms who stop emitting)
  filter(sum(allocatedTotal) > 0) %>%
  # 3. Drop firms without NACE identification (nace_id, nace_15_id, nace_20_id)
  filter(!is.na(nace_id) & !is.na(nace15_id) & !is.na(nace20_id)) %>% 
  ungroup()
  
# 4. Check and map 10 sector specification from ardeco to Installations nace id
# Reformat nace_id in ets_panel
wd_ets_panel <- wd_ets_panel %>% 
  # Coerce to character
  mutate(nace_id = as.character(nace_id)) %>% 
  # If second character is ".", add 0 to the beginning of the string
  mutate(nace_id = ifelse(substr(nace_id, 2, 2) == ".", paste0("0", nace_id), nace_id)) %>% 
  # Remove "." from the string
  mutate(nace_id = gsub("\\.", "", nace_id))
# Reformat mace_nomenclature to merge
nace_nomenclature <- statcodelists::CL_ACTIVITY_NACE2 %>% 
  # Create variable nace_id which omits the first character of "id"
  mutate(nace_id = substr(id, 2, nchar(id))) %>% 
  # Create variable that only contains the first character of "id"
  mutate(nace_section = substr(id, 1, 1)) %>% 
  # Remove observations where nace_id is an empty string
  filter(nace_id != "") %>% 
  select(name, nace_id, nace_section)
# Merge nace_nomenclature to wd_ets_panel to get the nace_section variable
wd_ets_panel <- wd_ets_panel %>% 
  left_join(nace_nomenclature %>% select(nace_id, nace_section), by = "nace_id")
# NB: In the base installations data, nace15_id was filled in for 2 observations in nace_id, I'll ignore this for now

# 5. Check distribution of ardeco-level NACE sectors for installations, restrict dataset to sectors with enough observations (vibe check) 
# Plot number of observations per ardeco-level NACE sector
# wd_ets_panel %>% 
#   group_by(nace_section) %>% 
#   summarise(n = n()) %>% 
#   ggplot(aes(x = nace_section, y = n)) + 
#   geom_bar(stat = "identity") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(x = "NACE sector", y = "Number of observations", title = "Number of observations per NACE sector in ARDECO panel")
# NB: Most observations in C, D. Need to restrict to B-E to merge to ARDECO panel

# Drop from wd_ets_panel all observations with nace_section != "B", "C", "D", "E"
wd_ets_panel <- wd_ets_panel %>% 
  filter(nace_section %in% c("B", "C", "D", "E"))

# 6. Create Variable for aggregate verified emission per ardeco industry per NUTS3 region and year
wd_ets_panel_agg <- wd_ets_panel %>% 
  group_by(NUTS_ID, year) %>% 
  summarise(verified = sum(verified, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Drop observations where NUTS_ID is ""
  filter(NUTS_ID != "")

# 7. Create Delta variable for this (Check for jumps between phases of ETS)
# Convert wd_ets_panel_agg to data table
wd_ets_panel_agg <- as.data.table(wd_ets_panel_agg)
# Create variable for lagged verified emissions by NUTS_ID
wd_ets_panel_agg[, verified_lag := shift(verified, n = 1, type = "lag"), by = NUTS_ID]
# Create variable for change in verified emissions
wd_ets_panel_agg[, verified_delta := verified - verified_lag]

# Plot levels of verified emissions over time (average over NUTS_ID)
# wd_ets_panel_agg %>%
#   group_by(year) %>% 
#   summarise(verified_avg = mean(verified, na.rm = TRUE)) %>% 
#   ggplot(aes(x = year, y = verified_avg)) + 
#   geom_line() +
#   geom_point() +
#   theme_bw() +
#   labs(x = "Year", y = "Average verified emissions", title = "Average verified emissions over time")

# Plot change in verified emissions over time (average over NUTS_ID)
# wd_ets_panel_agg %>% 
#   group_by(year) %>% 
#   summarise(verified_delta_avg = mean(verified_delta, na.rm = TRUE)) %>% 
#   ggplot(aes(x = year, y = verified_delta_avg)) + 
#   geom_line() +
#   geom_point() +
#   theme_bw() +
#   labs(x = "Year", y = "Average change in verified emissions", title = "Average change in verified emissions over time")

# Export the aggregated panel to the output directory
write.csv(wd_ets_panel_agg, file.path(output_dir, "ets_panel_NUTS3aggregate.csv"), row.names = FALSE)
# Export the installation-level panel to the output directory
write.csv(wd_ets_panel, file.path(output_dir, "ets_panel_final_raw.csv"), row.names = FALSE)

# Clean environment except "ardeco_panel", "wd_ets_panel_agg", "ardeco_input_dir", "ets_input_dir", "data_dir_ets", "data_dir_ardeco", "output_dir"
rm(list = setdiff(ls(), c("ardeco_panel", "wd_ets_panel_agg", "ardeco_input_dir", "ets_input_dir", "data_dir_ets", "data_dir_ardeco", "output_dir")))
}

### Preparing the ARDECO data
{
  # Define the ARDECO-level NACE sectors to keep (B-E)
  ardeco_nace_sections <- c("B.E")
  
  # Gather colnames that end in a capital letter
  ardeco_colnames <- colnames(ardeco_panel)
  ardeco_colnames_capital <- ardeco_colnames[grepl("[A-Z]$", ardeco_colnames)]
  
  # Filter colnames from ardeco_colnames_capital that end in ardeco_nace_sections
  relevant_colnames <- ardeco_colnames_capital[grepl(paste0(paste(ardeco_nace_sections, collapse = "|"), "$"), ardeco_colnames_capital)]
  
  # Final colnames: ardeco_colnames - ardeco_colnames_capital + relevant_colnames
  final_colnames <- c(setdiff(ardeco_colnames, ardeco_colnames_capital), relevant_colnames)

  # Return all colnames in ardeco_panel
  wd_ardeco_panel <- ardeco_panel %>% 
    # Keep only relevant columns
    select(all_of(final_colnames)) %>% 
    # Keep only level_id == 3
    filter(level_id == 3) %>%
    # Rename region_id to NUTS_ID for merge
    rename(NUTS_ID = region_id) %>% 
    select(-level_id)
  
  # Export the ARDECO panel to the output directory
  write.csv(wd_ardeco_panel, file.path(output_dir, "ardeco_panel_premerge.csv"), row.names = FALSE)
  
  # Clean environment except "wd_ardeco_panel", "wd_ets_panel_agg", "ardeco_input_dir", "ets_input_dir", "data_dir_ets", "data_dir_ardeco", "output_dir"
  rm(list = setdiff(ls(), c("wd_ardeco_panel", "wd_ets_panel_agg", "ardeco_input_dir", "ets_input_dir", "data_dir_ets", "data_dir_ardeco", "output_dir")))
}

### Merging NUTS 3 aggregate ETS data to ARDECO data

# Merge the ARDECO panel with the aggregated ETS panel by NUTS_ID and year
ardeco_ets_merged <- wd_ardeco_panel %>% 
  left_join(wd_ets_panel_agg %>% select(NUTS_ID, year, verified, verified_delta), by = c("NUTS_ID", "year")) %>% 
  select(NUTS_ID, year, verified, verified_delta, employmentbyindustry_sec_B.E, valueaddedperindustry_sec_B.E, grossfixedcapitalformationbyindustry_sec_B.E, everything())

# Constrain to NUTS3-year combinations where ETS panel data is available 
ardeco_ets_merged_noNA <- ardeco_ets_merged %>% 
  filter(!is.na(verified) & !is.na(verified_delta))

# Export the merged panels to the output directory
write.csv(ardeco_ets_merged, file.path(output_dir, "ardeco_ets_merged.csv"), row.names = FALSE)
write.csv(ardeco_ets_merged_noNA, file.path(output_dir, "ardeco_ets_merged_noNA.csv"), row.names = FALSE)
