library(data.table)
library(fixest)
library(tidyverse)
library(readr)
library(ggplot2)

## Creating some lagged variables and deltas for the regressions
# (I played around with this quite a bit, and under certain specifications the effect becomes significant. But I'm really not so good at interpreting dynamic models.)
{
# Get the merged ETS-ARDECO panel
df <- read_csv("C:/Users/Georg/Downloads/Geospatial_Project-main/Geospatial_Project-main/ETS_ARDECO_merged/outdir_ardeco_ets_merge/ardeco_ets_merged_noNA.csv")

# Create a new variable "empl_sh_B_E" which is the share of employment in industry B.E (employmentbyindustry_sec_B.E) in total population (averageannualpopulation)
df <- df %>% 
  mutate(empl_sh_B_E = (employmentbyindustry_sec_B.E * 1000) / averageannualpopulation)

# Turn df into data.table. Create a new variable "employmentbyindustry_sec_B.E_lag" using year as time variable and grouping by NUTS_ID. Create variable "B_Eemp_delta"
df <- as.data.table(df)
df[, employmentbyindustry_sec_B.E_lag := shift(employmentbyindustry_sec_B.E, n = 1, type = "lag"), by = NUTS_ID]
df[, employmentbyindustry_sec_B.E_lag2 := shift(employmentbyindustry_sec_B.E, n = 2, type = "lag"), by = NUTS_ID]
df[, B_Eemp_delta := employmentbyindustry_sec_B.E - employmentbyindustry_sec_B.E_lag]
# Do the same for empl_sh_B_E
df[, empl_sh_B_E_lag := shift(empl_sh_B_E, n = 1, type = "lag"), by = NUTS_ID]
df[, empl_sh_B_E_lag2 := shift(empl_sh_B_E, n = 2, type = "lag"), by = NUTS_ID]
df[, B_Eemp_sh_delta := empl_sh_B_E - empl_sh_B_E_lag]
df <- as_tibble(df)


# Histogram of "verified"
ggplot(df, aes(x = log(verified))) +
  geom_histogram(bins = 100, fill = "blue", color = "black") +
  labs(title = "Histogram of Verified", x = "Verified", y = "Frequency") +
  theme_minimal()

# Histogram of "employmentbyindustry_sec_B.E"
ggplot(df, aes(x = empl_sh_B_E)) +
  geom_histogram(bins = 100, fill = "blue", color = "black") +
  labs(title = "Histogram of Verified", x = "Verified", y = "Frequency") +
  theme_minimal()
}

# Creating some dummy variables
{
# Create dummy is_rich if gdppercapitaconstantprices is above median (varies over time)
df <- df %>%
  mutate(is_rich = ifelse(gdppercapitaconstantprices > median(gdppercapitaconstantprices, na.rm = TRUE), 1, 0))

# Create dummy if NUTS_ID belongs to Eastern Europe (BG, CZ, EE, HU, LT, LV, PL, RO, SI, SK) (time-invariant)
df <- df %>%
  mutate(is_eastern = ifelse(grepl("BG|CZ|EE|HU|LT|LV|PL|RO|SI|SK", NUTS_ID), 1, 0))

# Create a dummy for each wave of the ETS (2005-2007, 2008-2012, 2013-2020, 2021-2023) (based on year variable, not clear where the difference is to year-fixed effects, but I'll include it for now)
df <- df %>%
  mutate(ets_wave = case_when(
    year >= 2005 & year <= 2007 ~ "2005-2007",
    year >= 2008 & year <= 2012 ~ "2008-2012",
    year >= 2013 & year <= 2020 ~ "2013-2020",
    year >= 2021 & year <= 2023 ~ "2021-2023",
    TRUE ~ NA_character_
  ))

# Create a dummy only for the two most recent waves
df <- df %>%
  mutate(ets_wave_recent = ifelse(year >= 2013 & year <= 2023, 1, 0))

# Create country variable as the first two characters of NUTS_ID
df <- df %>%
  mutate(country = substr(NUTS_ID, 1, 2))


}

###############
# Regressions # 
###############
## Dependent variables: 

# employmentbyindustry_sec_B.E 
# hoursworkedbyindustry.NACE2._sec_B.E
# grossfixedcapitalformationbyindustry_sec_B.E
# valueaddedperindustry_sec_B.E

# 1. Fixed-Effect Regression: Checking for heterogenous spatial effects
# Two-way clustered errors (NUTS_ID and year)
# Year fixed effects (cannot use NUTS_ID because of time-invariant dummy is_eastern)
model1 <- feols(c(employmentbyindustry_sec_B.E*1000, 
                  hoursworkedbyindustry.NACE2._sec_B.E, 
                  grossfixedcapitalformationbyindustry_sec_B.E, 
                  valueaddedperindustry_sec_B.E) # depvars
                ~ verified_delta + is_eastern + is_rich + verified_delta*is_rich + verified_delta*is_eastern + gdppercapitaconstantprices # indepvars
                | year, data = df, # FEs
                vcov = "twoway", # two-way clustered errors
                panel.id = c("NUTS_ID", "year")) 
summary(model1)

# 2. Fixed-Effect Regression: Including wave fixed effects to check for heterogenous effects across ETS waves
# Two-way clustered errors (NUTS_ID, year, and ets_wave)
model2 <- feols(c(employmentbyindustry_sec_B.E*1000, 
                  hoursworkedbyindustry.NACE2._sec_B.E, 
                  grossfixedcapitalformationbyindustry_sec_B.E, 
                  valueaddedperindustry_sec_B.E) 
                ~ verified_delta + gdppercapitaconstantprices 
                | NUTS_ID + ets_wave + year, data = df, 
                cluster = c("NUTS_ID", "year", "ets_wave"), 
                panel.id = c("NUTS_ID", "year"))
summary(model2)

# 3. Two-way Fixed-Effects Regression
# Sparse fixed effects (NUTS_ID and year)
model3 <- feols(c(employmentbyindustry_sec_B.E*1000, 
                  hoursworkedbyindustry.NACE2._sec_B.E, 
                  grossfixedcapitalformationbyindustry_sec_B.E, 
                  valueaddedperindustry_sec_B.E) 
                ~ verified_delta + gdppercapitaconstantprices 
                | NUTS_ID + year, data = df, 
                vcov = "twoway", 
                panel.id = c("NUTS_ID", "year"))
summary(model3)

# 4. Country and Year Fixed Effects
# Two-way clustered errors (NUTS_ID and year)
model4 <- feols(c(employmentbyindustry_sec_B.E*1000, 
                  hoursworkedbyindustry.NACE2._sec_B.E, 
                  grossfixedcapitalformationbyindustry_sec_B.E, 
                  valueaddedperindustry_sec_B.E) 
                ~ verified_delta + gdppercapitaconstantprices 
                | country + year, data = df, 
                vcov = "twoway", 
                panel.id = c("NUTS_ID", "year"))
summary(model4)

# Check for interaction with ets_wave_recent (two most recent waves of the ETS, 2013-2023)
model5 <- feols(c(employmentbyindustry_sec_B.E*1000, 
                  hoursworkedbyindustry.NACE2._sec_B.E, 
                  grossfixedcapitalformationbyindustry_sec_B.E, 
                  valueaddedperindustry_sec_B.E) 
                ~ verified_delta*ets_wave_recent + gdppercapitaconstantprices 
                | NUTS_ID, data = df, 
                vcov = "twoway", 
                panel.id = c("NUTS_ID", "year"))
summary(model5)

