# Worldwide Bureaucracy Indicators
# 01. Data processing

### Libraries
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(DT)
library(maps)
library(mapdata)
library(leaflet)
library(rnaturalearth)
library(sf)
library(plotly)
library(officer)
library(flextable)
library(viridis)
library(here)
library(glue)
library(colourpicker)
library(wbstats)
library(htmlwidgets)
library(bs4Dash)
library(countrycode)
library(bslib)
library(lubridate)
library(scales)

### Loading data ----

data_path <- "C:/WBG/GitHub/WWBI"

data_wwbi <- read_dta(file.path(data_path, "Data", "data_wwbi.dta"))
gdp_pc    <- read_dta(file.path(data_path, "/Data/data_gdp.dta"))

data_table     <- as.data.table(data_wwbi)
data_table_gdp <- as.data.table(gdp_pc)

# ── Helper: keep latest year for countries, is_latest=TRUE for aggregates ─────
# Ensures exactly ONE row per group in cross-section datasets
keep_latest <- function(df, group_vars) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(max_year = suppressWarnings(max(year, na.rm = TRUE))) %>%
    filter(is_latest == TRUE | year == max_year) %>%
    filter(!(n() > 1 & (is_latest == FALSE | is.na(is_latest)))) %>%
    select(-max_year) %>%
    ungroup()
}

#Part II ----

setDT(data_wwbi)

data_wwbi[, wb_region := countrycode(country_name, origin = "country.name", destination = "region")]
data_wwbi[is.na(wb_region) & country_name == "Micronesia", wb_region := "East Asia & Pacific"]

wb_metadata <- wb_cachelist$countries[, c("iso3c", "income_level")]

data_wwbi[, iso3c := countrycode(country_name, origin = "country.name", destination = "iso3c")]
data_wwbi[country_name == "Kosovo",     iso3c := "XKX"]
data_wwbi[country_name == "Micronesia", iso3c := "FSM"]

wb_metadata <- wb_metadata %>% rename(income_group = income_level)
data_wwbi   <- merge(data_wwbi, wb_metadata, by = "iso3c", all.x = TRUE)
setnames(data_wwbi, "income_group", "income_level")

data_wwbi <- data_wwbi[!grepl("^P-Value:", data_wwbi$indicator_name), ]

data_gdp <- read_dta(file.path(data_path, "Data/data_gdp.dta"))

world_spdf <- ne_countries(scale = "medium", returnclass = "sf")
colnames(world_spdf)[colnames(world_spdf) == "name"] <- "country_name"
st_write(world_spdf, file.path(data_path, "Data/world_spatial.gpkg"), delete_layer = TRUE)

years     <- as.character(2000:2022)
data_wwbi <- as.data.frame(data_wwbi)
countries <- unique(data_wwbi$country_name)
indicator <- unique(data_wwbi$indicator_name)

selected_data_long <- data_wwbi %>%
  filter(indicator_name == indicator & country_name %in% countries) %>%
  select(country_name, indicator_name, wb_region, income_level, starts_with("year_")) %>%
  pivot_longer(cols = starts_with("year_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%
  filter(!is.na(value)) %>%
  mutate(value_percentage = value * 100)

write_dta(selected_data_long, file.path(data_path, "Data/selected_data_long.dta"))
write_dta(data_gdp,           file.path(data_path, "Data/data_gdp.dta"))

# ── Build long panel (country-level only, no aggregates yet) ──────────────────
data_wwbi_long <- data_wwbi %>%
  pivot_longer(cols = starts_with("year_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%
  filter(!is.na(value))

data_wwbi_countries <- data_wwbi_long

# ── PART A: Year-by-year aggregates (for time series in Shiny) ────────────────
regional_mean_ts <- data_wwbi_countries %>%
  filter(!is.na(wb_region)) %>%
  group_by(wb_region, year, indicator_name) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  rename(country_name = wb_region) %>%
  mutate(is_latest = FALSE)

income_mean_ts <- data_wwbi_countries %>%
  filter(!is.na(income_level)) %>%
  group_by(income_level, year, indicator_name) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  rename(country_name = income_level) %>%
  mutate(is_latest = FALSE)

global_mean_ts <- data_wwbi_countries %>%
  group_by(year, indicator_name) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    country_name = "Global",
    iso3c        = "GLB",
    country_code = "GLB",
    wb_region    = "All regions",
    income_level = "All incomes",
    is_latest    = FALSE
  )

# ── PART B: Latest-obs aggregates (for cross-country comparison) ──────────────
data_wwbi_latest <- data_wwbi_countries %>%
  filter(!is.na(value), !is.na(iso3c)) %>%
  group_by(country_name, indicator_name) %>%
  filter(year == max(year)) %>%
  ungroup()

regional_mean_latest <- data_wwbi_latest %>%
  filter(!is.na(wb_region)) %>%
  group_by(wb_region, indicator_name) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  rename(country_name = wb_region) %>%
  mutate(year = NA_real_, is_latest = TRUE)

income_mean_latest <- data_wwbi_latest %>%
  filter(!is.na(income_level)) %>%
  group_by(income_level, indicator_name) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  rename(country_name = income_level) %>%
  mutate(year = NA_real_, is_latest = TRUE)

global_mean_latest <- data_wwbi_latest %>%
  group_by(indicator_name) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    country_name = "Global",
    iso3c        = "GLB",
    country_code = "GLB",
    wb_region    = "All regions",
    income_level = "All incomes",
    year         = NA_real_,
    is_latest    = TRUE
  )

# ── Append everything once ────────────────────────────────────────────────────
data_wwbi_long <- bind_rows(
  data_wwbi_countries,
  regional_mean_ts,
  income_mean_ts,
  global_mean_ts,
  regional_mean_latest,
  income_mean_latest,
  global_mean_latest
)

write_dta(data_wwbi_long, file.path(data_path, "Data/data_wwbi_long.dta"))

# ── Downstream datasets ───────────────────────────────────────────────────────

# Wage bill as % of Public Expenditure
wage_bill_publicexp <- data_wwbi_long %>%
  filter(indicator_name == "Wage bill as a percentage of Public Expenditure")
write_dta(wage_bill_publicexp, file.path(data_path, "Data/wage_bill_publicexp.dta"))

# Wage bill as % of GDP
wage_bill_gdp <- data_wwbi_long %>%
  filter(indicator_name == "Wage bill as a percentage of GDP")
write_dta(wage_bill_gdp, file.path(data_path, "Data/wage_bill_gdp.dta"))

# Public sector employment
public_sector_emp <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Public sector employment, as a share of formal employment",
    "Public sector employment, as a share of paid employment",
    "Public sector employment, as a share of total employment"))

public_sector_emp_temp <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Public sector employment, as a share of formal employment",
    "Public sector employment, as a share of paid employment")) %>%
  select(year, indicator_name, value, wb_region, country_name, is_latest) %>%
  mutate(
    indicator_name   = factor(indicator_name),
    indicator_label  = recode(indicator_name,
                              "Public sector employment, as a share of formal employment" = "as a share of formal employment",
                              "Public sector employment, as a share of paid employment"   = "as a share of paid employment",
                              "Public sector employment, as a share of total employment"  = "as a share of total employment"),
    value            = as.numeric(value),
    value_percentage = value * 100
  )

saveRDS(public_sector_emp_temp, file.path(data_path, "Data", "public_sector_emp_temp.rds"))

public_sector_emp <- public_sector_emp %>%
  mutate(value_percentage = value * 100) %>%
  select(year, indicator_name, value, country_name, wb_region, value_percentage, is_latest) %>%
  mutate(
    indicator_name  = factor(indicator_name),
    indicator_label = recode(indicator_name,
                             "Public sector employment, as a share of formal employment" = "as a share of formal employment",
                             "Public sector employment, as a share of paid employment"   = "as a share of paid employment",
                             "Public sector employment, as a share of total employment"  = "as a share of total employment")
  )

saveRDS(public_sector_emp, file.path(data_path, "Data", "public_sector_emp.rds"))

# Cross-section: one row per country-indicator (keep_latest handles aggregates)
public_sector_emp_temp_last <- public_sector_emp %>%
  filter(!is.na(value)) %>%
  keep_latest(c("country_name", "indicator_name", "wb_region")) %>%
  mutate(
    value            = as.numeric(value),
    value_percentage = value * 100
  )

saveRDS(public_sector_emp_temp_last, file.path(data_path, "Data", "public_sector_emp_temp_last.rds"))

# Public sector workforce composition
public_sector_workforce <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Education workers, as a share of public paid employees",
    "Health workers, as a share of public paid employees",
    "Public Administration workers, as a share of public paid employees"
  )) %>%
  mutate(
    value_percentage = value * 100,
    indicator_name   = case_when(
      indicator_name == "Education workers, as a share of public paid employees"             ~ "Education",
      indicator_name == "Health workers, as a share of public paid employees"                ~ "Health",
      indicator_name == "Public Administration workers, as a share of public paid employees" ~ "Public Administration",
      TRUE ~ indicator_name
    )
  ) %>%
  group_by(country_name, year, wb_region) %>%
  mutate(other_value = 100 - sum(value_percentage, na.rm = TRUE)) %>%
  ungroup()

other_rows <- public_sector_workforce %>%
  group_by(country_name, year, wb_region) %>%
  summarise(indicator_name = "Other", value_percentage = first(other_value), .groups = "drop")

public_sector_workforce <- bind_rows(public_sector_workforce, other_rows)

write_dta(public_sector_workforce, file.path(data_path, "Data/public_sector_workforce.dta"))

# Clean workforce dataset
public_sector_workforce_clean <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Education workers, as a share of public paid employees",
    "Health workers, as a share of public paid employees",
    "Public Administration workers, as a share of public paid employees"
  )) %>%
  mutate(value_percentage = value * 100)

latest_values <- public_sector_workforce_clean %>%
  keep_latest(c("country_name", "indicator_name", "wb_region")) %>%
  group_by(country_name, indicator_name, wb_region, year) %>%
  summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")

other_values <- latest_values %>%
  group_by(country_name, wb_region) %>%
  summarise(
    indicator_name   = "Other",
    value_percentage = 100 - sum(value_percentage, na.rm = TRUE),
    year             = NA,
    .groups          = "drop"
  )

public_sector_workforce_clean <- bind_rows(latest_values, other_values) %>%
  mutate(indicator_name = case_when(
    indicator_name == "Education workers, as a share of public paid employees"             ~ "Education",
    indicator_name == "Health workers, as a share of public paid employees"                ~ "Health",
    indicator_name == "Public Administration workers, as a share of public paid employees" ~ "Public Administration",
    TRUE ~ indicator_name
  )) %>%
  filter(!country_name %in% unique(.$wb_region))

region_summary_partial <- public_sector_workforce_clean %>%
  filter(indicator_name != "Other") %>%
  group_by(wb_region, indicator_name) %>%
  summarise(mean_value = mean(value_percentage, na.rm = TRUE), .groups = "drop")

region_other <- region_summary_partial %>%
  group_by(wb_region) %>%
  summarise(indicator_name = "Other", mean_value = 100 - sum(mean_value, na.rm = TRUE), .groups = "drop")

region_summary <- bind_rows(region_summary_partial, region_other)

region_as_country <- region_summary %>%
  transmute(
    country_name     = wb_region,
    indicator_name,
    value_percentage = mean_value,
    year             = NA,
    wb_region,
    is_region        = TRUE
  )

public_sector_workforce_clean <- public_sector_workforce_clean %>%
  filter(!(country_name %in% unique(wb_region) & indicator_name == "Other")) %>%
  mutate(is_region = FALSE) %>%
  bind_rows(region_as_country)

write_dta(public_sector_workforce_clean, file.path(data_path, "Data/public_sector_workforce_clean.dta"))

# Gender workforce (full time series — no keep_latest, Shiny handles latest)
gender_workforce <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Females, as a share of public paid employees",
    "Females, as a share of private paid employees")) %>%
  mutate(
    indicator_name   = case_when(
      indicator_name == "Females, as a share of public paid employees"  ~ "as a share of public paid employees",
      indicator_name == "Females, as a share of private paid employees" ~ "as a share of private paid employees",
      TRUE ~ indicator_name
    ),
    value_percentage = value * 100
  )

write_dta(gender_workforce, file.path(data_path, "Data/gender_workforce.dta"))

# GDP 2015
gdp_2015 <- data_gdp %>%
  filter(year == 2015) %>%
  select(country_name, value) %>%
  rename(gdp_value = value)

write_dta(gdp_2015, file.path(data_path, "Data/gdp_2015.dta"))

# Merged data (uses time-series wage bill rows only)
merged_data <- wage_bill_publicexp %>%
  filter(is_latest == FALSE | is.na(is_latest)) %>%
  rename(indicator_value = value) %>%
  left_join(gdp_2015, by = "country_name") %>%
  select(country_name, indicator_name, country_code, indicator_value, gdp_value, wb_region, year) %>%
  mutate(log_gdp = log(gdp_value)) %>%
  filter(!is.na(indicator_value)) %>%
  group_by(country_name, indicator_name, wb_region, log_gdp) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup()

write_dta(merged_data, file.path(data_path, "Data/merged_data.dta"))

# Tertiary education — cross-section, one row per country
tertiary_education <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Individuals with tertiary education as a share of public paid employees",
    "Individuals with tertiary education as a share of private paid employees")) %>%
  mutate(value_percentage = value * 100) %>%
  filter(!is.na(value)) %>%
  keep_latest(c("country_name", "indicator_name", "wb_region")) %>%
  mutate(indicator_name = case_when(
    indicator_name == "Individuals with tertiary education as a share of private paid employees" ~ "as a share of private paid employees",
    indicator_name == "Individuals with tertiary education as a share of public paid employees"  ~ "as a share of public paid employees",
    TRUE ~ indicator_name
  ))

write_dta(tertiary_education, file.path(data_path, "Data/tertiary_education.dta"))

# Public wage premium — cross-section
public_wage_premium <- data_wwbi_long %>%
  filter(indicator_name == "Public sector wage premium (compared to all private employees)") %>%
  mutate(value_percentage = value * 100) %>%
  filter(!is.na(value)) %>%
  keep_latest(c("country_name", "indicator_name", "wb_region"))

write_dta(public_wage_premium, file.path(data_path, "Data/public_wage_premium.dta"))

# Public wage premium by education — cross-section
public_wage_premium_educ <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Public sector wage premium, by education level: Tertiary Education (compared to formal wage employees)",
    "Public sector wage premium, by education level: Secondary Education (compared to formal wage employees)",
    "Public sector wage premium, by education level: Primary Education (compared to formal wage employees)",
    "Public sector wage premium, by education level: No Education (compared to formal wage employees)")) %>%
  mutate(value_percentage = value * 100) %>%
  filter(!is.na(value)) %>%
  keep_latest(c("country_name", "indicator_name", "wb_region")) %>%
  mutate(indicator_name = case_when(
    indicator_name == "Public sector wage premium, by education level: Tertiary Education (compared to formal wage employees)"  ~ "Tertiary Education",
    indicator_name == "Public sector wage premium, by education level: Secondary Education (compared to formal wage employees)" ~ "Secondary Education",
    indicator_name == "Public sector wage premium, by education level: Primary Education (compared to formal wage employees)"   ~ "Primary Education",
    indicator_name == "Public sector wage premium, by education level: No Education (compared to formal wage employees)"        ~ "No Education",
    TRUE ~ indicator_name
  ))

write_dta(public_wage_premium_educ, file.path(data_path, "Data/public_wage_premium_educ.dta"))

# Gender wage premium (full time series for trend chart)
gender_wage_premium <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Public sector wage premium, by gender: Female (compared to all private employees)",
    "Public sector wage premium, by gender: Male (compared to all private employees)"
  )) %>%
  mutate(value = as.numeric(value)) %>%
  select(year, indicator_name, value, country_name, wb_region, is_latest) %>%
  mutate(
    indicator_name   = factor(indicator_name),
    indicator_label  = recode(indicator_name,
                              "Public sector wage premium, by gender: Female (compared to all private employees)" = "Female",
                              "Public sector wage premium, by gender: Male (compared to all private employees)"   = "Male"),
    value_percentage = as.numeric(value * 100),
    value_rescaled   = rescale(value * 100, to = c(0, 1))
  ) %>%
  mutate(across(where(is.numeric), as.numeric))

saveRDS(gender_wage_premium, file.path(data_path, "Data", "gender_wage_premium.rds"))

# Gender wage premium last — cross-section, one row per country
gender_wage_premium_last <- gender_wage_premium %>%
  filter(!is.na(value)) %>%
  keep_latest(c("country_name", "indicator_label", "wb_region")) %>%
  mutate(
    value_percentage = as.numeric(value * 100),
    value_rescaled   = rescale(value * 100, to = c(0, 1))
  ) %>%
  mutate(across(where(is.numeric), as.numeric))

saveRDS(gender_wage_premium_last, file.path(data_path, "Data", "gender_wage_premium_last.rds"))

# Gender leadership — cross-section
gender_leadership <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Females, as a share of public paid employees by occupational group: Managers",
    "Females, as a share of public paid employees by occupational group: Clerks",
    "Females, as a share of private paid employees by occupational group: Managers",
    "Females, as a share of private paid employees by occupational group: Clerks"
  )) %>%
  select(year, indicator_name, value, country_name, wb_region, is_latest) %>%
  mutate(
    indicator_name  = factor(indicator_name),
    value           = as.numeric(value),
    indicator_label = recode(indicator_name,
                             "Females, as a share of public paid employees by occupational group: Managers"  = "Managers-Public",
                             "Females, as a share of public paid employees by occupational group: Clerks"    = "Clerks-Public",
                             "Females, as a share of private paid employees by occupational group: Managers" = "Managers-Private",
                             "Females, as a share of private paid employees by occupational group: Clerks"   = "Clerks-Private")
  ) %>%
  filter(!is.na(value)) %>%
  keep_latest(c("country_name", "indicator_label", "wb_region")) %>%
  mutate(
    value_percentage = as.numeric(value * 100),
    value_rescaled   = rescale(value * 100, to = c(0, 1))
  ) %>%
  mutate(across(where(is.numeric), as.numeric))

saveRDS(gender_leadership, file.path(data_path, "Data", "gender_leadership.rds"))

# Gender wage premium in public sector — cross-section
gender_wage_premiumpublic <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)",
    "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)",
    "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"
  )) %>%
  mutate(value = as.numeric(value), value_percentage = value * 100) %>%
  filter(!is.na(value)) %>%
  keep_latest(c("country_name", "indicator_name", "wb_region")) %>%
  mutate(
    indicator_label = case_when(
      indicator_name == "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" ~ "Public Administration",
      indicator_name == "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)"            ~ "Education",
      indicator_name == "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"               ~ "Health",
      TRUE ~ indicator_name
    ),
    value_percentage = as.numeric(value_percentage),
    value_rescaled   = rescale(value_percentage, to = c(0, 1))
  ) %>%
  mutate(across(where(is.numeric), as.numeric))

saveRDS(gender_wage_premiumpublic, file.path(data_path, "Data", "gender_wage_premiumpublic.rds"))

# Pay compression — cross-section
pay_compression <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Pay compression ratio in public sector (ratio of 90th/10th percentile earners)",
    "Pay compression ratio in private sector (ratio of 90th/10th percentile earners)"
  )) %>%
  mutate(value = as.numeric(value), value_percentage = value * 100) %>%
  filter(!is.na(value)) %>%
  keep_latest(c("country_name", "wb_region", "indicator_name"))

pay_compression_wide <- pay_compression %>%
  group_by(country_name, indicator_name) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = indicator_name, values_from = value) %>%
  rename(
    Public_Sector  = "Pay compression ratio in public sector (ratio of 90th/10th percentile earners)",
    Private_Sector = "Pay compression ratio in private sector (ratio of 90th/10th percentile earners)"
  )

saveRDS(pay_compression_wide, file.path(data_path, "Data", "pay_compression_wide.rds"))

# end of script