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

# Load the data sets

data_path <- "C:/WBG/GitHub/WWBI"

# Load the data correctly

data_wwbi <- read_dta(file.path(data_path, "Data", "data_wwbi.dta"))



#Load GDP pc IMF for countries

gdp_pc      <- read_dta(file.path(data_path, "/Data/data_gdp.dta"))


#gdp_pc <-  read_excel("Data/IMF_GDP.xls", 
                      #sheet = "gdp") #228 obs


#Transform the data sets into a data.table

data_table <- as.data.table(data_wwbi)

data_table_gdp <- as.data.table(gdp_pc)

#View data

View(data_wwbi)
head(data_wwbi)
n_distinct(data_wwbi)
nrow(data_wwbi) # 61004 observations 
glimpse(data_wwbi)


#View data gdp

View(data_table_gdp)
head(data_table_gdp)
n_distinct(data_table_gdp)
nrow(data_table_gdp) # 228 observations 
glimpse(data_table_gdp)


#Part II ----

#Set data path 

# Add continent column

# Ensure data_wwbi is a data.table


setDT(data_wwbi)  

# Assign World Bank regions using countrycode()

data_wwbi[, wb_region := countrycode(country_name, origin = "country.name", destination = "region")]

unique(data_wwbi[, .(country_name, wb_region)])


# Manually assign continent for unmatched countries


data_wwbi[is.na(wb_region) & country_name == "Micronesia", wb_region := "East Asia & Pacific"]


# Get the latest World Bank country metadata, including income groups

wb_metadata <- wb_cachelist$countries[, c("iso3c", "income_level")]

# Ensure your data set has ISO3 country codes

data_wwbi[, iso3c := countrycode(country_name, origin = "country.name", destination = "iso3c")]

# Manually assign missing ISO3C codes

data_wwbi[country_name == "Kosovo", iso3c := "XKX"]
data_wwbi[country_name == "Micronesia", iso3c := "FSM"]

wb_metadata <- wb_metadata %>% rename(income_group = income_level)

# Merge income group data

data_wwbi <- merge(data_wwbi, wb_metadata, by = "iso3c", all.x = TRUE)

# Rename column for clarity

setnames(data_wwbi, "income_group", "income_level")

#Drop pvalue in the indicator column

data_wwbi <- data_wwbi[!grepl("^P-Value:", data_wwbi$indicator_name), ]


#Load gdp data base 

data_gdp <- read_dta(file.path(data_path, "Data/data_gdp.dta"))

# Load world spatial data

world_spdf <- ne_countries(scale = "medium", returnclass = "sf")

# Create a color palette for countries

color_palette <- colorFactor(c("lightgreen", "lightgray"), domain = c("reported", "not_reported"))


colnames(world_spdf)[colnames(world_spdf) == "name"] <- "country_name"

# Save the spatial dataset in the Shiny folder

st_write(world_spdf, file.path(data_path, "Data/world_spatial.gpkg"), , delete_layer = TRUE)

#Countries 

countries <- c(
  "Aruba", "Afghanistan", "Angola", "Anguilla", "Albania", "United Arab Emirates", 
  "Argentina", "Armenia", "Antigua and Barbuda", "Australia", "Austria", "Azerbaijan", 
  "Burundi", "Belgium", "Benin", "Burkina Faso", "Bangladesh", "Bulgaria", 
  "Bahrain", "Bahamas", "The, Bosnia and Herzegovina", "Belarus", "Belize", 
  "Bermuda", "Bolivia", "Brazil", "Barbados", "Brunei Darussalam", "Bhutan", 
  "Botswana", "Central African Republic", "Canada", "Switzerland", "Chile", 
  "China", "Cote d'Ivoire", "Cameroon", "Congo, Republic of", "Colombia", 
  "Comoros", "Cabo Verde", "Costa Rica", "Curacao", "Cayman Islands", 
  "Cyprus", "Czech Republic", "Germany", "Djibouti", "Dominica", "Denmark", "Dominican Republic", 
  "Algeria", "Ecuador", "Egypt", "Arab Republic of", "Eritrea", "Spain", 
  "Estonia", "Ethiopia", "Finland", "Fiji", "France", "Micronesia", 
  "Federated States of, Gabon", "United Kingdom", "Georgia", 
  "Ghana", "Guinea", "Gambia", "The, Guinea-Bissau", "Equatorial Guinea", 
  "Greece", "Grenada", "Guatemala", "Guyana", "Hong Kong SAR", "China", "Honduras", 
  "Croatia", "Haiti", "Hungary", "Indonesia", "India", "Ireland", "Iran", 
  "Islamic Republic of, Iraq", "Iceland", "Israel", "Italy", "Jamaica", "Jordan", 
  "Japan", "Kazakhstan", "Kenya", "Kyrgyz Republic", "Cambodia", "Kiribati", "St. Kitts and Nevis", 
  "Korea, Republic of, Kuwait", "Lao People's Democratic Republic, Lebanon", "Liberia", 
  "Libya", "St. Lucia", "Sri Lanka", "Lesotho", "Lithuania", "Luxembourg", "Latvia", 
  "Macao SAR", "China", "Morocco", "Moldova", "Madagascar", "Maldives", "Mexico", 
  "Marshall Islands", "North Macedonia", "Mali", "Malta", "Myanmar", "Montenegro", "Mongolia", 
  "Mozambique", "Mauritania", "Montserrat", "Mauritius", "Malawi", "Malaysia", "Namibia", 
  "Niger", "Nigeria", "Nicaragua", "Netherlands", "Norway", "Nepal", "Nauru", "New Zealand", 
  "Oman", "Pakistan", "Panama", "Peru", "Philippines", "Palau", "Papua New Guinea", 
  "Poland", "Puerto Rico", "Portugal", "Paraguay", "Qatar", "Romania", 
  "Russian Federation", "Rwanda", "Saudi Arabia", "Sudan", "Senegal", 
  "Singapore", "Solomon Islands", "Sierra Leone", "El Salvador", "San Marino", 
  "Somalia", "South Sudan", "Sao Tome and Principe", "Suriname", "Slovak Republic", 
  "Slovenia", "Sweden", "Eswatini", "Sint Maarten (Dutch part)", "Seychelles", 
  "Syrian Arab Republic", "Turks and Caicos Islands", "Chad", 
  "Togo", "Thailand", "Tajikistan", "Turkmenistan", "Tonga", 
  "Trinidad and Tobago", "Tunisia", "Türkiye", "Tuvalu", "Taiwan", 
  "China", "Tanzania", "Uganda", "Ukraine", "Uruguay", "United States", 
  "Uzbekistan", "St. Vincent and the Grenadines", "Venezuela", 
  "Republica Bolivariana de", "Vietnam", "Vanuatu", "Samoa", "Kosovo","South Africa",
  "Zambia", "Zimbabwe", "Americas", "Asia", "Europe", "MENA", "Oceania", "Sub-Saharan Africa")

#Drop pvalue in the indicator column

data_wwbi <- data_wwbi[!grepl("^P-Value:", data_wwbi$indicator_name), ]

# Extract available years and countries for select inputs

years <- as.character(2000:2022)  # Years are 2000 to 2022 based on column names in your data

countries <- unique(data_wwbi$country_name)  # Extract unique country names from the data set


indicator <- unique(data_wwbi$indicator_name)

# Transform data.table to a data.frame 

data_wwbi <- as.data.frame(data_wwbi)

# Filter the data using dplyr

selected_data_long <- data_wwbi %>%
  filter(indicator_name == indicator & country_name %in% countries) %>%
  select(country_name, indicator_name,wb_region,income_level,  starts_with("year_"))  # Select relevant columns


# Reshape the data using pivot_longer

selected_data_long <- selected_data_long %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value))  # Remove rows with NA values

selected_data_long <- selected_data_long %>%
  mutate(value_percentage = value * 100)


#Save data set

write_dta(selected_data_long, file.path(data_path, "Data/selected_data_long.dta"))

#Save data set

write_dta(data_gdp, file.path(data_path, "Data/data_gdp.dta"))


# View the reshaped data

print(selected_data_long)

print(data_gdp)

data_wwbi_long <- data_wwbi %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #4096 obs


regional_mean<- data_wwbi_long %>%
  group_by(wb_region, year, indicator_name) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')


regional_mean <- regional_mean %>%
  rename(country_name = wb_region)

regional_mean <- regional_mean %>%
  rename(value = mean_value)

data_wwbi_long <- bind_rows(data_wwbi_long, regional_mean)

income_mean <- data_wwbi_long %>%
  group_by(income_level, year, indicator_name) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')


income_mean <- income_mean %>%
  rename(country_name = income_level)

income_mean <- income_mean %>%
  rename(value = mean_value)


data_wwbi_long <- bind_rows(data_wwbi_long, income_mean)

#Drop Na observations in the column country_name

#data_wwbi_long <- data_wwbi_long %>% filter(!is.na(country_name))

#Save data set

write_dta(data_wwbi_long, file.path(data_path, "Data/data_wwbi_long.dta"))



# Filter the data for the specific indicator "Wage bill as a percentage of Public Expenditure"

wage_bill_publicexp <- data_wwbi_long[data_wwbi_long$indicator_name == "Wage bill as a percentage of Public Expenditure", ]

#Save data set

write_dta(wage_bill_publicexp, file.path(data_path, "Data/wage_bill_publicexp.dta"))



# Filter the data for the specific indicator "Wage bill as a percentage of GDP"

wage_bill_gdp <- data_wwbi_long[data_wwbi_long$indicator_name == "Wage bill as a percentage of GDP", ]

#Save data set

write_dta(wage_bill_gdp, file.path(data_path, "Data/wage_bill_gdp.dta"))



# Filter the data for the specific indicator "Public sector employment, as a share of formal employment and paid employment "

public_sector_emp <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Public sector employment, as a share of formal employment", 
                                                                         "Public sector employment, as a share of paid employment", 
                                                                         "Public sector employment, as a share of total employment"), ]

public_sector_emp_temp <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Public sector employment, as a share of formal employment", 
                                                                              "Public sector employment, as a share of paid employment"), ]



public_sector_emp_temp <- public_sector_emp_temp %>%
  select(year, indicator_name, value,wb_region, country_name) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
                                  "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
                                  "Public sector employment, as a share of total employment"= "as a share of total employment"))


# Ensure all numeric columns are converted before saving
public_sector_emp_temp <- public_sector_emp_temp %>%
  mutate(
    value = as.numeric(value),  # Convert haven-labelled values to numeric
    value_percentage = value * 100
  )



#Save data set

saveRDS(public_sector_emp_temp, file.path(data_path, "Data", "public_sector_emp_temp.rds"))


public_sector_emp <- public_sector_emp %>%
  mutate(value_percentage = value * 100)


public_sector_emp <- public_sector_emp %>%
  select(year, indicator_name, value, country_name, wb_region, value_percentage) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
                                  "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
                                  "Public sector employment, as a share of total employment"= "as a share of total employment"))



#Save data set

saveRDS(public_sector_emp, file.path(data_path, "Data", "public_sector_emp.rds"))




# Keep the last year available for each country

public_sector_emp_temp_last <- public_sector_emp %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, indicator_name, wb_region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data

# Ensure all numeric columns are converted before saving
public_sector_emp_temp_last <- public_sector_emp_temp_last %>%
  mutate(
    value = as.numeric(value),  # Convert haven-labelled values to numeric
    value_percentage = value * 100
  )




#Save data set

saveRDS(public_sector_emp_temp_last, file.path(data_path, "Data", "public_sector_emp_temp_last.rds"))



# Step 1: Filter for relevant indicators and convert to percentage

public_sector_workforce <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Education workers, as a share of public paid employees", 
    "Health workers, as a share of public paid employees", 
    "Public Administration workers, as a share of public paid employees"
  )) %>%
  mutate(value_percentage = value * 100)

public_sector_workforce <- public_sector_workforce %>%
  mutate(indicator_name = ifelse(indicator_name == "Education workers, as a share of public paid employees", "Education", indicator_name))

public_sector_workforce <- public_sector_workforce %>%
  mutate(indicator_name = ifelse(indicator_name == "Health workers, as a share of public paid employees", "Health", indicator_name))

public_sector_workforce <- public_sector_workforce %>%
  mutate(indicator_name = ifelse(indicator_name == "Public Administration workers, as a share of public paid employees", "Public Administration", indicator_name))


# Now compute the "Other" share per country and year


public_sector_workforce <- public_sector_workforce %>%
  group_by(country_name, year, wb_region) %>%
  mutate(other_value = 100 - sum(value_percentage, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Create a new row for the 'Other' indicator
other_rows <- public_sector_workforce %>%
  group_by(country_name, year, wb_region) %>%
  summarise(
    indicator_name = "Other",
    value_percentage = first(other_value),
    .groups = "drop"
  )


# Step 4: Bind the new 'Other' rows to the original data

public_sector_workforce <- bind_rows(public_sector_workforce, other_rows)


# Keep the first and  last year available for each country

public_sector_workforce_first_last <- public_sector_workforce %>%
  filter(!is.na(value_percentage)) %>%
  group_by(country_name, indicator_name, wb_region) %>%
  filter(year == max(year, na.rm = TRUE) | year == min(year, na.rm = TRUE)) %>%
  ungroup() 


public_sector_workforce_first_last <- public_sector_workforce_first_last %>%
  mutate(
    indicator_name = as.character(indicator_name),
    country_name = as.character(country_name),
    value_percentage = as.numeric(value_percentage)
  )

sapply(public_sector_workforce_first_last, class)


#Save data set

write_dta(public_sector_workforce, file.path(data_path, "Data/public_sector_workforce.dta"))

# Step 1: Filter and convert to percentage
public_sector_workforce_clean <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Education workers, as a share of public paid employees", 
    "Health workers, as a share of public paid employees", 
    "Public Administration workers, as a share of public paid employees"
  )) %>%
  mutate(value_percentage = value * 100)

# Step 2: For each country + indicator, get the latest available year
latest_values <- public_sector_workforce_clean %>%
  group_by(country_name, indicator_name, wb_region) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(country_name, indicator_name, wb_region, year) %>%
  summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")

# Step 3: Compute 'Other' as 100 - sum of the 3 latest indicator values
other_values <- latest_values %>%
  group_by(country_name, wb_region) %>%
  summarise(
    indicator_name = "Other",
    value_percentage = 100 - sum(value_percentage, na.rm = TRUE),
    year = NA,  # Optional: or max(year), but not meaningful here
    .groups = "drop"
  )

# Step 4: Bind the 'Other' row
public_sector_workforce_clean <- bind_rows(latest_values, other_values)

# Step 5: Clean indicator names
public_sector_workforce_clean <- public_sector_workforce_clean %>%
  mutate(indicator_name = case_when(
    indicator_name == "Education workers, as a share of public paid employees" ~ "Education",
    indicator_name == "Health workers, as a share of public paid employees" ~ "Health",
    indicator_name == "Public Administration workers, as a share of public paid employees" ~ "Public Administration",
    TRUE ~ indicator_name
  ))

# Step 6: Remove region-aggregate names from country list
public_sector_workforce_clean <- public_sector_workforce_clean %>%
  filter(!country_name %in% unique(public_sector_workforce_clean$wb_region))

# Step 7: Region summary (mean of latest indicator values, excluding 'Other')
region_summary_partial <- public_sector_workforce_clean %>%
  filter(indicator_name != "Other") %>%
  group_by(wb_region, indicator_name) %>%
  summarise(mean_value = mean(value_percentage, na.rm = TRUE), .groups = "drop")

# Step 8: Compute 'Other' for the region
region_other <- region_summary_partial %>%
  group_by(wb_region) %>%
  summarise(
    indicator_name = "Other",
    mean_value = 100 - sum(mean_value, na.rm = TRUE),
    .groups = "drop"
  )

# Step 9: Combine with partial summary
region_summary <- bind_rows(region_summary_partial, region_other)

# Step 10: Add region aggregates as pseudo-"countries"
region_as_country <- region_summary %>%
  transmute(
    country_name = wb_region,
    indicator_name,
    value_percentage = mean_value,
    year = NA,
    wb_region,
    is_region = TRUE
  )

# Step 11: Final dataset
public_sector_workforce_clean <- public_sector_workforce_clean %>%
  # Remove old incorrect region-level "Other" rows
  filter(!(country_name %in% unique(wb_region) & indicator_name == "Other")) %>%
  mutate(is_region = FALSE) %>%
  bind_rows(region_as_country)

# Step 10: Save it
write_dta(public_sector_workforce_clean, file.path(data_path, "Data/public_sector_workforce_clean.dta"))


# Filter the data for the specific indicator "Characteristics of the gender workforce"


gender_workforce <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Females, as a share of public paid employees", 
                                                                        "Females, as a share of private paid employees"), ]

#Rename Indicator 

gender_workforce <- gender_workforce %>%
  mutate(indicator_name = ifelse(indicator_name == "Females, as a share of public paid employees", "as a share of public paid employees", indicator_name))


gender_workforce <- gender_workforce %>%
  mutate(indicator_name = ifelse(indicator_name == "Females, as a share of private paid employees", "as a share of private paid employees", indicator_name))

#Multiply value

gender_workforce <- gender_workforce %>%
  mutate(value_percentage = value * 100)

#Save data set

write_dta(gender_workforce, file.path(data_path, "Data/gender_workforce.dta"))



# Filter GDP data for the year 2015

gdp_2015 <- data_gdp %>%
  filter(year == 2015) %>%
  select(country_name, value)

#Save data set

write_dta(gdp_2015, file.path(data_path, "Data/gdp_2015.dta"))



# Rename 'value' column to 'indicator_value' in data_indicators

data_indicator_wb <- wage_bill_publicexp %>%
  rename(indicator_value = value)

#Save data set

write_dta(data_indicator_wb, file.path(data_path, "Data/data_indicator_wb.dta"))



# Rename 'value' column to 'gdp_value' in data_gdp

gdp_2015 <- gdp_2015 %>%
  rename(gdp_value = value)

#Save data set

write_dta(gdp_2015, file.path(data_path, "Data/gdp_2015.dta"))




# Merge the datasets on 'country_name'


merged_data <- data_indicator_wb %>%
  left_join(gdp_2015, by = "country_name") %>%
  select(country_name, indicator_name, country_code, indicator_value, gdp_value, wb_region, year)


# Add the log of GDP as a new column

merged_data <- merged_data %>%
  mutate(log_gdp = log(gdp_value))


merged_data <- merged_data %>%
  filter(!is.na(indicator_value)) %>%  # Keep rows where `indicator_value` is not NA
  group_by(country_name, indicator_name, wb_region, log_gdp) %>%  
  filter(year == max(year, na.rm = TRUE)) %>%  # Get the last available year for each group
  ungroup()

#Save data set

write_dta(merged_data, file.path(data_path, "Data/merged_data.dta"))




# Tertiary education by sector 

tertiary_education <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Individuals with tertiary education as a share of public paid employees", 
                                                                          "Individuals with tertiary education as a share of private paid employees"), ]

tertiary_education <- tertiary_education %>%
  mutate(value_percentage = value * 100)

# Keep the last year available for each country

tertiary_education <- tertiary_education %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name,indicator_name, wb_region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data

#Rename the column indicator name 

tertiary_education <- tertiary_education %>%
  mutate(indicator_name = ifelse(indicator_name == "Individuals with tertiary education as a share of private paid employees", "as a share of private paid employees", indicator_name))

tertiary_education <- tertiary_education %>%
  mutate(indicator_name = ifelse(indicator_name == "Individuals with tertiary education as a share of public paid employees", "as a share of public paid employees", indicator_name))

#Save data set

write_dta(tertiary_education, file.path(data_path, "Data/tertiary_education.dta"))



# Public sector wage premium 

public_wage_premium <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Public sector wage premium (compared to all private employees)"), ]


public_wage_premium <- public_wage_premium %>%
  mutate(value_percentage = value * 100)

# Keep the last year available for each country

public_wage_premium <- public_wage_premium %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name,indicator_name, wb_region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data


#Save data set

write_dta(public_wage_premium, file.path(data_path, "Data/public_wage_premium.dta"))


#Public sector wage premium by education level (compared to private formal workers)


public_wage_premium_educ <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Public sector wage premium, by education level: Tertiary Education (compared to formal wage employees)", 
                                                                                "Public sector wage premium, by education level: Secondary Education (compared to formal wage employees)", 
                                                                                "Public sector wage premium, by education level: Primary Education (compared to formal wage employees)", 
                                                                                "Public sector wage premium, by education level: No Education (compared to formal wage employees)"), ]

public_wage_premium_educ <- public_wage_premium_educ %>%
  mutate(value_percentage = value * 100)

# Keep the last year available for each country

public_wage_premium_educ <- public_wage_premium_educ %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name,indicator_name, wb_region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data



public_wage_premium_educ <- public_wage_premium_educ %>%
  mutate(indicator_name = ifelse(indicator_name == "Public sector wage premium, by education level: Tertiary Education (compared to formal wage employees)", "Tertiary Education", indicator_name))

public_wage_premium_educ <- public_wage_premium_educ %>%
  mutate(indicator_name = ifelse(indicator_name == "Public sector wage premium, by education level: Secondary Education (compared to formal wage employees)", "Secondary Education", indicator_name))

public_wage_premium_educ <- public_wage_premium_educ %>%
  mutate(indicator_name = ifelse(indicator_name == "Public sector wage premium, by education level: Primary Education (compared to formal wage employees)", "Primary Education", indicator_name))


public_wage_premium_educ <- public_wage_premium_educ %>%
  mutate(indicator_name = ifelse(indicator_name == "Public sector wage premium, by education level: No Education (compared to formal wage employees)", "No Education", indicator_name))

#Save data set

write_dta(public_wage_premium_educ, file.path(data_path, "Data/public_wage_premium_educ.dta"))



# Filter the data for the specific indicator
gender_wage_premium <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Public sector wage premium, by gender: Female (compared to all private employees)", 
    "Public sector wage premium, by gender: Male (compared to all private employees)"
  )) %>%
  mutate(value = as.numeric(value))  # Ensure `value` is numeric before calculations

# Process gender wage premium data
gender_wage_premium <- gender_wage_premium %>%
  select(year, indicator_name, value, country_name, wb_region) %>%
  mutate(
    indicator_name = factor(indicator_name),
    indicator_label = recode(indicator_name, 
                             "Public sector wage premium, by gender: Female (compared to all private employees)" = "Female", 
                             "Public sector wage premium, by gender: Male (compared to all private employees)" = "Male"
    ),
    value_percentage = value * 100  # Convert to percentage
  )

gender_wage_premium <- gender_wage_premium %>%
  mutate(
    value_percentage = as.numeric(value_percentage),  # Convert haven-labelled to numeric
    value_rescaled = rescale(value_percentage, to = c(0, 1))  # Apply rescale safely
  )

# Convert all haven-labelled numeric columns to standard numeric before saving
gender_wage_premium <- gender_wage_premium %>%
  mutate(across(where(is.numeric), as.numeric))


#Save data set

saveRDS(gender_wage_premium, file.path(data_path, "Data", "gender_wage_premium.rds"))



# Keep the last year available for each country
gender_wage_premium_last <- gender_wage_premium %>%
  filter(!is.na(value)) %>%
  group_by(country_name, indicator_label, wb_region) %>%
  filter(year == max(year[!is.na(value)])) %>%
  ungroup()

# Convert value to percentage after ensuring numeric conversion
gender_wage_premium_last <- gender_wage_premium_last %>%
  mutate(value_percentage = value * 100)

gender_wage_premium_last <- gender_wage_premium_last %>%
  mutate(
    value_percentage = as.numeric(value_percentage),  # Convert haven-labelled to numeric
    value_rescaled = rescale(value_percentage, to = c(0, 1))  # Apply rescale safely
  )

# Convert all haven-labelled numeric columns to standard numeric before saving
gender_wage_premium_last <- gender_wage_premium_last %>%
  mutate(across(where(is.numeric), as.numeric))

#Save data set

saveRDS(gender_wage_premium_last, file.path(data_path, "Data", "gender_wage_premium_last.rds"))



# Filter the relevant indicators
gender_leadership <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Females, as a share of public paid employees by occupational group: Managers",
    "Females, as a share of public paid employees by occupational group: Clerks",
    "Females, as a share of private paid employees by occupational group: Managers",
    "Females, as a share of private paid employees by occupational group: Clerks"
  )) 

# Process gender leadership data
gender_leadership <- gender_leadership %>%
  select(year, indicator_name, value, country_name, wb_region) %>%
  mutate(
    indicator_name = factor(indicator_name),  # Convert to factor
    value = as.numeric(value),  # Ensure `value` is numeric
    indicator_label = recode(indicator_name, 
                             "Females, as a share of public paid employees by occupational group: Managers" = "Managers-Public", 
                             "Females, as a share of public paid employees by occupational group: Clerks" = "Clerks-Public", 
                             "Females, as a share of private paid employees by occupational group: Managers" = "Managers-Private",
                             "Females, as a share of private paid employees by occupational group: Clerks" = "Clerks-Private"
    )
  ) 

# Keep the last year available for each country
gender_leadership <- gender_leadership %>%
  filter(!is.na(value)) %>%   # Remove NA values
  group_by(country_name, indicator_label, wb_region) %>%
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year
  ungroup()

# Convert value to percentage
gender_leadership <- gender_leadership %>%
  mutate(value_percentage = value * 100)  

gender_leadership <- gender_leadership %>%
  mutate(
    value_percentage = as.numeric(value_percentage),  # Convert haven-labelled to numeric
    value_rescaled = rescale(value_percentage, to = c(0, 1))  # Apply rescale safely
  )

# Convert all haven-labelled numeric columns to standard numeric before saving
gender_leadership <- gender_leadership %>%
  mutate(across(where(is.numeric), as.numeric))

#Save data set

saveRDS(gender_leadership, file.path(data_path, "Data", "gender_leadership.rds"))



# Filter the relevant indicators
gender_wage_premiumpublic <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)", 
    "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)", 
    "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"
  )) %>%
  mutate(value = as.numeric(value))  # Convert to numeric before calculations

# Convert value to percentage
gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  mutate(value_percentage = value * 100)

# Calculate "Other" as 100 minus the sum of specified indicators
gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  group_by(country_name, year, wb_region) %>%
  mutate(
    other_value = 100 - sum(value_percentage[indicator_name %in% c(
      "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)",
      "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)",
      "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"
    )], na.rm = TRUE)
  ) %>%
  ungroup()

# Add "Other" category
gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  bind_rows(
    gender_wage_premiumpublic %>%
      filter(indicator_name %in% c(
        "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)",
        "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)",
        "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"
      )) %>%
      group_by(country_name, year, wb_region) %>%
      summarize(
        indicator_name = "Other",  # Set the indicator name to "Other"
        value_percentage = first(other_value),  # Replace the value with 'other_value'
        .groups = "drop"
      ) %>%
      ungroup()
  )

# Modify indicator labels for shorter text
gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  select(year, indicator_name, value, country_name, wb_region, value_percentage) %>%
  mutate(
    indicator_name = factor(indicator_name),
    indicator_label = recode(indicator_name, 
                             "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" = "Public Administration", 
                             "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)" = "Education", 
                             "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)" = "Health"
    )
  )

# Keep the last available year for each country
gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  filter(!is.na(value)) %>%
  group_by(country_name, indicator_label, wb_region) %>%
  filter(year == max(year[!is.na(value)])) %>%
  ungroup()

gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  mutate(
    value_percentage = as.numeric(value_percentage),  # Convert haven-labelled to numeric
    value_rescaled = rescale(value_percentage, to = c(0, 1))  # Apply rescale safely
  )

# Convert all haven-labelled numeric columns to standard numeric before saving
gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  mutate(across(where(is.numeric), as.numeric))

#Save data set

saveRDS(gender_wage_premiumpublic, file.path(data_path, "Data", "gender_wage_premiumpublic.rds"))

#Pay Compression 

# Filter the relevant indicators

pay_compression <- data_wwbi_long %>%
  filter(indicator_name %in% c(
    "Pay compression ratio in public sector (ratio of 90th/10th percentile earners)", 
    "Pay compression ratio in private sector (ratio of 90th/10th percentile earners)"
  )) %>%
  mutate(value = as.numeric(value))  # Convert to numeric before calculations


# Convert value to percentage

pay_compression <- pay_compression %>%
  mutate(value_percentage = value * 100)


# Keep the last available year for each country

pay_compression <- pay_compression %>%
  filter(!is.na(value)) %>%
  group_by(country_name, wb_region) %>%
  filter(year == max(year[!is.na(value)])) %>%
  ungroup()


pay_compression_wide <- pay_compression %>%
  select(country_name, indicator_name, value) %>%  # Keep only relevant columns
  pivot_wider(names_from = indicator_name, values_from = value)  # Convert long format to wide

#Rename columnns

colnames(pay_compression_wide)[colnames(pay_compression_wide) == "Pay compression ratio in public sector (ratio of 90th/10th percentile earners)"] <- "Public_Sector"
colnames(pay_compression_wide)[colnames(pay_compression_wide) == "Pay compression ratio in private sector (ratio of 90th/10th percentile earners)"] <- "Private_Sector"


#Save data base 

saveRDS(pay_compression_wide, file.path(data_path, "Data", "pay_compression_wide.rds"))



#end of script

