# Shiny Dashboard for Worldwide Bureaucracy Indicators

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
library(DT)
library(rnaturalearth)
library(sf)
library(plotly)
library(officer)
library(flextable)
library(rvg)
library(viridis)
library(here)
library(glue)
library(colourpicker)
library(rmarkdown)
library(quarto)
library(tinytex)
library(orca)
library(bslib)
library(shinythemes)
library(countrycode)
library(bs4Dash)
library(wbstats)




### INITIAL COMMANDS ----

#Set data path 

data_path <- "C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/Data/Intermediate"

#Load indicators data set 

data_wwbi <- read_dta(file.path(data_path, "data_wwbi.dta"))

# Add continent column

# Ensure data_wwbi is a data.table
setDT(data_wwbi)  

# Assign continents using countrycode()

data_wwbi[, continent := countrycode(country_name, origin = "country.name", destination = "continent")]

# Manually assign continent for unmatched countries

data_wwbi[country_name == "Kosovo", continent := "Europe"]
data_wwbi[country_name == "Micronesia", continent := "Oceania"]

# Define MENA countries

mena_countries <- c("Algeria", "Bahrain", "Egypt", "Iran", "Iraq", "Israel", "Jordan", 
                    "Kuwait", "Lebanon", "Libya", "Morocco", "Oman", "Palestine", "Qatar", 
                    "Saudi Arabia", "Syria", "Tunisia", "United Arab Emirates", "Yemen")

# Create a region column that classifies Africa into MENA and Sub-Saharan Africa

data_wwbi[, region := fifelse(country_name %in% mena_countries, "MENA",
                              fifelse(continent == "Africa", "Sub-Saharan Africa", continent))]


# Get the latest World Bank country metadata, including income groups

wb_metadata <- wb_cachelist$countries[, c("iso3c", "income_level")]

# Ensure your dataset has ISO3 country codes

data_wwbi[, iso3c := countrycode(country_name, origin = "country.name", destination = "iso3c")]

wb_metadata <- wb_metadata %>% rename(income_group = income_level)

# Merge income group data

data_wwbi <- merge(data_wwbi, wb_metadata, by = "iso3c", all.x = TRUE)

# Rename column for clarity

setnames(data_wwbi, "income_group", "income_level")


#Load gdp data base 

data_gdp <- read_dta(file.path(data_path, "data_gdp.dta"))

# Load world spatial data

world_spdf <- ne_countries(scale = "medium", returnclass = "sf")

# Create a color palette for countries

color_palette <- colorFactor(c("lightgreen", "lightgray"), domain = c("reported", "not_reported"))


selectInput("countrySelect", "Select Country", 
            choices = unique(data_wwbi$country_name), 
            multiple = TRUE)

colnames(world_spdf)[colnames(world_spdf) == "name"] <- "country_name"


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

# Assign continents using countrycode()

# Convert data_wwbi to a data.table 

setDT(data_wwbi)

# Now add the continent column using :=

data_wwbi[, continent := countrycode(country_name, origin = "country.name", destination = "continent")]




# Manually assign continent for unmatched countries

data_wwbi[country_name == "Kosovo", continent := "Europe"]
data_wwbi[country_name == "Micronesia", continent := "Oceania"]

# Define MENA countries

mena_countries <- c("Algeria", "Bahrain", "Egypt", "Iran", "Iraq", "Israel", "Jordan", 
                    "Kuwait", "Lebanon", "Libya", "Morocco", "Oman", "Palestine", "Qatar", 
                    "Saudi Arabia", "Syria", "Tunisia", "United Arab Emirates", "Yemen")

# Create a region column that classifies Africa into MENA and Sub-Saharan Africa

data_wwbi[, region := fifelse(country_name %in% mena_countries, "MENA",
                              fifelse(continent == "Africa", "Sub-Saharan Africa", continent))]

data_wwbi <- as.data.frame(data_wwbi)

# Filter the data using dplyr

selected_data_long <- data_wwbi %>%
  filter(indicator_name == indicator & country_name %in% countries) %>%
  select(country_name, indicator_name,region, starts_with("year_"))  # Select relevant columns


# Reshape the data using pivot_longer

selected_data_long <- selected_data_long %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value))  # Remove rows with NA values


# Reshape the data using pivot_longer gdp data base 

data_gdp <- data_gdp %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value))  # Remove rows with NA values




# View the reshaped data

print(selected_data_long)

print(data_gdp)


# Filter the data for the specific indicator "Wage bill as a percentage of Public Expenditure"

wage_bill_publicexp <- data_wwbi[data_wwbi$indicator_name == "Wage bill as a percentage of Public Expenditure", ]



wage_bill_publicexp <- wage_bill_publicexp %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #4096 obs


regional_mean <- wage_bill_publicexp %>%
  filter(indicator_name == "Wage bill as a percentage of Public Expenditure") %>%
  group_by(region, year, indicator_name) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')


regional_mean <- regional_mean %>%
  rename(country_name = region)

regional_mean <- regional_mean %>%
  rename(value = mean_value)

wage_bill_publicexp <- bind_rows(wage_bill_publicexp, regional_mean)


income_mean <- wage_bill_publicexp %>%
  filter(indicator_name == "Wage bill as a percentage of Public Expenditure") %>%
  group_by(income_level, year, indicator_name) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')


income_mean <- income_mean %>%
  rename(country_name = income_level)

income_mean <- income_mean %>%
  rename(value = mean_value)


wage_bill_publicexp <- bind_rows(wage_bill_publicexp, income_mean)

# Filter the data for the specific indicator "Wage bill as a percentage of GDP"

wage_bill_gdp <- data_wwbi[data_wwbi$indicator_name == "Wage bill as a percentage of GDP", ]


wage_bill_gdp <- wage_bill_gdp %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #4104 obs

regional_mean_wbgdp <- wage_bill_gdp %>%
  filter(indicator_name == "Wage bill as a percentage of GDP") %>%
  group_by(region, year, indicator_name) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')


regional_mean_wbgdp <- regional_mean_wbgdp %>%
  rename(country_name = region)

regional_mean <- regional_mean %>%
  rename(mean_value = value)

wage_bill_gdp <- bind_rows(wage_bill_gdp, regional_mean_wbgdp)


# Filter the data for the specific indicator "Public sector employment, as a share of formal employment and paid employment "

public_sector_emp <- data_wwbi[data_wwbi$indicator_name %in% c("Public sector employment, as a share of formal employment", 
                                                               "Public sector employment, as a share of paid employment", 
                                                               "Public sector employment, as a share of total employment"), ]

public_sector_emp_temp <- data_wwbi[data_wwbi$indicator_name %in% c("Public sector employment, as a share of formal employment", 
                                                               "Public sector employment, as a share of paid employment"), ]




public_sector_emp_temp <- public_sector_emp_temp %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #2015 obs


public_sector_emp <- public_sector_emp %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #2015 obs



public_sector_emp_temp <- public_sector_emp_temp %>%
  select(year, indicator_name, value,region, country_name) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
                                  "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
                                  "Public sector employment, as a share of total employment"= "as a share of total employment"))


public_sector_emp <- public_sector_emp %>%
  select(year, indicator_name, value, country_name, region) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
                                  "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
                                  "Public sector employment, as a share of total employment"= "as a share of total employment"))




# Keep the last year available for each country

public_sector_emp_temp_last <- public_sector_emp %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, indicator_name, region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data




# Filter the data for the specific indicator "Characteristics of the public sector workforce"

public_sector_workforce <- data_wwbi[data_wwbi$indicator_name %in% c("Education workers, as a share of public total employees", 
                                                               "Health workers, as a share of public total employees", 
                                                               "Public Administration workers, as a share of public total employees"), ]

public_sector_workforce <- public_sector_workforce %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #1043 obs


public_sector_workforce <- public_sector_workforce %>%
  mutate(value_percentage = value * 100)


# Step 1: Calculate the 'Other' value for each country and year

public_sector_workforce <- public_sector_workforce %>%
  group_by(country_name, year, region) %>%
  mutate(
    # Calculate the value for 'Other' as 100 minus the sum of specific indicators
    other_value = 100 - sum(value_percentage[indicator_name %in% c(
      "Education workers, as a share of public total employees",
      "Health workers, as a share of public total employees",
      "Public Administration workers, as a share of public total employees"
    )], na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Create a new dataset for the 'Other' indicator and update the value_percentage

public_sector_workforce <- public_sector_workforce %>%
  bind_rows(
    public_sector_workforce %>%
      # Filter rows for the specified indicators
      filter(indicator_name %in% c(
        "Education workers, as a share of public total employees",
        "Health workers, as a share of public total employees",
        "Public Administration workers, as a share of public total employees"
      )) %>%
      group_by(country_name, year, region) %>%
      summarize(
        indicator_name = "Other",  # Set the indicator name to "Other"
        value_percentage = first(other_value),  # Replace the value with the calculated 'other_value'
        .groups = "drop"  # Drop the grouping after summarizing
      ) %>%
      ungroup()
  )


public_sector_workforce <- public_sector_workforce %>%
  mutate(indicator_name = ifelse(indicator_name == "Education workers, as a share of public total employees", "Education", indicator_name))

public_sector_workforce <- public_sector_workforce %>%
  mutate(indicator_name = ifelse(indicator_name == "Health workers, as a share of public total employees", "Health", indicator_name))

public_sector_workforce <- public_sector_workforce %>%
  mutate(indicator_name = ifelse(indicator_name == "Public Administration workers, as a share of public total employees", "Public Administration", indicator_name))

public_sector_workforce <- public_sector_workforce %>%
  mutate(indicator_name = ifelse(indicator_name == "Publicd Administration", "Public Administration", indicator_name))





# Keep the first and  last year available for each country

public_sector_workforce_first_last <- public_sector_workforce %>%
  filter(!is.na(value_percentage)) %>%               # Keep rows where `value_percentage` is not NA
  group_by(country_name, indicator_name, region) %>%         # Group by country and indicator
  filter(year == max(year, na.rm = TRUE) |           # Keep rows for the last year
           year == min(year, na.rm = TRUE)) %>%      # Keep rows for the first year
  ungroup()


public_sector_workforce_first_last <- public_sector_workforce_first_last %>%
  mutate(indicator_name = ifelse(indicator_name == "Education workers, as a share of public total employees", "Education", indicator_name))

public_sector_workforce_first_last <- public_sector_workforce_first_last %>%
  mutate(indicator_name = ifelse(indicator_name == "Health workers, as a share of public total employees", "Health", indicator_name))

public_sector_workforce_first_last <- public_sector_workforce_first_last %>%
  mutate(indicator_name = ifelse(indicator_name == "Public Administration workers, as a share of public total employees", "Public Administration", indicator_name))


public_sector_workforce_first_last <- public_sector_workforce_first_last %>%
  mutate(indicator_name = ifelse(indicator_name == "Publicd Administration", "Public Administration", indicator_name))




# Filter the data for the specific indicator "Characteristics of the gender workforce"


gender_workforce <- data_wwbi[data_wwbi$indicator_name %in% c("Females, as a share of public paid employees", 
                                                               "Females, as a share of private paid employees"), ]

gender_workforce <- gender_workforce %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #1967 obs 


gender_workforce <- gender_workforce %>%
  mutate(value_percentage = value * 100)



# Filter GDP data for the year 2015

gdp_2015 <- data_gdp %>%
  filter(year == 2015) %>%
  select(country_name, value)


# Keep the last year available for each country

wage_bill_publicexp <- wage_bill_publicexp %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data



# Rename 'value' column to 'indicator_value' in data_indicators

data_indicator_wb <- wage_bill_publicexp %>%
  rename(indicator_value = value)

# Rename 'value' column to 'gdp_value' in data_gdp

gdp_2015 <- gdp_2015 %>%
  rename(gdp_value = value)


# Merge the datasets on 'country_name'


merged_data <- data_indicator_wb %>%
  left_join(gdp_2015, by = "country_name") %>%
  select(country_name, indicator_name, country_code, indicator_value, gdp_value, region)


# Add the log of GDP as a new column

merged_data <- merged_data %>%
  mutate(log_gdp = log(gdp_value))

# Tertiary education by sector 

tertiary_education <- data_wwbi[data_wwbi$indicator_name %in% c("Individuals with tertiary education as a share of public paid employees", 
                                                              "Individuals with tertiary education as a share of private paid employees"), ]

tertiary_education <- tertiary_education %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #1967 obs 


tertiary_education <- tertiary_education %>%
  mutate(value_percentage = value * 100)

# Keep the last year available for each country

tertiary_education <- tertiary_education %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name,indicator_name, region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data



# Public sector wage premium 

public_wage_premium <- data_wwbi[data_wwbi$indicator_name %in% c("Public sector wage premium (compared to all private employees)"), ]

public_wage_premium <- public_wage_premium %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #1967 obs 


public_wage_premium <- public_wage_premium %>%
  mutate(value_percentage = value * 100)

# Keep the last year available for each country

public_wage_premium <- public_wage_premium %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name,indicator_name, region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data


#Public sector wage premium by education level (compared to private formal workers)


public_wage_premium_educ <- data_wwbi[data_wwbi$indicator_name %in% c("Public sector wage premium, by education level: Tertiary Education (compared to formal wage employees)", 
                                                                      "Public sector wage premium, by education level: Secondary Education (compared to formal wage employees)", 
                                                                      "Public sector wage premium, by education level: Primary Education (compared to formal wage employees)", 
                                                                      "Public sector wage premium, by education level: No Education (compared to formal wage employees)"), ]

public_wage_premium_educ <- public_wage_premium_educ %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #1967 obs 


public_wage_premium_educ <- public_wage_premium_educ %>%
  mutate(value_percentage = value * 100)

# Keep the last year available for each country

public_wage_premium_educ <- public_wage_premium_educ %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name,indicator_name, region) %>%                      # Group by country_name (or any other variable)
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




#Gender Wage premium                                                                                                                                                                                                                                                                   

# Filter the data for the specific indicator "Public sector wage premium, by gender: Female (compared to all private employees) and
# Public sector wage premium, by gender: Male (compared to all private employees)"

gender_wage_premium <- data_wwbi[data_wwbi$indicator_name %in% c("Public sector wage premium, by gender: Female (compared to all private employees)", 
                                                               "Public sector wage premium, by gender: Male (compared to all private employees)"), ]

gender_wage_premium <- gender_wage_premium %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #1698 obs

gender_wage_premium <- gender_wage_premium %>%
  select(year, indicator_name, value, country_name,region) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Public sector wage premium, by gender: Female (compared to all private employees)" = "Female", 
                                  "Public sector wage premium, by gender: Male (compared to all private employees)" = "Male"))


# Keep the last year available for each country

gender_wage_premium_last <- gender_wage_premium %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, indicator_label, region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data




public_sector_emp_temp <- public_sector_emp_temp %>%
  select(country_name, indicator_name, year, value, region) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  mutate(indicator_label = recode(
    indicator_name, 
    "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
    "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
    "Public sector employment, as a share of total employment" = "as a share of total employment"
  ))


public_sector_emp_temp_last <- public_sector_emp_temp_last %>%
  select(country_name, indicator_name, year, value, region) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  mutate(indicator_label = recode(
    indicator_name, 
    "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
    "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
    "Public sector employment, as a share of total employment" = "as a share of total employment"
  ))

#Female Leadership 

gender_leadership <- data_wwbi[data_wwbi$indicator_name %in% c("Females, as a share of public paid employees by occupational group: Managers", 
                                                                 "Females, as a share of public paid employees by occupational group: Clerks", 
                                                               "Females, as a share of private paid employees by occupational group: Managers", 
                                                               "Females, as a share of private paid employees by occupational group: Clerks" ), ]

gender_leadership <- gender_leadership %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #1698 obs

gender_leadership <- gender_leadership %>%
  select(year, indicator_name, value, country_name,region) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Females, as a share of public paid employees by occupational group: Managers" = "Managers-Public", 
                                  "Females, as a share of public paid employees by occupational group: Clerks" = "Clerks-Public", 
                                  "Females, as a share of private paid employees by occupational group: Managers" = "Managers-Private",
                                  "Females, as a share of private paid employees by occupational group: Clerks" = "Clerks-Private"))


# Keep the last year available for each country

gender_leadership <- gender_leadership %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, indicator_label, region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data



gender_leadership <- gender_leadership %>%
  mutate(value_percentage = value * 100)





## Shiny Dashboard ----

# ---------------------------
# UI
# ---------------------------

ui <- bootstrapPage(
  theme = bs_theme(version = 5, bootswatch = 'quartz'),
  
  # Custom CSS for the sidebar
  tags$style(HTML("
    #sidebar {
      height: 100vh;
      background: linear-gradient(to bottom, #56ccf2, #2f80ed);
      padding: 20px;
      color: white;
    }
    
    /* General Sidebar Button Styling */
    .nav-item, .nav-sub-item {
      display: block;
      margin: 10px 0;
      padding: 12px 15px;
      font-size: 18px;
      font-weight: bold;
      text-align: left;
      text-decoration: none;
      color: white;
      border-radius: 25px; /* Rounded corners */
      background: transparent;
      transition: all 0.3s ease-in-out;
    }
    
    /* Pink Active Button */
    .nav-item.active, .nav-sub-item.active {
      background-color: #eb2f96 !important; /* Pink color */
      color: white !important;
    }
    
    /* Hover Effect */
    .nav-item:hover, .nav-sub-item:hover {
      cursor: pointer;
      background-color: rgba(255, 255, 255, 0.2);
      color: white;
    }

    /* Section Styling */
    .nav-section {
      font-size: 20px;
      font-weight: bold;
      margin-top: 15px;
      color: white;
      padding-left: 10px;
      cursor: pointer;
    }

    /* Hidden collapsible sections */
    #macro_section, 
    #public_sector_section, 
    #public_sector_workforce_section, 
    #public_sector_wages_section {
      padding-left: 15px;
      display: none;
    }
")),
  
  # JavaScript to Toggle Sections
  tags$script(HTML("
    function toggleSection(sectionId) {
      var section = document.getElementById(sectionId);
      if (section.style.display === 'none') {
        section.style.display = 'block';
      } else {
        section.style.display = 'none';
      }
    }
  ")),
  
  # Layout with sidebar and main content
  div(
    class = "d-flex",
    
    # Sidebar
    div(
      id = "sidebar",
      div(class = "nav-item", actionLink("nav_dashboard", "Dashboard")),
      div(class = "nav-item", actionLink("nav_metadata", "Metadata")),
      
      # Collapsible Section - The Macro Fundamentals
      div(class = "nav-section", onclick = "toggleSection('macro_section')", "The Macro Fundamentals"),
      div(id = "macro_section", style = "display: none;",
          div(class = "nav-sub-item", actionLink("nav_wagebill", "Wage Bill Graphs")),
          div(class = "nav-sub-item", actionLink("nav_wagebill_gdp", "Wage Bill & GDP Graphs"))
      ),
      
      # Collapsible Section - The Size of the Public Sector
      div(class = "nav-section", onclick = "toggleSection('public_sector_section')", "The Size of the Public Sector"),
      div(id = "public_sector_section", style = "display: none;",
          div(class = "nav-sub-item", actionLink("nav_public_workforce", "Public Sector Wage Premium")),
          div(class = "nav-sub-item", actionLink("nav_public_graphs", "Public Sector Graphs"))
      ),
      
      # Collapsible Section - Characteristics of Public Sector Workforce
      div(class = "nav-section", onclick = "toggleSection('public_sector_workforce_section')", "Characteristics of Public Sector Workforce"),
      div(id = "public_sector_workforce_section", style = "display: none;",
          div(class = "nav-sub-item", actionLink("nav_gender_workforce", "Gender Workforce Graphs")), 
          div(class = "nav-sub-item", actionLink("nav_education", "Tertiary Education Graphs")),
          div(class = "nav-sub-item", actionLink("nav_public_educ", "Public Sector Education Graphs")),
          div(class = "nav-sub-item", actionLink("nav_female_leadership", "Female Leadership Graphs"))
      ),
      
      # Collapsible Section - Competitiveness of Public Sector Wages
      div(class = "nav-section", onclick = "toggleSection('public_sector_wages_section')", "Competitiveness of Public Sector Wages"),
      div(id = "public_sector_wages_section", style = "display: none;",
          div(class = "nav-sub-item", actionLink("nav_wagepremium_gender", "Wage Premium Gender Graphs"))
      ),
      
      div(class = "nav-item", actionLink("nav_download_all", "Download All Graphs"))
    ),
    
    # Main content area
    div(
      class = "flex-grow-1 p-4",
      h2("WWB Indicators"),
      uiOutput("main_content")
    )
  )
)


# SERVER



server <- function(input, output, session) {
  
  # 1. Track the active tab via a reactive value  
  active_tab <- reactiveVal("dashboard")
  
  # Update active_tab when each sidebar link is clicked
  observeEvent(input$nav_dashboard,         { active_tab("dashboard") })
  observeEvent(input$nav_metadata,          { active_tab("metadata") })
  observeEvent(input$nav_wagebill,          { active_tab("wagebill") })
  observeEvent(input$nav_wagebill_gdp,      { active_tab("wagebill_gdp") })
  observeEvent(input$nav_public_workforce,  { active_tab("public_workforce") })
  observeEvent(input$nav_gender_workforce,  { active_tab("gender_workforce") })
  observeEvent(input$nav_education,         { active_tab("education") })
  observeEvent(input$nav_wagepremium,       { active_tab("wagepremium") })
  observeEvent(input$nav_public_educ,       { active_tab("public_educ") })
  observeEvent(input$nav_public_graphs,     { active_tab("public_graphs") })
  observeEvent(input$nav_wagepremium_gender,{ active_tab("wagepremium_gender") })
  observeEvent(input$nav_female_leadership, { active_tab("female_leadership") })
  observeEvent(input$nav_download_all,      { active_tab("download_all") })
  
  # 2. Render the main dynamic UI based on active_tab
  output$main_content <- renderUI({
    tab <- active_tab()
    
    if(tab == "dashboard") {
      tagList(
        h3("Dashboard"),
        fluidRow(
          div(style = "border: 1px solid #ddd; padding: 20px;",
              "The Worldwide Bureaucracy Indicators (WWBI) database is a unique cross-national dataset on public sector employment and wages that aims to fill an information gap, thereby helping researchers, development practitioners, and policymakers gain a better understanding of the personnel dimensions of state capability, the footprint of the public sector within the overall labor market, and the fiscal implications of the public sector wage bill. The dataset is derived from administrative data and household surveys, thereby complementing existing, expert perception-based approaches."
          )
        )
      )
      
    } else if(tab == "metadata") {
      # --- METADATA TAB NOW INCLUDES INDICATORS & WIDGETS ---
      tagList(
        h3("Metadata & Indicators"),
        fluidRow(
          infoBoxOutput("numberIndicatorsBox", width = 6),
          infoBoxOutput("numberCountriesBox", width = 6)
        ),
        fluidRow(
          infoBoxOutput("temporalCoverageAnnualBox", width = 6),
          infoBoxOutput("temporalCoverageYearsBox", width = 6)
        ),
        fluidRow(
          infoBoxOutput("lastUpdatedBox", width = 6)
        ),
        fluidRow(
          div(style = "border: 1px solid #ddd; padding: 10px;",
              "This map shows which countries have reported data for the selected indicator.")
        ),
        fluidRow(
          column(6,
                 selectInput("indicatorSelect", "Select Indicator", 
                             choices = unique(data_wwbi$indicator_name), selected = NULL)
          ),
          column(6,
                 selectInput("yearSelect", "Select Year", 
                             choices = 2010:2022, selected = 2022)
          )
        ),
        fluidRow(
          textOutput("countryCount")
        ),
        fluidRow(
          leafletOutput("worldMap", height = "600px")
        )
      )
    }else if(tab == "wagebill") {
      tagList(
        h3("Wage Bill Graphs"),
        fluidRow(
          column(4,
                 selectInput("countries", "Select Countries:",
                             choices = unique(wage_bill_publicexp$country_name),
                             selected = NULL,  # No default; user must select one or more
                             multiple = TRUE)
          ),
          column(4,
                 radioButtons("graph_choice", "Graph Type:",
                              choices = c("Wage Bill as % of Public Expenditure" = "Public",
                                          "Wage Bill as % of GDP" = "GDP"),
                              selected = "Public")
          )
        ),
        fluidRow(
          plotlyOutput("plotwagebill", height = "500px")
        ),
        fluidRow(
          downloadButton("downloadWord", "Download Report in Word")
        )
      )
    }else if(tab == "wagebill_gdp") {
      tagList(
        h3("Wage Bill & GDP Graphs"),
        fluidRow(
          div(style="border: 1px solid #ddd; padding: 10px;",
              p("This visualization explores the relationship between wage bill and GDP per capita (log scale).")
          )
        ),
        fluidRow(
          column(4,
                 selectInput("countries_first", "Select Countries:", 
                             choices = unique(merged_data$country_name), 
                             multiple = TRUE, width = "100%"),
                 downloadButton("downloadGDPDoc", "Download GDP Analysis Report")
          ),
          column(4,
                 radioButtons("label_type", "Choose Label Type", 
                              choices = c("Country", "Region"), selected = "Country")
          )
        ),
        fluidRow(
          plotlyOutput("dot_plot", height = "500px")
        )
      )
      
    } else if(tab == "public_workforce") {
      tagList(
        h3("Public Sector Workforce Graphs"),
        fluidRow(
          selectInput("countries_workforce", "Select Countries for Workforce Graph",
                      choices = unique(public_sector_workforce$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("stackedBarGraph", height = "600px")
        ),
        fluidRow(
          selectInput("selected_country", "Select Country for Second Graph",
                      choices = unique(public_sector_workforce$country_name), multiple = FALSE)
        ),
        fluidRow(
          plotlyOutput("horizontalStackedBar", height = "600px")
        ),
        fluidRow(
          checkboxGroupInput("selected_graphs_public", "Select Graphs to Download", 
                             choices = c("Multi-Country Graph" = "firstGraph", "Single-Country Graph" = "secondGraph"), 
                             selected = c("firstGraph", "secondGraph")),
          downloadButton("downloadGraphsWordworkforce", "Download Selected Graphs in Word")
        )
      )
      
    } else if(tab == "gender_workforce") {
      tagList(
        h3("Gender Workforce Graphs"),
        fluidRow(
          selectInput("countries_workforce", "Select Countries for Workforce Graph", 
                      choices = unique(public_sector_workforce$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("employment_plot", height = "600px")
        ),
        fluidRow(
          selectInput("selected_country", "Select Country for Second Graph", 
                      choices = unique(gender_workforce$country_name), multiple = FALSE)
        ),
        fluidRow(
          plotlyOutput("employment_plot_overtime", height = "600px")
        ),
        fluidRow(
          downloadButton("downloadGraphsWordworkforce", "Download Gender Workforce Report")
        )
      )
      
    } else if(tab == "education") {
      tagList(
        h3("Tertiary Education Graphs"),
        fluidRow(
          selectInput("selected_countries", "Select Countries", 
                      choices = unique(tertiary_education$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("barPlot", height = "600px")
        ),
        fluidRow(
          downloadButton("downloadGraphsWordEducation", "Download Tertiary Education Report")
        )
      )
      
    } else if(tab == "wagepremium") {
      tagList(
        h3("Public Sector Wage Premium"),
        fluidRow(
          selectInput("countries_first", "Select Countries for First Graph", 
                      choices = unique(public_wage_premium$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("dotPlot")
        )
      )
      
    } else if(tab == "public_educ") {
      tagList(
        h3("Public Sector Education Graphs"),
        fluidRow(
          selectInput("selected_country", "Select Country for Graph", 
                      choices = unique(public_wage_premium_educ$country_name), multiple = FALSE)
        ),
        fluidRow(
          plotlyOutput("barPloteduc")
        )
      )
      
    } else if(tab == "public_graphs") {
      tagList(
        h3("Public Sector Graphs"),
        fluidRow(
          selectInput("countries_first", "Select Countries for First Graph", 
                      choices = unique(public_sector_emp_temp_last$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("firstGraphpublic")
        ),
        fluidRow(
          selectInput("country_second", "Select Country for Second Graph", 
                      choices = unique(public_sector_emp_temp$country_name), multiple = FALSE)
        ),
        fluidRow(
          plotlyOutput("secondGraphpublic")
        ),
        fluidRow(
          checkboxGroupInput("selected_graphs_public", "Select Graphs to Download:", 
                             choices = c("Multi-Country Graph" = "firstGraph", "Single-Country Graph" = "secondGraph"), 
                             selected = c("firstGraph", "secondGraph")),
          downloadButton("downloadGraphsWord", "Download Graphs as Word File")
        )
      )
      
    } else if(tab == "wagepremium_gender") {
      tagList(
        h3("Wage Premium Gender Graphs"),
        fluidRow(
          selectInput("countries_first", "Select Countries for First Graph", 
                      choices = unique(gender_wage_premium_last$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("firstGraphgender")
        ),
        fluidRow(
          selectInput("country_second", "Select Country for Second Graph", 
                      choices = unique(gender_wage_premium$country_name), multiple = FALSE)
        ),
        fluidRow(
          plotlyOutput("secondGraphgender")
        ),
        fluidRow(
          checkboxGroupInput("selected_graphs_public", "Select Graphs to Download:", 
                             choices = c("Multi-Country Graph" = "firstGraph", "Single-Country Graph" = "secondGraph"), 
                             selected = c("firstGraph", "secondGraph")),
          downloadButton("downloadGraphsWordgender", "Download Graphs as Word File")
        )
      )
      
    } else if(tab == "female_leadership") {
      tagList(
        h3("Female Leadership Graphs"),
        fluidRow(
          selectInput("selected_countries", "Select Countries", 
                      choices = unique(gender_leadership$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("barPlotwomen", height = "600px")
        ),
        fluidRow(
          downloadButton("downloadGraphsWordfemale", "Download Female Leadership Report")
        )
      )
      
    } else if(tab == "download_all") {
      tagList(
        h3("Download All Graphs"),
        fluidRow(
          div(style = "border: 1px solid #ddd; padding: 10px;",
              "Download a comprehensive report containing all visualizations and analyses.")
        ),
        fluidRow(
          downloadButton("downloadAllGraphsDoc", "Download Full Report")
        )
      )
    }
  })
  
  # ---------------------------
  # 3. All your original outputs and downloadHandlers follow.
  # (For brevity, the code below is the same as in your original server code.)
  
  # Reactive expression: select the appropriate dataset based on the radio buttons
  
  selected_data <- reactive({
    req(input$countries)  # Wait until the user selects at least one country
    if (input$graph_choice == "GDP") {
      # Use wage_bill_gdp if the user selects "GDP"
      wage_bill_gdp %>% filter(country_name %in% input$countries)
    } else {
      # Otherwise, use wage_bill_publicexp
      wage_bill_publicexp %>% filter(country_name %in% input$countries)
    }
  })
  
  # Render the Plotly graph for the wage bill
  output$plotwagebill <- renderPlotly({
    d <- selected_data()
    
    # Set the title and y-axis label depending on the selection:
    title_text <- ifelse(input$graph_choice == "GDP",
                         "Wage Bill as % of GDP Over Time",
                         "Wage Bill as % of Public Expenditure Over Time")
    
    y_label <- ifelse(input$graph_choice == "GDP",
                      "Wage Bill (% of GDP)",
                      "Wage Bill (% of Public Expenditure)")
    
    # Create the Plotly graph
    plot_ly(data = d,
            x = ~year,
            y = ~value,         # Ensure this matches your column name in both datasets
            color = ~country_name,
            type = "scatter",
            mode = "lines+markers",
            marker = list(size = 8)) %>%
      layout(title = title_text,
             xaxis = list(title = "Year", dtick = 2),
             yaxis = list(title = y_label))
  })
  output$downloadWord <- downloadHandler(
    filename = function() {
      paste0("Wage_Bill_Analysis_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Use the first selected country from the input "countries"
      first_country <- if (!is.null(input$countries) && length(input$countries) > 0) {
        input$countries[1]
      } else {
        "Bangladesh"  # Default fallback
      }
      
      # Get the region using countrycode
      first_region <- countrycode(first_country, origin = "country.name", destination = "region")
      
      report_title <- paste("Wage Bill Analysis Report -", first_country)
      doc <- read_docx()
      
      # Define title style
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext(report_title, prop = title_style)))
      
      # Add a subtitle
      doc <- doc %>% body_add_par("The macro fundamentals of the public sector", style = "heading 3")
      
      # Create a dynamic introduction text
      intro_text <- paste0(
        "This note presents evidence on public sector employment and compensation practices in ", first_country,
        " using the Worldwide Bureaucracy Indicators (WWBI). The primary data source is the Labor Force Survey (LFS), conducted by the Bureau of Statistics, ",
        "which offers extensive, nationally representative data over multiple years up to 2022. ",
        "For international comparisons, the analysis includes a set of peer countries for benchmarking, with a particular focus on countries from the ",
        first_region, " region and other aspirational peers."
      )
      
      doc <- doc %>% body_add_par(intro_text, style = "Normal")
      
      # --- Dynamic Analysis Paragraph ---
      # Get data for the selected country
      data_country <- wage_bill_publicexp %>% filter(country_name == first_country)
      
      # Extract the wage bill values for 2010 and 2022 (if available)
      value_2010 <- data_country %>% filter(year == 2010) %>% pull(value)
      value_2022 <- data_country %>% filter(year == 2022) %>% pull(value)
      
      # Handle missing data
      if (length(value_2010) == 0) value_2010 <- NA
      if (length(value_2022) == 0) value_2022 <- NA
      
      # Construct the analysis text
      analysis_text <- paste0(
        first_country, " has a relatively low public sector wage compared to its peers. ",
        "The country’s wage bill as a percentage of public expenditures has followed a relatively low and stable trend over the past decade. ",
        "In 2010, the wage bill accounted for around ", 
        ifelse(is.na(value_2010), "N/A", round(value_2010, 1)), 
        " percent of public expenditures, but this gradually declined, reaching its lowest point of ", 
        ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), 
        " percent in 2022. ",
        "Compared to other countries in the region and global comparators, ", first_country, 
        " consistently allocates a smaller proportion of its budget to public sector wages. ",
        "For instance, in 2022, ", first_country, "’s wage bill stands at ", 
        ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), 
        " percent, whereas countries like Pakistan, Indonesia, and the Philippines have much higher wage bills during the same period. ",
        "This trend reflects ", first_country, "’s cautious approach to public sector wage spending, but it also raises questions about whether this low level of spending affects the government's ability to effectively deliver public services."
      )
      
      doc <- doc %>% body_add_par(analysis_text, style = "Normal")
      
      # --- Add the Graph Based on User Selection ---
      if (input$graph_choice == "GDP") {
        # Filter wage_bill_gdp data
        graph_data <- wage_bill_gdp %>% filter(country_name %in% input$countries)
        
        graph <- ggplot(graph_data, aes(x = year, y = value, color = country_name)) +
          geom_line(size = 1.2) + 
          geom_point(size = 3) +
          labs(title = "Wage Bill as % of GDP Over Time",
               x = "Year",
               y = "Wage Bill (% of GDP)") +
          theme_minimal()
        
        doc <- doc %>% 
          body_add_gg(value = graph, style = "centered") %>%
          body_add_par(paste0("This graph shows the wage bill as a percentage of GDP over time for the selected countries. ",
                              "For example, in 2022, ", first_country, " had a wage bill of ", 
                              ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), "% of GDP."), 
                       style = "Normal")
        
      } else {
        # Filter wage_bill_publicexp data
        graph_data <- wage_bill_publicexp %>% filter(country_name %in% input$countries)
        
        graph <- ggplot(graph_data, aes(x = year, y = value, color = country_name)) +
          geom_line(size = 1.2) + 
          geom_point(size = 3) +
          labs(title = "Wage Bill as % of Public Expenditure Over Time",
               x = "Year",
               y = "Wage Bill (% of Public Expenditure)") +
          theme_minimal()
        
        doc <- doc %>% 
          body_add_gg(value = graph, style = "centered") %>%
          body_add_par(paste0("This graph shows the wage bill as a percentage of public expenditure over time for the selected countries. ",
                              "For instance, in 2022, ", first_country, "’s wage bill accounted for ", 
                              ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), "% of public expenditure."), 
                       style = "Normal")
      }
      
      # Save the document
      print(doc, target = file)
    }
  )
  output$dot_plot <- renderPlotly({
    req(input$countries_first)
    filtered_data_df <- merged_data %>% 
      filter(country_name %in% input$countries_first) %>%
      mutate(color = ifelse(country_name == input$countries_first[1], "#B3242B", "#003366"))
    trendline_model <- lm(indicator_value ~ log_gdp, data = filtered_data_df)
    trendline_values <- predict(trendline_model, newdata = filtered_data_df)
    plot_ly() %>%
      add_trace(
        data = filtered_data_df,
        x = ~log_gdp,
        y = ~indicator_value,
        type = "scatter",
        mode = "markers+text",
        text = ~country_name,
        textposition = "top center",
        marker = list(size = 10, color = ~color, opacity = 0.7)
      ) %>%
      add_trace(
        x = filtered_data_df$log_gdp,
        y = trendline_values,
        type = "scatter",
        mode = "lines",
        line = list(color = "gray", dash = "dash"),
        name = "Trendline"
      ) %>%
      layout(title = "Wage Bill vs. Log(GDP per Capita)",
             xaxis = list(title = "Log(GDP per Capita, 2015)"),
             yaxis = list(title = "Wage Bill"),
             showlegend = FALSE)
  })
  
  output$downloadGDPDoc <- downloadHandler(
    filename = function() { paste0("Wage_Bill_vs_GDP_Report_", Sys.Date(), ".docx") },
    content = function(file) {
      filtered_data_df <- merged_data %>% filter(country_name %in% input$countries_first)
      req(nrow(filtered_data_df) > 0)
      countries <- if (!is.null(input$countries) && length(input$countries) > 0) input$countries[1] else "Unknown Country"
      report_title <- paste("Wage Bill vs. GDP Analysis Report -", countries)
      doc <- read_docx()
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext(report_title, prop = title_style)))
      doc <- doc %>% body_add_par("Introduction", style = "heading 2") %>% 
        body_add_par("This note presents evidence on public sector employment and compensation practices...", style = "Normal")
      plot <- ggplot(filtered_data_df, aes(x = log_gdp, y = indicator_value, color = country_name)) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
        labs(title = "Wage Bill vs. Log(GDP per Capita)", x = "Log(GDP per Capita, 2015)", y = "Wage Bill") +
        theme_minimal()
      doc <- doc %>% body_add_gg(value = plot, style = "centered")
      print(doc, target = file)
    }
  )
  
  filtered_workforce_data <- reactive({
    req(input$countries_workforce)
    public_sector_workforce %>% group_by(country_name, indicator_name) %>% slice_max(order_by = year, n = 1) %>% ungroup()
  })
  
  output$stackedBarGraph <- renderPlotly({
    req(input$countries_workforce)
    data_to_plot <- filtered_workforce_data() %>% filter(country_name %in% input$countries_workforce)
    req(nrow(data_to_plot) > 0)
    plot_ly(data = data_to_plot,
            x = ~country_name,
            y = ~value_percentage,
            color = ~indicator_name,
            type = "bar",
            text = ~paste("Country:", country_name, "<br>Indicator:", indicator_name, "<br>Value:", round(value_percentage, 1), "%"),
            textposition = "auto",
            colors = c("Public Administration" = "#568340", "Education" = "#B3242B", 
                       "Health" = "#003366", "Other" = "#A9A9A9")) %>%
      layout(barmode = "stack",
             title = "Public Workforce Distribution by Country",
             xaxis = list(title = "Country"),
             yaxis = list(title = "Workforce Distribution (%)", range = c(0, 100)),
             legend = list(title = list(text = "<b>Indicator</b>")))
  })
  
  output$messageOutput <- renderUI({
    filtered_data <- public_sector_workforce %>% filter(country_name == input$selected_country)
    if(nrow(filtered_data) < 2) {
      return(tags$p("Not enough data available for this country to create the graph.", style = "color: red; font-weight: bold;"))
    }
    return(NULL)
  })
  
  output$horizontalStackedBar <- renderPlotly({
    req(input$selected_country)
    filtered_data <- public_sector_workforce %>% filter(country_name == input$selected_country)
    if(nrow(filtered_data) == 0) return(NULL)
    first_year <- min(filtered_data$year, na.rm = TRUE)
    last_year <- max(filtered_data$year, na.rm = TRUE)
    if(is.infinite(first_year) || is.infinite(last_year)) return(NULL)
    data_to_plot <- filtered_data %>% filter(year %in% c(first_year, last_year)) %>% 
      group_by(year, indicator_name) %>% summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")
    plot_ly(data = data_to_plot,
            x = ~value_percentage,
            y = ~factor(year, levels = c(last_year, first_year)),
            color = ~indicator_name,
            type = "bar",
            orientation = "h",
            text = ~paste0(round(value_percentage, 1), "%"),
            textposition = "inside",
            colors = c("Public Administration" = "#568340", "Education" = "#B3242B", 
                       "Health" = "#003366", "Other" = "#A9A9A9")) %>%
      layout(barmode = "stack",
             title = paste("Sectoral Distribution of Public Sector Workforce in", input$selected_country, "(", first_year, "&", last_year, ")"),
             xaxis = list(title = "Percentage (%)"),
             yaxis = list(title = "Year"),
             legend = list(title = list(text = "Sector")))
  })
  
  output$downloadGraphsWordworkforce <- downloadHandler(
    filename = function() { paste0("Public_Sector_Analysis_", Sys.Date(), ".docx") },
    content = function(file) {
      doc <- read_docx()
      report_title <- "Public Sector Workforce Analysis"
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext(report_title, prop = title_style)))
      first_graph_data <- filtered_workforce_data() %>% filter(country_name %in% input$countries_workforce)
      first_graph_ggplot <- ggplot(first_graph_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c("Public Administration" = "#568340", "Education" = "#B3242B", 
                                     "Health" = "#003366", "Other" = "#A9A9A9")) +
        labs(title = "Public Workforce Distribution by Country", x = "Country", y = "Workforce Distribution (%)") +
        theme_minimal()
      ggsave("first_graph.png", plot = first_graph_ggplot, width = 6, height = 4)
      doc <- doc %>% body_add_par("First Graph: Public Workforce Distribution by Country", style = "heading 1") %>% 
        body_add_img(src = "first_graph.png", width = 6, height = 4) %>% 
        body_add_par("This graph shows the public workforce distribution across multiple countries.", style = "Normal")
      
      filtered_data <- public_sector_workforce %>% filter(country_name == input$selected_country)
      if(nrow(filtered_data) < 2) {
        doc <- doc %>% body_add_par("Not enough data available for the selected country to create the graph.", style = "Normal")
      } else {
        first_year <- min(filtered_data$year, na.rm = TRUE)
        last_year <- max(filtered_data$year, na.rm = TRUE)
        if(!is.finite(first_year) || !is.finite(last_year)) {
          doc <- doc %>% body_add_par("Invalid year data for the selected country.", style = "Normal")
        } else {
          second_graph_data <- filtered_data %>% filter(year %in% c(first_year, last_year)) %>% 
            group_by(year, indicator_name) %>% summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")
          second_graph_ggplot <- ggplot(second_graph_data, aes(x = value_percentage, y = factor(year, levels = c(last_year, first_year)), fill = indicator_name)) +
            geom_bar(stat = "identity", position = "stack", orientation = "horizontal") +
            scale_fill_manual(values = c("Public Administration" = "#568340", "Education" = "#B3242B", 
                                         "Health" = "#003366", "Other" = "#A9A9A9")) +
            labs(title = paste("Sectoral Distribution of Public Sector Workforce in", input$selected_country), x = "Percentage (%)", y = "Year") +
            theme_minimal()
          ggsave("second_graph.png", plot = second_graph_ggplot, width = 6, height = 4)
          doc <- doc %>% body_add_par("Second Graph: Sectoral Distribution of Public Sector Workforce", style = "heading 1") %>% 
            body_add_img(src = "second_graph.png", width = 6, height = 4) %>% 
            body_add_par("This graph shows the sectoral distribution of public sector workforce for the selected country.", style = "Normal")
        }
      }
      print(doc, target = file)
    }
  )
  
  output$employment_plot <- renderPlotly({
    filtered_data <- gender_workforce %>% filter(country_name %in% input$countries_workforce)
    if(nrow(filtered_data) == 0) return(NULL)
    public_latest <- filtered_data %>% filter(indicator_name == "Females, as a share of public paid employees") %>% 
      group_by(country_name) %>% filter(year == max(year, na.rm = TRUE)) %>% ungroup()
    private_latest <- filtered_data %>% filter(indicator_name == "Females, as a share of private paid employees") %>% 
      group_by(country_name) %>% filter(year == max(year, na.rm = TRUE)) %>% ungroup()
    if(nrow(public_latest) == 0 || nrow(private_latest) == 0) return(NULL)
    plot <- plot_ly(data = public_latest,
                    x = ~country_name,
                    y = ~value_percentage,
                    type = 'bar',
                    color = I("#003366"),
                    text = ~paste("Country: ", country_name, "<br>Last year available: ", year, "<br>Employment (%): ", round(value_percentage, 2)),
                    hoverinfo = "text",
                    name = "Public Sector",
                    showlegend = TRUE) %>%
      add_trace(data = private_latest,
                x = ~country_name,
                y = ~value_percentage,
                type = "scatter",
                mode = "markers",
                marker = list(size = 10, color = "#B3242B"),
                name = "Private Sector",
                text = ~paste("Country: ", country_name, "<br>Last year available: ", year, "<br>Employment (%): ", round(value_percentage, 2)),
                hoverinfo = "text",
                showlegend = TRUE) %>%
      layout(barmode = "group",
             title = "Female Employment by Sector (Last Year Available)",
             xaxis = list(title = "Country (Last Year Available)",
                          tickmode = 'array',
                          tickvals = public_latest$country_name,
                          ticktext = paste(public_latest$country_name, "(", public_latest$year, ")")),
             yaxis = list(title = "Employment (%)"),
             legend = list(title = list(text = "Sector")))
    plot
  })
  
  output$employment_plot_overtime <- renderPlotly({
    filtered_data <- gender_workforce %>% filter(country_name == input$selected_country)
    if(nrow(filtered_data) == 0) return(NULL)
    custom_colors <- c("Females, as a share of private paid employees" = "#003366", 
                       "Females, as a share of public paid employees" = "#B3242B")
    plot <- filtered_data %>% plot_ly(x = ~year,
                                      y = ~value_percentage,
                                      color = ~indicator_name,
                                      colors = custom_colors,
                                      type = 'scatter',
                                      mode = 'lines+markers',
                                      hoverinfo = 'text',
                                      text = ~paste("Country:", country_name, "<br>Sector:", indicator_name, "<br>Year:", year, "<br>Female Employment:", value_percentage)) %>%
      layout(title = paste("Female Employment by Sector Over Time in", input$selected_country),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Female Employment (%)"),
             legend = list(title = list(text = "<b>Sector</b>")),
             hovermode = "closest")
    plot <- plot %>% add_annotations(x = data_to_plot_long$year, 
                                     y = data_to_plot_long$value, 
                                     text = round(data_to_plot_long$value, 2),
                                     showarrow = FALSE,
                                     font = list(size = 12, color = "black"),
                                     xanchor = "center",
                                     yanchor = "bottom")
    plot
  })
  
  output$downloadGraphsWordworkforce <- downloadHandler(
    filename = function() { paste0("Wage_Premium_Gender_Graphs_", Sys.Date(), ".docx") },
    content = function(file) {
      print(input$countries_first)
      print(input$country_second)
      countries <- ifelse(!is.null(input$countries_first) && length(input$countries_first) > 0, input$countries_first[1], "Unknown Country")
      report_title <- paste("Wage Premium Gender Analysis Report -", countries)
      doc <- read_docx()
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext(report_title, prop = title_style)))
      doc <- doc %>% body_add_par("Introduction", style = "heading 2") %>% 
        body_add_par("This report presents evidence on public sector employment and compensation practices for the selected countries.", style = "Normal")
      if("firstGraphgender" %in% input$graphs_to_download && length(input$countries_first) > 0) {
        data_to_plot <- gender_wage_premium_last %>% filter(country_name %in% input$countries_first)
        data_to_plot_long <- data_to_plot %>% select(country_name, indicator_label, year, value) %>% 
          mutate(indicator_label = factor(indicator_label))
        first_graph_wage_premium_gender <- ggplot(data_to_plot_long, aes(x = country_name, y = value, color = indicator_label)) +
          geom_point(size = 3) +
          labs(title = "Wage Premium Gender (Multi-Country)", x = "Country", y = "Value") +
          theme_minimal()
        graph_path1 <- "C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/first_graph_wage_premium_gender.png"
        ggsave(graph_path1, plot = first_graph_wage_premium_gender, width = 6, height = 4)
        doc <- doc %>% body_add_par("First Graph: Wage Premium Gender (Multi-Country)", style = "heading 1") %>% 
          body_add_img(src = graph_path1, width = 6, height = 4) %>% 
          body_add_par("This graph shows the wage premium by gender across multiple countries.", style = "Normal")
      }
      if("secondGraphgender" %in% input$graphs_to_download && !is.null(input$country_second)) {
        data_to_plot <- gender_wage_premium %>% filter(country_name == input$country_second)
        data_to_plot_long <- data_to_plot %>% select(year, indicator_name, value) %>% 
          mutate(indicator_name = factor(indicator_name))
        second_graph_wage_premium_gender <- ggplot(data_to_plot_long, aes(x = year, y = value, color = indicator_name)) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          labs(title = paste("Public sector wage premium, by gender in", input$country_second, "Over Time"), x = "Year", y = "Employment Value") +
          theme_minimal()
        graph_path2 <- "C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/second_graph_wage_premium_gender.png"
        ggsave(graph_path2, plot = second_graph_wage_premium_gender, width = 6, height = 4)
        doc <- doc %>% body_add_par("Second Graph: Public Sector Employment (Single Country)", style = "heading 1") %>% 
          body_add_img(src = graph_path2, width = 6, height = 4) %>% 
          body_add_par("This graph shows the wage premium by gender trends over time for the selected country.", style = "Normal")
      }
      print(doc, target = file)
    }
  )
  
  output$barPlotwomen <- renderPlotly({
    if(is.null(input$selected_countries) || length(input$selected_countries) == 0) return(NULL)
    filtered_data <- gender_leadership %>% filter(country_name %in% input$selected_countries)
    if(nrow(filtered_data) == 0) return(NULL)
    plot_ly(data = filtered_data,
            x = ~country_name,
            y = ~value_percentage,
            color = ~indicator_label,
            colors = c("Clerks-Public" = "#003366", "Managers-Public" = "#ADD8E6",
                       "Clerks-Private" = "#006400", "Managers-Private" = "#90EE90"),
            type = 'bar',
            barmode = 'group') %>%
      layout(title = "Females by Occupational Group and Sector",
             xaxis = list(title = "Country"),
             yaxis = list(title = "Female Share (%)"),
             bargap = 0.2)
  })
  
  output$downloadGraphsWordfemale <- downloadHandler(
    filename = function() { paste0("Females_Occupation_Groups_Analysis_", Sys.Date(), ".docx") },
    content = function(file) {
      doc <- read_docx()
      report_title <- "Females by Occupational Group and Sector"
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext(report_title, prop = title_style)))
      filtered_data <- gender_leadership %>% filter(country_name %in% input$selected_countries)
      if(nrow(filtered_data) == 0) return(NULL)
      bar_plot <- plot_ly(data = filtered_data,
                          x = ~country_name,
                          y = ~value_percentage,
                          color = ~indicator_label,
                          colors = c("Clerks-Public" = "#003366", "Managers-Public" = "#ADD8E6",
                                     "Clerks-Private" = "#006400", "Managers-Private" = "#90EE90"),
                          type = 'bar',
                          barmode = 'group') %>%
        layout(title = "Females by Occupational Group and Sector",
               xaxis = list(title = "Country"),
               yaxis = list(title = "Female Share (%)"),
               bargap = 0.2)
      ggsave("bar_plot.png", plot = bar_plot, width = 6, height = 4)
      doc <- doc %>% body_add_par("Females by Occupational Group and Sector", style = "heading 1") %>% 
        body_add_img(src = "bar_plot.png", width = 6, height = 4) %>% 
        body_add_par("This graph shows the share of females in various occupational groups (Managers/Clerks) in the public and private sectors for the selected countries.", style = "Normal")
      print(doc, target = file)
    }
  )
  
  output$downloadAllGraphsDoc <- downloadHandler(
    filename = function() { paste0("Comprehensive_Wage_Bill_Report_", Sys.Date(), ".docx") },
    content = function(file) {
      report_title <- "Wage Bill and Public Employment Analysis Report"
      doc <- read_docx()
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext(report_title, prop = title_style)))
      doc <- doc %>% body_add_par("Introduction", style = "heading 2") %>% 
        body_add_par("This note presents evidence on public sector employment and compensation practices...", style = "Normal")
      graph1 <- ggplot(selected_data(), aes(x = year, y = value, color = country_name)) +
        geom_line() +
        labs(title = "Wage Bill as % of GDP Over Time", x = "Year", y = "Wage Bill (% of GDP)") +
        theme_minimal()
      doc <- doc %>% body_add_gg(graph1, style = "centered")
      graph2 <- ggplot(selected_data(), aes(x = year, y = value, color = country_name)) +
        geom_line() +
        labs(title = "Wage Bill as % of Public Expenditure Over Time", x = "Year", y = "Wage Bill (% of Public Expenditure)") +
        theme_minimal()
      doc <- doc %>% body_add_gg(graph2, style = "centered")
      dot_plot <- ggplot(filtered_data(), aes(x = log_gdp, y = indicator_value, color = country_name)) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
        labs(title = "Wage Bill vs. Log(GDP per Capita)", x = "Log(GDP per Capita, 2015)", y = "Wage Bill") +
        theme_minimal()
      doc <- doc %>% body_add_gg(dot_plot, style = "centered")
      print(doc, target = file)
    }
  )
  
  output$worldMap <- renderLeaflet({
    data_values <- filtered_data_for_map()
    color_pal <- colorNumeric("Greens", domain = c(min(data_values$value_percentage, na.rm = TRUE), 
                                                   max(data_values$value_percentage, na.rm = TRUE)))
    leaflet(world_spdf) %>% addTiles() %>% setView(lng = 0, lat = 20, zoom = 2) %>%
      addLegend(position = "bottomright", pal = color_pal, 
                values = c(min(data_values$value_percentage, na.rm = TRUE), 
                           max(data_values$value_percentage, na.rm = TRUE)),
                title = "Indicator Value", opacity = 1)
  })
  
  filtered_data_for_map <- reactive({
    req(input$indicatorSelect, input$yearSelect)
    data_wwbi %>% filter(indicator_name == input$indicatorSelect, 
                         !is.na(.data[[paste0("year_", input$yearSelect)]])) %>%
      transmute(country_name, indicator_name, value_percentage = .data[[paste0("year_", input$yearSelect)]])
  })
  
  observe({
    req(input$indicatorSelect, input$yearSelect)
    reported_countries <- filtered_data_for_map()
    if(is.null(reported_countries) || nrow(reported_countries) == 0) return()
    world_data_merged <- world_spdf %>% left_join(reported_countries, by = "country_name")
    color_pal <- colorNumeric("Greens", domain = c(min(reported_countries$value_percentage, na.rm = TRUE),
                                                   max(reported_countries$value_percentage, na.rm = TRUE)))
    leafletProxy("worldMap") %>% clearShapes() %>% 
      addPolygons(data = world_data_merged,
                  fillColor = ~ifelse(is.na(value_percentage), "#808080", color_pal(value_percentage)),
                  fillOpacity = 0.7,
                  color = "white",
                  weight = 1,
                  highlightOptions = highlightOptions(color = "#FFD700", weight = 2, fillOpacity = 0.9),
                  label = ~paste0("<strong>Country:</strong> ", country_name, "<br>",
                                  ifelse(!is.na(value_percentage), paste0("<strong>Value:</strong> ", round(value_percentage, 2)), "<strong>No Data</strong>")),
                  popup = ~paste("Country:", country_name, "<br>Indicator:", indicator_name,
                                 ifelse(!is.na(value_percentage), paste("<br>Value:", round(value_percentage, 2)), "<br>No Data Available"))
      )
    
    output$countryCount <- renderText({
      paste("Total Countries with Data:", nrow(reported_countries))
    })
  })
  
  # Change infoBox colors to "purple" to match the quartz theme
  output$numberIndicatorsBox <- renderInfoBox({
    infoBox("Indicators", 302, icon = icon("list"), color = "purple")
  })
  
  output$numberCountriesBox <- renderInfoBox({
    infoBox("Economies", length(unique(data_wwbi$country_name)), icon = icon("globe"), color = "purple")
  })
  
  output$temporalCoverageAnnualBox <- renderInfoBox({
    infoBox("Temporal Coverage (Annual)", "2000-2022", icon = icon("calendar"), color = "purple")
  })
  
  output$temporalCoverageYearsBox <- renderInfoBox({
    infoBox("Temporal Coverage (Years)", "22", icon = icon("calendar"), color = "purple")
  })
  
  output$lastUpdatedBox <- renderInfoBox({
    infoBox("Last Updated", "2022", icon = icon("clock"), color = "purple")
  })
  
}

shinyApp(ui = ui, server = server)

