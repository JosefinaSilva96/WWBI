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

data_wwbi <- as.data.frame(data_wwbi)

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
  rename(value = mean_value)

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


# Define UI ----

ui <- dashboardPage(
  dashboardHeader(title = "WWB Indicators"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Metadata", tabName = "indicators", icon = icon("globe")),
      menuItem("Wage Bill Graphs", tabName = "wageBillGraphs", icon = icon("chart-line")), 
      menuItem("Wage Bill and GDP Graphs", tabName = "wageBillgdpGraphs", icon = icon("chart-line")), 
      menuItem("Public Sector Workforce Graphs", tabName = "publicSectorWorkforceGraphs", icon = icon("chart-line")), 
      menuItem("Gender Workforce Graphs", tabName = "genderWorkforceGraphs", icon = icon("chart-line")), 
      menuItem("Tertiary Education Graphs", tabName = "educationGraphs", icon = icon("chart-line")),
      menuItem("Public Sector Wage Premium", tabName = "publicsectorwagepremiumGraphs", icon = icon("chart-line")),
      menuItem("Public Sector Education Workers", tabName = "publicsectoreducationGraphs", icon = icon("chart-line")), 
      menuItem("Public Sector Graphs", tabName = "publicsectorGraphs", icon = icon("chart-line")), 
      menuItem("Wage Premium Gender Graphs", tabName = "wagepremiumgenderGraphs", icon = icon("chart-line")), 
      menuItem("Female Leadership Graphs", tabName = "womenleadershipGraphs", icon = icon("chart-line")), 
      menuItem("Download All Graphs", tabName = "downloadAllGraphs", icon = icon("download")) 
    )
  ),
  dashboardBody(
    theme = bs_theme(version = 5, bootswatch = 'quartz'),
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Dashboard Description", status = "primary", solidHeader = TRUE, width = 12,
                    "Welcome to the Worldwide Bureaucracy Indicators (WWBI)
                    The Worldwide Bureaucracy Indicators (WWBI) database is a unique cross-national dataset on public sector employment and wages that aims to fill an information gap, thereby helping researchers, development practitioners, and policymakers gain a better understanding of the personnel dimensions of state capability, the footprint of the public sector within the overall labor market, and the fiscal implications of the public sector wage bill. The dataset is derived from administrative data and household surveys, thereby complementing existing, expert perception-based approaches.")
              )
      ),
      # Wage Bill Graphs Tab
      tabItem(
        tabName = "wageBillGraphs",
        
        # First Row with Indicator and Selection for Countries or Region
        fluidRow(
          # Indicator Selection Box
          box(
            title = "WWB Indicator Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            selectInput(
              "indicator",
              "Select a WWB Indicator",
              choices = c(
                "Wage bill (as % of public expenditure) over time",
                "Wage bill as a percentage of GDP"
              )
            )
          ),
          
          # Country and Graph Selection Box
          box(
            title = "Country and Graph Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            
            # Country or Region Selection
            selectInput(
              "countries",
              "Select Countries:",
              choices = unique(wage_bill_publicexp$country_name),
              selected = unique(wage_bill_publicexp$country_name)[1],
              multiple = TRUE
            ),
            
            # Checkbox group for graph selection
            checkboxGroupInput(
              "graphs_to_download",
              "Select Graphs to Download",
              choices = list(
                "Wage Bill (as % of public expenditure)" = "PublicExpenditure",
                "Wage Bill (as % of GDP)" = "GDP"
              ),
              selected = c("PublicExpenditure", "GDP")
            ),
            
            # Download button
            downloadButton("downloadWord", "Download Word Document")
          )
        ),
        
        # Second Row for displaying the graph
        fluidRow(
          box(
            title = "Graph",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot")
          )
        )
      ),
      
      # Wage Bill Graphs GDP Tab
      tabItem(
        tabName = "wageBillgdpGraphs",
        
        # Header row with description
        fluidRow(
          box(
            title = "Dot Plot: Wage Bill vs. GDP per Capita (Log Scale)", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            p("This visualization explores the relationship between wage bill (as an indicator) and GDP per capita (log scale). 
         You can select specific countries to highlight and observe their trends.")
          )
        ),
        
        # Row for country selection and download
        fluidRow(
          box(
            title = "Select Countries and Download", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,
            selectInput(
              "countries_first", 
              label = "Select Countries:", 
              choices = unique(merged_data$country_name), 
              selected = NULL, 
              multiple = TRUE,
              width = "100%"
            ),
            downloadButton("downloadGDPDoc", "Download GDP Analysis Report")
          ),
          
          # Radio Buttons for selecting between country or region
          box(
            title = "Select Label Type", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,
            radioButtons(
              "label_type", "Choose Label Type", 
              choices = c("Country", "Region"), 
              selected = "Country"
            )
          )
        ),
        
        # Row for displaying the plot
        fluidRow(
          box(
            title = "Dot Plot Visualization", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            plotlyOutput("dot_plot", height = "500px")
          )
        )
      ),
      
      # Public Sector Graphs Tab
      tabItem(tabName = "publicSectorGraphs",
              fluidRow(
                box(title = "First Graph - Multi-Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("countries_first", 
                                "Select Countries for First Graph", 
                                choices = unique(public_sector_emp_temp_last$country_name), 
                                selected = NULL, 
                                multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "First Graph", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("firstGraph")
                )
              ),
              fluidRow(
                box(title = "Second Graph - Single Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("country_second", 
                                "Select Country for Second Graph", 
                                choices = unique(public_sector_emp_temp_last$country_name), 
                                selected = NULL, 
                                multiple = FALSE)
                )
              ),
              fluidRow(
                box(title = "Second Graph", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("secondGraph")
                )
              ),
              # Add graph selection and download functionality
              fluidRow(
                box(
                  title = "Download Graphs",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  column(
                    width = 6,
                    checkboxGroupInput(
                      "selected_graphs_public", 
                      "Select Graphs to Download:", 
                      choices = c(
                        "Multi-Country Graph" = "firstGraph",
                        "Single-Country Graph" = "secondGraph"
                      ),
                      selected = c("firstGraph", "secondGraph") # Default: all selected
                    )
                  ),
                  column(
                    width = 6,
                    downloadButton("downloadGraphsWord", "Download Graphs as Word File")
                  )
                )
              )
      ),
      # Public Sector Workforce Graphs Tab
      tabItem(
        tabName = "publicSectorWorkforceGraphs",
        
        # Country Selection for Stacked Bar Chart
        fluidRow(
          box(
            title = "Country Selection for Stacked Bar Chart",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput(
              "countries_workforce",
              "Select Countries for Workforce Graph",
              choices = unique(public_sector_workforce$country_name),  # Use updated dataset
              selected = unique(public_sector_workforce$country_name)[1],  # Default selection
              multiple = TRUE  # Allow multiple country selection
            )
          )
        ),
        
        # Stacked Bar Chart
        fluidRow(
          box(
            title = "Stacked Bar Chart: Public Workforce Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("stackedBarGraph", height = "600px")  # Render first graph
          )
        ),
        
        # Country Selection for Horizontal Stacked Bar Chart
        fluidRow(
          box(
            title = "Country Selection for Horizontal Stacked Bar Chart",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput(
              "selected_country",
              "Select Country for Second Graph",
              choices = unique(public_sector_workforce$country_name),  # Ensure consistency with main dataset
              selected = NULL,
              multiple = FALSE  # Single country selection
            )
          )
        ),
        
        # Horizontal Stacked Bar Chart
        fluidRow(
          box(
            title = "Horizontal Stacked Bar Chart: Sectoral Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            uiOutput("messageOutput"),  # Display message if insufficient data
            plotlyOutput("horizontalStackedBar", height = "600px")  # Render second graph
          )
        ),
        
        # Graph Selection and Download Functionality
        fluidRow(
          box(
            title = "Download Selected Graphs",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            checkboxGroupInput(
              "selected_graphs_public", 
              "Select Graphs to Download", 
              choices = c(
                "First Graph: Multi-Country" = "firstGraph",
                "Second Graph: Single Country" = "secondGraph"
              ),
              selected = c("firstGraph", "secondGraph")  # Default to all selected
            ),
            downloadButton("downloadGraphsWordworkforce", "Download Selected Graphs in Word")
          )
        )
      ),
      
      # Gender Workforce Graphs Tab
      tabItem(
        tabName = "genderWorkforceGraphs",
        
        # First Graph: Multiple Countries Selection
        fluidRow(
          box(
            title = "Country Selection (Multiple Countries)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput(
              inputId = "countries_workforce", 
              label = "Select Countries for Workforce Graph", 
              choices = unique(public_sector_workforce$country_name), 
              selected = unique(public_sector_workforce$country_name)[1], 
              multiple = TRUE # Allow multiple countries selection
            )
          )
        ),
        fluidRow(
          box(
            title = "Female Employment by Sector (Multiple Countries)", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            plotlyOutput(outputId = "employment_plot", height = "600px")
          )
        ),
        
        # Second Graph: Single Country Selection
        fluidRow(
          box(
            title = "Country Selection (Single Country)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput(
              inputId = "selected_country", 
              label = "Select Country for Second Graph", 
              choices = unique(gender_workforce$country_name), 
              selected = unique(gender_workforce$country_name)[1], 
              multiple = FALSE # Allow single country selection
            )
          )
        ),
        fluidRow(
          box(
            title = "Gender Workforce Over Time (Single Country)", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            plotlyOutput(outputId = "employment_plot_overtime", height = "600px")
          )
        ),
        
        # Download Button for Word Report
        fluidRow(
          box(
            title = "Download Report",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            downloadButton(
              outputId = "downloadGraphsWordworkforce", 
              label = "Download Gender Workforce Report"
            )
          )
        )
      ),

      # Tertiary Education Graphs Tab
      tabItem(tabName = "educationGraphs",
              fluidRow(
                box(
                  title = "Country Selection", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  selectInput("selected_countries", "Select Countries", 
                              choices = unique(tertiary_education$country_name),
                              selected = unique(tertiary_education$country_name)[1:3], # Default selection
                              multiple = TRUE
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Tertiary Education Graphs", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  plotlyOutput("barPlot", height = "600px")
                )
              ),
              # Download Button for Tertiary Education Report
              fluidRow(
                box(
                  title = "Download Report",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  downloadButton(
                    outputId = "downloadGraphsWordEducation", 
                    label = "Download Tertiary Education Report"
                  )
                )
              )
      ),
      #Public Sector Wage Premium
      tabItem(tabName = "publicsectorwagepremiumGraphs",
              fluidRow(
                box(title = "Multi-Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("countries_first", 
                                "Select Countries for First Graph", 
                                choices = unique(public_wage_premium$country_name),
                                selected = unique(public_wage_premium$country_name)[1],  # Default selection
                                multiple = TRUE)                                        # Allow multiple selections
                )
              ),
              fluidRow(
                box(title = "Public Sector Wage Premium (Compared to All Private Employees) by Country", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("dotPlot")
                )
              )
      ),
      # Public Sector Education Graphs Tab
      tabItem(tabName = "publicsectoreducationGraphs",
              fluidRow(
                box(
                  title = "Single-Country Selection", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  selectInput(
                    inputId = "selected_country", 
                    label = "Select Country for Graph", 
                    choices = unique(public_wage_premium_educ$country_name),
                    selected = unique(public_wage_premium_educ$country_name)[1],  # Default selection
                    multiple = FALSE                                             # Single selection only
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Public Sector Wage Premium by Education Level (Compared to Private Formal Workers)", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  plotlyOutput(outputId = "barPloteduc")
                )
              )
      ),
      #Public Sector Graphs
      tabItem(tabName = "publicsectorGraphs",
              fluidRow(
                box(title = "First Graph - Multi-Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("countries_first", 
                                "Select Countries for First Graph", 
                                choices = unique(public_sector_emp_temp_last$country_name), 
                                selected = NULL, 
                                multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "First Graph", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("firstGraphpublic")
                )
              ),
              fluidRow(
                box(title = "Second Graph - Single Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("country_second", 
                                "Select Country for Second Graph", 
                                choices = unique(public_sector_emp_temp$country_name), 
                                selected = NULL, 
                                multiple = FALSE)
                )
              ),
              fluidRow(
                box(title = "Second Graph", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("secondGraphpublic")
                )
              ),
              # Add graph selection and download functionality
              fluidRow(
                box(
                  title = "Download Graphs",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  column(
                    width = 6,
                    checkboxGroupInput(
                      "selected_graphs_public", 
                      "Select Graphs to Download:", 
                      choices = c(
                        "Multi-Country Graph" = "firstGraph",
                        "Single-Country Graph" = "secondGraph"
                      ),
                      selected = c("firstGraph", "secondGraph") # Default: all selected
                    )
                  ),
                  column(
                    width = 6,
                    downloadButton("downloadGraphsWord", "Download Graphs as Word File", class = "btn-primary btn-block")
                  )
                )
              )
      ),
      # Wage Premium gender Graphs
      tabItem(tabName = "wagepremiumgenderGraphs",
              fluidRow(
                box(title = "First Graph - Multi-Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("countries_first", 
                                "Select Countries for First Graph", 
                                choices = unique(gender_wage_premium_last$country_name), 
                                selected = NULL, 
                                multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "First Graph", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("firstGraphgender")
                )
              ),
              fluidRow(
                box(title = "Second Graph - Single Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("country_second", 
                                "Select Country for Second Graph", 
                                choices = unique(gender_wage_premium$country_name), 
                                selected = NULL, 
                                multiple = FALSE)
                )
              ),
              fluidRow(
                box(title = "Second Graph", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("secondGraphgender")
                )
              ),
              # Add graph selection and download functionality
              fluidRow(
                box(
                  title = "Download Graphs",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  column(
                    width = 6,
                    checkboxGroupInput(
                      "selected_graphs_public", 
                      "Select Graphs to Download:", 
                      choices = c(
                        "Multi-Country Graph" = "firstGraph",
                        "Single-Country Graph" = "secondGraph"
                      ),
                      selected = c("firstGraph", "secondGraph") # Default: all selected
                    )
                  ),
                  column(
                    width = 6,
                    downloadButton("downloadGraphsWordgender", "Download Graphs as Word File", class = "btn-primary btn-block")
                  )
                )
              )
      ),
      # Women Leadership Graphs Tab
      tabItem(tabName = "womenleadershipGraphs",
              fluidRow(
                box(
                  title = "Country Selection", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  selectInput("selected_countries", "Select Countries", 
                              choices = unique(gender_leadership$country_name),
                              selected = unique(gender_leadership$country_name)[1:3], # Default selection
                              multiple = TRUE
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Women Leadership Graphs", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  plotlyOutput("barPlotwomen", height = "600px")
                )
              ),
              # Download Button for Women Leadership Report
              fluidRow(
                box(
                  title = "Download Report",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  downloadButton(
                    outputId = "downloadGraphsWordfemale", 
                    label = "Download Female Leadership Report"
                  )
                )
              )
      ),
      # Download All Graphs Tab
      tabItem(
        tabName = "downloadAllGraphs",
        fluidRow(
          box(
            title = "Download All Graphs and Report", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            p("Download a comprehensive report containing all visualizations and analyses."),
            downloadButton("downloadAllGraphsDoc", "Download Full Report")
          )
        )
      ),
      # Indicators and Widgets Tab
      tabItem(tabName = "indicators",
              # Widgets Section
              fluidRow(
                infoBoxOutput("numberIndicatorsBox", width = 6),
                infoBoxOutput("numberCountriesBox", width = 6),
                infoBoxOutput("temporalCoverageAnnualBox", width = 6),
                infoBoxOutput("temporalCoverageYearsBox", width = 6),
                infoBoxOutput("lastUpdatedBox", width = 6)
              ),
              
              # World Map Section
              fluidRow(
                box(title = "Indicator Status Across Countries", status = "primary", solidHeader = TRUE, width = 12,
                    "This map shows which countries have reported data for the selected indicator."
                )
              ),
              fluidRow(
                box(title = "Indicator Selection", status = "primary", solidHeader = TRUE, width = 4,
                    selectInput("indicatorSelect", "Select Indicator", 
                                choices = unique(data_wwbi$indicator_name), selected = NULL)
                ),
                box(title = "Year Selection", status = "primary", solidHeader = TRUE, width = 4,
                    selectInput("yearSelect", "Select Year", 
                                choices = 2010:2022, selected = 2022)
                )
              ), 
              fluidRow(
                box(title = "Country Count", status = "primary", solidHeader = TRUE, width = 12,
                    textOutput("countryCount")  # Display the country count
                )
              ), 
              fluidRow(
                box(title = "World Map", status = "primary", solidHeader = TRUE, width = 12,
                    leafletOutput("worldMap", height = "600px")
                )
              )
      )
  )
))



# Define Server ----

server <- function(input, output, session) {
  
  # Reactive expression to select the appropriate dataset based on the indicator
  selected_data <- reactive({
    req(input$countries)  # Ensure 'countries' input is available
    
    # Check which indicator was selected and filter corresponding data
    if (input$indicator == "Wage bill as a percentage of GDP") {
      data <- wage_bill_gdp %>% filter(country_name %in% input$countries)
    } else {
      data <- wage_bill_publicexp %>% filter(country_name %in% input$countries)
    }
    
    return(data)
  })
  
  # Render Plotly plot for the main indicator
  output$plot <- renderPlotly({
    data_to_plot <- selected_data()
    
    # Define plot titles and plot mode based on selected indicator
    title_text <- ifelse(input$indicator == "Wage bill as a percentage of GDP",
                         "Wage Bill as % of GDP Over Time",
                         "Wage Bill as % of Public Expenditure Over Time")
    plot_mode <- 'lines+markers'  # Common plot mode for both indicators
    
    # Dynamically assign color based on input$label_type (Country or Region)
    color_variable <- ifelse(input$label_type == "Country", "country_name", "region")
    
    # Create the Plotly plot
    plot <- plot_ly(data = data_to_plot, 
                    x = ~year, 
                    y = ~value, 
                    color = as.formula(paste("~", color_variable)),  # Dynamically assign color
                    type = 'scatter', 
                    mode = plot_mode,
                    marker = list(size = 8)) %>%
      layout(title = title_text,
             xaxis = list(title = "Year", dtick = 2),
             yaxis = list(title = ifelse(input$indicator == "Wage bill as a percentage of GDP", 
                                         "Wage Bill (% of GDP)", "Wage Bill (%)")),
             legend = list(title = list(text = ifelse(input$label_type == "Country", "Country", "Region"))))  # Dynamic legend title
    
    plot  # Return the final plot
  })
  # Create the download handler for Word document with graphs
  
  output$downloadWord <- downloadHandler(
    filename = function() {
      paste0("Wage_Bill_Analysis_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Debugging: Ensure the input is being passed correctly
      print(input$countries)
      
      # Extract the first selected country
      if (!is.null(input$countries) && length(input$countries) > 0) {
        countries <- input$countries[1]  # Take the first country
      } else {
        countries <- "Unknown Country"  # Fallback in case no country is selected
      }
      
      # Dynamic title with the first country
      report_title <- paste("Wage Bill Analysis Report -", countries)
      
      doc <- read_docx()  # Start a new Word document
      
      # Define the style for the title
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      
      # Apply the custom title style
      doc <- doc %>%
        body_add_fpar(fpar(ftext(report_title, prop = title_style)))
      
      # Add introduction
      doc <- doc %>%
        body_add_par("Introduction", style = "heading 2") %>%
        body_add_par("This note presents evidence on public sector employment and compensation practices in Bangladesh using the Worldwide Bureaucracy Indicators (WWBI). The primary data source is the Labor Force Survey (LFS), conducted by the Bangladesh Bureau of Statistics (BBS), which offers extensive, nationally representative data over multiple years up to 2022. The LFS’s comprehensive coverage of employment and wage issues across both public and private sectors, along with its frequency and national representativeness, makes it an ideal source for this analysis.
                     For international comparisons, the analysis includes a set of peer countries for benchmarking, with a particular focus on countries from the South Asia region and other aspirational peers. Information on these peers was also sourced from the WWBI.
                     The public sector is typically a major source of employment in most countries. The provision of basic services such as education, health, citizen security and justice, among others, makes it a central actor in labor markets, with significant impacts on the aggregate results of employment, wages, informality, and other economic variables. Moreover, public employment is an indicator of the state participation in the entire economy, which has implications for macroeconomic balances, allocation efficiency and income distribution. Thus, this analysis comprehensively documents the size of public employment, its changes over time, and the characteristics of its workforce.")
      
      # Separate data for each graph
      gdp_data <- wage_bill_gdp %>% filter(country_name %in% input$countries)
      public_exp_data <- wage_bill_publicexp %>% filter(country_name %in% input$countries)
      
      if ("GDP" %in% input$graphs_to_download) {
        # Graph 1: Wage Bill as % of GDP
        graph1 <- ggplot(gdp_data, aes(x = year, y = value, color = country_name)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          labs(
            title = "Wage Bill as % of GDP Over Time",
            x = "Year",
            y = "Wage Bill (% of GDP)"
          ) +
          theme_minimal()
        
        doc <- body_add_gg(doc, value = graph1, style = "centered") %>%
          body_add_par("This graph shows the wage bill as a percentage of GDP over time for the selected countries.", style = "Normal")
      }
      
      if ("PublicExpenditure" %in% input$graphs_to_download) {
        # Graph 2: Wage Bill as % of Public Expenditure
        graph2 <- ggplot(public_exp_data, aes(x = year, y = value, color = country_name)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          labs(
            title = "Wage Bill as % of Public Expenditure Over Time",
            x = "Year",
            y = "Wage Bill (% of Public Expenditure)"
          ) +
          theme_minimal()
        
        doc <- body_add_gg(doc, value = graph2, style = "centered") %>%
          body_add_par("This graph shows the wage bill as a percentage of public expenditure over time for the selected countries.", style = "Normal")
      }
      
      # Save the Word document
      print(doc, target = file)
    }
  )
  
  # Render the dot plot GDP 
  
  
  # Define a reactive expression for filtered data
  filtered_data <- reactive({
    req(input$countries_first)  # Ensure input exists
    
    # Filter merged_data based on the selected countries
    merged_data %>%
      filter(country_name %in% input$countries_first) %>%
      mutate(color = ifelse(country_name == input$countries_first[1], "#B3242B", "#003366"))  # Highlight the first country
  })
  
  # Render the dot plot
  output$dot_plot <- renderPlotly({
    # Use the reactive filtered data
    filtered_data_df <- filtered_data()
    
    # Ensure the filtered data has rows
    req(nrow(filtered_data_df) > 0)
    
    # Fit a linear model for the trendline
    trendline_model <- lm(indicator_value ~ log_gdp, data = filtered_data_df)
    trendline_values <- predict(trendline_model, newdata = filtered_data_df)
    
    # Create the Plotly plot
    plot_ly() %>%
      add_trace(
        data = filtered_data_df,
        x = ~log_gdp,
        y = ~indicator_value,
        type = "scatter",
        mode = "markers+text",
        text = ~country_name,
        textposition = "top center",
        marker = list(
          size = 10,
          color = ~color,
          opacity = 0.7
        )
      ) %>%
      add_trace(
        x = filtered_data_df$log_gdp,
        y = trendline_values,
        type = "scatter",
        mode = "lines",
        line = list(color = "gray", dash = "dash"),
        name = "Trendline"
      ) %>%
      layout(
        title = "Wage Bill vs. Log(GDP per Capita)",
        xaxis = list(title = "Log(GDP per Capita, 2015)", showticklabels = TRUE),
        yaxis = list(title = "Wage Bill", showticklabels = TRUE),
        showlegend = FALSE
      )
  }
  )
  # Generate static ggplot for the dot plot
  generate_dot_plot <- function(filtered_data, first_country) {
    ggplot(filtered_data, aes(x = log_gdp, y = indicator_value)) +
      geom_point(aes(color = color), size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", color = "gray") +
      geom_text(aes(label = country_name), vjust = -0.5, size = 3) +
      scale_color_identity() +
      labs(
        title = "Wage Bill vs. Log(GDP per Capita)",
        x = "Log(GDP per Capita, 2015)",
        y = "Wage Bill"
      ) +
      theme_minimal()
  }
  
  # Download Word Report
  output$downloadGDPDoc <- downloadHandler(
    filename = function() {
      paste0("Wage_Bill_vs_GDP_Report_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Use the reactive filtered data
      filtered_data_df <- filtered_data()
      req(nrow(filtered_data_df) > 0)  # Ensure data has rows
      
      # Extract the first selected country
      if (!is.null(input$countries) && length(input$countries) > 0) {
        countries <- input$countries[1]  # Take the first country
      } else {
        countries <- "Unknown Country"  # Fallback in case no country is selected
      }
      
      # Dynamic title with the first country
      report_title <- paste("Wage Bill vs. GDP Analysis Report -", countries)
      
      # Create a Word document
      doc <- read_docx()
      
      # Define the style for the title
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      
      
      # Add the Introduction
      doc <- doc %>%
        body_add_par("Introduction", style = "heading 2") %>%
        body_add_par(
          "This note presents evidence on public sector employment and compensation practices in Bangladesh using the Worldwide Bureaucracy Indicators (WWBI). The primary data source is the Labor Force Survey (LFS), conducted by the Bangladesh Bureau of Statistics (BBS), which offers extensive, nationally representative data over multiple years up to 2022. The LFS’s comprehensive coverage of employment and wage issues across both public and private sectors, along with its frequency and national representativeness, makes it an ideal source for this analysis.
                     For international comparisons, the analysis includes a set of peer countries for benchmarking, with a particular focus on countries from the South Asia region and other aspirational peers. Information on these peers was also sourced from the WWBI.
                     The public sector is typically a major source of employment in most countries. The provision of basic services such as education, health, citizen security and justice, among others, makes it a central actor in labor markets, with significant impacts on the aggregate results of employment, wages, informality, and other economic variables. Moreover, public employment is an indicator of the state participation in the entire economy, which has implications for macroeconomic balances, allocation efficiency and income distribution. Thus, this analysis comprehensively documents the size of public employment, its changes over time, and the characteristics of its workforce
",
          style = "Normal"
        )
      
      # Create the Dot Plot
      plot <- ggplot(filtered_data_df, aes(x = log_gdp, y = indicator_value, color = country_name)) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
        labs(
          title = "Wage Bill vs. Log(GDP per Capita)",
          x = "Log(GDP per Capita, 2015)",
          y = "Wage Bill"
        ) +
        theme_minimal()
      
      # Add the plot to the Word document
      doc <- doc %>% body_add_gg(value = plot, style = "centered")
      
      # Save the document to the specified file path
      print(doc, target = file)
    }
  )
  
  #Public Sector Workforce
  
  # Updated reactive expression (without filtering directly in the expression)
  filtered_workforce_data <- reactive({
    req(input$countries_workforce)  # Ensure input is not null
    public_sector_workforce %>%
      group_by(country_name, indicator_name) %>%
      slice_max(order_by = year, n = 1) %>%
      ungroup()
  })
  
  # Render the stacked bar graph
  output$stackedBarGraph <- renderPlotly({
    req(input$countries_workforce)  # Ensure countries_workforce input is valid
    
    # Apply filter after accessing the reactive data
    data_to_plot <- filtered_workforce_data() %>%
      filter(country_name %in% input$countries_workforce)  # Filter the data here
    
    req(nrow(data_to_plot) > 0)  # Ensure there's data to plot
    
    plot_ly(
      data = data_to_plot,
      x = ~country_name,
      y = ~value_percentage,
      color = ~indicator_name,
      type = "bar",
      text = ~paste(
        "Country:", country_name,
        "<br>Indicator:", indicator_name,
        "<br>Value:", round(value_percentage, 1), "%"
      ),  # Detailed hover information
      textposition = "auto",
      colors = c(
        "Public Administration" = "#568340", 
        "Education" = "#B3242B", 
        "Health" = "#003366", 
        "Other" = "#A9A9A9"  # Gray for "Other"
      )
    ) %>%
      layout(
        barmode = "stack",
        title = "Public Workforce Distribution by Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Workforce Distribution (%)", range = c(0, 100)),
        legend = list(title = list(text = "<b>Indicator</b>"))
      )
  })
  
  # Second Graph: Horizontal Stacked Bar Chart (Single Country)
  output$messageOutput <- renderUI({
    # Filter data for the selected country
    filtered_data <- public_sector_workforce %>%
      filter(country_name == input$selected_country)
    
    # Check if there is enough data
    if (nrow(filtered_data) < 2) {
      message <- "Not enough data available for this country to create the graph."
      return(tags$p(message, style = "color: red; font-weight: bold;"))
    }
    return(NULL)
  })
  
  # Check if filtered_data is empty before calculating min and max
  output$horizontalStackedBar <- renderPlotly({
    req(input$selected_country)
    
    filtered_data <- public_sector_workforce %>%
      filter(country_name == input$selected_country)
    
    # Check if there is enough data
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return NULL if there's no data for the selected country
    }
    
    first_year <- min(filtered_data$year, na.rm = TRUE)
    last_year <- max(filtered_data$year, na.rm = TRUE)
    
    # Check again if first_year and last_year are finite values
    if (is.infinite(first_year) | is.infinite(last_year)) {
      return(NULL)  # Return NULL if years are infinite (empty data)
    }
    
    data_to_plot <- filtered_data %>%
      filter(year %in% c(first_year, last_year)) %>%
      group_by(year, indicator_name) %>%
      summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")
    
    # Create the horizontal stacked bar chart
    plot_ly(data = data_to_plot,
            x = ~value_percentage,  # Horizontal bar
            y = ~factor(year, levels = c(last_year, first_year)),  # Ensure correct order
            color = ~indicator_name,
            type = "bar",
            orientation = "h",  # Horizontal orientation
            text = ~paste0(round(value_percentage, 1), "%"),  # Add percentage as text
            textposition = "inside",
            colors = c(
              "Public Administration" = "#568340",
              "Education" = "#B3242B",
              "Health" = "#003366", 
              "Other" = "#A9A9A9"
            )
    ) %>%
      layout(
        barmode = "stack",
        title = paste("Sectoral Distribution of Public Sector Workforce in", input$selected_country, 
                      "(", first_year, "&", last_year, ")"),
        xaxis = list(title = "Percentage (%)"),
        yaxis = list(title = "Year"),
        legend = list(title = list(text = "Sector"))
      )
  })
  
  # Download Handler for Word Report
  output$downloadGraphsWordworkforce <- downloadHandler(
    filename = function() {
      paste0("Public_Sector_Analysis_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()  # Start a new Word document
      
      # Title for the report
      report_title <- paste("Public Sector Workforce Analysis")
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>%
        body_add_fpar(fpar(ftext(report_title, prop = title_style)))
      
      # First Graph (Stacked Bar Graph for Multiple Countries using ggplot2)
      first_graph_data <- filtered_workforce_data() %>%
        filter(country_name %in% input$countries_workforce)
      first_graph_ggplot <- ggplot(first_graph_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c(
          "Public Administration" = "#568340", 
          "Education" = "#B3242B", 
          "Health" = "#003366", 
          "Other" = "#A9A9A9"
        )) +
        labs(title = "Public Workforce Distribution by Country", x = "Country", y = "Workforce Distribution (%)") +
        theme_minimal()
      
      # Save the first graph as a PNG file
      ggsave("first_graph.png", plot = first_graph_ggplot, width = 6, height = 4)
      
      # Add first graph to the Word document
      doc <- doc %>%
        body_add_par("First Graph: Public Workforce Distribution by Country", style = "heading 1") %>%
        body_add_img(src = "first_graph.png", width = 6, height = 4) %>%
        body_add_par("This graph shows the public workforce distribution across multiple countries.", style = "Normal")
      
      # Second Graph (Horizontal Stacked Bar Chart for Single Country using ggplot2)
      filtered_data <- public_sector_workforce %>%
        filter(country_name == input$selected_country)
      
      # Check if there is enough data
      if (nrow(filtered_data) < 2) {
        message <- "Not enough data available for this country to create the graph."
        doc <- doc %>%
          body_add_par(message, style = "Normal")
      } else {
        # Calculate first_year and last_year for the selected country
        first_year <- min(filtered_data$year, na.rm = TRUE)
        last_year <- max(filtered_data$year, na.rm = TRUE)
        
        # Check if first_year and last_year are valid (finite values)
        if (!is.finite(first_year) | !is.finite(last_year)) {
          message <- "Invalid year data for the selected country."
          doc <- doc %>%
            body_add_par(message, style = "Normal")
        } else {
          # Filter the data for the first and last year
          second_graph_data <- filtered_data %>%
            filter(year %in% c(first_year, last_year)) %>%
            group_by(year, indicator_name) %>%
            summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")
          
          second_graph_ggplot <- ggplot(second_graph_data, aes(x = value_percentage, y = factor(year, levels = c(last_year, first_year)), fill = indicator_name)) +
            geom_bar(stat = "identity", position = "stack", orientation = "horizontal") +
            scale_fill_manual(values = c(
              "Public Administration" = "#568340",
              "Education" = "#B3242B",
              "Health" = "#003366", 
              "Other" = "#A9A9A9"
            )) +
            labs(title = paste("Sectoral Distribution of Public Sector Workforce in", input$selected_country), 
                 x = "Percentage (%)", y = "Year") +
            theme_minimal()
          
          # Save the second graph as a PNG file
          ggsave("second_graph.png", plot = second_graph_ggplot, width = 6, height = 4)
          
          # Add second graph to the Word document
          doc <- doc %>%
            body_add_par("Second Graph: Sectoral Distribution of Public Sector Workforce", style = "heading 1") %>%
            body_add_img(src = "second_graph.png", width = 6, height = 4) %>%
            body_add_par("This graph shows the sectoral distribution of public sector workforce for the selected country.", style = "Normal")
        }
      }
      
      # Save the Word document
      print(doc, target = file)
    }
  )
  #Gender Workforce 
  output$employment_plot <- renderPlotly({
    # Filter data based on selected countries
    filtered_data <- gender_workforce %>%
      filter(country_name %in% input$countries_workforce)
    
    # Handle missing or incomplete data
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return NULL if no data available
    }
    
    # Last year available for public sector
    public_latest <- filtered_data %>%
      filter(indicator_name == "Females, as a share of public paid employees") %>%
      group_by(country_name) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      ungroup()
    
    # Last year available for private sector
    private_latest <- filtered_data %>%
      filter(indicator_name == "Females, as a share of private paid employees") %>%
      group_by(country_name) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      ungroup()
    
    # Ensure private_latest and public_latest exist
    if (nrow(public_latest) == 0 || nrow(private_latest) == 0) {
      return(NULL)  # Return NULL if no valid data
    }
    
    # Create the plot
    plot <- plot_ly(
      data = public_latest,
      x = ~country_name,
      y = ~value_percentage,
      type = 'bar',
      color = I("#003366"),
      text = ~paste("Country: ", country_name,
                    "<br>Last year available: ", year,
                    "<br>Employment (%): ", round(value_percentage, 2)),
      hoverinfo = "text",
      name = "Public Sector",
      showlegend = TRUE
    ) %>%
      add_trace(
        data = private_latest,
        x = ~country_name,
        y = ~value_percentage,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, color = "#B3242B"),
        name = "Private Sector",
        text = ~paste("Country: ", country_name,
                      "<br>Last year available: ", year,
                      "<br>Employment (%): ", round(value_percentage, 2)),
        hoverinfo = "text",
        showlegend = TRUE
      ) %>%
      layout(
        barmode = "group",
        title = "Female Employment by Sector (Last Year Available)",
        xaxis = list(
          title = "Country (Last Year Available)",
          tickmode = 'array',
          tickvals = public_latest$country_name,
          ticktext = paste(public_latest$country_name, 
                           "(", public_latest$year, ")")
        ),
        yaxis = list(title = "Employment (%)"),
        legend = list(title = list(text = "Sector"))
      )
    
    plot
  })
  
  # Second Graph: Female Employment by Sector Over Time (Single Country)
  output$employment_plot_overtime <- renderPlotly({
    # Filter the data for the selected country
    filtered_data <- gender_workforce %>% 
      filter(country_name == input$selected_country)
    
    # Handle missing or incomplete data
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return NULL if no data available
    }
    
    # Define a custom color palette
    custom_colors <- c(
      "Females, as a share of private paid employees" = "#003366", 
      "Females, as a share of public paid employees" = "#B3242B"
    )
    
    # Create the Plotly graph
    plot <- filtered_data %>%
      plot_ly(
        x = ~year,
        y = ~value_percentage,
        color = ~indicator_name,
        colors = custom_colors,
        type = 'scatter',
        mode = 'lines+markers',
        hoverinfo = 'text',
        text = ~paste(
          "Country:", country_name,
          "<br>Sector:", indicator_name,
          "<br>Year:", year,
          "<br>Female Employment:", value_percentage
        )
      ) %>%
      layout(
        title = paste("Female Employment by Sector Over Time in", input$selected_country),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Female Employment (%)"),
        legend = list(title = list(text = "<b>Sector</b>")),
        hovermode = "closest"
      )
    
    plot
  })

  # Download Handler 
  output$downloadGraphsWordworkforce <- downloadHandler(
    filename = function() {
      paste0("Gender_Workforce_Analysis_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Create the Word document
      doc <- read_docx()
      
      # Title for the document
      doc <- doc %>%
        body_add_par("Gender Workforce Analysis", style = "heading 1")
      
      # Recompute filtered data
      filtered_data <- gender_workforce %>%
        filter(country_name %in% input$countries_workforce)
      
      if (nrow(filtered_data) == 0) {
        stop("No data available for selected countries.")
      }
      
      # Recompute public_latest and private_latest
      public_latest <- filtered_data %>%
        filter(indicator_name == "Females, as a share of public paid employees") %>%
        group_by(country_name) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        ungroup()
      
      private_latest <- filtered_data %>%
        filter(indicator_name == "Females, as a share of private paid employees") %>%
        group_by(country_name) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        ungroup()
      
      # Generate the first plotly graph
      plotly_1 <- plot_ly(
        data = public_latest,
        x = ~country_name,
        y = ~value_percentage,
        type = 'bar',
        color = I("#003366"),
        name = "Public Sector"
      ) %>%
        add_trace(
          data = private_latest,
          x = ~country_name,
          y = ~value_percentage,
          type = "scatter",
          mode = "markers",
          marker = list(size = 10, color = "#B3242B"),
          name = "Private Sector"
        )
      
      # Save the first plotly graph as an image
      webshot::webshot(
        plotly_1,
        file = "first_graph.png",
        vwidth = 800,
        vheight = 600
      )
      
      # Add the first graph to the Word document
      doc <- doc %>%
        body_add_par("Female Employment by Sector (Last Year Available)", style = "heading 2") %>%
        body_add_img(src = "first_graph.png", width = 6, height = 4)
      
      # Generate the second plotly graph
      plotly_2 <- plot_ly(
        data = gender_workforce %>% filter(country_name == input$selected_country),
        x = ~year,
        y = ~value_percentage,
        color = ~indicator_name,
        type = 'scatter',
        mode = 'lines+markers'
      )
      
      # Save the second plotly graph as an image
      webshot::webshot(
        plotly_2,
        file = "second_graph.png",
        vwidth = 800,
        vheight = 600
      )
      
      # Add the second graph to the Word document
      doc <- doc %>%
        body_add_par("Female Employment by Sector Over Time", style = "heading 2") %>%
        body_add_img(src = "second_graph.png", width = 6, height = 4)
      
      # Save the Word document
      print(doc, target = file)
    }
  )
  #Tertiary Education
  
  # Render the Plotly bar graph
  output$barPlot <- renderPlotly({
    
    # Check if countries are selected
    if (is.null(input$selected_countries) || length(input$selected_countries) == 0) {
      return(NULL)  # Do nothing if no countries are selected
    }
    
    # Filter data based on selected countries
    filtered_data <- tertiary_education %>%
      filter(country_name %in% input$selected_countries)
    
    # Ensure the filtered dataset is not empty
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return nothing if the filtered dataset is empty
    }
    
    # Create the Plotly bar chart
    plot_ly(
      data = filtered_data,
      x = ~country_name,                      # X-axis: Country name
      y = ~value_percentage,                  # Y-axis: Tertiary education percentages
      color = ~indicator_name,                # Different color for Public/Private
      colors = c(
        "Individuals with tertiary education as a share of public paid employees" = "#003366", 
        "Individuals with tertiary education as a share of private paid employees" = "#B3242B"
      ),                                     # Custom color mapping
      type = 'bar',                           # Bar chart
      barmode = 'group'                       # Group bars for Public/Private
    ) %>%
      layout(
        title = "Tertiary Education by Sector and Country",
        xaxis = list(title = "Country"),      # Title for x-axis
        yaxis = list(title = "Tertiary Education (%)"), # Title for y-axis
        bargap = 0.2                          # Adjust gap between bars
      )
  })
  # Download Handler for Word Report
  output$downloadGraphsWordEducation <- downloadHandler(
    filename = function() {
      paste0("Tertiary_Education_Analysis_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()  # Start a new Word document
      
      # Title for the report
      report_title <- paste("Tertiary Education Analysis")
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>%
        body_add_fpar(fpar(ftext(report_title, prop = title_style)))
      
      # Tertiary Education Bar Plot
      # Filter data based on selected countries
      filtered_data <- tertiary_education %>%
        filter(country_name %in% input$selected_countries)
      
      # Ensure the filtered dataset is not empty
      if (nrow(filtered_data) == 0) {
        return(NULL)  # Return nothing if the filtered dataset is empty
      }
      
      # Create the bar plot for tertiary education by sector
      bar_plot <- plot_ly(
        data = filtered_data,
        x = ~country_name,                      # X-axis: Country name
        y = ~value_percentage,                  # Y-axis: Tertiary education percentages
        color = ~indicator_name,                # Different color for Public/Private
        colors = c(
          "Individuals with tertiary education as a share of public paid employees" = "#003366", 
          "Individuals with tertiary education as a share of private paid employees" = "#B3242B"
        ),                                     # Custom color mapping
        type = 'bar',                           # Bar chart
        barmode = 'group'                       # Group bars for Public/Private
      ) %>%
        layout(
          title = "Tertiary Education by Sector and Country",
          xaxis = list(title = "Country"),      # Title for x-axis
          yaxis = list(title = "Tertiary Education (%)"), # Title for y-axis
          bargap = 0.2                          # Adjust gap between bars
        )
      
      # Save the bar plot as a PNG file
      ggsave("bar_plot.png", plot = bar_plot, width = 6, height = 4)
      
      # Add the bar plot to the Word document
      doc <- doc %>%
        body_add_par("Tertiary Education by Sector and Country", style = "heading 1") %>%
        body_add_img(src = "bar_plot.png", width = 6, height = 4) %>%
        body_add_par("This graph shows the share of individuals with tertiary education in the public and private sectors for the selected countries.", style = "Normal")
      
      # Save the Word document
      print(doc, target = file)
    }
  )
  
  #Public sector wage Premium
  
  # Render the Plotly dot plot
  output$dotPlot <- renderPlotly({
    # Ensure countries are selected
    req(input$countries_first)
    
    # Filter data based on selected countries
    filtered_data <- public_wage_premium[public_wage_premium$country_name %in% input$countries_first, ]
    
    # Ensure there's data to plot
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return nothing if no data is available
    }
    
    # Create a new column to assign color based on the first selected country
    filtered_data$color <- ifelse(filtered_data$country_name == input$countries_first[1], "red", "blue")
    
    # Create the Plotly dot plot
    plot_ly(
      data = filtered_data,
      x = ~country_name,                          # X-axis: Country name
      y = ~value_percentage,                      # Y-axis: Wage premium percentage
      color = ~color,                             # Color by the new color column
      colors = c("#003366", "#B3242B"),           # Custom color mapping
      type = 'scatter',                           # Scatter plot (for dot plot)
      mode = 'markers',                           # Markers to create dots
      marker = list(size = 12)                    # Set dot size
    ) %>%
      layout(
        title = "Public Sector Wage Premium (Compared to All Private Employees) by Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Public Sector Wage Premium (%)"),
        showlegend = FALSE                          # Optional: Hide legend
      )
  })
  #Public sector wage premium by education level
  
  # Render the Plotly bar graph
  output$barPloteduc <- renderPlotly({
    # Filter data based on selected country
    filtered_data <- public_wage_premium_educ[public_wage_premium_educ$country_name == input$selected_country, ]
    
    plot_ly(
      data = filtered_data,
      x = ~indicator_name,                         # X-axis: Education levels
      y = ~value_percentage,                      # Y-axis: Public sector wage premium
      color = ~indicator_name,                    # Color bars by indicator_name (Education level)
      colors = c("No Education" = "#003366",
                 "Primary Education" = "#B3242B", 
                 "Secondary Education" = "#333333", 
                 "Tertiary Education" = "#006400"),  # Custom color mapping
      type = 'bar'                                # Bar chart type
    ) %>%
      layout(
        title = "Public Sector Wage Premium by Education Level (Compared to Private Formal Workers)",
        xaxis = list(
          title = "Education Level",              # X-axis title
          tickangle = 0                           # Horizontal labels
        ),
        yaxis = list(
          title = "Wage Premium (%)"              # Y-axis title
        ),
        barmode = 'group',                        # Group bars by education level
        bargap = 0.2,                             # Adjust gap between bars
        showlegend = TRUE,                        # Show legend to distinguish between education levels
        legend = list(
          title = list(text = "Education Level")  # Title for the legend
        )
      )
  })
 output$downloadAllGraphsWord <- downloadHandler(
  filename = function() {
    paste0("Selected_Graphs_", Sys.Date(), ".docx")
  },
  content = function(file) {
    # Create a new Word document
    doc <- read_docx()
    
    
    # Add graphs based on user selection
    if ("wageBillGraph" %in% input$selected_graphs_all) {
      # Render the Wage Bill Graph
      wage_bill_graph <- ggplot(selected_data(), aes(x = year, y = value, color = country_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(
          title = ifelse(input$indicator == "Wage bill as a percentage of GDP",
                         "Wage Bill as % of GDP Over Time",
                         "Wage Bill as % of Public Expenditure Over Time"),
          x = "Year",
          y = ifelse(input$indicator == "Wage bill as a percentage of GDP", 
                     "Wage Bill (% of GDP)", "Wage Bill (%)")
        ) +
        theme_minimal()
      
      # Add Wage Bill Graph to the document
      doc <- doc %>%
        body_add_par("Wage Bill Graph", style = "heading 1") %>%
        body_add_gg(value = wage_bill_graph, width = 6, height = 4)
    }
    
    if ("firstGraph" %in% input$selected_graphs_all) {
      # Render the First Public Sector Graph
      data_to_plot <- public_sector_emp_temp_last %>%
        filter(country_name %in% input$countries_first)
      
      data_to_plot_long <- data_to_plot %>%
        select(country_name, indicator_label, year, value) %>%
        mutate(indicator_name = factor(indicator_label))
      
      first_graph <- ggplot(data_to_plot_long, aes(x = country_name, y = value, color = indicator_name)) +
        geom_point(size = 3) +
        labs(
          title = "Public Sector Employment (Multi-Country)",
          x = "Country",
          y = "Value"
        ) +
        theme_minimal()
      
      # Add First Public Sector Graph to the document
      doc <- doc %>%
        body_add_par("First Graph: Public Sector Employment (Multi-Country)", style = "heading 1") %>%
        body_add_gg(value = first_graph, width = 6, height = 4)
    }
    
    if ("secondGraph" %in% input$selected_graphs_all) {
      # Render the Second Public Sector Graph
      data_to_plot <- public_sector_emp_temp %>%
        filter(country_name == input$country_second)
      
      data_to_plot_long <- data_to_plot %>%
        select(year, indicator_label, value) %>%
        mutate(indicator_label = factor(indicator_label))
      
      second_graph <- ggplot(data_to_plot_long, aes(x = year, y = value, color = indicator_label)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        labs(
          title = paste("Public Sector Employment in", input$country_second, "Over Time"),
          x = "Year",
          y = "Employment Value"
        ) +
        theme_minimal()
      
      # Add Second Public Sector Graph to the document
      doc <- doc %>%
        body_add_par("Second Graph: Public Sector Employment (Single Country)", style = "heading 1") %>%
        body_add_gg(value = second_graph, width = 6, height = 4)
    }
    
    if ("dotPlot" %in% input$selected_graphs_all) {
      # Render the Dot Plot Graph
      filtered_data <- merged_data %>%
        filter(country_name %in% input$countries_first)  # Ensure countries_first is used
      
      first_country <- ifelse(length(input$countries_first) > 0, input$countries_first[1], NULL)
      
      filtered_data <- filtered_data %>%
        mutate(color = ifelse(country_name == first_country, "#B3242B", "#003366"))
      
      dot_plot_graph <- ggplot(filtered_data, aes(x = log_gdp, y = indicator_value, label = country_name)) +
        geom_point(aes(color = color), size = 3) +
        geom_smooth(method = "lm", se = FALSE, color = "gray", linetype = "dashed") +
        scale_color_identity() +
        geom_text(nudge_y = 0.2, size = 3) +
        labs(
          title = "Wage Bill vs. Log(GDP per Capita)",
          x = "Log(GDP per Capita, 2015)",
          y = "Wage Bill"
        ) +
        theme_minimal()
      
      # Add Dot Plot Graph to the document
      doc <- doc %>%
        body_add_par("Dot Plot: Wage Bill vs. Log(GDP per Capita)", style = "heading 1") %>%
        body_add_gg(value = dot_plot_graph, width = 6, height = 4)
    }
    
    # Save the document
    print(doc, target = file)
  }
)
 

 #Public Sector Graphs
 
 # First Graph (Multiple Countries)
 output$firstGraphpublic <- renderPlotly({
   data_to_plot <- public_sector_emp_temp_last %>%
     filter(country_name %in% input$countries_first)
   
   data_to_plot_long <- data_to_plot %>%
     select(country_name, indicator_label, year, value) %>%
     mutate(indicator_label = factor(indicator_label)) # Fixed: Added missing parenthesis
   
   plot <- plot_ly(data = data_to_plot_long, 
                   x = ~country_name, 
                   y = ~value, 
                   color = ~indicator_label, 
                   type = 'scatter',
                   mode = 'markers',  
                   marker = list(size = 8)) %>%
     layout(title = "Public sector employment (last year available)",
            xaxis = list(title = "Country", tickangle = 45),
            yaxis = list(title = "Value"),
            legend = list(title = list(text = "Indicator")))
   
   plot
 })
 # Second Graph (Single Country) by Time
 output$secondGraphpublic <- renderPlotly({
   data_to_plot <- public_sector_emp_temp %>%
     filter(country_name == input$country_second)  # Single country selection
   
   data_to_plot_long <- data_to_plot %>%
     select(year, indicator_label, value) %>%
     mutate(indicator_label = factor(indicator_label))  # Fixed: Ensure indicator_label is a factor
   
   plot <- plot_ly(
     data = data_to_plot_long, 
     x = ~year, 
     y = ~value, 
     color = ~indicator_label,  # Color each indicator differently
     text = ~paste("Value:", round(value, 2)),  # Tooltip with value
     type = 'scatter', 
     mode = 'lines+markers',  # Add lines and markers
     marker = list(size = 8)  # Set marker size
   ) %>%
     layout(
       title = paste("Public sector employment,", input$country_second, "over time"),
       xaxis = list(title = "Year", tickangle = 45, dtick = 2),
       yaxis = list(title = "Wage Premium Value"),
       legend = list(title = list(text = "Indicator"))
     )
   
   # Add annotations
   plot <- plot %>%
     add_annotations(
       x = data_to_plot_long$year, 
       y = data_to_plot_long$value,  # Add offset to place annotation above the point
       text = round(data_to_plot_long$value, 2),  # Display value as annotation
       showarrow = FALSE,  # Remove arrows
       font = list(size = 12, color = "black"),
       xanchor = "center",  # Center annotation horizontally
       yanchor = "bottom"   # Position annotation above the point
     )
   
   plot
 })
 
 # Download Handler

 output$downloadGraphsWord <-  downloadHandler(
   filename = function() {
     paste0("Public_Sector_Analysis_", Sys.Date(), ".docx")
   },
   content = function(file) {
     # Ensure input is passed correctly
     print(input$countries_first)
     print(input$country_second)
     
     # Extract the first selected country for dynamic title
     if (!is.null(input$countries_first) && length(input$countries_first) > 0) {
       countries <- input$countries_first[1]  # Take the first country
     } else {
       countries <- "Unknown Country"  # Fallback in case no country is selected
     }
     
     # Dynamic title with the first country
     report_title <- paste("Public Sector Employment Analysis Report -", countries)
     
     doc <- read_docx()  # Start a new Word document
     
     # Define the style for the title
     title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
     
     # Apply the custom title style
     doc <- doc %>%
       body_add_fpar(fpar(ftext(report_title, prop = title_style)))
     
     # Add introduction
     doc <- doc %>%
       body_add_par("Introduction", style = "heading 2") %>%
       body_add_par("This report presents evidence on public sector employment and compensation practices for the selected countries. The analysis uses the latest data on public sector employment trends and provides insights into the composition and trends in public sector workforces.")
     
     # Add graphs to the Word document
     if ("firstGraph" %in% input$selected_graphs_public && length(input$countries_first) > 0) {
       # Data for the first graph (Multi-Country)
       data_to_plot <- public_sector_emp_temp_last %>%
         filter(country_name %in% input$countries_first)
       
       data_to_plot_long <- data_to_plot %>%
         select(country_name, indicator_label, year, value) %>%
         mutate(indicator_label = factor(indicator_label))
       
       # Generate first graph (Multi-Country)
       first_graph <- ggplot(data_to_plot_long, aes(x = country_name, y = value, color = indicator_label)) +
         geom_point(size = 3) +
         labs(title = "Public Sector Employment (Multi-Country)", x = "Country", y = "Value") +
         theme_minimal()
       
       # Save first graph as PNG (to the 'www' folder or appropriate path)
       ggsave("C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/first_graph.png", plot = first_graph, width = 6, height = 4)
       
       # Add first graph to the Word document
       doc <- doc %>%
         body_add_par("First Graph: Public Sector Employment (Multi-Country)", style = "heading 1") %>%
         body_add_img(src = "C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/first_graph.png", width = 6, height = 4) %>%
         body_add_par("This graph shows public sector employment across multiple countries.", style = "Normal")
     }
     
     if ("secondGraph" %in% input$selected_graphs_public && !is.null(input$country_second)) {
       # Data for the second graph (Single Country)
       data_to_plot <- public_sector_emp_temp %>%
         filter(country_name == input$country_second)
       
       data_to_plot_long <- data_to_plot %>%
         select(year, indicator_label, value) %>%
         mutate(indicator_label = factor(indicator_label))
       
       # Generate second graph (Single Country)
       second_graph <- ggplot(data_to_plot_long, aes(x = year, y = value, color = indicator_label)) +
         geom_line(size = 1) +
         geom_point(size = 3) +
         labs(title = paste("Public Sector Employment in", input$country_second, "Over Time"), x = "Year", y = "Employment Value") +
         theme_minimal()
       
       # Save second graph as PNG (to the 'www' folder or appropriate path)
       ggsave("C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/second_graph.png", plot = second_graph, width = 6, height = 4)
       
       # Add second graph to the Word document
       doc <- doc %>%
         body_add_par("Second Graph: Public Sector Employment (Single Country)", style = "heading 1") %>%
         body_add_img(src = "C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/second_graph.png", width = 6, height = 4) %>%
         body_add_par("This graph shows public sector employment trends over time for the selected country.", style = "Normal")
     }
     
     # Save the Word document
     print(doc, target = file)
   }
 )
 
 #Public Sector Wage Premium by gender 
 
 # First Graph (Multiple Countries)
 output$firstGraphgender <- renderPlotly({
   data_to_plot <- gender_wage_premium_last %>%
     filter(country_name %in% input$countries_first)
   
   data_to_plot_long <- data_to_plot %>%
     select(country_name, indicator_label, year, value) %>%
     mutate(indicator_label = factor(indicator_label)) # Fixed: Added missing parenthesis
   
   plot <- plot_ly(data = data_to_plot_long, 
                   x = ~country_name, 
                   y = ~value, 
                   color = ~indicator_label, 
                   type = 'scatter',
                   mode = 'markers',  
                   marker = list(size = 8)) %>%
     layout(title = "Public sector wage premium, by gender (last year available)",
            xaxis = list(title = "Country", tickangle = 45),
            yaxis = list(title = "Value"),
            legend = list(title = list(text = "Indicator")))
   
   plot
 })
 # Second Graph (Single Country) by Time
 output$secondGraphgender <- renderPlotly({
   data_to_plot <- gender_wage_premium %>%
     filter(country_name == input$country_second)  # Single country selection
   
   data_to_plot_long <- data_to_plot %>%
     select(year, indicator_label, value) %>%
     mutate(indicator_label = factor(indicator_label))  # Fixed: Ensure indicator_label is a factor
   
   plot <- plot_ly(
     data = data_to_plot_long, 
     x = ~year, 
     y = ~value, 
     color = ~indicator_label,  # Color each indicator differently
     text = ~paste("Value:", round(value, 2)),  # Tooltip with value
     type = 'scatter', 
     mode = 'lines+markers',  # Add lines and markers
     marker = list(size = 8)  # Set marker size
   ) %>%
     layout(
       title = paste("Public sector wage premium, by gender in", input$country_second, "over time"),
       xaxis = list(title = "Year", tickangle = 45, dtick = 2),
       yaxis = list(title = "Wage Premium Value"),
       legend = list(title = list(text = "Indicator"))
     )
   
   # Add annotations
   plot <- plot %>%
     add_annotations(
       x = data_to_plot_long$year, 
       y = data_to_plot_long$value,  # Add offset to place annotation above the point
       text = round(data_to_plot_long$value, 2),  # Display value as annotation
       showarrow = FALSE,  # Remove arrows
       font = list(size = 12, color = "black"),
       xanchor = "center",  # Center annotation horizontally
       yanchor = "bottom"   # Position annotation above the point
     )
   
   plot
 })
 
 # Download Handler
 
 output$downloadGraphsWordgender <- downloadHandler(
   filename = function() {
     paste0("Wage_Premium_Gender_Graphs_", Sys.Date(), ".docx")
   },
   content = function(file) {
     # Ensure input is passed correctly
     print(input$countries_first)
     print(input$country_second)
     
     # Extract the first selected country for dynamic title
     countries <- ifelse(!is.null(input$countries_first) && length(input$countries_first) > 0, input$countries_first[1], "Unknown Country")
     
     # Dynamic title with the first country
     report_title <- paste("Wage Premium Gender Analysis Report -", countries)
     
     doc <- read_docx()  # Start a new Word document
     
     # Define the style for the title
     title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
     
     # Apply the custom title style
     doc <- doc %>%
       body_add_fpar(fpar(ftext(report_title, prop = title_style)))
     
     # Add introduction
     doc <- doc %>%
       body_add_par("Introduction", style = "heading 2") %>%
       body_add_par("This report presents evidence on public sector employment and compensation practices for the selected countries. The analysis uses the latest data on public sector employment trends and provides insights into the composition and trends in public sector workforces.")
     
     # Add First Graph if selected
     if ("firstGraphgender" %in% input$graphs_to_download && length(input$countries_first) > 0) {
       data_to_plot <- gender_wage_premium_last %>%
         filter(country_name %in% input$countries_first)
       
       data_to_plot_long <- data_to_plot %>%
         select(country_name, indicator_label, year, value) %>%
         mutate(indicator_label = factor(indicator_label))
       
       # Generate first graph (Multi-Country)
       first_graph_wage_premium_gender <- ggplot(data_to_plot_long, aes(x = country_name, y = value, color = indicator_label)) +
         geom_point(size = 3) +
         labs(title = "Wage Premium Gender (Multi-Country)", x = "Country", y = "Value") +
         theme_minimal()
       
       # Save first graph as PNG
       graph_path1 <- "C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/first_graph_wage_premium_gender.png"
       ggsave(graph_path1, plot = first_graph_wage_premium_gender, width = 6, height = 4)
       
       # Add first graph to the Word document
       doc <- doc %>%
         body_add_par("First Graph: Wage Premium Gender (Multi-Country)", style = "heading 1") %>%
         body_add_img(src = graph_path1, width = 6, height = 4) %>%
         body_add_par("This graph shows the wage premium by gender across multiple countries.", style = "Normal")
     }
     
     # Add Second Graph if selected
     if ("secondGraphgender" %in% input$graphs_to_download && !is.null(input$country_second)) {
       data_to_plot <- gender_wage_premium %>%
         filter(country_name == input$country_second)
       
       data_to_plot_long <- data_to_plot %>%
         select(year, indicator_name, value) %>%
         mutate(indicator_name = factor(indicator_name))
       
       second_graph_wage_premium_gende <- ggplot(data_to_plot_long, aes(x = year, y = value, color = indicator_name)) +
         geom_line(size = 1) +
         geom_point(size = 3) +
         labs(
           title = paste("Public sector wage premium, by gender in", input$country_second, "Over Time"),
           x = "Year",
           y = "Employment Value"
         ) +
         theme_minimal()
       
       # Save second graph as PNG
       graph_path2 <- "C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/second_graph_wage_premium_gender.png"
       ggsave(graph_path2, plot = second_graph_wage_premium_gender, width = 6, height = 4)
       
       # Add second graph to the Word document
       doc <- doc %>%
         body_add_par("Second Graph: Public Sector Employment (Single Country)", style = "heading 1") %>%
         body_add_img(src = graph_path2, width = 6, height = 4) %>%
         body_add_par("This graph shows the wage premium by gender trends over time for the selected country.", style = "Normal")
     }
     
     # Save the Word document
     print(doc, target = file)
   }
 )

 #Female Leadership
 
 # Render the Plotly bar graph
 output$barPlotwomen <- renderPlotly({
   
   # Check if countries are selected
   if (is.null(input$selected_countries) || length(input$selected_countries) == 0) {
     return(NULL)  # Do nothing if no countries are selected
   }
   
   # Filter data based on selected countries
   filtered_data <- gender_leadership %>%
     filter(country_name %in% input$selected_countries)
   
   # Ensure the filtered dataset is not empty
   if (nrow(filtered_data) == 0) {
     return(NULL)  # Return nothing if the filtered dataset is empty
   }
   
   # Create the Plotly bar chart
   plot_ly(
     data = filtered_data,
     x = ~country_name,                      # X-axis: Country name
     y = ~value_percentage,                  # Y-axis: Female percentage in occupation
     color = ~indicator_label,                # Different color for Managers/Clerks in Public/Private
     colors = c(
       "Clerks-Public" = "#003366", 
       "Managers-Public" = "#ADD8E6",
       "Clerks-Private" = "#006400",
       "Managers-Private" = "#90EE90"
     ),                                     # Custom color mapping
     type = 'bar',                           # Bar chart
     barmode = 'group'                       # Group bars for Public/Private, Managers/Clerks
   ) %>%
     layout(
       title = "Females by Occupational Group and Sector",
       xaxis = list(title = "Country"),      # Title for x-axis
       yaxis = list(title = "Female Share (%)"), # Title for y-axis
       bargap = 0.2                          # Adjust gap between bars
     )
 })
 
 # Download Handler for Word Report
 output$downloadGraphsWordfemale <- downloadHandler(
   filename = function() {
     paste0("Females_Occupation_Groups_Analysis_", Sys.Date(), ".docx")
   },
   content = function(file) {
     doc <- read_docx()  # Start a new Word document
     
     # Title for the report
     report_title <- paste("Females by Occupational Group and Sector")
     title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
     doc <- doc %>%
       body_add_fpar(fpar(ftext(report_title, prop = title_style)))
     
     # Filter data based on selected countries
     filtered_data <- gender_leadership %>%
       filter(country_name %in% input$selected_countries)
     
     # Ensure the filtered dataset is not empty
     if (nrow(filtered_data) == 0) {
       return(NULL)  # Return nothing if the filtered dataset is empty
     }
     
     # Create the bar plot for Females by Occupation Group and Sector
     bar_plot <- plot_ly(
       data = filtered_data,
       x = ~country_name,                      # X-axis: Country name
       y = ~value_percentage,                  # Y-axis: Female percentage in occupation
       color = ~indicator_label,                # Different color for Managers/Clerks in Public/Private
       colors = c(
         "Clerks-Public" = "#003366", 
         "Managers-Public" = "#ADD8E6",
         "Clerks-Private" = "#006400",
         "Managers-Private" = "#90EE90"
       ),                                     # Custom color mapping
       type = 'bar',                           # Bar chart
       barmode = 'group'                       # Group bars for Public/Private, Managers/Clerks
     ) %>%
       layout(
         title = "Females by Occupational Group and Sector",
         xaxis = list(title = "Country"),      # Title for x-axis
         yaxis = list(title = "Female Share (%)"), # Title for y-axis
         bargap = 0.2                          # Adjust gap between bars
       )
     
     # Save the bar plot as a PNG file
     ggsave("bar_plot.png", plot = bar_plot, width = 6, height = 4)
     
     # Add the bar plot to the Word document
     doc <- doc %>%
       body_add_par("Females by Occupational Group and Sector", style = "heading 1") %>%
       body_add_img(src = "bar_plot.png", width = 6, height = 4) %>%
       body_add_par("This graph shows the share of females in various occupational groups (Managers/Clerks) in the public and private sectors for the selected countries.", style = "Normal")
     
     # Save the Word document
     print(doc, target = file)
   }
 )
 
 
 #Download all graphs for report
 output$downloadAllGraphsDoc <- downloadHandler(
   filename = function() {
     paste0("Comprehensive_Wage_Bill_Report_", Sys.Date(), ".docx")
   },
   content = function(file) {
     # Dynamic title with the first country
     report_title <- paste("Wage Bill and public employment Analysis Report")
     
     # Create a Word document
     doc <- read_docx()
     
     # Define the style for the title with a specific color and bold
     title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)  # Custom color and bold text
     
     # Apply the custom title style with color and bold
     doc <- doc %>%
       body_add_fpar(
         fpar(ftext(report_title, prop = title_style))  # Apply custom title style with dynamic title
       )
     
     # Add the introduction heading without numbering
     doc <- doc %>%
       body_add_par("Introduction", style = "heading 2") %>%  # Use heading style without numbering
       body_add_par("This note presents evidence on public sector employment and compensation practices in Bangladesh using the Worldwide Bureaucracy Indicators (WWBI). The primary data source is the Labor Force Survey (LFS), conducted by the Bangladesh Bureau of Statistics (BBS), which offers extensive, nationally representative data over multiple years up to 2022. The LFS’s comprehensive coverage of employment and wage issues across both public and private sectors, along with its frequency and national representativeness, makes it an ideal source for this analysis.
For international comparisons, the analysis includes a set of peer countries for benchmarking, with a particular focus on countries from the South Asia region and other aspirational peers. Information on these peers was also sourced from the WWBI.
The public sector is typically a major source of employment in most countries. The provision of basic services such as education, health, citizen security and justice, among others, makes it a central actor in labor markets, with significant impacts on the aggregate results of employment, wages, informality, and other economic variables. Moreover, public employment is an indicator of the state participation in the entire economy, which has implications for macroeconomic balances, allocation efficiency and income distribution. Thus, this analysis comprehensively documents the size of public employment, its changes over time, and the characteristics of its workforce.")
     
     # List of key indicators in the introduction
     doc <- doc %>%
       body_add_par("- Wage Bill as a Percentage of GDP", style = "Normal") %>%
       body_add_par("- Wage Bill as a Percentage of Public Expenditure", style = "Normal")
     
     
     # Section 1: Wage Bill (as % of GDP)
     doc <- doc %>%
       body_add_par("Wage Bill (as % of GDP)", style = "heading 2")
     
     graph1 <- ggplot(selected_data(), aes(x = year, y = value, color = country_name)) +
       geom_line() +
       labs(title = "Wage Bill as % of GDP Over Time", x = "Year", y = "Wage Bill (% of GDP)")
     doc <- doc %>% body_add_gg(graph1, style = "centered")
     
     # Section 2: Wage Bill (as % of Public Expenditure)
     doc <- doc %>%
       body_add_par("Wage Bill (as % of Public Expenditure)", style = "heading 2")
     
     graph2 <- ggplot(selected_data(), aes(x = year, y = value, color = country_name)) +
       geom_line() +
       labs(title = "Wage Bill as % of Public Expenditure Over Time", x = "Year", y = "Wage Bill (% of Public Expenditure)")
     doc <- doc %>% body_add_gg(graph2, style = "centered")
     
     # Section 3: Wage Bill vs. GDP
     doc <- doc %>%
       body_add_par("Wage Bill vs. GDP per Capita", style = "heading 2")
     
     dot_plot <- ggplot(filtered_data(), aes(x = log_gdp, y = indicator_value, color = country_name)) +
       geom_point(size = 3) +
       geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
       labs(title = "Wage Bill vs. Log(GDP per Capita)", x = "Log(GDP per Capita, 2015)", y = "Wage Bill")
     doc <- doc %>% body_add_gg(dot_plot, style = "centered")
     
     # Save the document
     print(doc, target = file)
   }
 )
 # Define the color scale based on the dynamic data range
 output$worldMap <- renderLeaflet({
   # Determine the min and max values from the filtered data
   data_values <- filtered_data_for_map()
   color_pal <- colorNumeric("Greens", domain = c(min(data_values$value_percentage, na.rm = TRUE), 
                                                  max(data_values$value_percentage, na.rm = TRUE)))
   
   leaflet(world_spdf) %>%
     addTiles() %>%
     setView(lng = 0, lat = 20, zoom = 2) %>%
     addLegend(
       position = "bottomright",
       pal = color_pal,
       values = c(min(data_values$value_percentage, na.rm = TRUE), 
                  max(data_values$value_percentage, na.rm = TRUE)),
       title = "Indicator Value",
       opacity = 1
     )
 })
 
 # Reactive expression to filter data based on the selected indicator and year
 filtered_data_for_map <- reactive({
   req(input$indicatorSelect, input$yearSelect)  # Ensure inputs are not null
   data_wwbi %>%
     filter(
       indicator_name == input$indicatorSelect, 
       !is.na(.data[[paste0("year_", input$yearSelect)]])
     ) %>%
     transmute(
       country_name, 
       indicator_name, 
       value_percentage = .data[[paste0("year_", input$yearSelect)]]
     )
 })
 
 # Observe and update the map
 observe({
   req(input$indicatorSelect, input$yearSelect)  # Ensure inputs are not null
   
   # Filter the data based on the selected indicator and year
   reported_countries <- filtered_data_for_map()
   
   # Debugging: If no countries are reported for the selected indicator
   if (is.null(reported_countries) || nrow(reported_countries) == 0) {
     warning("No reported countries for the selected indicator.")
     return()  # Exit if no data is available
   }
   
   # Merge the filtered data with world_spdf
   world_data_merged <- world_spdf %>%
     left_join(reported_countries, by = "country_name")
   
   # Determine the min and max values from the filtered data for dynamic scaling
   color_pal <- colorNumeric("Greens", domain = c(min(reported_countries$value_percentage, na.rm = TRUE),
                                                  max(reported_countries$value_percentage, na.rm = TRUE)))
   
   # Update the Leaflet map with new polygons based on the selected indicator
   leafletProxy("worldMap") %>%
     clearShapes() %>%
     addPolygons(
       data = world_data_merged,
       fillColor = ~ifelse(is.na(value_percentage), "#808080", color_pal(value_percentage)),
       fillOpacity = 0.7,
       color = "white",
       weight = 1,
       highlightOptions = highlightOptions(color = "#FFD700", weight = 2, fillOpacity = 0.9),
       label = ~paste0(
         "<strong>Country:</strong> ", country_name, "<br>",
         ifelse(!is.na(value_percentage), 
                paste0("<strong>Value:</strong> ", round(value_percentage, 2)), 
                "<strong>No Data</strong>")
       ),
       popup = ~paste(
         "Country: ", country_name,
         "<br>Indicator: ", indicator_name,
         ifelse(!is.na(value_percentage), 
                paste("<br>Value: ", round(value_percentage, 2)), 
                "<br>No Data Available")
       )
     )
   
   # Render the country count
   output$countryCount <- renderText({
     paste("Total Countries with Data: ", nrow(reported_countries))
   })
 })
   # Render the country count
   output$countryCount <- renderText({
     paste("Total Countries with Data: ", nrow(reported_countries))
   })
 
  # Dummy outputs for widgets
  output$numberIndicatorsBox <- renderInfoBox({
    infoBox("Indicators", 302, icon = icon("list"), color = "blue")
  })
  
  output$numberCountriesBox <- renderInfoBox({
    infoBox("Economies", length(unique(data_wwbi$country_name)), icon = icon("globe"), color = "blue")
  })
  
  output$temporalCoverageAnnualBox <- renderInfoBox({
    infoBox("Temporal Coverage (Annual)", "2000-2022", icon = icon("calendar"), color = "blue")
  })
  
  output$temporalCoverageYearsBox <- renderInfoBox({
    infoBox("Temporal Coverage (Years)", "22", icon = icon("calendar"), color = "blue")
  })
  
  output$lastUpdatedBox <- renderInfoBox({
    infoBox("Last Updated", "2022", icon = icon("clock"), color = "blue")
  })
  
}

# Run the application 

shinyApp(ui = ui, server = server)



###############################################################################



ui <- bs4DashPage(
  dark = TRUE,
  header = bs4DashNavbar(
    skin = "dark",
    status = "primary",
    border = TRUE,
    compact = TRUE,
    brandText = "Dashboard"
  ),
  sidebar = bs4DashSidebar(
    skin = "dark",
    status = "primary",
    title = "Dashboard",
    brandColor = "primary",
    elevation = 4,
    opacity = 0.8,
    collapsed = FALSE,
    bs4SidebarMenu(
      bs4SidebarMenuItem("Overview", tabName = "overview", icon = icon("home")),
      bs4SidebarMenuItem("Standard Table", tabName = "standard_table", icon = icon("table")),
      bs4SidebarMenuItem("Patterns of Development", tabName = "patterns_dev", icon = icon("th")),
      bs4SidebarMenuItem("Growth and Convergence", tabName = "growth_convergence", icon = icon("chart-line"), status = "danger"),
      bs4SidebarHeader("Drivers of Growth and Jobs"),
      bs4SidebarMenuItem("Structural Transformation", tabName = "structural", icon = icon("sync")),
      bs4SidebarMenuItem("Demographics and Jobs", tabName = "demographics", icon = icon("users")),
      bs4SidebarMenuItem("Poverty and Inclusion", tabName = "poverty", icon = icon("hand-holding-heart")),
      bs4SidebarMenuItem("Environmental Sustainability", tabName = "sustainability", icon = icon("tree")),
      bs4SidebarHeader("Subcategories"),
      bs4SidebarMenuItem("Aggregate Firm Dynamics", tabName = "firm_dynamics", icon = icon("industry")),
      bs4SidebarMenuItem("Innovation and Capabilities", tabName = "innovation", icon = icon("lightbulb")),
      bs4SidebarMenuItem("Investment", tabName = "investment", icon = icon("dollar-sign")),
      bs4SidebarMenuItem("Competition and Markets", tabName = "competition", icon = icon("shopping-cart")),
      bs4SidebarMenuItem("Trade and GVCs", tabName = "trade", icon = icon("globe"))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(tabName = "overview", h1("Overview Content")),
      bs4TabItem(tabName = "standard_table", h1("Standard Table Content")),
      bs4TabItem(tabName = "patterns_dev", h1("Patterns of Development Content")),
      bs4TabItem(tabName = "growth_convergence", h1("Growth and Convergence Content")),
      bs4TabItem(tabName = "structural", h1("Structural Transformation Content")),
      bs4TabItem(tabName = "demographics", h1("Demographics and Jobs Content")),
      bs4TabItem(tabName = "poverty", h1("Poverty and Inclusion Content")),
      bs4TabItem(tabName = "sustainability", h1("Environmental Sustainability Content")),
      bs4TabItem(tabName = "firm_dynamics", h1("Aggregate Firm Dynamics Content")),
      bs4TabItem(tabName = "innovation", h1("Innovation and Capabilities Content")),
      bs4TabItem(tabName = "investment", h1("Investment Content")),
      bs4TabItem(tabName = "competition", h1("Competition and Markets Content")),
      bs4TabItem(tabName = "trade", h1("Trade and GVCs Content"))
    )
  )
)


server <- function(input, output) {}

shinyApp(ui, server)
