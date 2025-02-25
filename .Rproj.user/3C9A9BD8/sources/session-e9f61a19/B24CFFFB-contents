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
library(webshots)
library(htmlwidgets)



### INITIAL COMMANDS ----

#Set data path 

data_path <- "C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/Data/Intermediate"

#Load indicators data set 

data_wwbi <- read_dta(file.path(data_path, "data_wwbi.dta"))

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

# Ensure your dataset has ISO3 country codes

data_wwbi[, iso3c := countrycode(country_name, origin = "country.name", destination = "iso3c")]

# Manually assign missing ISO3C codes

data_wwbi[country_name == "Kosovo", iso3c := "XKX"]
data_wwbi[country_name == "Micronesia", iso3c := "FSM"]

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

data_wwbi_long <- data_wwbi_long %>% filter(!is.na(country_name))



# Filter the data for the specific indicator "Wage bill as a percentage of Public Expenditure"

wage_bill_publicexp <- data_wwbi_long[data_wwbi_long$indicator_name == "Wage bill as a percentage of Public Expenditure", ]


# Filter the data for the specific indicator "Wage bill as a percentage of GDP"

wage_bill_gdp <- data_wwbi_long[data_wwbi_long$indicator_name == "Wage bill as a percentage of GDP", ]


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


public_sector_emp_temp <- public_sector_emp_temp %>%
  mutate(value_percentage = value * 100)

public_sector_emp <- public_sector_emp %>%
  select(year, indicator_name, value, country_name, wb_region) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
                                  "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
                                  "Public sector employment, as a share of total employment"= "as a share of total employment"))



public_sector_emp <- public_sector_emp %>%
  mutate(value_percentage = value * 100)


# Keep the last year available for each country

public_sector_emp_temp_last <- public_sector_emp %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, indicator_name, wb_region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data




# Filter the data for the specific indicator "Characteristics of the public sector workforce"

public_sector_workforce <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Education workers, as a share of public total employees", 
                                                               "Health workers, as a share of public total employees", 
                                                               "Public Administration workers, as a share of public total employees"), ]

public_sector_workforce <- public_sector_workforce %>%
  mutate(value_percentage = value * 100)


# Step 1: Calculate the 'Other' value for each country and year

public_sector_workforce <- public_sector_workforce %>%
  group_by(country_name, year, wb_region) %>%
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
      group_by(country_name, year, wb_region) %>%
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
  group_by(country_name, indicator_name, wb_region) %>%         # Group by country and indicator
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


gender_workforce <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Females, as a share of public paid employees", 
                                                               "Females, as a share of private paid employees"), ]

gender_workforce <- gender_workforce %>%
  mutate(value_percentage = value * 100)



# Filter GDP data for the year 2015

gdp_2015 <- data_gdp %>%
  filter(year == 2015) %>%
  select(country_name, value)


# Rename 'value' column to 'indicator_value' in data_indicators

data_indicator_wb <- wage_bill_publicexp %>%
  rename(indicator_value = value)

# Rename 'value' column to 'gdp_value' in data_gdp

gdp_2015 <- gdp_2015 %>%
  rename(gdp_value = value)


# Merge the datasets on 'country_name'


merged_data <- data_indicator_wb %>%
  left_join(gdp_2015, by = "country_name") %>%
  select(country_name, indicator_name, country_code, indicator_value, gdp_value, wb_region)


# Add the log of GDP as a new column

merged_data <- merged_data %>%
  mutate(log_gdp = log(gdp_value))

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




#Gender Wage premium                                                                                                                                                                                                                                                                   

# Filter the data for the specific indicator "Public sector wage premium, by gender: Female (compared to all private employees) and
# Public sector wage premium, by gender: Male (compared to all private employees)"

gender_wage_premium <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Public sector wage premium, by gender: Female (compared to all private employees)", 
                                                               "Public sector wage premium, by gender: Male (compared to all private employees)"), ]

gender_wage_premium <- gender_wage_premium %>%
  select(year, indicator_name, value, country_name,wb_region) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Public sector wage premium, by gender: Female (compared to all private employees)" = "Female", 
                                  "Public sector wage premium, by gender: Male (compared to all private employees)" = "Male"))


# Keep the last year available for each country

gender_wage_premium_last <- gender_wage_premium %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, indicator_label, wb_region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data




public_sector_emp_temp <- public_sector_emp_temp %>%
  select(country_name, indicator_name, year, value, wb_region) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  mutate(indicator_label = recode(
    indicator_name, 
    "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
    "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
    "Public sector employment, as a share of total employment" = "as a share of total employment"
  ))


public_sector_emp_temp_last <- public_sector_emp_temp_last %>%
  select(country_name, indicator_name, year, value, wb_region) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  mutate(indicator_label = recode(
    indicator_name, 
    "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
    "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
    "Public sector employment, as a share of total employment" = "as a share of total employment"
  ))

#Female Leadership 

gender_leadership <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Females, as a share of public paid employees by occupational group: Managers", 
                                                                 "Females, as a share of public paid employees by occupational group: Clerks", 
                                                               "Females, as a share of private paid employees by occupational group: Managers", 
                                                               "Females, as a share of private paid employees by occupational group: Clerks" ), ]

gender_leadership <- gender_leadership %>%
  select(year, indicator_name, value, country_name,wb_region) %>%
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
  group_by(country_name, indicator_label, wb_region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data



gender_leadership <- gender_leadership %>%
  mutate(value_percentage = value * 100)


#Gender Wage premium in the public sector, by industry 

gender_wage_premiumpublic <- data_wwbi_long[data_wwbi_long$indicator_name %in% c("Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)", 
                                                                         "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)", 
                                                                         "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"), ]



gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  mutate(value_percentage = value * 100)

gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  group_by(country_name, year, wb_region) %>%
  mutate(
    # Calculate the value for 'Other' as 100 minus the sum of specific indicators
    other_value = 100 - sum(value_percentage[indicator_name %in% c(
      "Gender wage premium in the public sector, by industry: Core Public Administration (compared to male paid employees)",
      "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)",
      "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"
    )], na.rm = TRUE)
  ) %>%
  ungroup()


gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  bind_rows(
    gender_wage_premiumpublic %>%
      # Filter rows for the specified indicators
      filter(indicator_name %in% c(
        "Gender wage premium in the public sector, by industry: Core Public Administration (compared to male paid employees)",
        "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)",
        "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"
      )) %>%
      group_by(country_name, year, wb_region) %>%
      summarize(
        indicator_name = "Other",  # Set the indicator name to "Other"
        value_percentage = first(other_value),  # Replace the value with the calculated 'other_value'
        .groups = "drop"  # Drop the grouping after summarizing
      ) %>%
      ungroup()
  )


gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  select(year, indicator_name, value, country_name,wb_region,value_percentage) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" = "Public Administration", 
                                  "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)" = "Education", 
                                  "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)" = "Health"))


# Keep the last year available for each country

gender_wage_premiumpublic <- gender_wage_premiumpublic %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, indicator_label, wb_region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data








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
    /* Custom Info Box Styling */
    .custom-info-box {
      background: linear-gradient(to right, #56ccf2, #eb2f96);
      color: white;
      font-size: 16px;
      font-weight: bold;
      text-align: center;
      padding: 15px;
      border-radius: 10px;
      box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2);
      margin: 10px 0;
      border: 1px solid white;
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
          div(class = "nav-sub-item", actionLink("nav_wagepremium", "Public Sector Wage Premium")),
          div(class = "nav-sub-item", actionLink("nav_public_workforce", "Public Sector Workforce Graphs")), 
          div(class = "nav-sub-item", actionLink("nav_public_graphs", "Public Sector Employment"))
      ),
      
      # Collapsible Section - Characteristics of Public Sector Workforce
      div(class = "nav-section", onclick = "toggleSection('public_sector_workforce_section')", "Characteristics of Public Sector Workforce"),
      div(id = "public_sector_workforce_section", style = "display: none;",
          div(class = "nav-sub-item", actionLink("nav_gender_workforce", "Gender Workforce Graphs")), 
          div(class = "nav-sub-item", actionLink("nav_education", "Tertiary Education Graphs")),
          div(class = "nav-sub-item", actionLink("nav_public_educ", "Public Sector Education Graphs")),
          div(class = "nav-sub-item", actionLink("nav_female_leadership", "Female Leadership Graphs")),
          div(class = "nav-sub-item", actionLink("nav_gender_wage_premium", "Gender Wage Premium Graphs")),
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
  observeEvent(input$nav_gender_wage_premium, { active_tab("gender_wage_premium") })
  observeEvent(input$nav_download_all,      { active_tab("download_all") })
  
  # 2. Render the main dynamic UI based on active_tab
  output$main_content <- renderUI({
    tab <- active_tab()
    
    if(tab == "dashboard") {
      tagList(
        h3("Dashboard"),
        fluidRow(
          div(style = "border: 2px solid white; padding: 15px; border-radius: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              "The Worldwide Bureaucracy Indicators (WWBI) database is a unique cross-national dataset on public sector employment and wages that aims to fill an information gap, thereby helping researchers, development practitioners, and policymakers gain a better understanding of the personnel dimensions of state capability, the footprint of the public sector within the overall labor market, and the fiscal implications of the public sector wage bill. The dataset is derived from administrative data and household surveys, thereby complementing existing, expert perception-based approaches.")
        )
      )
      
    } else if(tab == "metadata") {
      tagList(
        h3("Metadata"),
        # Custom-styled InfoBoxes with gradient background
        fluidRow(
          column(4, div(class = "custom-infobox", infoBox("Indicators", 302, icon = icon("list")))),
          column(4, div(class = "custom-infobox", infoBox("Economies", length(unique(data_wwbi$country_name)), icon = icon("globe")))),
          column(4, div(class = "custom-infobox", infoBox("Temporal Coverage (Annual)", "2000-2022", icon = icon("calendar"))))
        ),
        fluidRow(
          column(4, div(class = "custom-infobox", infoBox("Temporal Coverage (Years)", "22", icon = icon("calendar")))),
          column(4, div(class = "custom-infobox", infoBox("Last Updated", "2022", icon = icon("clock"))))
        ),
        fluidRow(
          div(style = "border: 1px solid white; padding: 10px;",
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
      
    } else if(tab == "wagebill") {
      tagList(
        h3("Wage Bill Graphs"),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              "This visualization explores the wage bill over time for selected countries.")
        ),
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
          div(style = "border: 2px solid white; 
              padding: 10px; 
              background: linear-gradient(to right, #4A90E2, #D4145A);
              color: white; 
              font-size: 14px; 
              text-align: center; 
              border-radius: 5px;
              margin-top: 10px;",
              textOutput("note_wagebill"))
        ),
        fluidRow(
          downloadButton("downloadWord", "Download Report in Word")
        )
      )
      
    } else if(tab == "wagebill_gdp") {
      tagList(
        h3("Wage Bill & GDP Graphs"),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              "This visualization explores the relationship between wage bill and GDP per capita (log scale).")
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
          plotlyOutput("dot_plot", height = "500px"), 
        ), 
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_dotplot")
          )
        )
      )
      
    } else if(tab == "public_workforce") {
      tagList(
        h3("Public Sector Workforce Graphs"),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              "This visualization explores the relationship between wage bill and GDP per capita (log scale).")
        ),
        fluidRow(
          selectInput("countries_workforce", "Select Countries for Workforce Graph",
                      choices = unique(public_sector_workforce$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("stackedBarGraph", height = "600px")
        ),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_stackedBarGraph"))
        ),
        fluidRow(
          selectInput("selected_country", "Select Country for Second Graph",
                      choices = unique(public_sector_workforce$country_name), multiple = FALSE)
        ),
        fluidRow(
          plotlyOutput("horizontalStackedBar", height = "600px")
        ),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_horizontalStackedBar"))
        ),
        fluidRow(
          checkboxGroupInput("selected_graphs_public", "Select Graphs to Download", 
                             choices = c("Multi-Country Graph" = "firstGraph", "Single-Country Graph" = "secondGraph"), 
                             selected = c("firstGraph", "secondGraph")),
          downloadButton("downloadGraphsWordworkforce", "Download Selected Graphs in Word")
        )
      )
      
    } else if(tab == "education") {
      tagList(
        h3("Tertiary Education Graphs"),
        fluidRow(
          fluidRow(
            div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
                "This visualization explores the relationship between wage bill and GDP per capita (log scale).")
          ),
          selectInput("selected_countries", "Select Countries", 
                      choices = unique(tertiary_education$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("barPlot", height = "600px")
        ),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_tertiaryEducation"))
        ),
        fluidRow(
          downloadButton("downloadGraphsWordEducation", "Download Tertiary Education Report")
        )
      )
    } else if(tab == "female_leadership") {
        tagList(
          h3("Females by Occupational Group and Sector"),
          fluidRow(
            div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
                "This visualization shows the share of females in various occupational groups (Managers/Clerks) in the public and private sectors across selected countries.")
          ),
          fluidRow(
            selectInput("selected_countries", "Select Countries", 
                        choices = unique(gender_leadership$country_name), multiple = TRUE)
          ),
          fluidRow(
            plotlyOutput("barPlotwomen", height = "600px")
          ),
          fluidRow(
            div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
                textOutput("note_barPlotwomen"))
          ),
          fluidRow(
            downloadButton("downloadGraphsWordfemale", "Download Female Leadership Report")
          )
        )
    } else if(tab == "wagepremium_gender") {
      tagList(
        h3("Wage Premium by Gender Graphs"),
        
        # Description Box
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              "This visualization explores the public sector wage premium by gender across selected countries and its trend over time.")
        ),
        
        # Multi-Country Selection for First Graph
        fluidRow(
          selectInput("countries_first", "Select Countries for First Graph", 
                      choices = unique(gender_wage_premium_last$country_name), 
                      multiple = TRUE)
        ),
        
        # First Graph Output - Multi-Country Dot Plot
        fluidRow(
          plotlyOutput("firstGraphGenderWagePremium", height = "600px")
        ),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_firstGraphGenderWagePremium"))
        ),
        # Single-Country Selection for Second Graph
        fluidRow(
          selectInput("country_second", "Select Country for Second Graph", 
                      choices = unique(gender_wage_premium$country_name), 
                      multiple = FALSE)
        ),
        
        # Second Graph Output - Single-Country Line Plot
        fluidRow(
          plotlyOutput("secondGraphGenderWagePremium", height = "600px")
        ),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_secondGraphGenderWagePremium"))
        ),
        
        # Download Button
        fluidRow(
          downloadButton("downloadGraphsWordGenderWagePremium", "Download Gender Wage Premium Report (Word)")
        )
      )
      
    } else if(tab == "wagepremium") {
      tagList(
        h3("Public Sector Wage Premium"),
        fluidRow(
          selectInput("countries_wage_premium", "Select Countries for First Graph", 
                      choices = unique(public_wage_premium$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("dotPlot", height = "500px")
        ),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_wage_premium"))
        ),
        fluidRow(
          downloadButton("downloadWagePremiumReport", "Download Public Sector Wage Premium Report")
        )
      )
    } else if(tab == "public_educ") {
      tagList(
        h3("Public Sector Education Graphs"),
        
        # Description Box
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              "This visualization explores the public sector wage premium by education level, compared to private formal workers.")
        ),
        
        # Country Selection
        fluidRow(
          selectInput("selected_country", "Select Country for Graph", 
                      choices = unique(public_wage_premium_educ$country_name), 
                      multiple = FALSE)
        ),
        
        # Bar Plot Output
        fluidRow(
          plotlyOutput("education_wage_premium_plot", height = "600px")
        ),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_education_wage_premium"))
        ),
        # Download Button
        fluidRow(
          downloadButton("downloadEducationWagePremium", "Download Wage Premium Report (Word)")
        )
      )
    } else if(tab == "public_graphs") {
      tagList(
        h3("Public Sector Employment Graphs"),
        
        # Description Box
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              "This visualization explores public sector employment across selected countries and its trend over time.")
        ),
        
        # Multi-Country Selection for First Graph
        fluidRow(
          selectInput("countries_first", "Select Countries for First Graph", 
                      choices = unique(public_sector_emp_temp_last$country_name), multiple = TRUE)
        ),
        
        # Dot Plot Output
        fluidRow(
          plotlyOutput("firstGraphpublic", height = "600px")
        ),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_firstGraphpublic"))
        ),
        # Single-Country Selection for Line Graph
        fluidRow(
          selectInput("country_second", "Select Country for Second Graph", 
                      choices = unique(public_sector_emp_temp$country_name), multiple = FALSE)
        ),
        
        # Line Plot Output
        fluidRow(
          plotlyOutput("secondGraphpublic", height = "600px")
        ),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_secondGraphpublic"))
        ),
        # Download Button
        fluidRow(
          downloadButton("downloadGraphsWord", "Download Graphs as Word File")
        )
      )
    } else if(tab == "gender_workforce") {
      tagList(
        h3("Female Employment by Sector"),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              "This visualization explores female employment in the public and private sectors across selected countries.")
        ),
        fluidRow(
          selectInput("countries_gender", "Select Countries for First Graph", 
                      choices = unique(gender_workforce$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("firstGraphGenderWorkforce")
        ),
        fluidRow(
          selectInput("country_gender", "Select Country for Second Graph", 
                      choices = unique(gender_workforce$country_name), multiple = FALSE)
        ),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_firstGraphGenderWorkforce"))
        ),
        fluidRow(
          plotlyOutput("secondGraphGenderWorkforce")
        ),
        fluidRow(
          downloadButton("downloadGraphsWordGender", "Download Gender Workforce Report")
        ), 
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_secondGraphGenderWorkforce"))
        )
      )
    } else if(tab == "gender_wage_premium") {
      tagList(
        h3("Gender Wage Premium in Public Sector by Industry"),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              "This visualization explores the gender wage premium in the public sector by industry across selected countries.")
        ),
        fluidRow(
          selectInput("selected_countries", "Select Countries", 
                      choices = unique(gender_wage_premium$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotOutput("gender_wage_barplot", height = "600px")
        ),
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              textOutput("note_gender_wage_barplot"))
        ),
        fluidRow(
          downloadButton("downloadGenderWagePremium", "Download Gender Wage Premium Report")
        )
      )
    } else if(tab == "download_all") {
      tagList(
        h3("Download All Graphs"),
        fluidRow(
          div(style = "border: 1px solid white; padding: 10px;",
              "Download a comprehensive report containing all visualizations and analyses.")
        ),
        fluidRow(
          downloadButton("downloadAllGraphsDoc", "Download Full Report")
        )
      )
    }
  }
)

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
            y = ~value,         
            color = ~country_name,  # Keeps color mapping by country
            type = "scatter",
            mode = "lines+markers",
            marker = list(size = 8)) %>%
      layout(
        title = title_text,
        xaxis = list(title = "Year", dtick = 2),
        yaxis = list(title = y_label),
        legend = list(title = list(text = "Indicator"))  # Renames legend title
      )
  })
  output$note_wagebill <- renderText({
    if (input$graph_choice == "GDP") {
      "Note: This indicator represents the wage bill as a percentage of GDP, measuring the public sector's wage cost relative to the total economy."
    } else {
      "Note: This indicator represents the wage bill as a percentage of public expenditure, reflecting how much of government spending goes to wages."
    }
  })
  output$downloadWord <- downloadHandler(
    filename = function() {
      paste0("Wage_Bill_Analysis_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Use the first selected country from the input "countries"
      first_country <- if (!is.null(input$countries) & length(input$countries) > 0) {
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
      selected_countries <- input$countries  # User-selected comparison countries
      
      # Get data for the selected country
      data_country <- wage_bill_publicexp %>% filter(country_name == first_country)
      
      # Extract the wage bill values for 2010 and 2022 (if available)
      value_2010 <- data_country %>% filter(year == 2010) %>% pull(value)
      value_2022 <- data_country %>% filter(year == 2022) %>% pull(value)
      
      # Handle missing data
      if (length(value_2010) == 0) value_2010 <- NA
      if (length(value_2022) == 0) value_2022 <- NA
      
      # Determine whether the wage bill is "low," "moderate," or "high" compared to selected peers
      avg_peer_wage <- wage_bill_publicexp %>%
        filter(country_name %in% selected_countries, year == 2022) %>%
        summarise(mean_wage = mean(value, na.rm = TRUE)) %>%
        pull(mean_wage)
      
      comparison_text <- if (!is.na(value_2022) & !is.na(avg_peer_wage)) {
        if (value_2022 < avg_peer_wage * 0.8) {
          "a relatively low"
        } else if (value_2022 > avg_peer_wage * 1.2) {
          "a relatively high"
        } else {
          "a moderate"
        }
      } else {
        "an uncertain"
      }
      
      # Select top 3 highest wage bill countries among the user's selection
      top_countries <- wage_bill_publicexp %>%
        filter(country_name %in% selected_countries, year == 2022) %>%
        arrange(desc(value)) %>%
        slice(1:3) %>%
        pull(country_name)
      
      top_countries_text <- paste(top_countries, collapse = ", ")
      
      # Determine the wage bill comparison text
      # Ensure we correctly compare first_country's wage bill to the selected countries
      wage_difference_text <- if (!is.na(value_2022) & !is.na(avg_peer_wage)) {
        if (value_2022 > avg_peer_wage) {
          "lower"
        } else if (value_2022 < avg_peer_wage) {
          "higher"
        } else {
          "similar"
        }
      } else {
        "uncertain"
      }
      
      # Get the last available year for the selected country
      last_year <- wage_bill_publicexp %>%
        filter(country_name == first_country) %>%
        summarise(last_year = max(year, na.rm = TRUE)) %>%
        pull(last_year)
      
      # Construct the final analysis text dynamically
      analysis_text <- paste0(
        first_country, " has ", comparison_text, " public sector wage bill compared to its peers. ",
        "The country’s wage bill as a percentage of public expenditures has ",
        "In 2010, the wage bill accounted for around ", 
        ifelse(is.na(value_2010), "N/A", round(value_2010, 1)), 
        " percent of public expenditures, but this gradually changed, reaching ", 
        ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), 
        " percent in ", last_year, ". ",
        "Compared to other countries in the region and global comparators, ", first_country, 
        " allocates ", comparison_text, " proportion of its budget to public sector wages. ",
        "For instance, in ", last_year, ", ", first_country, "’s wage bill stands at ",  
        ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), 
        " percent, whereas countries like ", top_countries_text, 
        " had ", wage_difference_text, " wage bills during the same period. ",
        "This trend reflects ", first_country, "’s approach to public sector wage spending, but it also raises questions about whether this level of spending affects the government's ability to effectively deliver public services."
      )
      
      
      doc <- doc %>% body_add_par(analysis_text, style = "Normal")
      
      # --- Add the Graph Based on User Selection ---
      if (input$graph_choice == "GDP") {
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
          body_add_par(ifelse(input$graph_choice == "GDP",
                              "Note: This indicator represents the wage bill as a percentage of GDP, measuring the public sector's wage cost relative to the total economy.",
                              "Note: This indicator represents the wage bill as a percentage of public expenditure, reflecting how much of government spending goes to wages."), 
                       style = "Normal")  %>%
          body_add_par(paste0("This graph shows the wage bill as a percentage of GDP over time for the selected countries. ",
                              "For example, in 2022, ", first_country, " had a wage bill of ", 
                              ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), "% of GDP."), 
                       style = "Normal")
        
      } else {
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
  generate_wage_bill_analysis_section <- function(doc) {
    # Use the first selected country from input
    first_country <- if (!is.null(input$countries) & length(input$countries) > 0) {
      input$countries[1]
    } else {
      "Bangladesh"  # Default fallback
    }
    
    # Get the region using countrycode
    first_region <- countrycode(first_country, origin = "country.name", destination = "region")
    
    report_title <- paste("Wage Bill Analysis Report -", first_country)
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
    selected_countries <- input$countries  # User-selected comparison countries
    
    # Get data for the selected country
    data_country <- wage_bill_publicexp %>% filter(country_name == first_country)
    
    # Extract the wage bill values for 2010 and 2022 (if available)
    value_2010 <- data_country %>% filter(year == 2010) %>% pull(value)
    value_2022 <- data_country %>% filter(year == 2022) %>% pull(value)
    
    # Handle missing data
    value_2010 <- ifelse(length(value_2010) == 0, NA, value_2010)
    value_2022 <- ifelse(length(value_2022) == 0, NA, value_2022)
    
    # Get the last available year for the selected country
    last_year <- wage_bill_publicexp %>%
      filter(country_name == first_country) %>%
      summarise(last_year = max(year, na.rm = TRUE)) %>%
      pull(last_year)
    
    # Construct the final analysis text dynamically
    analysis_text <- paste0(
      first_country, "’s public wage bill trends have changed over time. ",
      "In 2010, the wage bill accounted for around ", 
      ifelse(is.na(value_2010), "N/A", round(value_2010, 1)), 
      " percent of public expenditures, but this gradually changed, reaching ", 
      ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), 
      " percent in ", last_year, "."
    )
    
    doc <- doc %>% body_add_par(analysis_text, style = "Normal")
    
    # --- Add the Graph Based on User Selection ---
    graph_data <- if (input$graph_choice == "GDP") {
      wage_bill_gdp %>% filter(country_name %in% input$countries)
    } else {
      wage_bill_publicexp %>% filter(country_name %in% input$countries)
    }
    
    graph <- ggplot(graph_data, aes(x = year, y = value, color = country_name)) +
      geom_line(size = 1.2) + 
      geom_point(size = 3) +
      labs(
        title = ifelse(input$graph_choice == "GDP", 
                       "Wage Bill as % of GDP Over Time", 
                       "Wage Bill as % of Public Expenditure Over Time"),
        x = "Year",
        y = ifelse(input$graph_choice == "GDP", "Wage Bill (% of GDP)", "Wage Bill (% of Public Expenditure)")
      ) +
      theme_minimal()
    
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = graph, width = 8, height = 6, dpi = 300)
    
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows wage bill trends over time.", style = "Normal")
    
    return(doc)
  }
  
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
             showlegend = FALSE, 
             plot_bgcolor = "white",   # Add this line
             paper_bgcolor = "white")
  })
  output$note_dotplot <- renderText({
    "Note: This indicator represents the relationship between wage bill and log(GDP per capita). The trendline provides a visual reference for the overall pattern."
  })
  output$downloadGDPDoc <- downloadHandler(
    filename = function() { paste0("Wage_Bill_vs_GDP_Report_", Sys.Date(), ".docx") },
    content = function(file) {
      filtered_data_df <- merged_data %>% filter(country_name %in% input$countries_first)
      req(nrow(filtered_data_df) > 0)
      countries <- if (!is.null(input$countries) & length(input$countries) > 0) input$countries[1] else "Unknown Country"
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
      doc <- doc %>% body_add_gg(value = plot, style = "centered") %>% 
        body_add_par("Note: This indicator represents the relationship between wage bill and log(GDP per capita). The trendline provides a visual reference for the overall pattern.", 
                     style = "Normal")
      print(doc, target = file)
    }
  )
  generate_gdp_analysis_section <- function(doc) {
    # Filter data for selected countries
    filtered_data_df <- merged_data %>% filter(country_name %in% input$countries_first)
    
    # Ensure the dataset is not empty
    if (nrow(filtered_data_df) == 0) {
      doc <- doc %>% body_add_par("No data available for Wage Bill vs. GDP analysis.", style = "Normal")
      return(doc)
    }
    
    # Get the first country name
    countries <- if (!is.null(input$countries) & length(input$countries) > 0) {
      input$countries[1]
    } else {
      "Unknown Country"
    }
    
    # Add Section Title
    report_title <- paste("Wage Bill vs. GDP Analysis -", countries)
    title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
    doc <- doc %>% body_add_fpar(fpar(ftext(report_title, prop = title_style)))
    
    # Add Introduction
    doc <- doc %>% 
      body_add_par("Introduction", style = "heading 2") %>%
      body_add_par("This note presents evidence on public sector employment and compensation practices in relation to GDP per capita.", style = "Normal")
    
    # Create and Save Plot
    plot <- ggplot(filtered_data_df, aes(x = log_gdp, y = indicator_value, color = country_name)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
      labs(title = "Wage Bill vs. Log(GDP per Capita)", x = "Log(GDP per Capita, 2015)", y = "Wage Bill") +
      theme_minimal()
    
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = plot, width = 8, height = 6, dpi = 300)
    
    # Add Image and Note
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>%
      body_add_par("Note: This indicator represents the relationship between wage bill and log(GDP per capita). The trendline provides a visual reference for the overall pattern.", 
                   style = "Normal")
    
    return(doc)
  }
  
  
  
  #Workforce graphs 
  
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
  
  output$note_stackedBarGraph <- renderText({
    "Note: This indicator represents the distribution of public sector employment across different industries (Public Administration, Education, Health, and Other) as a percentage of total public employment."
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
  output$note_horizontalStackedBar <- renderText({
    paste0("Note: This indicator represents the distribution of the public sector workforce across different industries in ", 
           input$selected_country, 
           " for the earliest and latest available years in the dataset. It highlights the changes in sectoral employment over time.")
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
  generate_public_sector_workforce_section <- function(doc) {
    doc <- doc %>% body_add_par("Public Sector Workforce Analysis", style = "heading 1")
    
    # Add introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of public workforce distribution across different sectors and countries.", 
      style = "Normal"
    )
    
    # First Graph: Public Workforce Distribution by Country
    first_graph_data <- filtered_workforce_data() %>% filter(country_name %in% input$countries_workforce)
    
    if (nrow(first_graph_data) == 0) {
      doc <- doc %>% body_add_par("No data available for Public Workforce Distribution by Country.", style = "Normal")
    } else {
      first_graph_ggplot <- ggplot(first_graph_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c("Public Administration" = "#568340", "Education" = "#B3242B", 
                                     "Health" = "#003366", "Other" = "#A9A9A9")) +
        labs(title = "Public Workforce Distribution by Country", x = "Country", y = "Workforce Distribution (%)") +
        theme_minimal()
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = first_graph_ggplot, width = 6, height = 4)
      
      doc <- doc %>% 
        body_add_par("Public Workforce Distribution by Country", style = "heading 2") %>% 
        body_add_img(src = img_path1, width = 6, height = 4) %>% 
        body_add_par("This graph shows the public workforce distribution across multiple countries.", style = "Normal")
    }
    
    # Second Graph: Sectoral Distribution of Public Sector Workforce
    filtered_data <- public_sector_workforce %>% filter(country_name == input$selected_country)
    
    if (nrow(filtered_data) < 2) {
      doc <- doc %>% body_add_par("Not enough data available for the selected country to create the graph.", style = "Normal")
    } else {
      first_year <- min(filtered_data$year, na.rm = TRUE)
      last_year <- max(filtered_data$year, na.rm = TRUE)
      
      if (!is.finite(first_year) || !is.finite(last_year)) {
        doc <- doc %>% body_add_par("Invalid year data for the selected country.", style = "Normal")
      } else {
        second_graph_data <- filtered_data %>% 
          filter(year %in% c(first_year, last_year)) %>% 
          group_by(year, indicator_name) %>% 
          summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")
        
        second_graph_ggplot <- ggplot(second_graph_data, aes(x = value_percentage, y = factor(year, levels = c(last_year, first_year)), fill = indicator_name)) +
          geom_bar(stat = "identity", position = "stack", orientation = "horizontal") +
          scale_fill_manual(values = c("Public Administration" = "#568340", "Education" = "#B3242B", 
                                       "Health" = "#003366", "Other" = "#A9A9A9")) +
          labs(title = paste("Sectoral Distribution of Public Sector Workforce in", input$selected_country), x = "Percentage (%)", y = "Year") +
          theme_minimal()
        
        img_path2 <- tempfile(fileext = ".png")
        ggsave(img_path2, plot = second_graph_ggplot, width = 6, height = 4)
        
        doc <- doc %>% 
          body_add_par("Sectoral Distribution of Public Sector Workforce", style = "heading 2") %>% 
          body_add_img(src = img_path2, width = 6, height = 4) %>% 
          body_add_par("This graph shows the sectoral distribution of public sector workforce for the selected country.", style = "Normal")
      }
    }
    
    return(doc)
  }
  
  #Tertiary Education 
  
  output$barPlot <- renderPlotly({
    req(input$selected_countries)
    
    # Filter Data
    filtered_data <- tertiary_education %>% 
      filter(country_name %in% input$selected_countries)
    
    # Define Colors
    custom_colors <- c("Individuals with tertiary education as a share of private paid employees" = "#B3242B", 
                       "Individuals with tertiary education as a share of public paid employees" = "#003366")
    
    # Generate Bar Plot
    plot <- filtered_data %>%
      plot_ly(x = ~country_name, y = ~value_percentage, 
              color = ~indicator_name, colors = custom_colors, 
              type = 'bar', barmode = 'group', text = ~paste0(round(value_percentage, 1), "%"),
              textposition = "auto") %>%
      layout(title = "Tertiary Education by Sector and Country",
             xaxis = list(title = "Country"),
             yaxis = list(title = "Tertiary Education (%)"),
             legend = list(title = list(text = "<b>Sector</b>")))
    
    plot
  })
  output$note_tertiaryEducation <- renderText({
    "Note: This indicator represents the proportion of individuals with tertiary education in the public and private sectors across selected countries. It highlights differences in educational attainment among paid employees by sector."
  })
  
  output$downloadGraphsWordEducation <- downloadHandler(
    filename = function() { paste0("Tertiary_Education_Report_", Sys.Date(), ".docx") },
    content = function(file) {
      
      # Create Word Document
      doc <- read_docx()
      
      # Title Style
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Tertiary Education Analysis", prop = title_style)))
      
      # Introduction
      doc <- doc %>% body_add_par("This report presents an analysis of tertiary education among public and private sector employees across selected countries.", style = "Normal")
      
      # Save Bar Plot as an Image
      filtered_data <- tertiary_education %>% 
        filter(country_name %in% input$selected_countries)
      
      ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("Individuals with tertiary education as a share of private paid employees" = "#B3242B", 
                                     "Individuals with tertiary education as a share of public paid employees" = "#003366")) +
        labs(title = "Tertiary Education by Sector and Country", x = "Country", y = "Tertiary Education (%)") +
        theme_minimal()
      
      img_path <- tempfile(fileext = ".png")
      ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
      
      # Add Image to Word
      doc <- doc %>% body_add_img(src = img_path, width = 6, height = 4)
      
      # Save the Word Document
      print(doc, target = file)
    }
  )
  generate_tertiary_education_section <- function(doc) {
    # Add Section Title
    doc <- doc %>% body_add_par("Tertiary Education Analysis", style = "heading 1")
    
    # Add introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of tertiary education among public and private sector employees across selected countries.", 
      style = "Normal"
    )
    
    # Filter data for selected countries
    filtered_data <- tertiary_education %>% 
      filter(country_name %in% input$selected_countries)
    
    # Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # Generate Tertiary Education ggplot
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Individuals with tertiary education as a share of private paid employees" = "#B3242B", 
                                   "Individuals with tertiary education as a share of public paid employees" = "#003366")) +
      labs(title = "Tertiary Education by Sector and Country", x = "Country", y = "Tertiary Education (%)") +
      theme_minimal()
    
    # Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # Add image to the document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the proportion of individuals with tertiary education working in public and private sector employment.", style = "Normal")
    
    return(doc)
  }
  
  #Public Sector Wage Premium 
  
  # Render the Dot Plot for Public Sector Wage Premium
  
  output$dotPlot <- renderPlotly({
    req(input$countries_wage_premium)
    
    # Filter dataset & select only relevant columns
    filtered_data <- public_wage_premium %>% 
      filter(country_name %in% input$countries_wage_premium) %>%
      select(country_name, value_percentage) %>%
      drop_na(value_percentage)  # Remove any NA values
    
    # Check if there's data after filtering
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Prevents errors when no data is available
    }
    
    # Assign colors: First country -> Red, Others -> Dark Blue
    filtered_data <- filtered_data %>%
      mutate(color = ifelse(country_name == input$countries_wage_premium[1], "#B3242B", "#003366"))
    
    # Generate Dot Plot
    plot <- plot_ly(
      data = filtered_data,
      x = ~country_name,
      y = ~value_percentage,
      type = "scatter",
      mode = "markers",
      marker = list(size = 10, opacity = 0.8),
      color = ~color,  # Assign color dynamically
      colors = c("#B3242B", "#003366"),  # Red & Dark Blue
      text = ~paste0("Country: ", country_name, "<br>Wage Premium: ", round(value_percentage, 1), "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Public Sector Wage Premium (Compared to All Private Employees) by Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Public Sector Wage Premium (%)"),
        showlegend = FALSE
      )
    
    plot
  })
  
  output$note_wage_premium <- renderText({
    "Note: This indicator represents the public sector wage premium compared to all private sector employees. A positive value indicates that public sector workers earn more than their private-sector counterparts, on average."
  })

  # Download the Report as a Word Document
  output$downloadWagePremiumReport <- downloadHandler(
    filename = function() { paste0("Public_Sector_Wage_Premium_", Sys.Date(), ".docx") },
    content = function(file) {
      
      # Create Word Document
      doc <- read_docx()
      
      # Define Title Style
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Public Sector Wage Premium Analysis", prop = title_style)))
      
      # Introduction
      doc <- doc %>% body_add_par("This report presents an analysis of public sector wage premium compared to all private sector employees across selected countries.", style = "Normal")
      
      # Filter Data for Selected Countries
      filtered_data <- public_wage_premium %>% 
        filter(country_name %in% input$countries_wage_premium) %>%
        drop_na(value_percentage)  # Remove NA values
      
      
      # Ensure the data exists
      if (nrow(filtered_data) == 0) {
        doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
        print(doc, target = file)
        return()
      }
      
      # Check if the dataset is empty BEFORE proceeding
      if (nrow(filtered_data) == 0) {
        warning("No data available for selected countries in public wage premium.")
        
        # Assign default values to avoid errors
        avg_wage_premium <- NA
        highest_country <- "N/A"
        lowest_country <- "N/A"
      } else {
        # Convert to numeric safely
        filtered_data$value_percentage <- as.numeric(filtered_data$value_percentage)
        
        # Calculate key statistics safely
        avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
        
        highest_country <- filtered_data %>%
          filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
          pull(country_name)
        
        lowest_country <- filtered_data %>%
          filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
          pull(country_name)
      }
      
      # Convert value_percentage to numeric (just in case)
      filtered_data$value_percentage <- as.numeric(filtered_data$value_percentage)
      
      # Assign Colors: First Selected Country = Red, Others = Dark Blue
      filtered_data <- filtered_data %>%
        mutate(color = ifelse(country_name == input$countries_wage_premium[1], "#B3242B", "#003366"))
      
      # Create the Dot Plot with Different Colors
      ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = color)) +
        geom_point(size = 5) +
        scale_color_identity() +  # Use assigned colors directly
        labs(title = "Public Sector Wage Premium by Country", x = "Country", y = "Wage Premium (%)") +
        theme_minimal()
      
      # Save Dot Plot as Image
      img_path <- tempfile(fileext = ".png")
      ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
      
      # Add Image to Word Document
      doc <- doc %>% body_add_img(src = img_path, width = 6, height = 4)
      
      # Save the Word Document
      print(doc, target = file)
    }
  )
  generate_wage_premium_report_section <- function(doc) {
    # Add Section Title
    doc <- doc %>% body_add_par("Public Sector Wage Premium Analysis", style = "heading 1")
    
    # Add introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of public sector wage premiums compared to private sector employees across selected countries.", 
      style = "Normal"
    )
    
    # Filter data for selected countries
    filtered_data <- public_wage_premium %>% 
      filter(country_name %in% input$countries_wage_premium) %>%
      drop_na(value_percentage)  # Remove NA values
    
    # Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # Convert to numeric safely
    filtered_data$value_percentage <- as.numeric(filtered_data$value_percentage)
    
    # Calculate key statistics safely
    avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
    
    highest_country <- filtered_data %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_country <- filtered_data %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    # Assign Colors: First Selected Country = Red, Others = Dark Blue
    filtered_data <- filtered_data %>%
      mutate(color = ifelse(country_name == input$countries_wage_premium[1], "#B3242B", "#003366"))
    
    # Create the Dot Plot with Different Colors
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = color)) +
      geom_point(size = 5) +
      scale_color_identity() +  # Use assigned colors directly
      labs(title = "Public Sector Wage Premium by Country", x = "Country", y = "Wage Premium (%)") +
      theme_minimal()
    
    # Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # Add image to the document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the wage premium in the public sector relative to private sector employees across selected countries.", style = "Normal")
    
    return(doc)
  }
  
  
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
  output$note_female_employment <- renderText({
    "Note: This indicator represents female employment as a percentage of paid employees in the public and private sectors. Public sector data is displayed as bars, while private sector data is represented as scatter points."
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
      countries <- ifelse(!is.null(input$countries_first) & length(input$countries_first) > 0, input$countries_first[1], "Unknown Country")
      report_title <- paste("Wage Premium Gender Analysis Report -", countries)
      doc <- read_docx()
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext(report_title, prop = title_style)))
      doc <- doc %>% body_add_par("Introduction", style = "heading 2") %>% 
        body_add_par("This report presents evidence on public sector employment and compensation practices for the selected countries.", style = "Normal")
      if("firstGraphgender" %in% input$graphs_to_download & length(input$countries_first) > 0) {
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
      if("secondGraphgender" %in% input$graphs_to_download & !is.null(input$country_second)) {
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
  generate_wage_premium_gender_section <- function(doc) {
    # Add Section Title
    doc <- doc %>% body_add_par("Wage Premium Gender Analysis", style = "heading 1")
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      "This section presents evidence on public sector employment and compensation practices for selected countries.", 
      style = "Normal"
    )
    
    # First Graph - Wage Premium by Gender (Multi-Country)
    if ("firstGraphgender" %in% input$graphs_to_download & length(input$countries_first) > 0) {
      data_to_plot <- gender_wage_premium_last %>% filter(country_name %in% input$countries_first)
      data_to_plot_long <- data_to_plot %>% 
        select(country_name, indicator_label, year, value) %>% 
        mutate(indicator_label = factor(indicator_label))
      
      first_graph_wage_premium_gender <- ggplot(data_to_plot_long, aes(x = country_name, y = value, color = indicator_label)) +
        geom_point(size = 3) +
        labs(title = "Wage Premium Gender (Multi-Country)", x = "Country", y = "Value") +
        theme_minimal()
      
      # Save the plot as an image
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = first_graph_wage_premium_gender, width = 6, height = 4)
      
      # Add Image and Description
      doc <- doc %>% 
        body_add_par("First Graph: Wage Premium Gender (Multi-Country)", style = "heading 2") %>% 
        body_add_img(src = img_path1, width = 6, height = 4) %>% 
        body_add_par("This graph shows the wage premium by gender across multiple countries.", style = "Normal")
    }
    
    # Second Graph - Wage Premium by Gender (Single Country Over Time)
    if ("secondGraphgender" %in% input$graphs_to_download & !is.null(input$country_second)) {
      data_to_plot <- gender_wage_premium %>% filter(country_name == input$country_second)
      data_to_plot_long <- data_to_plot %>% 
        select(year, indicator_name, value) %>% 
        mutate(indicator_name = factor(indicator_name))
      
      second_graph_wage_premium_gender <- ggplot(data_to_plot_long, aes(x = year, y = value, color = indicator_name)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        labs(title = paste("Public Sector Wage Premium by Gender in", input$country_second, "Over Time"), 
             x = "Year", y = "Employment Value") +
        theme_minimal()
      
      # Save the plot as an image
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph_wage_premium_gender, width = 6, height = 4)
      
      # Add Image and Description
      doc <- doc %>% 
        body_add_par("Second Graph: Public Sector Employment (Single Country)", style = "heading 2") %>% 
        body_add_img(src = img_path2, width = 6, height = 4) %>% 
        body_add_par("This graph shows the wage premium by gender trends over time for the selected country.", style = "Normal")
    }
    
    return(doc)
  }
  
  
  # Wage premium by Education Level 
  
  # Render the Public Sector Wage Premium by Education Level Graph
  output$education_wage_premium_plot <- renderPlotly({
    
    req(input$selected_country)  # Ensure a country is selected
    
    # Filter the dataset for the selected country
    filtered_data <- public_wage_premium_educ %>%
      filter(country_name == input$selected_country) %>%
      drop_na(value_percentage)  # Remove NAs
    
    # Ensure data exists
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    # Define custom colors for education levels
    education_colors <- c(
      "No Education" = "#003366",       # Dark Blue
      "Primary Education" = "#B3242B",  # Dark Red
      "Secondary Education" = "#3B3B3B",# Dark Gray/Black
      "Tertiary Education" = "#006400"  # Dark Green
    )
    
    # Create the bar plot
    p <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = education_colors) +
      labs(
        title = "Public Sector Wage Premium by Education Level (Compared to Private Formal Workers)",
        x = "Indicator",  # Updated label
        y = "Wage Premium (%)",
        fill = "Education Level"  # Label for legend
      ) +
      theme_minimal()
    
    ggplotly(p)  # Convert ggplot to Plotly for interactivity
    
  })
  output$note_education_wage_premium <- renderText({
    "Note: This indicator represents the public sector wage premium across different education levels, comparing public sector wages to those of private formal workers."
  })
  # -----------------------------------
  # 📄 Download Handler for Word Report
  # -----------------------------------
  output$downloadEducationWagePremium <- downloadHandler(
    filename = function() {
      paste0("Public_Sector_Wage_Premium_Education_", Sys.Date(), ".docx")
    },
    content = function(file) {
      
      # Create Word Document
      doc <- read_docx()
      
      # Define Title Style
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Public Sector Wage Premium by Education Level", prop = title_style)))
      
      # Add Introduction
      doc <- doc %>% body_add_par(
        paste0("This report presents an analysis of public sector wage premiums based on different education levels for ", 
               input$selected_country, ". The comparison is made against private sector formal workers."), 
        style = "Normal"
      )
      
      # Filter Data
      filtered_data <- public_wage_premium_educ %>%
        filter(country_name == input$selected_country) %>%
        drop_na(value_percentage)
      
      # Ensure data exists
      if (nrow(filtered_data) == 0) {
        doc <- doc %>% body_add_par("No data available for the selected country.", style = "Normal")
        print(doc, target = file)
        return()
      }
      
      # Save Bar Plot as Image
      img_path <- tempfile(fileext = ".png")
      
      ggplot_obj <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c(
          "No Education" = "#003366",
          "Primary Education" = "#B3242B",
          "Secondary Education" = "#3B3B3B",
          "Tertiary Education" = "#006400"
        )) +
        labs(
          title = "Public Sector Wage Premium by Education Level (Compared to Private Formal Workers)",
          x = "Education Level",
          y = "Wage Premium (%)"
        ) +
        theme_minimal()
      
      ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
      
      # Add Image to Word Document
      doc <- doc %>% body_add_img(src = img_path, width = 6, height = 4)
      
      # Save the Word Document
      print(doc, target = file)
    }
  )
  
  generate_wage_premium_education_section <- function(doc) {
    # Add Section Title
    doc <- doc %>% body_add_par("Public Sector Wage Premium by Education Level", style = "heading 1")
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      paste0("This section presents an analysis of public sector wage premiums based on different education levels for ", 
             input$selected_country, ". The comparison is made against private sector formal workers."), 
      style = "Normal"
    )
    
    # Filter data for the selected country
    filtered_data <- public_wage_premium_educ %>%
      filter(country_name == input$selected_country) %>%
      drop_na(value_percentage)
    
    # Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected country.", style = "Normal")
      return(doc)
    }
    
    # Generate Wage Premium by Education Level ggplot
    ggplot_obj <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(
        "No Education" = "#003366",
        "Primary Education" = "#B3242B",
        "Secondary Education" = "#3B3B3B",
        "Tertiary Education" = "#006400"
      )) +
      labs(
        title = "Public Sector Wage Premium by Education Level (Compared to Private Formal Workers)",
        x = "Education Level",
        y = "Wage Premium (%)"
      ) +
      theme_minimal()
    
    # Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # Add image to the document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the public sector wage premium by education level, comparing earnings with private sector formal workers.", style = "Normal")
    
    return(doc)
  }
  

  #Public Sector Graphs 
  
  # First Graph - Multi-Country Dot Plot
  output$firstGraphpublic <- renderPlotly({
    filtered_data <- public_sector_emp_temp_last %>% 
      filter(country_name %in% input$countries_first)
    
    ggplotly(
      ggplot(filtered_data, aes(x = country_name, y = value, color = indicator_name)) +
        geom_point(size = 4) +
        labs(title = "Public Sector Employment (Last Year Available)", 
             x = "Country", y = "Value") +
        theme_minimal()
    ) %>% 
      layout(legend = list(title = list(text = "Indicator")))  # Rename legend title
  })
  
  output$note_firstGraphpublic <- renderText({
    "Note: This indicator represents public sector employment as a percentage of total employment for the most recent year available in each country."
  })
  
  # Second Graph - Single-Country Line Plot
  output$secondGraphpublic <- renderPlotly({
    filtered_data <- public_sector_emp_temp %>% 
      filter(country_name == input$country_second)
    
    ggplotly(
      ggplot(filtered_data, aes(x = year, y = value_percentage, color = indicator_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(title = "Public Sector Employment Over Time", 
             x = "Year", y = "Value") +
        theme_minimal()
    ) %>% 
      layout(legend = list(title = list(text = "Indicator")))  # Rename legend title
  })
  
  output$note_secondGraphpublic <- renderText({
    "Note: This indicator represents the trend in public sector employment over time, showing how employment levels have evolved across different years."
  })
  
  
  # Download Handler - Save Graphs to Word Document
  output$downloadGraphsWord <- downloadHandler(
    filename = function() {
      paste0("Public_Sector_Employment_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      
      # Title
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Public Sector Employment Analysis", prop = title_style)))
      
      # Intro Text
      doc <- doc %>% body_add_par("This report presents the analysis of public sector employment across selected countries and its trend over time.", style = "Normal")
      
      # First Graph - Save as Image
      first_graph <- ggplot(public_sector_emp_temp_last %>% filter(country_name %in% input$countries_first), 
                            aes(x = country_name, y = value, color = indicator_name)) +
        geom_point(size = 4) +
        labs(title = "Public Sector Employment (Last Year Available)", x = "Country", y = "Value") +
        theme_minimal()
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = first_graph, width = 8, height = 6)
      
      doc <- doc %>% body_add_par("Public Sector Employment - Last Year Available", style = "heading 2")
      doc <- doc %>% body_add_img(src = img_path1, width = 6, height = 4)
      
      # Second Graph - Save as Image
      second_graph <- ggplot(public_sector_emp_temp %>% filter(country_name == input$country_second), 
                             aes(x = year, y = value_percentage, color = indicator_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(title = "Public Sector Employment Over Time", x = "Year", y = "Value") +
        theme_minimal()
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph, width = 8, height = 6)
      
      doc <- doc %>% body_add_par("Public Sector Employment Over Time", style = "heading 2")
      doc <- doc %>% body_add_img(src = img_path2, width = 6, height = 4)
      
      # Save the Document
      print(doc, target = file)
    }
  )
  
  
  generate_public_sector_employment_section <- function(doc) {
    # Add Section Title
    doc <- doc %>% body_add_par("Public Sector Employment Analysis", style = "heading 1")
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      "This section presents the analysis of public sector employment across selected countries and its trend over time.", 
      style = "Normal"
    )
    
    # First Graph - Public Sector Employment (Last Year Available)
    first_graph <- ggplot(public_sector_emp_temp_last %>% filter(country_name %in% input$countries_first), 
                          aes(x = country_name, y = value, color = indicator_name)) +
      geom_point(size = 4) +
      labs(title = "Public Sector Employment (Last Year Available)", x = "Country", y = "Value") +
      theme_minimal()
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # Add First Graph to the Document
    doc <- doc %>% 
      body_add_par("Public Sector Employment - Last Year Available", style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4)
    
    # Second Graph - Public Sector Employment Over Time
    second_graph <- ggplot(public_sector_emp_temp %>% filter(country_name == input$country_second), 
                           aes(x = year, y = value_percentage, color = indicator_name)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(title = "Public Sector Employment Over Time", x = "Year", y = "Value") +
      theme_minimal()
    
    img_path2 <- tempfile(fileext = ".png")
    ggsave(img_path2, plot = second_graph, width = 8, height = 6)
    
    # Add Second Graph to the Document
    doc <- doc %>% 
      body_add_par("Public Sector Employment Over Time", style = "heading 2") %>% 
      body_add_img(src = img_path2, width = 6, height = 4)
    
    return(doc)
  }
  

#Gender Wage premium 
  
  # First Graph - Multi-Country Dot Plot for Wage Premium by Gender
  output$firstGraphGenderWagePremium <- renderPlotly({
    filtered_data <- gender_wage_premium_last %>% 
      filter(country_name %in% input$countries_first)
    
    ggplotly(
      ggplot(filtered_data, aes(x = country_name, y = value, color = indicator_label)) +
        geom_point(size = 4) +
        labs(title = "Public Sector Wage Premium by Gender (Last Year Available)", 
             x = "Country", 
             y = "Wage Premium (%)",
             color = "Indicator") +  # Updated label for legend
        theme_minimal()
    )
  })
  output$note_firstGraphGenderWagePremium <- renderText({
    "Note: This indicator represents the public sector wage premium by gender, comparing wage differences between men and women in different sectors."
  })
  
  # Second Graph - Single-Country Line Plot for Wage Premium by Gender Over Time
  output$secondGraphGenderWagePremium <- renderPlotly({
    filtered_data <- gender_wage_premium %>% 
      filter(country_name == input$country_second)
    
    ggplotly(
      ggplot(filtered_data, aes(x = year, y = value, color = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(title = "Public Sector Wage Premium by Gender Over Time", 
             x = "Year", 
             y = "Wage Premium (%)",
             color = "Indicator") +  # Updated label for legend
        theme_minimal() +
        annotate("text", x = Inf, y = min(filtered_data$value) - 5,  # Adjusted variable name
                 label = "This indicator represents the gender wage premium across industries in the public sector.", 
                 hjust = 1, size = 4, color = "black", fontface = "italic")
      
    )
  })
  output$note_secondGraphGenderWagePremium <- renderText({
    "Note: This indicator represents the gender wage premium across industries in the public sector, showing differences between men and women over time."
  })
  
  # Download Handler - Save Gender Wage Premium Graphs to Word Document
  output$downloadGraphsWordGenderWagePremium <- downloadHandler(
    filename = function() {
      paste0("Public_Sector_Wage_Premium_Gender_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      
      # Title
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Public Sector Wage Premium by Gender", prop = title_style)))
      
      # Intro Text
      doc <- doc %>% body_add_par(
        "This report presents an analysis of public sector wage premium by gender across selected countries and its trend over time.", 
        style = "Normal"
      )
      
      # First Graph - Save as Image
      first_graph <- ggplot(gender_wage_premium_last %>% filter(country_name %in% input$countries_first), 
                            aes(x = country_name, y = value, color = indicator_label)) +
        geom_point(size = 4) +
        labs(title = "Public Sector Wage Premium by Gender (Last Year Available)", 
             x = "Country", 
             y = "Wage Premium (%)") +
        theme_minimal()
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = first_graph, width = 8, height = 6)
      
      doc <- doc %>% body_add_par("Public Sector Wage Premium by Gender - Last Year Available", style = "heading 2")
      doc <- doc %>% body_add_img(src = img_path1, width = 6, height = 4)
      
      # Second Graph - Save as Image
      second_graph <- ggplot(gender_wage_premium %>% filter(country_name == input$country_second), 
                             aes(x = year, y = value, color = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(title = "Public Sector Wage Premium by Gender Over Time", 
             x = "Year", 
             y = "Wage Premium (%)") +
        theme_minimal()
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph, width = 8, height = 6)
      
      doc <- doc %>% body_add_par("Public Sector Wage Premium by Gender Over Time", style = "heading 2")
      doc <- doc %>% body_add_img(src = img_path2, width = 6, height = 4) %>% 
        body_add_par("Note: This indicator represents the gender wage premium across industries in the public sector.", 
                     style = "Normal")
      
      # Save the Document
      print(doc, target = file)
    }
  )
  
  generate_wage_premium_gender_report_section <- function(doc) {
    # Add Section Title
    doc <- doc %>% body_add_par("Public Sector Wage Premium by Gender", style = "heading 1")
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of public sector wage premium by gender across selected countries and its trend over time.", 
      style = "Normal"
    )
    
    # First Graph - Public Sector Wage Premium by Gender (Last Year Available)
    first_graph <- ggplot(gender_wage_premium_last %>% filter(country_name %in% input$countries_first), 
                          aes(x = country_name, y = value, color = indicator_label)) +
      geom_point(size = 4) +
      labs(title = "Public Sector Wage Premium by Gender (Last Year Available)", 
           x = "Country", 
           y = "Wage Premium (%)") +
      theme_minimal()
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # Add First Graph to the Document
    doc <- doc %>% 
      body_add_par("Public Sector Wage Premium by Gender - Last Year Available", style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4)
    
    # Second Graph - Public Sector Wage Premium by Gender Over Time
    second_graph <- ggplot(gender_wage_premium %>% filter(country_name == input$country_second), 
                           aes(x = year, y = value, color = indicator_label)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(title = "Public Sector Wage Premium by Gender Over Time", 
           x = "Year", 
           y = "Wage Premium (%)") +
      theme_minimal()
    
    img_path2 <- tempfile(fileext = ".png")
    ggsave(img_path2, plot = second_graph, width = 8, height = 6)
    
    # Add Second Graph to the Document
    doc <- doc %>% 
      body_add_par("Public Sector Wage Premium by Gender Over Time", style = "heading 2") %>% 
      body_add_img(src = img_path2, width = 6, height = 4) %>% 
      body_add_par("Note: This indicator represents the gender wage premium across industries in the public sector.", 
                   style = "Normal")
    
    return(doc)
  }
  
  
  # Gender Workforce Graphs
  
  # First Graph - Multi-Country Bar Plot
  output$firstGraphGenderWorkforce <- renderPlotly({
    filtered_data <- gender_workforce %>% 
      filter(country_name %in% input$countries_gender)
    
    # Ensure factor levels match color scale
    filtered_data$indicator_name <- factor(filtered_data$indicator_name, 
                                           levels = c("Females, as a share of private paid employees", 
                                                      "Females, as a share of public paid employees"))
    
    ggplotly(
      ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("Females, as a share of private paid employees" = "#B3242B", 
                                     "Females, as a share of public paid employees" = "#003366")) +
        labs(title = "Female Employment by Sector (Last Year Available)", 
             x = "Country", y = "Employment (%)", fill = "Sector") +
        theme_minimal()
    )
  })
  output$note_firstGraphGenderWorkforce <- renderText({
    "Note: This indicator represents the share of females employed in the public and private sectors. It highlights gender differences in workforce participation by sector."
  })
  
  # Second Graph - Single-Country Line Plot
  output$secondGraphGenderWorkforce <- renderPlotly({
    filtered_data <- gender_workforce %>% 
      filter(country_name == input$country_gender)
    
    # Ensure factor levels match color scale
    filtered_data$indicator_name <- factor(filtered_data$indicator_name, 
                                           levels = c("Females, as a share of private paid employees", 
                                                      "Females, as a share of public paid employees"))
    
    ggplotly(
      ggplot(filtered_data, aes(x = year, y = value_percentage, color = indicator_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c("Females, as a share of private paid employees" = "#B3242B", 
                                      "Females, as a share of public paid employees" = "#003366")) +
        labs(title = paste("Female Employment by Sector Over Time in", input$country_gender), 
             x = "Year", y = "Female Employment (%)", color = "Sector") +
        theme_minimal()
    )
  })
  output$note_secondGraphGenderWorkforce <- renderText({
    "Note: This indicator represents the trend of female employment in the public and private sectors over time, allowing for a comparison of sectoral changes."
  })
  
  # Download Handler - Save Graphs to Word Document
  output$downloadGraphsWordGender <- downloadHandler(
    filename = function() {
      paste0("Gender_Workforce_Analysis_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      
      # Title
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Gender Workforce Analysis", prop = title_style)))
      
      # Intro Text
      doc <- doc %>% body_add_par("This report presents the analysis of female employment in the public and private sectors across selected countries.", style = "Normal")
      
      # First Graph - Save as Image
      first_graph <- ggplot(gender_workforce %>% filter(country_name %in% input$countries_gender), 
                            aes(x = country_name, y = value_percentage, fill = indicator_name)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("Females, as a share of private paid employees" = "#B3242B", 
                                     "Females, as a share of public paid employees" = "#003366")) +
        labs(title = "Female Employment by Sector (Last Year Available)", 
             x = "Country", y = "Employment (%)", fill = "Sector") +
        theme_minimal()
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = first_graph, width = 8, height = 6)
      doc <- doc %>% body_add_par("Female Employment by Sector (Last Year Available)", style = "heading 2")
      doc <- doc %>% body_add_img(src = img_path1, width = 6, height = 4)
      
      # Second Graph - Save as Image
      second_graph <- ggplot(gender_workforce %>% filter(country_name == input$country_gender), 
                             aes(x = year, y = value_percentage, color = indicator_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c("Females, as a share of private paid employees" = "#B3242B", 
                                      "Females, as a share of public paid employees" = "#003366")) +
        labs(title = paste("Female Employment by Sector Over Time in", input$country_gender), 
             x = "Year", y = "Female Employment (%)", color = "Sector") +
        theme_minimal()
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph, width = 8, height = 6)
      doc <- doc %>% body_add_par("Female Employment by Sector Over Time", style = "heading 2")
      doc <- doc %>% body_add_img(src = img_path2, width = 6, height = 4)
      
   
      # Save the Document
      print(doc, target = file)
    }
  )
  
  generate_gender_workforce_section <- function(doc) {
    # Add Section Title
    doc <- doc %>% body_add_par("Gender Workforce Analysis", style = "heading 1")
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of female employment in the public and private sectors across selected countries.", 
      style = "Normal"
    )
    
    # First Graph - Female Employment by Sector (Last Year Available)
    first_graph <- ggplot(gender_workforce %>% filter(country_name %in% input$countries_gender), 
                          aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Females, as a share of private paid employees" = "#B3242B", 
                                   "Females, as a share of public paid employees" = "#003366")) +
      labs(title = "Female Employment by Sector (Last Year Available)", 
           x = "Country", y = "Employment (%)", fill = "Sector") +
      theme_minimal()
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # Add First Graph to the Document
    doc <- doc %>% 
      body_add_par("Female Employment by Sector (Last Year Available)", style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4)
    
    # Second Graph - Female Employment by Sector Over Time
    second_graph <- ggplot(gender_workforce %>% filter(country_name == input$country_gender), 
                           aes(x = year, y = value_percentage, color = indicator_name)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Females, as a share of private paid employees" = "#B3242B", 
                                    "Females, as a share of public paid employees" = "#003366")) +
      labs(title = paste("Female Employment by Sector Over Time in", input$country_gender), 
           x = "Year", y = "Female Employment (%)", color = "Sector") +
      theme_minimal()
    
    img_path2 <- tempfile(fileext = ".png")
    ggsave(img_path2, plot = second_graph, width = 8, height = 6)
    
    # Add Second Graph to the Document
    doc <- doc %>% 
      body_add_par("Female Employment by Sector Over Time", style = "heading 2") %>% 
      body_add_img(src = img_path2, width = 6, height = 4)
    
    return(doc)
  }
  
  
  
  # Women Leadership 
  
  output$barPlotwomen <- renderPlotly({
    if (is.null(input$selected_countries) || length(input$selected_countries) == 0) return(NULL)
    
    filtered_data <- gender_leadership %>% filter(country_name %in% input$selected_countries)
    if (nrow(filtered_data) == 0) return(NULL)
    
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
  
  output$note_barPlotwomen <- renderText({
    "Note: This indicator represents the share of females in different occupational groups (Managers/Clerks) in both the public and private sectors, highlighting gender representation disparities."
  })
  
  output$downloadGraphsWordfemale <- downloadHandler(
    filename = function() { paste0("Females_Occupation_Groups_Analysis_", Sys.Date(), ".docx") },
    content = function(file) {
      
      # Create Word Document
      doc <- read_docx()
      
      # Title Style
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Females by Occupational Group and Sector", prop = title_style)))
      
      # Introduction
      doc <- doc %>% body_add_par("This report presents an analysis of female representation in different occupational groups across selected countries.", style = "Normal")
      
      # Filter Data
      filtered_data <- gender_leadership %>% filter(country_name %in% input$selected_countries)
      
      if(nrow(filtered_data) == 0) return(NULL)  # Ensure there's data to plot
      
      # Convert to ggplot Object for ggsave()
      ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("Clerks-Public" = "#003366", "Managers-Public" = "#ADD8E6",
                                     "Clerks-Private" = "#006400", "Managers-Private" = "#90EE90")) +
        labs(title = "Females by Occupational Group and Sector", x = "Country", y = "Female Share (%)") +
        theme_minimal()
      
      # Save ggplot as Image
      img_path <- tempfile(fileext = ".png")
      ggsave(filename = img_path, plot = ggplot_obj, width = 8, height = 6, dpi = 300)
      
      # Add Image to Word
      doc <- doc %>% 
        body_add_img(src = img_path, width = 6, height = 4) %>% 
        body_add_par("This graph shows the share of females in various occupational groups (Managers/Clerks) in the public and private sectors for the selected countries.", style = "Normal")
      
      # Save the Word Document
      print(doc, target = file)
    }
  )
  
  generate_females_occupation_groups_section <- function(doc) {
    # Add Section Title
    doc <- doc %>% body_add_par("Females by Occupational Group and Sector", style = "heading 1")
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of female representation in different occupational groups across selected countries.", 
      style = "Normal"
    )
    
    # Filter Data for Selected Countries
    filtered_data <- gender_leadership %>% filter(country_name %in% input$selected_countries)
    
    # Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # Generate ggplot for Female Occupational Groups
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Clerks-Public" = "#003366", "Managers-Public" = "#ADD8E6",
                                   "Clerks-Private" = "#006400", "Managers-Private" = "#90EE90")) +
      labs(title = "Females by Occupational Group and Sector", x = "Country", y = "Female Share (%)") +
      theme_minimal()
    
    # Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # Add Image to the Document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the share of females in various occupational groups (Managers/Clerks) in the public and private sectors for the selected countries.", style = "Normal")
    
    return(doc)
  }
  
  
  #Gender Wage premium in the public sector, by industry 
  output$gender_wage_barplot <- renderPlot({
    
    # Filter data based on user selection
    filtered_data <- gender_wage_premiumpublic %>%
      filter(country_name %in% input$selected_countries, 
             indicator_label %in% c("Public Administration", 
                                    "Education", 
                                    "Health", 
                                    "Other")) 
    
    # Create ggplot
    ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Blues", name = "Indicator") +  # Rename legend title
      labs(title = "Gender Wage Premium in Public Sector by Industry",
           x = "Country", y = "Wage Premium (%)") +
      theme_minimal() +
      annotate("text", x = Inf, y = min(filtered_data$value_percentage) - 5, 
               label = "This indicator represents the gender wage premium across industries in the public sector.", 
               hjust = 1, size = 4, color = "black", fontface = "italic")
  })
  
  output$note_gender_wage_barplot <- renderText({
    "Note: This indicator represents the gender wage premium in the public sector across different industries, comparing wages of female employees to male employees."
  })
  
  
  # Download Handler for Word Document
  output$downloadGenderWagePremium <- downloadHandler(
    filename = function() { paste0("Gender_Wage_Premium_Report_", Sys.Date(), ".docx") },
    content = function(file) {
      
      # Create Word Document
      doc <- read_docx()
      
      # Title Style
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Gender Wage Premium in Public Sector by Industry", prop = title_style)))
      
      # Introduction
      doc <- doc %>% body_add_par("This report presents an analysis of gender wage premiums in the public sector by industry (Core Public Administration, Education, and Health) across selected countries.", style = "Normal")
      
      # Generate ggplot Object
      filtered_data <- gender_wage_premiumpublic %>%
        filter(country_name %in% input$selected_countries, 
               indicator_name %in% c("Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)", 
                                     "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)", 
                                     "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)", 
                                     "Other"))
      
      filtered_data$indicator_label <- recode(filtered_data$indicator_name,
                                              "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" = "Core Public Administration",
                                              "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)" = "Education",
                                              "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)" = "Health")
      
      gender_wage_plot <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_brewer(palette = "Blues") +  # Using Blues color palette
        labs(title = "Gender Wage Premium in Public Sector by Industry",
             x = "Country", y = "Wage Premium (%)") +
        theme_minimal()
      
      # Save ggplot as Image
      img_path <- tempfile(fileext = ".png")
      ggsave(filename = img_path, plot = gender_wage_plot, width = 8, height = 6, dpi = 300)
      
      # Add Image to Word
      doc <- doc %>% 
        body_add_img(src = img_path, width = 6, height = 4) %>% 
        body_add_par("This graph shows the gender wage premium in the public sector across different industries.", style = "Normal") %>% 
        body_add_par("Note: This indicator represents the gender wage premium across industries in the public sector.", 
                     style = "Normal")
      
      # Save the Word Document
      print(doc, target = file)
    }
  )
  generate_gender_wage_premium_section <- function(doc) {
    doc <- doc %>% body_add_par("Gender Wage Premium in Public Sector by Industry", style = "heading 1")
    
    # Add introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of gender wage premiums in the public sector by industry 
    (Core Public Administration, Education, and Health) across selected countries.", 
      style = "Normal"
    )
    
    # Filter data for Gender Wage Premium
    filtered_data <- gender_wage_premiumpublic %>%
      filter(country_name %in% input$selected_countries, 
             indicator_name %in% c("Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)", 
                                   "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)", 
                                   "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)", 
                                   "Other"))
    
    # Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # Rename indicators for readability
    filtered_data$indicator_label <- recode(filtered_data$indicator_name,
                                            "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" = "Core Public Administration",
                                            "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)" = "Education",
                                            "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)" = "Health")
    
    # Generate Gender Wage Premium ggplot
    gender_wage_plot <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Blues") +  # Using Blues color palette
      labs(title = "Gender Wage Premium in Public Sector by Industry",
           x = "Country", y = "Wage Premium (%)") +
      theme_minimal()
    
    # Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = gender_wage_plot, width = 8, height = 6, dpi = 300)
    
    # Add image and text to document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the gender wage premium in the public sector across different industries.", style = "Normal") %>% 
      body_add_par("Note: This indicator represents the gender wage premium across industries in the public sector.", 
                   style = "Normal")
    
    return(doc)
  }
  
  
  #Download one single report
 
  output$downloadAllGraphsDoc <- downloadHandler(
    filename = function() { 
      paste0("Wage_bill_and_public_employment_analysis_", Sys.Date(), ".docx") 
    },
    content = function(file) {
      
      # Initialize Word document
      doc <- read_docx() 
      
      # Add Report Title
      title_style <- fp_text(color = "#722F37", font.size = 20, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Wage bill and public employment analysis", prop = title_style)))
      
      # Add Sections from Each Tab
      doc <- generate_wage_bill_analysis_section(doc) #  Wage Bill Analysis
      doc <- generate_gdp_analysis_section(doc)       #GDP 
      doc <- generate_public_sector_workforce_section(doc) #Public Sector Workforce Analysis
      doc <- generate_tertiary_education_section(doc) # Tertiary Education Analysis
      doc <- generate_wage_premium_report_section(doc) #Public Sector Wage Premium Report
      doc <- generate_wage_premium_gender_section(doc) #Wage Premium Gender Analysis
      doc <- generate_wage_premium_education_section(doc) #Wage Premium by Education
      doc <- generate_public_sector_employment_section(doc)  #Public Sector Employment
      doc <- generate_wage_premium_gender_report_section(doc) #Wage Premium Gender Report
      doc <- generate_gender_workforce_section(doc) #Gender Workforce Analysis
      doc <- generate_females_occupation_groups_section(doc) #Females by Occupational Groups
      doc <- generate_gender_wage_premium_section(doc)    # Wage Premium by industry Analysis
      
      # Save the Final Report
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


###############################################


    
output$downloadAllGraphsDoc <- downloadHandler(
  filename = function() { 
    paste0("Wage_Bill_and_public_employment_analysis_", Sys.Date(), ".docx") 
  },
  content = function(file) {
    
    # Initialize Word document
    doc <- read_docx() 
    
    # Get the first selected country
    first_country <- if (!is.null(input$countries) & length(input$countries) > 0) {
      input$countries[1]
    } else {
      "Unknown Country"
    }
    
    # Ensure first_region is defined properly
    first_region <- if (!is.null(input$region) & input$region != "") {
      input$region
    } else {
      "the selected region"
    }
    
    # Add Title to Document
    title_text <- paste0(first_country, "\nWage Bill and Public Employment Analysis")
    
    # Format title
    title_style <- fp_text(color = "#722F37", font.size = 20, bold = TRUE)
    subtitle_style <- fp_text(color = "black", font.size = 16, bold = TRUE)
    
    doc <- doc %>% 
      body_add_fpar(fpar(ftext(first_country, prop = title_style))) %>% 
      body_add_fpar(fpar(ftext("Wage Bill and Public Employment Analysis", prop = subtitle_style)))
    
    # Extract the World Bank Region for the Selected Country
    first_region <- countrycode(first_country, origin = "country.name", destination = "region")
    
    # Handle missing values in case countrycode() fails
    if (is.na(first_region)) {
      first_region <- "its respective region"  # Default fallback if region is not found
    }
    
    # Construct Introduction Text Dynamically
    intro_text <- paste0(
      "This note presents evidence on public sector employment and compensation practices in ", first_country,
      " using the Worldwide Bureaucracy Indicators (WWBI). ",
      "For international comparisons, peer countries from ", first_region, " are included.",
      "\n\n",  # Add line break for a new paragraph
      "The public sector is typically a major source of employment in most countries. ",
      "The provision of basic services such as education, health, citizen security, and justice, among others, ",
      "makes it a central actor in labor markets, with significant impacts on the aggregate results of employment, ",
      "wages, informality, and other economic variables. Moreover, public employment is an indicator of the state’s ",
      "participation in the entire economy, which has implications for macroeconomic balances, allocation efficiency, ",
      "and income distribution. Thus, this analysis comprehensively documents the size of public employment, ",
      "its changes over time, and the characteristics of its workforce.",
      "\n\n",
      "This work documents and analyzes the size, composition, and changes in the levels of employment and wages of Peru’s public employees compared to the private sector and how these metrics compare to regional peers."
    )
    
    # Add intro text
    doc <- doc %>% body_add_par(intro_text, style = "Normal")
    
    # Helper Function to Add Graphs
    add_plot_to_doc <- function(doc, plot_obj, title, interpretation = NULL) {
      img_path <- tempfile(fileext = ".png")
      ggsave(img_path, plot = plot_obj, width = 8, height = 6)
      
      doc <- doc %>% 
        body_add_par(title, style = "heading 2") %>% 
        body_add_img(src = img_path, width = 6, height = 4)
      
      if (!is.null(interpretation)) {
        interpretation_fixed <- paste(interpretation, collapse = " ")
        doc <- doc %>% body_add_par(interpretation_fixed, style = "Normal")
      }
      
      return(doc)
    }
    
    # The macro fundamentals of the public sector
    # Wage Bill Trends
    
    # Define text formatting: Blue color, bold, and size 16
    blue_text <- fp_text(color = "#003366", font.size = 16, bold = TRUE)
    
    # Add the formatted text to the Word document
    doc <- doc %>%
      body_add_fpar(fpar(ftext("The macro fundamentals of the public sector", prop = blue_text)))
    
    doc <- doc %>% body_add_par("The macro fundamentals of the public sector", style = "heading 1")
    
    selected_data_df <- selected_data()
    
    # Extract country-specific wage bill trends
    wage_bill_trend <- wage_bill_publicexp %>%
      filter(country_name == first_country)
    
    # Get the last available year
    last_year <- max(wage_bill_trend$year, na.rm = TRUE)
    
    # Compute wage bill as % of GDP for the most recent year
    wage_bill_gdp_latest <- wage_bill_gdp %>%
      filter(year == last_year) %>%
      pull(value)
    
    # Compute wage bill as % of Public Expenditures for 2010 and latest year
    wage_bill_exp_2010 <- wage_bill_trend %>%
      filter(year == 2010) %>%
      pull(value)
    
    wage_bill_exp_latest <- wage_bill_trend %>%
      filter(year == last_year) %>%
      pull(value)
    
    # Compute trend over the past decade (if data is available)
    trend_decade <- wage_bill_trend %>%
      filter(year >= last_year - 10) %>%
      summarise(trend = ifelse(sd(value, na.rm = TRUE) < 2, "stable", "fluctuated")) %>%
      pull(trend)
    
    # Identify the COVID-19 effect (if there was a significant spike in 2020)
    covid_spike <- wage_bill_trend %>%
      filter(year == 2020) %>%
      pull(value)
    
    covid_spike_text <- if (!is.na(covid_spike) & covid_spike > (wage_bill_gdp_latest * 1.1)) {
      "temporarily spiked in 2020 as the COVID-19 crisis depressed GDP growth"
    } else {
      "remained relatively stable even during the COVID-19 crisis"
    }
    
    # Compare against regional peers
    avg_peer_wage <- wage_bill_publicexp %>%
      filter(region == first_region, year == last_year) %>%
      summarise(mean_wage = mean(value, na.rm = TRUE)) %>%
      pull(mean_wage)
    
    comparison_text <- if (!is.na(wage_bill_exp_latest) & !is.na(avg_peer_wage)) {
      if (wage_bill_exp_latest < avg_peer_wage * 0.8) {
        "lower than its regional peers"
      } else if (wage_bill_exp_latest > avg_peer_wage * 1.2) {
        "higher than its regional peers"
      } else {
        "similar to its regional peers"
      }
    } else {
      "uncertain compared to regional peers"
    }
    
    # Identify the change direction
    change_text <- if (!is.na(wage_bill_exp_2010) & !is.na(wage_bill_exp_latest)) {
      if (wage_bill_exp_latest > wage_bill_exp_2010) {
        "has increased from"
      } else if (wage_bill_exp_latest < wage_bill_exp_2010) {
        "has decreased from"
      } else {
        "has remained stable since"
      }
    } else {
      "has changed over time from"
    }
    
    # Calculate correlation between GDP per capita and wage bill spending in the selected region
    regional_trend <- wage_bill_gdp %>%
      filter(wb_region == first_region) %>%
      summarise(correlation = cor(gdp_per_capita, value, use = "complete.obs")) %>%
      pull(correlation)
    
    # Determine relationship direction
    relationship_text <- if (!is.na(regional_trend)) {
      if (regional_trend < -0.2) {
        "a negative relationship"  # Strong inverse relationship
      } else if (regional_trend > 0.2) {
        "a positive relationship"  # Wage bill increases with GDP
      } else {
        "no strong relationship"  # Weak correlation
      }
    } else {
      "an uncertain relationship"
    }
    
    # Determine spending trend dynamically
    spending_trend <- if (regional_trend < -0.2) {
      "tends to decrease"
    } else if (regional_trend > 0.2) {
      "tends to increase"
    } else {
      "remains relatively stable"
    }
    
    # Determine if spending aligns with expectations
    spending_alignment <- if (!is.na(wage_bill_gap)) {
      if (abs(wage_bill_gap) <= 1) {
        "is in line with expectations"
      } else if (wage_bill_gap > 1) {
        "is higher than expected"
      } else {
        "is lower than expected"
      }
    } else {
      "is uncertain compared to expectations"
    }
    
    # Fully Automated Dynamic Text
    automated_text <- paste0(
      first_country, "’s public wage bill has ", trend_decade, " over the past ten years. ",
      "The wage bill has remained at ", ifelse(is.na(wage_bill_gdp_latest), "N/A", round(wage_bill_gdp_latest, 1)), 
      " percent of GDP since 2014. ",
      "The wage bill-to-GDP ratio ", covid_spike_text, " but has since returned to pre-pandemic levels. ",
      "Although the wage bill as a share of public expenditures ", change_text, " ", 
      ifelse(is.na(wage_bill_exp_2010), "N/A", round(wage_bill_exp_2010, 1)), 
      " percent in 2010 to ", ifelse(is.na(wage_bill_exp_latest), "N/A", round(wage_bill_exp_latest, 1)), 
      " percent in ", last_year, ", as shown in Figure 1. ",
      "The public sector wage bill in ", first_country, " is ", comparison_text, ". ",
      "This could be attributed to various factors, including prudent fiscal management and consistent government policies ",
      "aimed at maintaining budgetary discipline; however, this requires a deeper analysis.", 
      "\n\n",
      first_country, "’s wage bill spending ", spending_alignment, 
      " given its level of economic development. Figure 2 illustrates ", relationship_text,
      " between a country’s level of economic development (as measured by GDP per capita) and the size of its public sector ",
      "in the ", first_region, " region. This means that as GDP per capita increases, the public sector wage bill ", spending_trend, 
      ". Given ", first_country, "’s proximity to the line of best fit, it indicates that ",
      first_country, " spends ", spending_alignment, " on its public sector wage bill than would be expected given its level of economic development."
    )
    
    # Add Dynamic Text to Document
    doc <- doc %>% body_add_par(automated_text, style = "Normal")
    
    # Wage Bill as % of GDP
    graph_gdp <- ggplot(selected_data_df, aes(x = year, y = value, color = country_name)) +
      geom_line(size = 1.2) + geom_point(size = 3) +
      labs(title = "Wage Bill as % of GDP Over Time", x = "Year", y = "Wage Bill (% of GDP)") +
      theme_minimal()
    
    doc <- add_plot_to_doc(doc, graph_gdp, "Wage Bill as % of GDP Over Time")
    
    # Wage Bill as % of Public Expenditure
    graph_publicexp <- ggplot(selected_data_df, aes(x = year, y = value, color = country_name)) +
      geom_line(size = 1.2) + geom_point(size = 3) +
      labs(title = "Wage Bill as % of Public Expenditure Over Time", x = "Year", y = "Wage Bill (% of Public Expenditure)") +
      theme_minimal()
    
    doc <- add_plot_to_doc(doc, graph_publicexp, "Wage Bill as % of Public Expenditure Over Time")
    
    # Public Sector Wage Premium
    doc <- doc %>% body_add_par("Public Sector Wage Premium Distribution", style = "heading 1")
    filtered_data <- public_wage_premium %>% filter(country_name %in% input$countries_wage_premium) %>% drop_na(value_percentage)
    
    # Handle case when filtered_data is empty or has only NAs
    if (nrow(filtered_data) == 0 || all(is.na(filtered_data$value_percentage))) {
      warning("No data available for selected countries in public wage premium.")
      avg_wage_premium <- NA
      highest_country <- "N/A"
      lowest_country <- "N/A"
    } else {
      filtered_data$value_percentage <- as.numeric(filtered_data$value_percentage)
      avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
      highest_country <- filtered_data %>% filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>% pull(country_name)
      lowest_country <- filtered_data %>% filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>% pull(country_name)
    }
    
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = country_name)) +
      geom_point(size = 5) +
      labs(title = "Public Sector Wage Premium by Country", x = "Country", y = "Wage Premium (%)", color = "Country") +
      theme_minimal()
    
    interpretation_wage_premium <- paste0(
      "This graph displays the wage premium in the public sector relative to the private sector across selected countries. ",
      "In ", first_country, ", the public sector wage premium is approximately ", ifelse(is.na(avg_wage_premium), "N/A", avg_wage_premium), "%. ",
      highest_country, " has the highest wage premium among the selected countries, while ", lowest_country, 
      " has the lowest."
    )
    
    doc <- add_plot_to_doc(doc, ggplot_obj, "Public Sector Wage Premium Distribution", interpretation_wage_premium)
    
    # Public Sector Employment
    doc <- doc %>% body_add_par("Public Sector Employment", style = "heading 1")
    
    # First Graph - Public Sector Employment (Last Year Available)
    first_graph <- ggplot(public_sector_emp_temp_last %>% filter(country_name %in% input$countries_first), 
                          aes(x = country_name, y = value, color = indicator_name)) +
      geom_point(size = 4) +
      labs(
        title = "Public Sector Employment (Last Year Available)", 
        x = "Country", 
        y = "Public Sector Employment (% of Total Employment)",  # Clearer label
        color = "Indicator"  # Legend for employment category
      ) +
      theme_minimal()
    
    # Generate dynamic interpretation for the first graph
    avg_employment <- round(mean(public_sector_emp_temp_last$value, na.rm = TRUE), 1)
    highest_country <- public_sector_emp_temp_last %>% 
      filter(value == max(value, na.rm = TRUE)) %>% pull(country_name)
    lowest_country <- public_sector_emp_temp_last %>% 
      filter(value == min(value, na.rm = TRUE)) %>% pull(country_name)
    
    interpretation_employment_last <- paste0(
      "This graph shows the share of total employment in the public sector across selected countries. ",
      "In ", first_country, ", public sector employment accounts for approximately ", avg_employment, 
      "% of total employment. ", highest_country, " has the highest share, while ", lowest_country, 
      " has the lowest. Differences in public sector employment levels may reflect varying government employment policies and labor market structures."
    )
    
    doc <- add_plot_to_doc(doc, first_graph, "Public Sector Employment (Last Year Available)", interpretation_employment_last)
    
    # Second Graph - Public Sector Employment Over Time
    second_graph <- ggplot(public_sector_emp_temp %>% filter(country_name == input$country_second), 
                           aes(x = year, y = value, color = indicator_name)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        title = paste("Public Sector Employment Over Time in", input$country_second), 
        x = "Year", 
        y = "Public Sector Employment (% of Total Employment)",  # Clearer label
        color = "Indicator"  # Legend for employment category
      ) +
      theme_minimal()
    
    # Generate dynamic interpretation for the second graph
    latest_value <- public_sector_emp_temp %>% 
      filter(country_name == input$country_second) %>% 
      filter(year == max(year, na.rm = TRUE)) %>% pull(value)
    
    interpretation_employment_trend <- paste0(
      "This graph presents the trend in public sector employment over time in ", input$country_second, 
      ". As of the latest available data, the public sector employment rate stands at approximately ", 
      ifelse(is.na(latest_value), "N/A", round(latest_value, 1)), "%. The trend over time helps to assess the evolution of government employment relative to overall labor force dynamics."
    )
    
    doc <- add_plot_to_doc(doc, second_graph, paste("Public Sector Employment Over Time in", input$country_second), interpretation_employment_trend)
    
    # Gender Workforce Graphs
    
    doc <- doc %>% body_add_par("Gender Workforce", style = "heading 1")
    
    # First Graph - Female Employment by Sector (Last Year Available)
    first_graph <- ggplot(gender_workforce %>% filter(country_name %in% input$countries_gender), 
                          aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "Females, as a share of private paid employees" = "#B3242B",  # Dark Red
        "Females, as a share of public paid employees" = "#003366"    # Dark Blue
      )) +
      labs(
        title = "Female Employment by Sector (Last Year Available)", 
        x = "Country", 
        y = "Female Employment (% of Total Employment)",  # Clearer label
        fill = "Sector"  # Legend title updated
      ) +
      theme_minimal()
    
    # Generate dynamic interpretation for the first graph
    avg_public_female <- round(mean(gender_workforce %>% 
                                      filter(indicator_name == "Females, as a share of public paid employees") %>% 
                                      pull(value_percentage), na.rm = TRUE), 1)
    
    avg_private_female <- round(mean(gender_workforce %>% 
                                       filter(indicator_name == "Females, as a share of private paid employees") %>% 
                                       pull(value_percentage), na.rm = TRUE), 1)
    
    highest_public_country <- gender_workforce %>%
      filter(indicator_name == "Females, as a share of public paid employees") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    interpretation_gender_workforce <- paste0(
      "This graph compares female employment in the public and private sectors across selected countries. ",
      "On average, female representation in public sector employment is around ", avg_public_female, "%, while in the private sector it is ",
      avg_private_female, "%. ", highest_public_country, " has the highest female participation in the public sector, ",
      "indicating potential differences in gender hiring practices or workforce policies."
    )
    
    # Save and add first graph to document
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    doc <- add_plot_to_doc(doc, first_graph, "Female Employment by Sector (Last Year Available)", interpretation_gender_workforce)
    
    # Second Graph - Female Employment Over Time
    second_graph <- ggplot(gender_workforce %>% filter(country_name == input$country_gender), 
                           aes(x = year, y = value_percentage, color = indicator_name)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = c(
        "Females, as a share of private paid employees" = "#B3242B",  # Dark Red
        "Females, as a share of public paid employees" = "#003366"    # Dark Blue
      )) +
      labs(
        title = paste("Female Employment by Sector Over Time in", input$country_gender), 
        x = "Year", 
        y = "Female Employment (% of Total Employment)",  # Clearer label
        color = "Sector"  # Updated legend title
      ) +
      theme_minimal()
    
    # Generate dynamic interpretation for the second graph
    latest_public_female <- gender_workforce %>%
      filter(country_name == input$country_gender, 
             indicator_name == "Females, as a share of public paid employees") %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      pull(value_percentage)
    
    latest_private_female <- gender_workforce %>%
      filter(country_name == input$country_gender, 
             indicator_name == "Females, as a share of private paid employees") %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      pull(value_percentage)
    
    interpretation_gender_trend <- paste0(
      "This graph shows the trend in female employment in the public and private sectors over time in ", input$country_gender, 
      ". As of the most recent year, the share of women in public sector employment is ", 
      ifelse(is.na(latest_public_female), "N/A", paste0(latest_public_female, "%")), 
      ", compared to ", 
      ifelse(is.na(latest_private_female), "N/A", paste0(latest_private_female, "%")), 
      " in the private sector. This trend can indicate gender differences in hiring, job stability, and sectoral preferences."
    )
    
    # Save and add second graph to document
    img_path2 <- tempfile(fileext = ".png")
    ggsave(img_path2, plot = second_graph, width = 8, height = 6)
    doc <- add_plot_to_doc(doc, second_graph, paste("Female Employment by Sector Over Time in", input$country_gender), interpretation_gender_trend)
    
    # ---Wage Premium Graphs ️--
    
    doc <- doc %>% body_add_par("Gender Wage Premium Across Industries", style = "heading 1")
    
    # Filter data for selected countries and relevant industries
    filtered_data <- gender_wage_premiumpublic %>%
      filter(country_name %in% input$selected_countries, 
             indicator_name %in% c("Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)", 
                                   "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)", 
                                   "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)", 
                                   "Other"))
    
    # Rename industry labels for clarity
    filtered_data$indicator_label <- recode(filtered_data$indicator_name,
                                            "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" = "Core Public Administration",
                                            "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)" = "Education",
                                            "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)" = "Health",
                                            "Other" = "Other Public Sector Jobs"
    )
    
    # Create the bar chart
    gender_wage_plot <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Blues") +  # Using a blue color palette for readability
      labs(
        title = "Gender Wage Premium in Public Sector by Industry",
        x = "Country", 
        y = "Wage Premium (%)",  # Clearer Y-axis label
        fill = "Industry"  # Updated legend title
      ) +
      theme_minimal()
    
    # Generate dynamic interpretation
    
    # Filter public sector wage premium data
    filtered_data <- public_wage_premium %>% 
      filter(country_name %in% input$countries_wage_premium) %>% 
      drop_na(value_percentage)
    
    # Check if the dataset is empty
    if (nrow(filtered_data) == 0) {
      warning("No data available for selected countries in public wage premium.")
      
      # Assign default values to avoid errors
      avg_wage_premium <- NA
      highest_country <- "N/A"
      lowest_country <- "N/A"
    } else {
      # Convert to numeric safely
      filtered_data$value_percentage <- as.numeric(filtered_data$value_percentage)
      
      # Calculate key statistics safely
      avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
      
      highest_country <- filtered_data %>%
        filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
        pull(country_name)
      
      lowest_country <- filtered_data %>%
        filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
        pull(country_name)
    }
    
    interpretation_gender_wage_premium <- paste0(
      "This graph displays the gender wage premium across different public sector industries, comparing wage differences between men and women. ",
      "On average, the gender wage premium is approximately ", avg_wage_premium, "% across industries in selected countries. ",
      highest_wage_gap$country_name, " has the highest wage premium in the ", highest_wage_gap$indicator_label, 
      " sector at ", round(highest_wage_gap$value_percentage, 1), "%, whereas ", lowest_wage_gap$country_name, 
      " has the lowest premium in ", lowest_wage_gap$indicator_label, " at ", round(lowest_wage_gap$value_percentage, 1), "%.",
      " This suggests that wage disparities between men and women vary significantly across industries."
    )
    
    # Save and add the plot to the document
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = gender_wage_plot, width = 8, height = 6, dpi = 300)
    doc <- add_plot_to_doc(doc, gender_wage_plot, "Gender Wage Premium Across Industries", interpretation_gender_wage_premium)
    
    # ---Public Sector Education Graphs ️--
    
    doc <- doc %>% body_add_par("Public Sector by Education Levels", style = "heading 1")
    
    # Filter Data
    filtered_data <- public_wage_premium_educ %>%
      filter(country_name == input$selected_country) %>%
      drop_na(value_percentage)
    
    # Handle missing data case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected country.", style = "Normal")
      print(doc, target = file)
      return()
    }
    
    # Create the Bar Chart
    ggplot_obj <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(
        "No Education" = "#003366",       # Dark Blue
        "Primary Education" = "#B3242B",  # Dark Red
        "Secondary Education" = "#3B3B3B",# Dark Gray/Black
        "Tertiary Education" = "#006400"  # Dark Green
      )) +
      labs(
        title = "Public Sector Wage Premium by Education Level",
        x = "Education Level",  # Clearer label
        y = "Wage Premium (%)",  # Clearer label
        fill = "Education Level"  # Updated legend title
      ) +
      theme_minimal()
    
    # Generate dynamic interpretation
    avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
    
    highest_premium <- filtered_data %>% 
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      select(indicator_name, value_percentage)
    
    lowest_premium <- filtered_data %>% 
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      select(indicator_name, value_percentage)
    
    interpretation_education_wage_premium <- paste0(
      "This graph illustrates the public sector wage premium by education level in ", input$selected_country, 
      ". On average, public employees earn a wage premium of approximately ", avg_wage_premium, "% over their private sector counterparts. ",
      highest_premium$indicator_name, " has the highest wage premium at ", round(highest_premium$value_percentage, 1), 
      "%, while ", lowest_premium$indicator_name, " has the lowest premium at ", round(lowest_premium$value_percentage, 1), "%.",
      " The wage premium tends to increase with higher education levels, suggesting that better-educated public sector workers may receive stronger compensation benefits."
    )
    
    # Save and add the plot to the document
    img_path_educ <- tempfile(fileext = ".png")
    ggsave(img_path_educ, plot = ggplot_obj, width = 8, height = 6)
    
    doc <- add_plot_to_doc(doc, ggplot_obj, "Public Sector by Education Level", interpretation_education_wage_premium)
    
    
    # --- 4️⃣ Education in Public vs. Private Sectors ---
    doc <- doc %>% body_add_par("Education in Public vs. Private Sectors", style = "heading 1")
    
    # Filter Data
    filtered_education <- tertiary_education %>% filter(country_name %in% input$selected_countries)
    
    # Create the Bar Chart
    education_graph <- ggplot(filtered_education, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Tertiary Education by Sector and Country",
        x = "Country", 
        y = "Share of Workforce with Tertiary Education (%)",  # Clearer label
        fill = "Sector"  # Updated legend title
      ) +
      theme_minimal()
    
    # Generate dynamic interpretation
    avg_tertiary_public <- round(mean(filtered_education %>% 
                                        filter(indicator_name == "Public Sector") %>% 
                                        pull(value_percentage), na.rm = TRUE), 1)
    
    avg_tertiary_private <- round(mean(filtered_education %>% 
                                         filter(indicator_name == "Private Sector") %>% 
                                         pull(value_percentage), na.rm = TRUE), 1)
    
    highest_tertiary_public <- filtered_education %>%
      filter(indicator_name == "Public Sector") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    interpretation_education_sector <- paste0(
      "This graph compares the share of workers with tertiary education across the public and private sectors in different countries. ",
      "On average, ", avg_tertiary_public, "% of public sector employees hold a tertiary degree, compared to ", 
      avg_tertiary_private, "% in the private sector. ", highest_tertiary_public, 
      " has the highest share of tertiary-educated employees in the public sector. These trends suggest that education requirements and hiring preferences may differ between sectors."
    )
    
    # Save and add the plot to the document
    img_path_educ <- tempfile(fileext = ".png")
    ggsave(img_path_educ, plot = education_graph, width = 8, height = 6)
    
    doc <- add_plot_to_doc(doc, education_graph, "Tertiary Education by Sector", interpretation_education_sector)
    
    # --- 5️⃣ Wage Premium Analysis ---
    doc <- doc %>% body_add_par("Wage Premium Trends", style = "heading 1")
    
    # Filter Data
    
    filtered_data <- public_wage_premium %>%
      filter(country_name %in% input$countries_wage_premium & !is.na(value_percentage))
    
    
    # Create the Scatter Plot
    wage_premium_graph <- ggplot(filtered_wage_premium, aes(x = country_name, y = value_percentage, color = country_name)) +
      geom_point(size = 4) +
      labs(
        title = "Public Sector Wage Premium by Country",
        x = "Country", 
        y = "Wage Premium (%)",  # Clearer label
        color = "Country"  # Legend title for clarity
      ) +
      theme_minimal()
    
    # Generate dynamic interpretation
    avg_wage_premium <- round(mean(filtered_wage_premium$value_percentage, na.rm = TRUE), 1)
    
    highest_wage_premium <- filtered_wage_premium %>% 
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>% 
      select(country_name, value_percentage)
    
    lowest_wage_premium <- filtered_wage_premium %>% 
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>% 
      select(country_name, value_percentage)
    
    interpretation_wage_premium_trends <- paste0(
      "This graph presents the public sector wage premium by country, indicating how much more (or less) public employees earn relative to private sector workers. ",
      "On average, the wage premium is ", avg_wage_premium, "% in the selected countries. ", highest_wage_premium$country_name, 
      " has the highest public sector wage premium at ", round(highest_wage_premium$value_percentage, 1), "%, while ", 
      lowest_wage_premium$country_name, " has the lowest at ", round(lowest_wage_premium$value_percentage, 1), "%.",
      " This variation may reflect different government compensation structures, labor market conditions, or collective bargaining agreements."
    )
    
    # Save and add the plot to the document
    img_path_wage <- tempfile(fileext = ".png")
    ggsave(img_path_wage, plot = wage_premium_graph, width = 8, height = 6)
    
    doc <- add_plot_to_doc(doc, wage_premium_graph, "Public Sector Wage Premium by Country", interpretation_wage_premium_trends)
    
    # --- 6️⃣ Gender Wage Premium ---
    doc <- doc %>% body_add_par("Gender Wage Premium Analysis", style = "heading 1")
    
    # Filter data for selected countries
    filtered_gender_wage <- gender_wage_premium_last %>% filter(country_name %in% input$countries_first)
    
    # Create the scatter plot
    gender_wage_graph <- ggplot(filtered_gender_wage, aes(x = country_name, y = value, color = indicator_label)) +
      geom_point(size = 4) +
      labs(
        title = "Public Sector Wage Premium by Gender (Last Year Available)", 
        x = "Country", 
        y = "Wage Premium (%)",  # Clearer label
        color = "Gender"  # Updated legend title
      ) +
      theme_minimal()
    
    # Generate dynamic interpretation
    avg_gender_wage_premium <- round(mean(filtered_gender_wage$value, na.rm = TRUE), 1)
    
    highest_gender_gap <- filtered_gender_wage %>% 
      filter(value == max(value, na.rm = TRUE)) %>%
      select(country_name, indicator_label, value)
    
    lowest_gender_gap <- filtered_gender_wage %>% 
      filter(value == min(value, na.rm = TRUE)) %>%
      select(country_name, indicator_label, value)
    
    interpretation_gender_wage <- paste0(
      "This graph displays the gender wage premium in the public sector, highlighting wage differences between male and female employees. ",
      "On average, the gender wage premium is approximately ", avg_gender_wage_premium, "% in the selected countries. ",
      highest_gender_gap$country_name, " has the highest gender wage gap in favor of ", highest_gender_gap$indicator_label,
      " at ", round(highest_gender_gap$value, 1), "%, whereas ", lowest_gender_gap$country_name, 
      " shows the lowest wage premium in ", lowest_gender_gap$indicator_label, " at ", round(lowest_gender_gap$value, 1), "%.",
      " These disparities may be influenced by differences in seniority, sector-specific pay scales, or gender representation in high-paying roles."
    )
    
    # Save and add the plot to the document
    img_path_gender_wage <- tempfile(fileext = ".png")
    ggsave(img_path_gender_wage, plot = gender_wage_graph, width = 8, height = 6)
    
    doc <- add_plot_to_doc(doc, gender_wage_graph, "Public Sector Wage Premium by Gender", interpretation_gender_wage)
    
    
    # ✅ Conclusion
    doc <- doc %>% body_add_par("Conclusion", style = "heading 1")
    doc <- doc %>% body_add_par(
      "This report provides a comprehensive analysis of wage bill trends, gender employment representation, and workforce participation in the public sector. The findings highlight key trends and disparities across different sectors and countries.",
      style = "Normal"
    )
    
    # ✅ Save the Word Document
    print(doc, target = file)
  }
)

   
   