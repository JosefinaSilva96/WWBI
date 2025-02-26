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
    
    # --- Add Both Graphs Based on User Selection ---
    
    # GDP Graph Data
    graph_data_gdp <- wage_bill_gdp %>% filter(country_name %in% input$countries)
    
    # Public Expenditure Graph Data
    graph_data_exp <- wage_bill_publicexp %>% filter(country_name %in% input$countries)
    
    # Create GDP Graph
    graph_gdp <- ggplot(graph_data_gdp, aes(x = year, y = value, color = country_name)) +
      geom_line(size = 1.2) + 
      geom_point(size = 3) +
      labs(
        title = "Wage Bill as % of GDP Over Time",
        x = "Year",
        y = "Wage Bill (% of GDP)", 
        color= "Country"
      ) +
      theme_minimal()
    
    # Create Public Expenditure Graph
    graph_exp <- ggplot(graph_data_exp, aes(x = year, y = value, color = country_name)) +
      geom_line(size = 1.2) + 
      geom_point(size = 3) +
      labs(
        title = "Wage Bill as % of Public Expenditure Over Time",
        x = "Year",
        y = "Wage Bill (% of Public Expenditure)"
      ) +
      theme_minimal()
    
    # Save GDP Graph
    img_path_gdp <- tempfile(fileext = ".png")
    ggsave(img_path_gdp, plot = graph_gdp, width = 8, height = 6, dpi = 300)
    
    # Save Public Expenditure Graph
    img_path_exp <- tempfile(fileext = ".png")
    ggsave(img_path_exp, plot = graph_exp, width = 8, height = 6, dpi = 300)
    
    # Add GDP Graph to the Document
    # Compute correlation between GDP per capita and wage bill spending in the selected region
    regional_trend <- wage_bill_gdp %>%
      filter(wb_region == first_region) %>%
      summarise(avg_value = mean(value, na.rm = TRUE)) %>%
      pull(avg_value)
    
    # Determine relationship type
    relationship_text <- if (!is.na(regional_trend)) {
      if (regional_trend < -0.2) {
        "a negative relationship"  # Public wage bill decreases as GDP per capita increases
      } else if (regional_trend > 0.2) {
        "a positive relationship"  # Public wage bill increases as GDP per capita increases
      } else {
        "no strong relationship"  # Weak correlation
      }
    } else {
      "an uncertain relationship"
    }
    
    # Get actual wage bill for the selected country
    first_country_wage_bill <- wage_bill_gdp %>%
      filter(country_name == first_country) %>%
      summarise(latest_wage_bill = max(value, na.rm = TRUE)) %>%
      pull(latest_wage_bill)
    
    # Compute regional average for comparison
    regional_avg_wage_bill <- wage_bill_gdp %>%
      filter(wb_region == first_region) %>%
      summarise(mean_wage = mean(value, na.rm = TRUE)) %>%
      pull(mean_wage)
    
    # Compare country to regional peers
    spending_pattern <- if (!is.na(first_country_wage_bill) && !is.na(regional_avg_wage_bill)) {
      if (first_country_wage_bill > regional_avg_wage_bill * 1.2) {
        "more than expected"
      } else if (first_country_wage_bill < regional_avg_wage_bill * 0.8) {
        "less than expected"
      } else {
        "roughly as expected"
      }
    } else {
      "uncertain compared to regional peers"
    }
    
    # Construct dynamic interpretation text
    gdp_interpretation_text <- paste0(
      "Figure 1.1 illustrates ", relationship_text, " between a country’s level of economic development ",
      "(as measured by GDP per capita) and the size of its public sector in the ", first_region, " region. ",
      "This means that as GDP per capita increases, the public sector wage bill tends to ",
      ifelse(regional_trend < -0.2, "decrease.", ifelse(regional_trend > 0.2, "increase.", "remain relatively stable.")), 
      " Given ", first_country, "’s position in this trend, it indicates that ", first_country, 
      " spends ", spending_pattern, " on its public sector wage bill compared to other countries in ", first_region, "."
    )
    
    doc <- doc %>% 
      body_add_par("Wage Bill as % of GDP Over Time", style = "heading 2") %>% 
      body_add_img(src = img_path_gdp, width = 6, height = 4) %>% 
      body_add_par(gdp_interpretation_text, style = "Normal")
    
    # Add Public Expenditure Graph to the Document
    # Extract wage bill as % of public expenditure for the selected country
    wage_bill_exp_trend <- wage_bill_publicexp %>%
      filter(country_name == first_country)
    
    # Get the wage bill values for 2010 and latest available year
    wage_bill_exp_2010 <- wage_bill_exp_trend %>%
      filter(year == 2010) %>%
      summarise(value_2010 = max(value, na.rm = TRUE)) %>%
      pull(value_2010)
    
    latest_year <- max(wage_bill_exp_trend$year, na.rm = TRUE)
    wage_bill_exp_latest <- wage_bill_exp_trend %>%
      filter(year == latest_year) %>%
      summarise(value_latest = max(value, na.rm = TRUE)) %>%
      pull(value_latest)
    
    # Compute volatility (standard deviation) of the selected country
    country_volatility <- wage_bill_exp_trend %>%
      summarise(volatility = sd(value, na.rm = TRUE)) %>%
      pull(volatility)
    
    # Compute average volatility for the region
    regional_volatility <- wage_bill_publicexp %>%
      filter(wb_region == first_region) %>%
      group_by(country_name) %>%
      summarise(volatility = sd(value, na.rm = TRUE)) %>%
      ungroup() %>%
      summarise(mean_volatility = mean(volatility, na.rm = TRUE)) %>%
      pull(mean_volatility)
    
    # Compare country to regional volatility
    stability_text <- if (!is.na(country_volatility) && !is.na(regional_volatility)) {
      if (country_volatility < regional_volatility * 0.8) {
        "more stable"
      } else if (country_volatility > regional_volatility * 1.2) {
        "more volatile"
      } else {
        "similar in stability"
      }
    } else {
      "uncertain compared to regional peers"
    }
    
    # Construct dynamic interpretation text
    public_exp_interpretation_text <- paste0(
      "The wage bill as a share of public expenditures has ",
      ifelse(!is.na(wage_bill_exp_2010) & !is.na(wage_bill_exp_latest),
             paste0("changed from ", round(wage_bill_exp_2010, 1), " percent in 2010 to ",
                    round(wage_bill_exp_latest, 1), " percent in ", latest_year, ", as shown in Figure 1."),
             "varied over time, as shown in Figure 1."),
      " The public sector wage bill in ", first_country, " has exhibited ", stability_text, 
      " compared to the volatility observed in regional peers."
    )
    
    doc <- doc %>% 
      body_add_par("Wage Bill as % of Public Expenditure Over Time", style = "heading 2") %>% 
      body_add_img(src = img_path_exp, width = 6, height = 4) %>% 
      body_add_par(public_exp_interpretation_text, style = "Normal")
    
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
    
    # Get the first selected country
    selected_country <- if (!is.null(input$countries) & length(input$countries) > 0) {
      input$countries[1]
    } else {
      "Unknown Country"
    }
    
    # Extract the region for the selected country
    region <- filtered_data_df %>% filter(country_name == selected_country) %>% pull(wb_region) %>% unique()
    region <- ifelse(length(region) > 0, region, "the region")
    
    # Calculate regional averages
    avg_gdp <- mean(filtered_data_df$log_gdp, na.rm = TRUE)
    avg_wage_bill <- mean(filtered_data_df$indicator_value, na.rm = TRUE)
    
    # Get selected country's values
    country_gdp <- filtered_data_df %>% filter(country_name == selected_country) %>% pull(log_gdp) %>% unique()
    country_wage_bill <- filtered_data_df %>% filter(country_name == selected_country) %>% pull(indicator_value) %>% unique()
    
    # Generate automated text based on values
    analysis_text <- paste0(
      "This graph compares the public sector wage bill across selected countries in relation to economic development, measured as GDP per capita. ",
      "On average, the public sector wage bill in ", region, " is around ", round(avg_wage_bill, 1), "% of total government expenditure, ",
      "while the average GDP per capita (log scale) is ", round(avg_gdp, 1), "."
    )
    
    country_text <- paste0(
      selected_country, " has a public sector wage bill of ", round(country_wage_bill, 1), 
      "% and a GDP per capita (log scale) of ", round(country_gdp, 1), 
      ", indicating how it compares to other countries in the region."
    )
    
    # Add text to document
    doc <- doc %>%
      body_add_par("Wage bill (% of public expenditures) and GDP per capita ", style = "heading 2") %>% 
      body_add_par(analysis_text, style = "Normal") %>%
      body_add_par(country_text, style = "Normal")
    
    
    
    # Create and save plot with renamed axis label
    plot <- ggplot(filtered_data_df, aes(x = log_gdp, y = indicator_value, color = country_name)) +
      geom_point(size = 3) +
      labs(
        title = "Wage Bill vs. Log(GDP per Capita)",
        x = "Log(GDP per Capita, 2015)", 
        y = "Wage Bill",
        color = "Country"  # Rename legend label
      ) +
      theme_minimal()
    
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = plot, width = 8, height = 6, dpi = 300)
    
    # Add image and note
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>%
      body_add_par("Note: This graph compares public sector wage bill levels with GDP per capita (log scale) for selected countries.", 
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
    first_graph_data <- public_sector_workforce %>% filter(country_name %in% input$countries_workforce)
    
    if (nrow(first_graph_data) == 0) {
      doc <- doc %>% body_add_par("No data available for Public Workforce Distribution by Country.", style = "Normal")
    } else {
      first_graph_ggplot <- ggplot(first_graph_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c("Public Administration" = "#568340", "Education" = "#B3242B", 
                                     "Health" = "#003366", "Other" = "#A9A9A9")) +
        labs(title = "Public Workforce Distribution by Country", x = "Country", y = "Workforce Distribution (%)", fill = "Sector") +
        theme_minimal()
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = first_graph_ggplot, width = 6, height = 4)
      
      # Extract the first selected country
      first_country <- if (!is.null(input$countries_workforce) && length(input$countries_workforce) > 0) {
        input$countries_workforce[1]  # Use the first selected country
      } else {
        "Unknown Country"  # Default fallback
      }
      
      # Extract region for the selected country
      first_region <- public_sector_workforce %>%
        filter(country_name == first_country) %>%
        pull(wb_region) %>%
        unique()
      
      # ✅ FIX: Ensure single value before using ifelse()
      first_region <- if (length(first_region) > 0) first_region[1] else "the selected region"
      
      
      # Extract workforce distribution for the selected country
      sector_distribution <- public_sector_workforce %>%
        filter(country_name == first_country) %>%
        group_by(indicator_name) %>%
        summarise(share = mean(value_percentage, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = indicator_name, values_from = share, values_fill = list(share = 0))
      
      # Regional workforce distribution
      regional_distribution <- public_sector_workforce %>%
        filter(wb_region == first_region) %>%
        group_by(indicator_name) %>%
        summarise(region_avg = mean(value_percentage, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = indicator_name, values_from = region_avg, values_fill = list(region_avg = 0))
      
      # Extract sector shares safely
      public_admin_share <- sector_distribution$`Public Administration` %||% 0
      education_share <- sector_distribution$Education %||% 0
      health_share <- sector_distribution$Health %||% 0
      
      regional_admin_share <- regional_distribution$`Public Administration` %||% 0
      regional_education_share <- regional_distribution$Education %||% 0
      regional_health_share <- regional_distribution$Health %||% 0
      
      # If all values are zero (no data available), return a message
      if (public_admin_share == 0 && education_share == 0 && health_share == 0) {
        doc <- doc %>% body_add_par(
          paste0("No public sector employment data available for ", first_country, "."),
          style = "Normal"
        )
        return(doc)
      }
      
      # Identify the sector with the highest employment
      sector_shares <- c(public_admin_share, education_share, health_share)
      sector_names <- c("Public Administration", "Education", "Health")
      largest_sector <- sector_names[which.max(sector_shares)]
      largest_sector_share <- max(sector_shares)
      
      # Construct text comparison with regional averages
      education_emphasis <- if (education_share > regional_education_share) {
        paste0("This figure is notably higher than the regional average of ", round(regional_education_share, 1), 
               " percent for ", first_region, " countries.")
      } else {
        paste0("This figure is comparable to the regional average of ", round(regional_education_share, 1), 
               " percent for ", first_region, " countries.")
      }
      
      health_emphasis <- if (health_share > regional_health_share) {
        paste0("The health sector, while representing a smaller segment of the public sector workforce at ", 
               round(health_share, 1), " percent, still surpasses the regional average of ", 
               round(regional_health_share, 1), " percent.")
      } else {
        paste0("The health sector accounts for ", round(health_share, 1), 
               " percent of the public sector workforce, aligning closely with the regional average of ", 
               round(regional_health_share, 1), " percent.")
      }
      
      # Construct workforce text
      sector_interpretation_text <- paste0(
        largest_sector, " holds the highest share of employment in the public sector in ", first_country,
        ", with ", round(largest_sector_share, 1), " percent of paid public sector workers employed in this area. ",
        "Following ", largest_sector, ", the education sector also represents a substantial portion of the public sector workforce, accounting for ",
        round(education_share, 1), " percent. ", education_emphasis, " ", health_emphasis
      )
      
      # FIX: Convert to single string
      sector_interpretation_text <- paste(sector_interpretation_text, collapse = " ")
      
      doc <- doc %>% 
        body_add_par("Public Workforce Distribution by Country", style = "heading 2") %>% 
        body_add_img(src = img_path1, width = 6, height = 4) %>% 
        body_add_par(sector_interpretation_text, style = "Normal")
    }
    
    # Second Graph: Sectoral Distribution of Public Sector Workforce
    second_graph_data <- public_sector_workforce %>% filter(country_name %in% input$countries_workforce)
    
    if (nrow(second_graph_data) < 2) {
      doc <- doc %>% body_add_par("Not enough data available for the selected countries to create the graph.", style = "Normal")
    } else {
      first_year <- min(second_graph_data$year, na.rm = TRUE)
      last_year <- max(second_graph_data$year, na.rm = TRUE)
      
      second_graph_filtered <- second_graph_data %>% 
        filter(year %in% c(first_year, last_year)) %>% 
        group_by(year, indicator_name) %>% 
        summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")
      
      second_graph_ggplot <- ggplot(second_graph_filtered, aes(x = value_percentage, y = factor(year, levels = c(last_year, first_year)), fill = indicator_name)) +
        geom_bar(stat = "identity", position = "stack", orientation = "horizontal") +
        scale_fill_manual(values = c("Public Administration" = "#568340", "Education" = "#B3242B", 
                                     "Health" = "#003366", "Other" = "#A9A9A9")) +
        labs(title = "Sectoral Distribution of Public Sector Workforce", x = "Percentage (%)", y = "Year", fill = "Sector") +
        theme_minimal()
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph_ggplot, width = 6, height = 4)
      
      doc <- doc %>% 
        body_add_par("Sectoral Distribution of Public Sector Workforce", style = "heading 2") %>% 
        body_add_img(src = img_path2, width = 6, height = 4) %>% 
        body_add_par("This graph shows the sectoral distribution of public sector workforce for the selected countries.", style = "Normal")
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
    
    # Extract first selected country
    first_country <- if (!is.null(input$selected_countries) && length(input$selected_countries) > 0) {
      input$selected_countries[1]  # Use the first selected country
    } else {
      "Unknown Country"
    }
    
    # Extract region for the first selected country
    first_region <- tertiary_education %>%
      filter(country_name == first_country) %>%
      pull(wb_region) %>%
      unique()
    
    first_region <- if (length(first_region) > 0) first_region[1] else "the selected region"
    
    # Calculate country-level tertiary education averages
    country_tertiary <- filtered_data %>%
      filter(country_name == first_country) %>%
      group_by(indicator_name) %>%
      summarise(avg_tertiary = mean(value_percentage, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = indicator_name, values_from = avg_tertiary, values_fill = list(avg_tertiary = 0))
    
    # Calculate regional tertiary education averages for the detected region
    regional_tertiary <- tertiary_education %>%
      filter(wb_region == first_region) %>%
      group_by(indicator_name) %>%
      summarise(region_avg = mean(value_percentage, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = indicator_name, values_from = region_avg, values_fill = list(region_avg = 0))
    
    # Extract values safely
    public_tertiary_country <- coalesce(country_tertiary$`Individuals with tertiary education as a share of public paid employees`, 0)
    private_tertiary_country <- coalesce(country_tertiary$`Individuals with tertiary education as a share of private paid employees`, 0)
    
    public_tertiary_region <- coalesce(regional_tertiary$`Individuals with tertiary education as a share of public paid employees`, 0)
    private_tertiary_region <- coalesce(regional_tertiary$`Individuals with tertiary education as a share of private paid employees`, 0)
    
    # Generate dynamic text interpretation
    public_comparison <- if (public_tertiary_country > public_tertiary_region) {
      paste0("In ", first_country, ", ", round(public_tertiary_country, 1), "% of public sector employees have a tertiary education, ",
             "which is higher than the ", first_region, " average of ", round(public_tertiary_region, 1), "%.")
    } else {
      paste0("In ", first_country, ", ", round(public_tertiary_country, 1), "% of public sector employees have a tertiary education, ",
             "which is below the ", first_region, " average of ", round(public_tertiary_region, 1), "%.")
    }
    
    private_comparison <- if (private_tertiary_country > private_tertiary_region) {
      paste0("Similarly, in the private sector, tertiary education attainment is ", round(private_tertiary_country, 1), "%, ",
             "which is higher than the ", first_region, " average of ", round(private_tertiary_region, 1), "%.")
    } else {
      paste0("Meanwhile, tertiary education attainment in the private sector is ", round(private_tertiary_country, 1), "%, ",
             "which is below the ", first_region, " average of ", round(private_tertiary_region, 1), "%.")
    }
    
    interpretation_text <- paste(public_comparison, private_comparison)
    
    # Generate Tertiary Education ggplot
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Individuals with tertiary education as a share of private paid employees" = "#B3242B", 
                                   "Individuals with tertiary education as a share of public paid employees" = "#003366")) +
      labs(title = "Tertiary Education by Sector and Country", x = "Country", y = "Tertiary Education (%)", fill = "Sector") +
      theme_minimal()
    
    # Ensure the dataset is not empty before generating graph
    if (nrow(filtered_data) > 0) {
      img_path <- tempfile(fileext = ".png")
      ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
      
      doc <- doc %>% 
        body_add_img(src = img_path, width = 6, height = 4)
    }
    
    doc <- doc %>% body_add_par(interpretation_text, style = "Normal")
    
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
    
    # ✅ Ensure highest and lowest country selection is single-value
    highest_country <- filtered_data %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_country <- filtered_data %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    # ✅ Fix wage premium phrasing to avoid vectorized issues
    wage_premium_phrasing <- if (avg_wage_premium > 0) {
      "higher"
    } else if (avg_wage_premium < 0) {
      "lower"
    } else {
      "equal to"
    }
    
    # ✅ Fix mutate() potential failure if no country is selected
    first_selected_country <- if (!is.null(input$countries_wage_premium) && length(input$countries_wage_premium) > 0) {
      input$countries_wage_premium[1]
    } else {
      "Unknown"
    }
    
    filtered_data <- filtered_data %>%
      mutate(color = ifelse(country_name == first_selected_country, "#B3242B", "#003366"))
    
    # Create the Dot Plot with Different Colors
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = color)) +
      geom_point(size = 5) +
      scale_color_identity() +  
      labs(title = "Public Sector Wage Premium by Country", x = "Country", y = "Wage Premium (%)") +
      theme_minimal()
    
    # ✅ Ensure dataset exists before generating graph
    if (nrow(filtered_data) > 0) {
      img_path <- tempfile(fileext = ".png")
      ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
      
      doc <- doc %>% 
        body_add_img(src = img_path, width = 6, height = 4)
    }
    
    # ✅ Add dynamic interpretation text
    interpretation_text <- paste0(
      "Across the selected countries, the **average public sector wage premium** is ", avg_wage_premium, "%, ",
      "indicating that public sector wages are generally **", wage_premium_phrasing, "** than private sector wages.\n\n",
      "The **highest wage premium** is observed in **", highest_country, "**, suggesting that public sector employees in this country earn significantly more relative to their private sector counterparts. ",
      "On the other hand, **", lowest_country, "** has the **lowest public sector wage premium**, indicating that public sector wages in this country are closer to or even below those in the private sector."
    )
    
    # Add Interpretation Text to Word Document
    doc <- doc %>% body_add_par(interpretation_text, style = "Normal")
    
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
    
    # ✅ Fix NULL Check for First Graph Condition
    if ("firstGraphgender" %in% input$graphs_to_download && !is.null(input$countries_first) && length(input$countries_first) > 0) {
      data_to_plot <- gender_wage_premium_last %>% filter(country_name %in% input$countries_first)
      
      if (nrow(data_to_plot) > 0) {
        data_to_plot_long <- data_to_plot %>% 
          select(country_name, indicator_label, year, value) %>% 
          mutate(indicator_label = factor(indicator_label))
        
        first_graph_wage_premium_gender <- ggplot(data_to_plot_long, aes(x = country_name, y = value, color = indicator_label)) +
          geom_point(size = 3) +
          labs(title = "Wage Premium Gender (Multi-Country)", x = "Country", y = "Value", color = "Indicator") +
          theme_minimal()
        
        # Save the plot as an image
        img_path1 <- tempfile(fileext = ".png")
        ggsave(img_path1, plot = first_graph_wage_premium_gender, width = 6, height = 4)
        
        # Add Image and Description
        doc <- doc %>% 
          body_add_par("First Graph: Wage Premium Gender (Multi-Country)", style = "heading 2") %>% 
          body_add_img(src = img_path1, width = 6, height = 4) %>% 
          body_add_par("This graph shows the wage premium by gender across multiple countries.", style = "Normal")
      } else {
        doc <- doc %>% body_add_par("No data available for the first graph (multi-country wage premium gender).", style = "Normal")
      }
    }
    
    # ✅ Fix NULL Check for Second Graph Condition
    if ("secondGraphgender" %in% input$graphs_to_download && !is.null(input$country_second) && input$country_second != "") {
      data_to_plot <- gender_wage_premium %>% filter(country_name == input$country_second)
      
      if (nrow(data_to_plot) > 0) {
        data_to_plot_long <- data_to_plot %>% 
          select(year, indicator_name, value) %>% 
          mutate(indicator_name = factor(indicator_name))
        
        second_graph_wage_premium_gender <- ggplot(data_to_plot_long, aes(x = year, y = value, color = indicator_name)) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          labs(title = paste("Public Sector Wage Premium by Gender in", input$country_second, "Over Time"), 
               x = "Year", y = "Employment Value", color = "Indicator") +
          theme_minimal()
        
        # Save the plot as an image
        img_path2 <- tempfile(fileext = ".png")
        ggsave(img_path2, plot = second_graph_wage_premium_gender, width = 6, height = 4)
        
        # Add Image and Description
        doc <- doc %>% 
          body_add_par("Second Graph: Public Sector Employment (Single Country)", style = "heading 2") %>% 
          body_add_img(src = img_path2, width = 6, height = 4) %>% 
          body_add_par("This graph shows the wage premium by gender trends over time for the selected country.", style = "Normal")
      } else {
        doc <- doc %>% body_add_par("No data available for the second graph (single country wage premium gender).", style = "Normal")
      }
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
    
    # ✅ Fix NULL Check for Selected Country
    if (is.null(input$selected_country) || input$selected_country == "") {
      doc <- doc %>% body_add_par("No country selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      paste0("This section presents an analysis of public sector wage premiums based on different education levels for ", 
             input$selected_country, ". The comparison is made against private sector formal workers."), 
      style = "Normal"
    )
    
    # Filter Data for the Selected Country
    filtered_data <- public_wage_premium_educ %>%
      filter(country_name == input$selected_country) %>%
      drop_na(value_percentage)
    
    # Handle Empty Dataset Case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected country.", style = "Normal")
      return(doc)
    }
    
    # Identify the Last Available Year for the Selected Country
    last_year <- max(filtered_data$year, na.rm = TRUE)
    
    # Filter Data for the Last Available Year
    filtered_data <- filtered_data %>% filter(year == last_year)
    
    # Convert `value_percentage` to Numeric
    filtered_data$value_percentage <- as.numeric(filtered_data$value_percentage)
    
    # ✅ Ensure Highest and Lowest Education Selection is Single-Value
    highest_education <- filtered_data %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(indicator_name) %>%
      first()
    
    lowest_education <- filtered_data %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(indicator_name) %>%
      first()
    
    highest_value <- round(max(filtered_data$value_percentage, na.rm = TRUE), 1)
    lowest_value <- round(min(filtered_data$value_percentage, na.rm = TRUE), 1)
    
    # Determine if the Public Sector Wage Premium is Higher or Lower Overall
    avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
    
    wage_premium_phrasing <- if (avg_wage_premium > 0) {
      "higher"
    } else if (avg_wage_premium < 0) {
      "lower"
    } else {
      "equal to"
    }
    
    # Construct Automated Interpretation Text with Last Available Year
    interpretation_text <- paste0(
      "In ", input$selected_country, ", for the **last available year (", last_year, ")**, the **public sector wage premium varies significantly** across education levels. ",
      "On average, public sector workers earn **", wage_premium_phrasing, "** wages than their private sector counterparts across all education groups.\n\n",
      "The **highest public sector wage premium** is observed among workers with **", highest_education, "**, where public sector employees earn **", highest_value, "%** more than private sector workers. ",
      "Conversely, the **lowest wage premium** is found among those with **", lowest_education, "**, where the difference is **", lowest_value, "%**.\n\n",
      "These differences highlight how wage structures in the public sector reward certain education levels differently compared to private sector employment."
    )
    
    # Generate Wage Premium by Education Level ggplot with "Indicator" as the Legend Label
    ggplot_obj <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(
        "No Education" = "#003366",
        "Primary Education" = "#B3242B",
        "Secondary Education" = "#3B3B3B",
        "Tertiary Education" = "#006400"
      )) +
      labs(
        title = paste0("Public Sector Wage Premium by Education Level (", last_year, ")"),
        x = "Education Level",
        y = "Wage Premium (%)",
        fill = "Indicator"
      ) +
      theme_minimal()
    
    # ✅ Ensure Dataset Exists Before Generating Graph
    if (nrow(filtered_data) > 0) {
      img_path <- tempfile(fileext = ".png")
      ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
      
      doc <- doc %>% 
        body_add_img(src = img_path, width = 6, height = 4)
    } else {
      doc <- doc %>% body_add_par("No data available to generate the graph.", style = "Normal")
    }
    
    # Add Interpretation Text to Word Document
    doc <- doc %>% body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  

  #Public Sector Employment Graphs 
  
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
    
    # Filter data for the last available year
    last_year <- max(public_sector_emp_temp_last$year, na.rm = TRUE)
    first_graph_data <- public_sector_emp_temp_last %>% 
      filter(country_name %in% input$countries_first, year == last_year)
    
    # Generate First Graph - Public Sector Employment (Last Year Available)
    first_graph <- ggplot(first_graph_data, 
                          aes(x = country_name, y = value, color = indicator_name)) +
      geom_point(size = 4) +
      labs(
        title = paste0("Public Sector Employment (", last_year, ")"),
        x = "Country",
        y = "Employment (%)",
        color = "Indicator"  # ✅ Renames legend label
      ) +
      theme_minimal()
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # Add First Graph to the Document
    doc <- doc %>% 
      body_add_par(paste0("Public Sector Employment - ", last_year), style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4)
    
    # ✅ Interpretation for Last Year Available
    highest_country <- first_graph_data %>%
      filter(value == max(value, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_country <- first_graph_data %>%
      filter(value == min(value, na.rm = TRUE)) %>%
      pull(country_name)
    
    avg_employment <- round(mean(first_graph_data$value, na.rm = TRUE), 1)
    
    interpretation_text1 <- paste0(
      "For the last available year (", last_year, "), public sector employment rates vary across countries. ",
      "On average, public sector employment accounts for **", avg_employment, "%** of total employment. ",
      "The highest public sector employment is in **", highest_country, "**, while the lowest is in **", lowest_country, "**."
    )
    
    doc <- doc %>% body_add_par(interpretation_text1, style = "Normal")
    
    # ✅ Second Graph - Public Sector Employment Over Time
    second_graph_data <- public_sector_emp_temp %>% filter(country_name == input$country_second)
    
    # Ensure the dataset is not empty
    if (nrow(second_graph_data) == 0) {
      doc <- doc %>% body_add_par("No data available for Public Sector Employment trends over time.", style = "Normal")
      return(doc)
    }
    
    # Identify the first available year (2010) and last available year
    first_year <- 2010
    last_year_time_series <- max(second_graph_data$year, na.rm = TRUE)
    
    # Extract values for comparison
    employment_2010 <- second_graph_data %>%
      filter(year == first_year) %>%
      pull(value_percentage)
    
    employment_last_year <- second_graph_data %>%
      filter(year == last_year_time_series) %>%
      pull(value_percentage)
    
    # Handle cases where 2010 data is missing
    if (length(employment_2010) == 0) {
      employment_2010 <- NA
    }
    if (length(employment_last_year) == 0) {
      employment_last_year <- NA
    }
    
    # Generate Second Graph - Public Sector Employment Over Time
    second_graph <- ggplot(second_graph_data, 
                           aes(x = year, y = value_percentage, color = indicator_name)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        title = paste0("Public Sector Employment Over Time (", first_year, " - ", last_year_time_series, ")"),
        x = "Year",
        y = "Employment (%)",
        color = "Indicator"  # ✅ Renames legend label
      ) +
      theme_minimal()
    
    img_path2 <- tempfile(fileext = ".png")
    ggsave(img_path2, plot = second_graph, width = 8, height = 6)
    
    # Add Second Graph to the Document
    doc <- doc %>% 
      body_add_par("Public Sector Employment Over Time", style = "heading 2") %>% 
      body_add_img(src = img_path2, width = 6, height = 4)
    
    # ✅ Interpretation for Public Sector Employment Trend (2010 vs. Last Year Available)
    trend_direction <- if (employment_last_year > employment_2010) {
      "increased"
    } else if (employment_last_year < employment_2010) {
      "decreased"
    } else {
      "remained stable"
    }
    
    interpretation_text2 <- paste0(
      "In **", input$country_second, "**, public sector employment has **", trend_direction, "** from **", 
      round(employment_2010, 1), "%** in **", first_year, "** to **", 
      round(employment_last_year, 1), "%** in **", last_year_time_series, "**."
    )
    
    doc <- doc %>% body_add_par(interpretation_text2, style = "Normal")
    
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
    
    # Filter data for the last available year
    last_year <- max(gender_wage_premium_last$year, na.rm = TRUE)
    first_graph_data <- gender_wage_premium_last %>% 
      filter(country_name %in% input$countries_first, year == last_year)
    
    # Generate First Graph - Public Sector Wage Premium by Gender (Last Year Available)
    first_graph <- ggplot(first_graph_data, 
                          aes(x = country_name, y = value, color = indicator_label)) +
      geom_point(size = 4) +
      labs(
        title = paste0("Public Sector Wage Premium by Gender (", last_year, ")"),
        x = "Country",
        y = "Wage Premium (%)",
        color = "Indicator"  # ✅ Renames legend label
      ) +
      theme_minimal()
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # Add First Graph to the Document
    doc <- doc %>% 
      body_add_par(paste0("Public Sector Wage Premium by Gender - ", last_year), style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4)
    
    # ✅ Interpretation for Last Year Available
    highest_country <- first_graph_data %>%
      filter(value == max(value, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_country <- first_graph_data %>%
      filter(value == min(value, na.rm = TRUE)) %>%
      pull(country_name)
    
    avg_wage_premium <- round(mean(first_graph_data$value, na.rm = TRUE), 1)
    
    interpretation_text1 <- paste0(
      "For the last available year (", last_year, "), gender-based public sector wage premiums vary across countries. ",
      "On average, the wage premium is **", avg_wage_premium, "%**, indicating differences in how wages are set across genders. ",
      "The highest wage premium is observed in **", highest_country, "**, while the lowest wage premium is in **", lowest_country, "**."
    )
    
    doc <- doc %>% body_add_par(interpretation_text1, style = "Normal")
    
    # ✅ Second Graph - Public Sector Wage Premium by Gender Over Time
    second_graph_data <- gender_wage_premium %>% filter(country_name == input$country_second)
    
    # Ensure the dataset is not empty
    if (nrow(second_graph_data) == 0) {
      doc <- doc %>% body_add_par("No data available for Public Sector Wage Premium by Gender trends over time.", style = "Normal")
      return(doc)
    }
    
    # Identify the first available year (2010) and last available year
    first_year <- 2010
    last_year_time_series <- max(second_graph_data$year, na.rm = TRUE)
    
    # Extract values for comparison
    wage_premium_2010 <- second_graph_data %>%
      filter(year == first_year) %>%
      pull(value)
    
    wage_premium_last_year <- second_graph_data %>%
      filter(year == last_year_time_series) %>%
      pull(value)
    
    # Handle cases where 2010 data is missing
    if (length(wage_premium_2010) == 0) {
      wage_premium_2010 <- NA
    }
    if (length(wage_premium_last_year) == 0) {
      wage_premium_last_year <- NA
    }
    
    # Generate Second Graph - Public Sector Wage Premium by Gender Over Time
    second_graph <- ggplot(second_graph_data, 
                           aes(x = year, y = value, color = indicator_label)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        title = paste0("Public Sector Wage Premium by Gender Over Time (", first_year, " - ", last_year_time_series, ")"),
        x = "Year",
        y = "Wage Premium (%)",
        color = "Indicator"  # ✅ Renames legend label
      ) +
      theme_minimal()
    
    img_path2 <- tempfile(fileext = ".png")
    ggsave(img_path2, plot = second_graph, width = 8, height = 6)
    
    # Add Second Graph to the Document
    doc <- doc %>% 
      body_add_par("Public Sector Wage Premium by Gender Over Time", style = "heading 2") %>% 
      body_add_img(src = img_path2, width = 6, height = 4)
    
    # ✅ Interpretation for Public Sector Wage Premium Trend (2010 vs. Last Year Available)
    trend_direction <- if (wage_premium_last_year > wage_premium_2010) {
      "increased"
    } else if (wage_premium_last_year < wage_premium_2010) {
      "decreased"
    } else {
      "remained stable"
    }
    
    interpretation_text2 <- paste0(
      "In **", input$country_second, "**, the public sector wage premium by gender has **", trend_direction, "** from **", 
      round(wage_premium_2010, 1), "%** in **", first_year, "** to **", 
      round(wage_premium_last_year, 1), "%** in **", last_year_time_series, "**."
    )
    
    doc <- doc %>% body_add_par(interpretation_text2, style = "Normal") %>% 
      body_add_par("Note: This indicator represents the gender wage premium across industries in the public sector.", style = "Normal")
    
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
    
    # Filter data for the last available year
    last_year <- max(gender_workforce$year, na.rm = TRUE)
    first_graph_data <- gender_workforce %>% 
      filter(country_name %in% input$countries_gender, year == last_year)
    
    # Generate First Graph - Female Employment by Sector (Last Year Available)
    first_graph <- ggplot(first_graph_data, 
                          aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Females, as a share of private paid employees" = "#B3242B", 
                                   "Females, as a share of public paid employees" = "#003366")) +
      labs(
        title = paste0("Female Employment by Sector (", last_year, ")"),
        x = "Country",
        y = "Employment (%)",
        fill = "Sector"  # ✅ Renames legend label
      ) +
      theme_minimal()
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # Add First Graph to the Document
    doc <- doc %>% 
      body_add_par(paste0("Female Employment by Sector - ", last_year), style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4)
    
    # ✅ Interpretation for Last Year Available
    highest_public_country <- first_graph_data %>%
      filter(indicator_name == "Females, as a share of public paid employees") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_public_country <- first_graph_data %>%
      filter(indicator_name == "Females, as a share of public paid employees") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    highest_private_country <- first_graph_data %>%
      filter(indicator_name == "Females, as a share of private paid employees") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_private_country <- first_graph_data %>%
      filter(indicator_name == "Females, as a share of private paid employees") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    avg_public <- round(mean(first_graph_data$value_percentage[first_graph_data$indicator_name == "Females, as a share of public paid employees"], na.rm = TRUE), 1)
    avg_private <- round(mean(first_graph_data$value_percentage[first_graph_data$indicator_name == "Females, as a share of private paid employees"], na.rm = TRUE), 1)
    
    interpretation_text1 <- paste0(
      "For the last available year (", last_year, "), female employment rates in the public and private sectors show clear differences across countries. ",
      "On average, **", avg_public, "%** of public sector employees are female, compared to **", avg_private, "%** in the private sector. ",
      "The highest female employment in the public sector is in **", highest_public_country, "**, while the lowest is in **", lowest_public_country, "**. ",
      "In the private sector, **", highest_private_country, "** has the highest share of female employees, whereas **", lowest_private_country, "** has the lowest."
    )
    
    doc <- doc %>% body_add_par(interpretation_text1, style = "Normal")
    
    # ✅ Second Graph - Female Employment by Sector Over Time
    second_graph_data <- gender_workforce %>% filter(country_name == input$country_gender)
    
    # Ensure the dataset is not empty
    if (nrow(second_graph_data) == 0) {
      doc <- doc %>% body_add_par("No data available for Female Employment by Sector trends over time.", style = "Normal")
      return(doc)
    }
    
    # Identify the first available year (2010) and last available year
    first_year <- 2010
    last_year_time_series <- max(second_graph_data$year, na.rm = TRUE)
    
    # Extract values for comparison
    public_2010 <- second_graph_data %>%
      filter(year == first_year, indicator_name == "Females, as a share of public paid employees") %>%
      pull(value_percentage)
    
    public_last_year <- second_graph_data %>%
      filter(year == last_year_time_series, indicator_name == "Females, as a share of public paid employees") %>%
      pull(value_percentage)
    
    private_2010 <- second_graph_data %>%
      filter(year == first_year, indicator_name == "Females, as a share of private paid employees") %>%
      pull(value_percentage)
    
    private_last_year <- second_graph_data %>%
      filter(year == last_year_time_series, indicator_name == "Females, as a share of private paid employees") %>%
      pull(value_percentage)
    
    # Handle cases where 2010 data is missing
    if (length(public_2010) == 0) public_2010 <- NA
    if (length(public_last_year) == 0) public_last_year <- NA
    if (length(private_2010) == 0) private_2010 <- NA
    if (length(private_last_year) == 0) private_last_year <- NA
    
    # Generate Second Graph - Female Employment by Sector Over Time
    second_graph <- ggplot(second_graph_data, 
                           aes(x = year, y = value_percentage, color = indicator_name)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Females, as a share of private paid employees" = "#B3242B", 
                                    "Females, as a share of public paid employees" = "#003366")) +
      labs(
        title = paste("Female Employment by Sector Over Time in", input$country_gender),
        x = "Year",
        y = "Female Employment (%)",
        color = "Sector"  # ✅ Renames legend label
      ) +
      theme_minimal()
    
    img_path2 <- tempfile(fileext = ".png")
    ggsave(img_path2, plot = second_graph, width = 8, height = 6)
    
    # Add Second Graph to the Document
    doc <- doc %>% 
      body_add_par("Female Employment by Sector Over Time", style = "heading 2") %>% 
      body_add_img(src = img_path2, width = 6, height = 4)
    
    # ✅ Interpretation for Female Employment Trends Over Time
    interpretation_text2 <- paste0(
      "In **", input$country_gender, "**, female employment in the public sector has changed from **", 
      round(public_2010, 1), "%** in **", first_year, "** to **", 
      round(public_last_year, 1), "%** in **", last_year_time_series, "**. ",
      "In the private sector, female employment was **", round(private_2010, 1), "%** in **", first_year, 
      "** and changed to **", round(private_last_year, 1), "%** in **", last_year_time_series, "**."
    )
    
    doc <- doc %>% body_add_par(interpretation_text2, style = "Normal")
    
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
    last_year <- max(gender_leadership$year, na.rm = TRUE)
    filtered_data <- gender_leadership %>% 
      filter(country_name %in% input$selected_countries, year == last_year)
    
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
      labs(
        title = paste0("Females by Occupational Group and Sector (", last_year, ")"),
        x = "Country",
        y = "Female Share (%)",
        fill = "Occupational Group"  # ✅ Renames legend label
      ) +
      theme_minimal()
    
    # Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # Add Image to the Document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) 
    
    # ✅ Dynamic Interpretation for Female Representation in Occupational Groups
    highest_public_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Public") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_public_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Public") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    highest_private_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Private") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_private_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Private") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    highest_public_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Public") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_public_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Public") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    highest_private_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Private") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_private_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Private") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    avg_public_managers <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Managers-Public"], na.rm = TRUE), 1)
    avg_private_managers <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Managers-Private"], na.rm = TRUE), 1)
    avg_public_clerks <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Clerks-Public"], na.rm = TRUE), 1)
    avg_private_clerks <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Clerks-Private"], na.rm = TRUE), 1)
    
    interpretation_text <- paste0(
      "For the last available year (", last_year, "), female representation across different occupational groups varies significantly. ",
      "On average, **", avg_public_managers, "%** of public sector managers are female, while in the private sector, female managers account for **", avg_private_managers, "%**. ",
      "The highest female representation among public sector managers is in **", highest_public_managers, "**, whereas the lowest is in **", lowest_public_managers, "**. ",
      "In the private sector, the highest female manager share is in **", highest_private_managers, "**, while the lowest is in **", lowest_private_managers, "**.\n\n",
      "For clerical positions, **", avg_public_clerks, "%** of public sector clerks are female, compared to **", avg_private_clerks, "%** in the private sector. ",
      "The highest female clerk representation in the public sector is in **", highest_public_clerks, "**, whereas the lowest is in **", lowest_public_clerks, "**. ",
      "In the private sector, **", highest_private_clerks, "** has the highest share of female clerks, while **", lowest_private_clerks, "** has the lowest."
    )
    
    doc <- doc %>% 
      body_add_par(interpretation_text, style = "Normal")
    
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
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of gender wage premiums in the public sector by industry 
      (Public Administration, Education, and Health) across selected countries.", 
      style = "Normal"
    )
    
    # Filter Data for Gender Wage Premium (Last Available Year)
    last_year <- max(gender_wage_premiumpublic$year, na.rm = TRUE)
    filtered_data <- gender_wage_premiumpublic %>%
      filter(country_name %in% input$selected_countries, 
             indicator_name %in% c("Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)", 
                                   "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)", 
                                   "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)", 
                                   "Other"),
             year == last_year)
    
    # Handle Empty Dataset Case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # Rename Indicators for Readability
    filtered_data$indicator_label <- recode(filtered_data$indicator_name,
                                            "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" = "Core Public Administration",
                                            "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)" = "Education",
                                            "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)" = "Health")
    
    # Generate Gender Wage Premium ggplot
    gender_wage_plot <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Blues") +  
      labs(
        title = paste0("Gender Wage Premium in Public Sector by Industry (", last_year, ")"),
        x = "Country", 
        y = "Wage Premium (%)",
        fill = "Industry"  # ✅ Renames legend label
      ) +
      theme_minimal()
    
    # Save the Plot as an Image
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = gender_wage_plot, width = 8, height = 6, dpi = 300)
    
    # Add Image to the Document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) 
    
    # ✅ Dynamic Interpretation for Gender Wage Premium in Public Sector
    highest_admin <- filtered_data %>%
      filter(indicator_label == "Core Public Administration") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_admin <- filtered_data %>%
      filter(indicator_label == "Core Public Administration") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    highest_education <- filtered_data %>%
      filter(indicator_label == "Education") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_education <- filtered_data %>%
      filter(indicator_label == "Education") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    highest_health <- filtered_data %>%
      filter(indicator_label == "Health") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    lowest_health <- filtered_data %>%
      filter(indicator_label == "Health") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name)
    
    avg_admin <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Core Public Administration"], na.rm = TRUE), 1)
    avg_education <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Education"], na.rm = TRUE), 1)
    avg_health <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Health"], na.rm = TRUE), 1)
    
    interpretation_text <- paste0(
      "For the last available year (", last_year, "), gender wage premiums in the public sector differ across industries. ",
      "On average, the wage premium in **Core Public Administration** is **", avg_admin, "%**, in **Education** it is **", avg_education, "%**, ",
      "and in **Health** it is **", avg_health, "%**.\n\n",
      "The highest wage premium in **Core Public Administration** is in **", highest_admin, "**, while the lowest is in **", lowest_admin, "**. ",
      "In **Education**, the highest wage premium is observed in **", highest_education, "**, whereas the lowest is in **", lowest_education, "**. ",
      "For **Health**, the highest gender wage premium is in **", highest_health, "**, while the lowest is in **", lowest_health, "**."
    )
    
    doc <- doc %>% 
      body_add_par(interpretation_text, style = "Normal") %>% 
      body_add_par("Note: This indicator represents the gender wage premium across industries in the public sector.", 
                   style = "Normal")
    
    return(doc)
  }
  
  generate_intro_section <- function(doc) {
    # Initialize Word document
    doc <- read_docx() 
    
    # Get the first selected country
    first_country <- if (!is.null(input$countries) && length(input$countries) > 0) {
      input$countries[1]
    } else {
      "Unknown Country"
    }
    
    # Ensure first_region is defined properly
    first_region <- if (!is.null(input$region) && input$region != "") {
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
      "This work documents and analyzes the size, composition, and changes in the levels of employment and wages of ", 
      first_country, "’s public employees compared to the private sector and how these metrics compare to regional peers."
    )
    
    # ✅ Add introduction text
    doc <- doc %>% body_add_par(intro_text, style = "Normal")
    
    # Define a bold, blue style for section headings
    section_style <- fp_text(color = "#003366", font.size = 14, bold = TRUE)
    
    # ✅ Add section title using styled text
    doc <- doc %>% body_add_fpar(fpar(ftext("The Macro Fundamentals of the Public Sector", prop = section_style)))
    
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
      doc <- generate_intro_section(doc)  # Add the Intro First
      
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
      
      # Add Conclusion Section
      doc <- doc %>% 
        body_add_par("Conclusion", style = "heading 1") %>% 
        body_add_par(
          "This report provides a comprehensive analysis of wage bill trends, gender employment representation, and workforce participation in the public sector. 
          The findings highlight key trends and disparities across different sectors and countries, offering valuable insights into public employment structures and workforce dynamics.",
          style = "Normal"
        )
      
      
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


    

   