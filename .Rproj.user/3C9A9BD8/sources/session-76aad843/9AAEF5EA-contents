# Worldwide Bureaucracy Indicators
# 01. Data processing

### Libraries

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(readxl)
library(data.table)
library(lubridate)

### Loading data ----

# Load the data sets


data_wwbi      <- read_dta(file.path(data_path, "/Data/data_wwbi.dta"))



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

# Ensure the base data path is set correctly
data_path <- getwd()  # Automatically detects the working directory

# Debugging: Print the paths to check correctness
print(file.path(data_path, "Data", "data_wwbi.dta"))

# Load the data correctly
data_wwbi <- read_dta(file.path(data_path, "Data", "data_wwbi.dta"))

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

data_gdp <- read_dta(file.path(data_path, "data_gdp.dta"))

# Load world spatial data

world_spdf <- ne_countries(scale = "medium", returnclass = "sf")

# Create a color palette for countries

color_palette <- colorFactor(c("lightgreen", "lightgray"), domain = c("reported", "not_reported"))


colnames(world_spdf)[colnames(world_spdf) == "name"] <- "country_name"

# Save the spatial dataset in the Shiny folder

st_write(world_spdf, file.path(datapath, "Data/world_spatial.gpkg"))

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

write_dta(selected_data_long, file.path(datapathshiny, "/selected_data_long.dta"))


# Reshape the data using pivot_longer gdp data base 

data_gdp <- data_gdp %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value))  # Remove rows with NA values

#Save data set

write_dta(data_gdp, file.path(datapath, "Data/data_gdp.dta"))


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

#Save data set

write_dta(data_wwbi_long, file.path(datapath, "Data/data_wwbi_long.dta"))



# Filter the data for the specific indicator "Wage bill as a percentage of Public Expenditure"

wage_bill_publicexp <- data_wwbi_long[data_wwbi_long$indicator_name == "Wage bill as a percentage of Public Expenditure", ]

#Save data set

write_dta(wage_bill_publicexp, file.path(datapath, "Data/wage_bill_publicexp.dta"))



# Filter the data for the specific indicator "Wage bill as a percentage of GDP"

wage_bill_gdp <- data_wwbi_long[data_wwbi_long$indicator_name == "Wage bill as a percentage of GDP", ]

#Save data set

write_dta(wage_bill_gdp, file.path(datapath, "Data/wage_bill_gdp.dta"))



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

#Save data set

write_dta(public_sector_emp_temp, file.path(datapath, "Data/public_sector_emp_temp.dta"))



public_sector_emp <- public_sector_emp %>%
  select(year, indicator_name, value, country_name, wb_region, value_percentage) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
                                  "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
                                  "Public sector employment, as a share of total employment"= "as a share of total employment"))



public_sector_emp <- public_sector_emp %>%
  mutate(value_percentage = value * 100)

#Save data set

write_dta(public_sector_emp, file.path(datapath, "Data/public_sector_emp.dta"))



# Keep the last year available for each country

public_sector_emp_temp_last <- public_sector_emp %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, indicator_name, wb_region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data

public_sector_emp_temp_last <- public_sector_emp_temp_last %>%
  mutate(value_percentage = value * 100)

#Save data set

write_dta(public_sector_emp_temp_last, file.path(datapath, "Data/public_sector_emp_temp_last.dta"))



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

#Save data set

write_dta(public_sector_workforce, file.path(datapath, "Data/public_sector_workforce.dta"))



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
  mutate(indicator_name = ifelse(indicator_name == "Public Administration", "Public Administration", indicator_name))

#Save data set

write_dta(public_sector_workforce_first_last, file.path(datapath, "Data/public_sector_workforce_first_last.dta"))



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

write_dta(gender_workforce, file.path(datapath, "Data/gender_workforce.dta"))



# Filter GDP data for the year 2015

gdp_2015 <- data_gdp %>%
  filter(year == 2015) %>%
  select(country_name, value)

#Save data set

write_dta(gdp_2015, file.path(datapath, "Data/gdp_2015.dta"))



# Rename 'value' column to 'indicator_value' in data_indicators

data_indicator_wb <- wage_bill_publicexp %>%
  rename(indicator_value = value)

#Save data set

write_dta(data_indicator_wb, file.path(datapath, "Data/data_indicator_wb.dta"))



# Rename 'value' column to 'gdp_value' in data_gdp

gdp_2015 <- gdp_2015 %>%
  rename(gdp_value = value)

#Save data set

write_dta(gdp_2015, file.path(datapath, "Data/gdp_2015.dta"))




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

write_dta(merged_data, file.path(datapath, "Data/merged_data.dta"))




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

write_dta(tertiary_education, file.path(datapath, "Data/tertiary_education.dta"))



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

write_dta(public_wage_premium, file.path(datapath, "Data/public_wage_premium.dta"))


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

write_dta(public_wage_premium_educ, file.path(datapath, "Data/public_wage_premium_educ.dta"))



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

gender_wage_premium <- gender_wage_premium %>%
  mutate(value_percentage = value * 100)

#Save data set

write_dta(gender_wage_premium, file.path(datapath, "Data/gender_wage_premium.dta"))



# Keep the last year available for each country

gender_wage_premium_last <- gender_wage_premium %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, indicator_label, wb_region) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data

gender_wage_premium_last <- gender_wage_premium_last %>%
  mutate(value_percentage = value * 100)

#Save data set

write_dta(gender_wage_premium_last, file.path(datapath, "Data/gender_wage_premium_last.dta"))


#Public Sector Employment

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

#Save data sets

write_dta(public_sector_emp_temp, file.path(datapath, "Data/public_sector_emp_temp.dta"))
write_dta(public_sector_emp_temp_last, file.path(datapath, "Data/public_sector_emp_temp_last.dta"))


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

#Save data set

write_dta(gender_leadership, file.path(datapath, "Data/gender_leadership.dta"))


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





#Save data set

write_dta(gender_wage_premiumpublic, file.path(datapath, "Data/gender_wage_premiumpublic.dta"))


#end of script

