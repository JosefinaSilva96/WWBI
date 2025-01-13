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


### INITIAL COMMANDS ----

#Set data path 

data_path <- "C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/Data/Intermediate"

#Load indicators data set 

data_wwbi <- read_dta(file.path(data_path, "data_wwbi.dta"))


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
  "Zambia", "Zimbabwe")

#Drop pvalue in the indicator column

data_wwbi <- data_wwbi[!grepl("^P-Value:", data_wwbi$indicator_name), ]

# Extract available years and countries for select inputs

years <- as.character(2000:2022)  # Years are 2000 to 2022 based on column names in your data

countries <- unique(data_wwbi$country_name)  # Extract unique country names from the data set


indicator <- unique(data_wwbi$indicator_name)

# Filter the data using dplyr

selected_data_long <- data_wwbi %>%
  filter(indicator_name == indicator & country_name %in% countries) %>%
  select(country_name, indicator_name, starts_with("year_"))  # Select relevant columns


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


# Filter the data for the specific indicator "Wage bill as a percentage of GDP"

wage_bill_gdp <- data_wwbi[data_wwbi$indicator_name == "Wage bill as a percentage of GDP", ]


wage_bill_gdp <- wage_bill_gdp %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #4104 obs



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
  select(year, indicator_name, value, country_name) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
                                  "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
                                  "Public sector employment, as a share of total employment"= "as a share of total employment"))


public_sector_emp <- public_sector_emp %>%
  select(year, indicator_name, value, country_name) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
                                  "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
                                  "Public sector employment, as a share of total employment"= "as a share of total employment"))




# Keep the last year available for each country

public_sector_emp_temp_last <- public_sector_emp %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, indicator_name) %>%                      # Group by country_name (or any other variable)
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
  group_by(country_name, year) %>%
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
      group_by(country_name, year) %>%
      summarize(
        indicator_name = "Other",  # Set the indicator name to "Other"
        value_percentage = first(other_value),  # Replace the value with the calculated 'other_value'
        .groups = "drop"  # Drop the grouping after summarizing
      ) %>%
      ungroup()
  )


# Keep the first and  last year available for each country

public_sector_workforce_first_last <- public_sector_workforce %>%
  filter(!is.na(value_percentage)) %>%               # Keep rows where `value_percentage` is not NA
  group_by(country_name, indicator_name) %>%         # Group by country and indicator
  filter(year == max(year, na.rm = TRUE) |           # Keep rows for the last year
           year == min(year, na.rm = TRUE)) %>%      # Keep rows for the first year
  ungroup()

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
  group_by(country_name) %>%                      # Group by country_name (or any other variable)
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
  select(country_name, indicator_name, country_code, indicator_value, gdp_value)


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
  group_by(country_name,indicator_name) %>%                      # Group by country_name (or any other variable)
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
  group_by(country_name,indicator_name) %>%                      # Group by country_name (or any other variable)
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
  group_by(country_name,indicator_name) %>%                      # Group by country_name (or any other variable)
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
  select(year, indicator_name, value, country_name) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  # Modify indicator labels for shorter text
  mutate(indicator_label = recode(indicator_name, 
                                  "Public sector wage premium, by gender: Female (compared to all private employees)" = "Female", 
                                  "Public sector wage premium, by gender: Male (compared to all private employees)" = "Male"))


# Keep the last year available for each country

gender_wage_premium_last <- gender_wage_premium %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name, indicator_label) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data




public_sector_emp_temp <- public_sector_emp_temp %>%
  select(country_name, indicator_name, year, value) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  mutate(indicator_label = recode(
    indicator_name, 
    "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
    "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
    "Public sector employment, as a share of total employment" = "as a share of total employment"
  ))


public_sector_emp_temp_last <- public_sector_emp_temp_last %>%
  select(country_name, indicator_name, year, value) %>%
  mutate(indicator_name = factor(indicator_name)) %>%
  mutate(indicator_label = recode(
    indicator_name, 
    "Public sector employment, as a share of formal employment" = "as a share of formal employment", 
    "Public sector employment, as a share of paid employment" = "as a share of paid employment", 
    "Public sector employment, as a share of total employment" = "as a share of total employment"
  ))


## Shiny Dashboard ----


# Define UI ----

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "WWB Indicators"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
      menuItem("Variable List", tabName = "variableList", icon = icon("table")),
      menuItem("Indicators Status", tabName = "indicators", icon = icon("globe")),
      menuItem("Wage Bill Graphs", tabName = "wageBillGraphs", icon = icon("chart-line")), 
      menuItem("Wage Bill and GDP Graphs", tabName = "wageBillgdpGraphs", icon = icon("chart-line")), 
      menuItem("Public Sector Graphs", tabName = "publicSectorGraphs", icon = icon("chart-line")), 
      menuItem("Public Sector Workforce Graphs", tabName = "publicSectorWorkforceGraphs", icon = icon("chart-line")), 
      menuItem("Gender Workforce Graphs", tabName = "genderWorkforceGraphs", icon = icon("chart-line")), 
      menuItem("Tertiary Education Graphs", tabName = "educationGraphs", icon = icon("chart-line")),
      menuItem("Public Sector Wage Premium", tabName = "publicsectorwagepremiumGraphs", icon = icon("chart-line")),
      menuItem("Public Sector Education Workers", tabName = "publicsectoreducationGraphs", icon = icon("chart-line")), 
      menuItem("Wage Premium Gender", tabName = "wagepremiumGraphs", icon = icon("chart-line")), 
      menuItem("Download All Graphs", tabName = "downloadAllGraphs", icon = icon("download")) 
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Dashboard Description", status = "primary", solidHeader = TRUE, width = 12,
                    "Welcome to the World Bank Indicators Dashboard!")
              )
      ),
      # Widgets Tab
      tabItem(tabName = "widgets",
              fluidRow(
                infoBoxOutput("numberIndicatorsBox", width = 6),
                infoBoxOutput("numberCountriesBox", width = 6),
                infoBoxOutput("temporalCoverageAnnualBox", width = 6),
                infoBoxOutput("temporalCoverageYearsBox", width = 6),
                infoBoxOutput("lastUpdatedBox", width = 6)
              )
      ),
      # Variable List Tab
      tabItem(tabName = "variableList",
              fluidRow(
                box(title = "Year Filter", status = "primary", solidHeader = TRUE, width = 3,
                    selectInput("yearFilter", "Select Year", 
                                choices = paste0("year_", 2000:2022), selected = "year_2022", multiple = TRUE)
                ),
                box(title = "Available Variables", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("variableTable")
                )
              )
      ),
      # Wage Bill Graphs Tab
      tabItem(
        tabName = "wageBillGraphs",
        fluidRow(
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
          box(
            title = "Country and Graph Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            selectInput(
              "countries",
              "Countries",
              choices = unique(wage_bill_gdp$country_name),
              selected = unique(wage_bill_gdp$country_name)[1],
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
                # Combined Selection Box
                box(
                  title = "Country and Graph Selection",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # Multi-country selection for the first graph
                  selectInput(
                    "countries_first",
                    "Select Countries for First Graph (Multi-Country)",
                    choices = unique(public_sector_emp_temp$country_name),
                    selected = NULL,
                    multiple = TRUE
                  ),
                  # Single-country selection for the second graph
                  selectInput(
                    "country_second",
                    "Select Country for Second Graph (Single Country)",
                    choices = unique(public_sector_emp_temp$country_name),
                    selected = NULL,
                    multiple = FALSE
                  ),
                  # Checkbox for selecting graphs to download
                  checkboxGroupInput(
                    "graphs_to_download",
                    "Select Graphs to Download",
                    choices = list(
                      "First Graph - Multi-Country" = "firstGraph",
                      "Second Graph - Single Country" = "secondGraph"
                    ),
                    selected = c("firstGraph", "secondGraph")
                  ),
                  # Download button
                  downloadButton("downloadPublicGraphsWord", "Download Word Document")
                )
              ),
              # Row for displaying both graphs
              fluidRow(
                box(
                  title = "First Graph - Multi-Country",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("firstGraph")
                ),
                box(
                  title = "Second Graph - Single Country",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("secondGraph")
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
            downloadButton("downloadGraphsWord", "Download Selected Graphs in Word")
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
        )
      ),
      # Tertiary Education Graphs Tab
      tabItem(tabName = "educationGraphs",
              fluidRow(
                box(title = "Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("selected_countries", "Select Countries", 
                                choices = unique(tertiary_education$country_name),
                                selected = unique(tertiary_education$country_name)[1:3], # Default selection
                                multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "Tertiary Education Graphs", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("barPlot", height = "600px")
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
      #Wage Premium Gender Graphs Tab
      tabItem(tabName = "wagepremiumGraphs",
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
                    downloadButton("downloadGraphsWord", "Download Graphs as Word File", class = "btn-primary btn-block")
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
      # Indicators Tab
      tabItem(tabName = "indicators",
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
  )
)



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
    
    # Get the maximum year in the filtered data
    max_year <- max(data_to_plot$year, na.rm = TRUE)
    
    # Extract data for the last year
    last_year_data <- data_to_plot %>%
      filter(year == max_year) %>%
      select(country_name, year, value)
    
    # Define plot titles and plot mode based on selected indicator
    title_text <- ifelse(input$indicator == "Wage bill as a percentage of GDP",
                         "Wage Bill as % of GDP Over Time",
                         "Wage Bill as % of Public Expenditure Over Time")
    plot_mode <- 'lines+markers'  # Common plot mode for both indicators
    
    # Create the Plotly plot
    plot <- plot_ly(data = data_to_plot, 
                    x = ~year, 
                    y = ~value, 
                    color = ~country_name, 
                    type = 'scatter', 
                    mode = plot_mode,
                    marker = list(size = 8)) %>%
      layout(title = title_text,
             xaxis = list(title = "Year", dtick = 2),
             yaxis = list(title = ifelse(input$indicator == "Wage bill as a percentage of GDP", 
                                         "Wage Bill (% of GDP)", "Wage Bill (%)")),
             legend = list(title = list(text = "Country")))
    
    # Add annotations for the last year values
    for (i in 1:nrow(last_year_data)) {
      plot <- plot %>%
        add_annotations(
          x = last_year_data$year[i], 
          y = last_year_data$value[i],
          text = paste(round(last_year_data$value[i], 2)),
          showarrow = FALSE,
          font = list(size = 12, color = "black"),
          bgcolor = "white",
          xanchor = "center",
          yanchor = "bottom"
        )
    }
    
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
  
  # Render the dot plot
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
      
      # Create a Word document
      doc <- read_docx()
      
      # Add the Title
      doc <- doc %>%
        body_add_par("Wage Bill vs. GDP Analysis Report", style = "heading 1") %>%
        body_add_par(
          "This report presents the relationship between wage bill and GDP per capita (log scale) for selected countries.",
          style = "Normal"
        )
      
      # Add the Introduction
      doc <- doc %>%
        body_add_par("Introduction", style = "heading 2") %>%
        body_add_par(
          "The analysis explores the correlation between public sector wage bills and GDP per capita using data from various countries. 
          A trendline is provided for benchmarking and understanding the general patterns across nations.",
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
  
  #Public Sector Graphs 

  # First Graph (Multiple Countries)
  
  output$firstGraph <- renderPlotly({
    req(input$countries_first)  # Ensure countries_first is selected
    
    data_to_plot <- public_sector_emp_temp_last %>%
      filter(country_name %in% input$countries_first)
    
    data_to_plot_long <- data_to_plot %>%
      select(country_name, indicator_label, year, value) %>%
      mutate(indicator_label = factor(indicator_label)) # Fixed: Added missing parenthesis
    
    
    plot_ly(data = data_to_plot_long, 
            x = ~country_name, 
            y = ~value, 
            color = ~indicator_label, 
            type = 'scatter',
            mode = 'markers',  
            marker = list(size = 8)) %>%
      layout(title = "Public Sector Employment (Multi-Country)",
             xaxis = list(title = "Country", tickangle = 45),
             yaxis = list(title = "Value"),
             legend = list(title = list(text = "Indicator")))
  })
  
  # Second Graph (Single Country)
  output$secondGraph <- renderPlotly({
    req(input$country_second)  # Ensure country_second is selected
    
    data_to_plot <- public_sector_emp_temp %>%
      filter(country_name == input$country_second)
    
    
    plot_ly(data = data_to_plot_long, 
            x = ~year, 
            y = ~value, 
            color = ~indicator_label, 
            text = ~paste("Value:", round(value, 2)), 
            type = 'scatter', 
            mode = 'lines+markers',  
            marker = list(size = 8)) %>%
      layout(title = paste("Public Sector Employment in", input$country_second, "Over Time"),
             xaxis = list(title = "Year", tickangle = 45, dtick = 2),
             yaxis = list(title = "Employment Value"),
             legend = list(title = list(text = "Indicator")))
  })
  
  # Download Handler
  output$downloadGraphsWord <- downloadHandler(
    filename = function() {
      paste0("Public_Sector_Graphs_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      
      # Add First Graph if selected
      if ("firstGraph" %in% input$graphs_to_download && length(input$countries_first) > 0) {
        data_to_plot <- public_sector_emp_temp_last %>%
          filter(country_name %in% input$countries_first)
        
        data_to_plot_long <- data_to_plot %>%
          select(country_name, indicator_name, year, value) %>%
          mutate(indicator_name = factor(indicator_name))
        
        first_graph <- ggplot(data_to_plot_long, aes(x = country_name, y = value, color = indicator_name)) +
          geom_point(size = 3) +
          labs(
            title = "Public Sector Employment (Multi-Country)",
            x = "Country",
            y = "Value"
          ) +
          theme_minimal()
        
        doc <- doc %>%
          body_add_par("First Graph: Public Sector Employment (Multi-Country)", style = "heading 1") %>%
          body_add_gg(value = first_graph, width = 6, height = 4)
      }
      
      # Add Second Graph if selected
      if ("secondGraph" %in% input$graphs_to_download && !is.null(input$country_second)) {
        data_to_plot <- public_sector_emp_temp %>%
          filter(country_name == input$country_second)
        
        data_to_plot_long <- data_to_plot %>%
          select(year, indicator_name, value) %>%
          mutate(indicator_name = factor(indicator_name))
        
        second_graph <- ggplot(data_to_plot_long, aes(x = year, y = value, color = indicator_name)) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          labs(
            title = paste("Public Sector Employment in", input$country_second, "Over Time"),
            x = "Year",
            y = "Employment Value"
          ) +
          theme_minimal()
        
        doc <- doc %>%
          body_add_par("Second Graph: Public Sector Employment (Single Country)", style = "heading 1") %>%
          body_add_gg(value = second_graph, width = 6, height = 4)
      }
      
      print(doc, target = file)
    }
  )
  
  #Public Sector Workforce
  
  # Reactive expression to filter workforce data
  filtered_workforce_data <- reactive({
    req(input$countries_workforce)  # Ensure input is not null
    public_sector_workforce %>%
      filter(country_name %in% input$countries_workforce) %>%
      group_by(country_name, indicator_name) %>%
      slice_max(order_by = year, n = 1) %>%  # Get the latest year available for each country
      ungroup()
  })
  
  # Render the stacked bar graph
  output$stackedBarGraph <- renderPlotly({
    data_to_plot <- filtered_workforce_data()
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
        "Public Administration workers, as a share of public total employees" = "#568340", 
        "Education workers, as a share of public total employees" = "#B3242B", 
        "Health workers, as a share of public total employees" = "#003366", 
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
  
  output$horizontalStackedBar <- renderPlotly({
    req(input$selected_country)
    
    # Filter data for the selected country
    filtered_data <- public_sector_workforce %>%
      filter(country_name == input$selected_country)
    
    # Find the first and last year available for the selected country
    first_year <- min(filtered_data$year, na.rm = TRUE)
    last_year <- max(filtered_data$year, na.rm = TRUE)
    
    # Filter data for the first and last year
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
              "Public Administration workers, as a share of public total employees" = "#568340",
              "Education workers, as a share of public total employees" = "#B3242B",
              "Health workers, as a share of public total employees" = "#003366", 
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
  
  # Word Graph Download Handler
  output$downloadGraphsWord <- downloadHandler(
    filename = function() {
      paste0("Public_Sector_Workforce_Graphs_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Create a new Word document
      doc <- read_docx()
      
      # Check if the first graph is selected
      if ("firstGraph" %in% input$selected_graphs_public) {
        # Render the first graph
        data_to_plot <- filtered_workforce_data()
        if (nrow(data_to_plot) > 0) {
          stacked_graph <- plot_ly(
            data = data_to_plot,
            x = ~country_name,
            y = ~value_percentage,
            color = ~indicator_name,
            type = "bar",
            text = ~paste(
              "Country:", country_name,
              "<br>Indicator:", indicator_name,
              "<br>Value:", round(value_percentage, 1), "%"
            ),
            textposition = "auto",
            colors = c(
              "Public Administration workers, as a share of public total employees" = "#568340", 
              "Education workers, as a share of public total employees" = "#B3242B", 
              "Health workers, as a share of public total employees" = "#003366", 
              "Other" = "#A9A9A9"
            )
          )
          
          # Save the first graph as an image
          tmp_file1 <- tempfile(fileext = ".png")
          export(stacked_graph, file = tmp_file1)
          
          # Add the first graph to the document
          doc <- doc %>%
            body_add_par("First Graph: Public Workforce Distribution by Country", style = "heading 1") %>%
            body_add_img(src = tmp_file1, width = 6, height = 4)
        }
      }
      
      # Check if the second graph is selected
      if ("secondGraph" %in% input$selected_graphs_public) {
        filtered_data <- public_sector_workforce %>%
          filter(country_name == input$selected_country)
        if (nrow(filtered_data) > 0) {
          first_year <- min(filtered_data$year, na.rm = TRUE)
          last_year <- max(filtered_data$year, na.rm = TRUE)
          
          if (is.finite(first_year) && is.finite(last_year)) {
            data_to_plot <- filtered_data %>%
              filter(year %in% c(first_year, last_year)) %>%
              group_by(year, indicator_name) %>%
              summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")
            
            horizontal_graph <- plot_ly(
              data = data_to_plot,
              x = ~value_percentage,
              y = ~factor(year, levels = c(last_year, first_year)),
              color = ~indicator_name,
              type = "bar",
              orientation = "h",
              text = ~paste0(round(value_percentage, 1), "%"),
              textposition = "inside",
              colors = c(
                "Public Administration workers, as a share of public total employees" = "#568340",
                "Education workers, as a share of public total employees" = "#B3242B",
                "Health workers, as a share of public total employees" = "#003366",
                "Other" = "#A9A9A9"
              )
            )
            
            # Save the second graph as an image
            tmp_file2 <- tempfile(fileext = ".png")
            export(horizontal_graph, file = tmp_file2)
            
            # Add the second graph to the document
            doc <- doc %>%
              body_add_par(paste("Horizontal Stacked Bar Graph for", input$selected_country), style = "heading 1") %>%
              body_add_img(src = tmp_file2, width = 6, height = 4)
          }
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
    
    #Last year available
    public_latest <- filtered_data %>%
      filter(indicator_name == "Females, as a share of public paid employees") %>%
      group_by(country_name) %>%
      filter(year == max(year, na.rm = TRUE)) %>%  # Keep only the most recent year
      ungroup()
    
    #Last year available
    private_latest <- filtered_data %>%
      filter(indicator_name == "Females, as a share of private paid employees") %>%
      group_by(country_name) %>%
      filter(year == max(year, na.rm = TRUE)) %>%  # Keep only the most recent year
      ungroup()
    
    # Create the plot
    plot <- plot_ly(
      data = public_latest,
      x = ~country_name,
      y = ~value_percentage,  # Use the percentage for the last year available
      type = 'bar',
      color = I("#003366"),  # Color for public sector
      text = ~paste("Country: ", country_name,
                    "<br>Last year available: ", year,
                    "<br>Employment (%): ", round(value_percentage, 2)),  # Add detailed hover text
      hoverinfo = "text",  # Show hover info
      name = "Public Sector",
      showlegend = TRUE
    ) %>%
      add_trace(
        data = private_latest,
        x = ~country_name,
        y = ~value_percentage,  # Use the percentage for the last year available
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, color = "#B3242B"),
        name = "Private Sector",
        text = ~paste("Country: ", country_name,
                      "<br>Last year available: ", year,
                      "<br>Employment (%): ", round(value_percentage, 2)),  # Add detailed hover text
        hoverinfo = "text",  # Show hover info
        showlegend = TRUE  # Show the legend
      ) %>%
      layout(
        barmode = "group",
        title = "Female Employment by Sector (Last Year Available)",
        xaxis = list(
          title = "Country (Last Year Available)",
          tickmode = 'array', 
          tickvals = public_latest$country_name,  # Explicitly set the tick values
          ticktext = paste(public_latest$country_name, 
                           "(", public_latest$year, ")")  # Append year to country names
        ),
        yaxis = list(title = "Employment (%)"),
        legend = list(title = list(text = "Sector"))
      )
    
    return(plot)
  })
  # Second Graph: Female Employment by Sector Over Time (Single Country)
  output$employment_plot_overtime <- renderPlotly({
    # Filter the data for the selected country
    filtered_data <- gender_workforce %>% 
      filter(country_name == input$selected_country) # Ensure single country selection
    
    # Define a custom color palette
    custom_colors <- c("Females, as a share of private paid employees" = "#003366", "Females, as a share of public paid employees" = "#B3242B")
    
    # Create the Plotly graph
    plot <- filtered_data %>%
      plot_ly(
        x = ~year,
        y = ~value_percentage,             
        color = ~indicator_name,
        colors = custom_colors, # Apply custom colors
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
 
  # Define the initial world map render
  output$worldMap <- renderLeaflet({
    leaflet(world_spdf) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2)  # Adjust view to show the world
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
    
    # Calculate the number of countries with data
    total_countries_with_data <- nrow(reported_countries)
    
    # Update the Leaflet map
    leafletProxy("worldMap") %>%
      clearShapes() %>%
      addPolygons(
        data = world_data_merged,
        fillColor = ~ifelse(is.na(value_percentage), "#808080", colorNumeric("Greens", domain = world_data_merged$value_percentage)(value_percentage)),
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
      paste("Total Countries with Data: ", total_countries_with_data)
    })
  })
  # Dummy outputs for widgets
  output$numberIndicatorsBox <- renderInfoBox({
    infoBox("Indicators", 100, icon = icon("list"), color = "blue")
  })
  
  output$numberCountriesBox <- renderInfoBox({
    infoBox("Countries", length(unique(data_wwbi$country_name)), icon = icon("globe"), color = "blue")
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

