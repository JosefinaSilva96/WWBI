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


# Keep the last year available for each country

public_sector_workforce <- public_sector_workforce %>%
  filter(!is.na(value_percentage)) %>%               # Keep rows where `value_percentage` is not NA
  group_by(country_name, indicator_name) %>%         # Group by country and indicator
  filter(year == max(year, na.rm = TRUE)) %>%        # Get the last available year for each group
  ungroup()                                          # Ungroup the data



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


public_wage_premium <- pubic_wage_premium %>%
  mutate(value_percentage = value * 100)

# Keep the last year available for each country

public_wage_premium <- pubic_wage_premium %>%
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
      menuItem("Public Sector Graphs", tabName = "publicSectorGraphs", icon = icon("chart-line")), 
      menuItem("Public Sector Workforce Graphs", tabName = "publicSectorWorkforceGraphs", icon = icon("chart-line")), 
      menuItem("Gender Workforce Graphs", tabName = "genderWorkforceGraphs", icon = icon("chart-line")), 
      menuItem("Wage Bill Graphs", tabName = "wageBillGraphs", icon = icon("chart-line"))
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
      tabItem(tabName = "wageBillGraphs",
              fluidRow(
                box(title = "WWB Indicator Selection", status = "primary", solidHeader = TRUE, width = 4,
                    selectInput("indicator", "Select a WWB Indicator", 
                                choices = c("Wage bill (as % of public expenditure) over time", 
                                            "Wage bill as a percentage of GDP"))
                ),
                box(title = "Country Selection", status = "primary", solidHeader = TRUE, width = 8,
                    selectInput("countries", "Countries", 
                                choices = unique(filtered_data$country_name), 
                                selected = unique(filtered_data$country_name)[1], 
                                multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "Graph", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("plot")
                )
              )
      ),
      # Public Sector Graphs Tab
      tabItem(tabName = "publicSectorGraphs",
              fluidRow(
                box(title = "First Graph - Multi-Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("countries_first", 
                                "Select Countries for First Graph", 
                                choices = unique(public_sector_emp_temp$country_name), 
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
                                choices = unique(public_sector_emp_temp$country_name), 
                                selected = NULL, 
                                multiple = FALSE)
                )
              ),
              fluidRow(
                box(title = "Second Graph", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("secondGraph")
                )
              )
      ),
      # Public Sector Workforce Graphs Tab
      tabItem(tabName = "publicSectorWorkforceGraphs",
              fluidRow(
                box(title = "Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("countries_workforce", 
                                "Select Countries for Workforce Graph", 
                                choices = unique(public_sector_workforce$country_name), 
                                selected = unique(public_sector_workforce$country_name)[1], 
                                multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "Stacked Bar Chart", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("stackedBarGraph", height = "600px")
                )
              ),
              fluidRow(
                box(title = "Select Country (Second Graph)", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("selected_country", 
                                "Select Country for Second Graph", 
                                choices = unique(public_sector_workforce$country_name), 
                                selected = NULL, 
                                multiple = FALSE)
                )
              ),
              fluidRow(
                box(title = "Second Graph: Horizontal Stacked Bar (Single Country)", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("messageOutput"),
                    plotlyOutput("horizontalStackedBar")
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
      # Indicators Tab
      tabItem(tabName = "indicators",
              fluidRow(
                box(title = "Indicator Status Across Countries", status = "primary", solidHeader = TRUE, width = 12,
                    "This map shows which countries have reported data for the selected indicator."
                )
              ),
              fluidRow(
                box(title = "Select Indicator", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("indicatorSelect", "Choose Indicator", 
                                choices = c("Wage bill (as % of public expenditure) over time", 
                                            "Wage bill as a percentage of GDP", 
                                            "Public sector employment as a share of total workforce"))
                )
              ),
              fluidRow(
                box(title = "World Map", status = "primary", solidHeader = TRUE, width = 12,
                    leafletOutput("worldMap", height = 500)
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
    
    if (input$indicator == "Wage bill as a percentage of GDP") {
      # Filter wage_bill_gdp dataset based on selected countries
      data <- wage_bill_gdp %>% filter(country_name %in% input$countries)
    } else {
      data <- filtered_data %>% filter(country_name %in% input$countries)
    }
    return(data)
  })
  
  # Render Plotly plot for the main indicator
  output$plot <- renderPlotly({
    data_to_plot <- selected_data()
    
    max_year <- max(data_to_plot$year, na.rm = TRUE)
    
    last_year_data <- data_to_plot %>%
      group_by(country_name) %>%
      filter(year == max_year) %>%
      ungroup() %>%
      select(country_name, year, value)
    
    title_text <- ifelse(input$indicator == "Wage bill as a percentage of GDP",
                         "Wage Bill as % of GDP Over Time",
                         "Wage Bill as % of Public Expenditure Over Time")
    plot_mode <- ifelse(input$indicator == "Wage bill as a percentage of GDP", 'lines+markers', 'lines+markers')
    
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
    
    plot 
  })
  # First Graph (Multiple Countries)
  output$firstGraph <- renderPlotly({
    data_to_plot <- public_sector_emp_temp_last %>%
      filter(country_name %in% input$countries_first)
    
    data_to_plot_long <- data_to_plot %>%
      select(country_name, indicator_name, year, value) %>%
      mutate(indicator_name = factor(indicator_name))
    
    plot <- plot_ly(data = data_to_plot_long, 
                    x = ~country_name, 
                    y = ~value, 
                    color = ~indicator_name, 
                    type = 'scatter',
                    mode = 'markers',  
                    marker = list(size = 8)) %>%
      layout(title = "Public sector employment as a share of",
             xaxis = list(title = "Country", tickangle = 45),
             yaxis = list(title = "Value"),
             legend = list(title = list(text = "Indicator")))
    
    plot
  })
  # Second Graph (Single Country)
  output$secondGraph <- renderPlotly({
    data_to_plot <- public_sector_emp_temp %>%
      filter(country_name == input$country_second)  # Single country selection
    
    data_to_plot_long <- data_to_plot %>%
      select(year, indicator_name, value) %>%
      mutate(indicator_name = factor(indicator_name))
    
    plot <- plot_ly(
      data = data_to_plot_long, 
      x = ~year, 
      y = ~value, 
      color = ~indicator_name,  # Color each indicator differently
      text = ~paste("Value:", round(value, 2)),  # Tooltip with value
      type = 'scatter', 
      mode = 'lines+markers',  # Add lines and markers
      marker = list(size = 8)  # Set marker size
    ) %>%
      layout(
        title = paste("Public Sector Employment in", input$country_second, "Over Time"),
        xaxis = list(title = "Year", tickangle = 45, dtick = 2),
        yaxis = list(title = "Employment Value"),
        legend = list(title = list(text = "Indicator"))
      ) %>%
      add_annotations(
        x = ~year, 
        y = ~value,  # Add offset to place annotation above the point
        text = ~round(value, 2),  # Display value as annotation
        showarrow = FALSE,  # Remove arrows
        font = list(size = 12, color = "black"),
        xanchor = "center",  # Center annotation horizontally
        yanchor = "bottom"   # Position annotation above the point
      )
    
    plot
  })
  
  # Reactive expression to filter workforce data
  filtered_workforce_data <- reactive({
    req(input$countries_first) # Ensure input is not null
    public_sector_workforce %>%
      filter(country_name %in% input$countries_first) %>%
      group_by(country_name, indicator_name) %>%
      slice_max(order_by = year, n = 1) %>% # Get the latest year available for each country
      ungroup()
  })
  
  # Render the stacked bar graph
  
  output$stackedBarGraph <- renderPlotly({
    data_to_plot <- filtered_workforce_data()
    req(nrow(data_to_plot) > 0) # Ensure there's data to plot
    
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
      ), # Detailed hover information
      textposition = "auto",
      colors = c(
        "Public Administration workers, as a share of public total employees" = "#568340", 
        "Education workers, as a share of public total employees" = "#B3242B", 
        "Health workers, as a share of public total employees" = "#003366", 
        "Other" = "#A9A9A9" # Gray for "Other"
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
  
  # Define the initial world map render
  output$worldMap <- renderLeaflet({
    leaflet(world_spdf) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2)  # Adjust view to show the world
  })
  # Reactive expression to filter data based on selected countries
  filtered_workforce_data <- reactive({
    req(input$countries_workforce)  # Ensure countries are selected
    public_sector_workforce %>%
      filter(country_name %in% input$countries_workforce) %>%
      group_by(country_name, indicator_name) %>%
      summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")
  })
  
  # Update the map based on indicator selection
  observe({
    req(input$indicatorSelect)
    
    # Filter the dataset to get countries that reported data
    reported_countries <- data_wwbi %>%
      filter(!is.na(.data[[paste0("year_", 2022)]])) %>%
      pull(country_name)
    
    leafletProxy("worldMap") %>%
      clearShapes() %>%
      addPolygons(
        data = world_spdf,
        fillColor = ~ifelse(world_spdf$country_name %in% reported_countries, "#28a745", "#CCCCCC"),
        fillOpacity = 0.7,
        color = "#FFFFFF",
        weight = 1,
        highlightOptions = highlightOptions(color = "#FFD700", weight = 2, fillOpacity = 0.9),
        label = ~name,
        labelOptions = labelOptions(style = list("font-weight" = "bold"), textsize = "12px", direction = "auto"),
        popup = ~paste("<strong>Country:</strong>", name)
      )
  })
  
  output$variableTable <- renderDT({
    data_wwbi %>%
      select(country_name, indicator_name, matches("^year_20(1[0-9]|2[0-2])"))
  })
  # Render Data Table of variables
  output$variableTable <- renderDT({
    data_wwbi %>%
      select(country_name, indicator_name, matches("^year_20(1[0-9]|2[0-2])"))
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

# Test ----

# Define UI ----

ui <- fluidPage(
  titlePanel("Dot Plot: Wage Bill vs. GDP per Capita (Log Scale)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_countries",
        label = "Select Countries:",
        choices = unique(merged_data$country_name),
        selected = unique(merged_data$country_name)[1:3], # Default selection
        multiple = TRUE
      )
    ),
    
    mainPanel(
      plotlyOutput("dot_plot")
    )
  )
)

# Define Server ----

# Server function

server <- function(input, output) {
  
  # Render the dot plot
  output$dot_plot <- renderPlotly({
    
    # Filter merged_data based on selected countries
    filtered_data <- merged_data %>%
      filter(country_name %in% input$selected_countries)
    
    # Get the first country for highlighting
    first_country <- input$selected_countries[1]
    
    # Create color column: first country will have a different color
    filtered_data <- filtered_data %>%
      mutate(color = ifelse(country_name == first_country, "#B3242B", "#003366"))
    
    # Fit a linear model for the trendline (log_gdp vs indicator_value)
    trendline_model <- lm(indicator_value ~ log_gdp, data = filtered_data)
    
    # Get fitted values for the trendline
    trendline_values <- predict(trendline_model, newdata = filtered_data)
    
    # Create the plot
    plot <- plot_ly() %>%
      # Data points for countries
      add_trace(
        data = filtered_data,
        x = ~log_gdp,
        y = ~indicator_value,
        type = "scatter",
        mode = "markers+text",  # Add dots for countries
        text = ~country_code,  # Display country code as label
        textposition = "top center",  # Position labels above the dots
        marker = list(
          size = 10,
          color = ~color,  # Use the color column for different colors
          opacity = 0.7
        )
      ) %>%
      # Trendline (only line, no dots for trendline)
      add_trace(
        x = filtered_data$log_gdp,
        y = trendline_values,
        type = "scatter",
        mode = "lines",  # Only lines for the trendline
        line = list(color = "gray", dash = "dash"),
        name = "Trendline"  # This is used for the legend, but we will hide it
      ) %>%
      layout(
        title = "Wage Bill vs. Log(GDP per Capita)",
        xaxis = list(
          title = "Log(GDP per Capita, 2015)",  # Axis label for x-axis
          showticklabels = TRUE  # Show tick labels on x-axis
        ),
        yaxis = list(
          title = "Wage Bill",  # Axis label for y-axis
          showticklabels = TRUE  # Show tick labels on y-axis
        ),
        showlegend = FALSE  # Hide the legend
      )
    
    return(plot)
  })
}


# Run App ----

shinyApp(ui = ui, server = server)



# Define the UI- Tertiary Education

# Define the UI
ui <- fluidPage(
  titlePanel("Tertiary Education by Employment Sector and Country"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_countries",
        label = "Select Countries:",
        choices = unique(tertiary_education$country_name),
        selected = unique(tertiary_education$country_name)[1],  # Default selection
        multiple = TRUE                                         # Allow multiple selections
      )
    ),
    
    mainPanel(
      plotlyOutput("barPlot")  # Output the Plotly chart
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Render the Plotly bar graph
  output$barPlot <- renderPlotly({
    # Filter data based on selected countries
    filtered_data <- tertiary_education[tertiary_education$country_name %in% input$selected_countries, ]
    
    # Create the Plotly bar chart with specified colors
    plot_ly(
      data = filtered_data,
      x = ~country_name,                      # X-axis: Country name
      y = ~value_percentage,                  # Y-axis: Tertiary education percentages
      color = ~indicator_name,                        # Different color for Public/Private
      colors = c("Individuals with tertiary education as a share of public paid employees" = "#003366", "Individuals with tertiary education as a share of private paid employees" = "#B3242B"),  # Custom color mapping
      type = 'bar',                           # Bar chart
      barmode = 'group'                       # Group bars for Public/Private
    ) %>%
      layout(
        title = "Tertiary Education by Sector and Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Tertiary Education (%)"),
        bargap = 0.2  # Adjust gap between bars
      )
  })
}  

# Run the Shiny app
shinyApp(ui = ui, server = server)

# Define the UI- Public sector wage premium

# Define the UI

ui <- fluidPage(
  titlePanel("Public Sector Wage Premium (Compared to All Private Sector Workers)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_countries",
        label = "Select Countries:",
        choices = unique(public_wage_premium$country_name),
        selected = unique(public_wage_premium$country_name)[1],  # Default selection
        multiple = TRUE                                         # Allow multiple selections
      )
    ),
    
    mainPanel(
      plotlyOutput("dotPlot")  # Output the Plotly chart
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Render the Plotly dot plot
  output$dotPlot <- renderPlotly({
    # Filter data based on selected countries
    filtered_data <- public_wage_premium[public_wage_premium$country_name %in% input$selected_countries, ]
    
    # Create a new column to assign color based on the first selected country
    filtered_data$color <- ifelse(filtered_data$country_name == input$selected_countries[1], "red", "blue")
    
    # Create the Plotly dot plot
    plot_ly(
      data = filtered_data,
      x = ~country_name,                          # X-axis: Country name
      y = ~value_percentage,               # Y-axis: Wage premium percentage
      color = ~color,                              # Color by the new color column
      colors = c("#003366", "#B3242B"),                   # Custom color mapping
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
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

# Define the UI- Public sector wage premium by education level (compared to private formal workers)

# Define the UI
ui <- fluidPage(
  titlePanel("Public sector wage premium by education level (compared to private formal workers)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_countries",
        label = "Select Countries:",
        choices = unique(public_wage_premium_educ$country_name),
        selected = unique(public_wage_premium_educ$country_name)[1],  # Default selection
        multiple = FALSE                                        # Allow multiple selections
      )
    ),
    
    mainPanel(
      plotlyOutput("barPlot")  # Output the Plotly chart
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Render the Plotly bar graph
  output$barPlot <- renderPlotly({
    # Filter data based on selected countries
    filtered_data <- public_wage_premium_educ[public_wage_premium_educ$country_name %in% input$selected_countries, ]
    plot_ly(
      data = filtered_data,
      x = ~country_name,                         # X-axis: Country names (labels)
      y = ~value_percentage,                     # Y-axis: Public sector wage premium by education level
      color = ~indicator_name,                    # Color bars by indicator_name (Education level)
      colors = c("Public sector wage premium, by education level: No Education (compared to formal wage employees)" = "#003366",
                 "Public sector wage premium, by education level: Primary Education (compared to formal wage employees)" = "#B3242B", 
                 "Public sector wage premium, by education level: Secondary Education (compared to formal wage employees)" = "#333333", 
                 "Public sector wage premium, by education level: Tertiary Education (compared to formal wage employees)" = "#006400"),  # Custom color mapping
      type = 'bar',                              # Bar chart type
      text = ~paste('Country: ', country_name, '<br>', 'Value: ', value_percentage, '%'), # Tooltips
      hoverinfo = 'text'                          # Show tooltip info
    ) %>%
      layout(
        title = "Public Sector Wage Premium by Education Level (Compared to Private Formal Workers)",
        xaxis = list(
          title = "Country", 
          tickangle = 45, 
          tickmode = 'array', 
          tickvals = filtered_data$country_name,  # Set the country names as ticks
          ticktext = filtered_data$country_name   # Ensure the country names are displayed on the x-axis
        ),
        yaxis = list(title = "Wage Premium (%)"),  # Y-axis label
        barmode = 'group',                         # Group bars by education level
        bargap = 0.2,                              # Adjust gap between bars
        showlegend = TRUE,                         # Show legend to distinguish between education levels
        legend = list(title = list(text = "Education Level")) # Set title for the legend
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


#Test 2 ----

# Define UI ----

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "WWB Indicators"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Variable List", tabName = "variableList", icon = icon("table")),
      menuItem("Indicators Status", tabName = "indicators", icon = icon("globe")),
      menuItem("Wage Bill Graphs", tabName = "wageBillGraphs", icon = icon("chart-line")), 
      menuItem("Wage Bill and GDP Graphs", tabName = "wageBillgdpGraphs", icon = icon("chart-line")),
      menuItem("Public Sector Graphs", tabName = "publicSectorGraphs", icon = icon("chart-line")), 
      menuItem("Public Sector Workforce Graphs", tabName = "publicSectorWorkforceGraphs", icon = icon("chart-line")), 
      menuItem("Gender Workforce Graphs", tabName = "genderWorkforceGraphs", icon = icon("chart-line")), 
      menuItem("Tertiary Education Graphs", tabName = "educationGraphs", icon = icon("chart-line"))
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
      tabItem(tabName = "wageBillGraphs",
              fluidRow(
                box(title = "WWB Indicator Selection", status = "primary", solidHeader = TRUE, width = 4,
                    selectInput("indicator", "Select a WWB Indicator", 
                                choices = c("Wage bill (as % of public expenditure) over time", 
                                            "Wage bill as a percentage of GDP"))
                ),
                box(title = "Country Selection", status = "primary", solidHeader = TRUE, width = 8,
                    selectInput("countries", "Countries", 
                                choices = unique(filtered_data$country_name), 
                                selected = unique(filtered_data$country_name)[1], 
                                multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "Graph", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("plot")
                )
              )
      ),
      # Wage Bill and GDP Graphs Tab
      tabItem(tabName = "wageBillgdpGraphs",
              fluidRow(
                box(title = "Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("gdp_countries", "Select Countries", 
                                choices = unique(merged_data$country_name),
                                selected = unique(merged_data$country_name)[1:3], # Default selection
                                multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "Wage Bill and GDP Graphs", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("dot_plot", height = "600px")
                )
              )
      ),
      # Public Sector Graphs Tab
      tabItem(tabName = "publicSectorGraphs",
              fluidRow(
                box(title = "First Graph - Multi-Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("countries_first", "Select Countries", 
                                choices = unique(public_sector_emp_temp$country_name), 
                                multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "First Graph", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("firstGraph")
                )
              )
      ),
      # Public Sector Workforce Graphs Tab
      tabItem(tabName = "publicSectorWorkforceGraphs",
              fluidRow(
                box(title = "Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("workforce_countries", "Select Countries", 
                                choices = unique(public_sector_workforce$country_name), 
                                multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "Stacked Bar Chart", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("stackedBarGraph", height = "600px")
                )
              )
      ),
      # Gender Workforce Graphs Tab
      tabItem(tabName = "genderWorkforceGraphs",
              fluidRow(
                box(title = "Country Selection (Multiple)", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("gender_countries", "Select Countries", 
                                choices = unique(gender_workforce$country_name), 
                                multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "Gender Workforce Graph", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("employment_plot", height = "600px")
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
      # Indicators Tab
      tabItem(tabName = "indicators",
              fluidRow(
                box(title = "Indicator Status Across Countries", status = "primary", solidHeader = TRUE, width = 12,
                    "This map shows which countries have reported data for the selected indicator."
                )
              ),
              fluidRow(
                box(title = "World Map", status = "primary", solidHeader = TRUE, width = 12,
                    leafletOutput("worldMap", height = 500)
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
    
    if (input$indicator == "Wage bill as a percentage of GDP") {
      # Filter wage_bill_gdp dataset based on selected countries
      data <- wage_bill_gdp %>% filter(country_name %in% input$countries)
    } else {
      data <- filtered_data %>% filter(country_name %in% input$countries)
    }
    return(data)
  })
  
  # Render Plotly plot for the main indicator
  output$plot <- renderPlotly({
    data_to_plot <- selected_data()
    
    max_year <- max(data_to_plot$year, na.rm = TRUE)
    
    last_year_data <- data_to_plot %>%
      group_by(country_name) %>%
      filter(year == max_year) %>%
      ungroup() %>%
      select(country_name, year, value)
    
    title_text <- ifelse(input$indicator == "Wage bill as a percentage of GDP",
                         "Wage Bill as % of GDP Over Time",
                         "Wage Bill as % of Public Expenditure Over Time")
    plot_mode <- ifelse(input$indicator == "Wage bill as a percentage of GDP", 'lines+markers', 'lines+markers')
    
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
    
    plot 
  })
  
  #GDP graphs 
  
  # Render the dot plot
  output$dot_plot <- renderPlotly({
    
    # Filter merged_data based on selected countries
    filtered_data <- merged_data %>%
      filter(country_name %in% input$gdp_countries)
    
    # Handle case where no countries are selected
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    # Get the first country for highlighting
    first_country <- input$gdp_countries[1]
    
    # Create a color column: first country will have a different color
    filtered_data <- filtered_data %>%
      mutate(color = ifelse(country_name == first_country, "#B3242B", "#003366"))
    
    # Fit a linear model for the trendline (log_gdp vs indicator_value)
    trendline_model <- lm(indicator_value ~ log_gdp, data = filtered_data)
    
    # Predict fitted values for the trendline
    filtered_data <- filtered_data %>%
      mutate(trendline_fit = predict(trendline_model, newdata = filtered_data))
    
    # Create the plot
    plot_gdp <- plot_ly() %>%
      # Add data points for countries
      add_trace(
        data = filtered_data,
        x = ~log_gdp,
        y = ~indicator_value,
        type = "scatter",
        mode = "markers+text",  # Add dots and labels
        text = ~country_code,  # Display country code as labels
        textposition = "top center",  # Position labels above the dots
        marker = list(
          size = 10,
          color = ~color,  # Use the color column for different colors
          opacity = 0.7
        ),
        name = "Country Points"
      ) %>%
      # Add the trendline
      add_trace(
        data = filtered_data,
        x = ~log_gdp,
        y = ~trendline_fit,
        type = "scatter",
        mode = "lines",  # Only lines for the trendline
        line = list(color = "gray", dash = "dash"),
        name = "Trendline"
      ) %>%
      layout(
        title = "Wage Bill vs. Log(GDP per Capita)",
        xaxis = list(
          title = "Log(GDP per Capita, 2015)",  # Axis label for x-axis
          showticklabels = TRUE
        ),
        yaxis = list(
          title = "Wage Bill",  # Axis label for y-axis
          showticklabels = TRUE
        ),
        showlegend = FALSE  # Hide the legend
      )
    
    # Return the final plot
    return(plot_gdp)
  })
  
  # First Graph (Multiple Countries)
  output$firstGraph <- renderPlotly({
    data_to_plot <- public_sector_emp_temp_last %>%
      filter(country_name %in% input$countries_first)
    
    data_to_plot_long <- data_to_plot %>%
      select(country_name, indicator_name, year, value) %>%
      mutate(indicator_name = factor(indicator_name))
    
    plot <- plot_ly(data = data_to_plot_long, 
                    x = ~country_name, 
                    y = ~value, 
                    color = ~indicator_name, 
                    type = 'scatter',
                    mode = 'markers',  
                    marker = list(size = 8)) %>%
      layout(title = "Public sector employment as a share of",
             xaxis = list(title = "Country", tickangle = 45),
             yaxis = list(title = "Value"),
             legend = list(title = list(text = "Indicator")))
    
    plot
  })
  # Second Graph (Single Country)
  output$secondGraph <- renderPlotly({
    data_to_plot <- public_sector_emp_temp %>%
      filter(country_name == input$country_second)  # Single country selection
    
    data_to_plot_long <- data_to_plot %>%
      select(year, indicator_name, value) %>%
      mutate(indicator_name = factor(indicator_name))
    
    plot <- plot_ly(
      data = data_to_plot_long, 
      x = ~year, 
      y = ~value, 
      color = ~indicator_name,  # Color each indicator differently
      text = ~paste("Value:", round(value, 2)),  # Tooltip with value
      type = 'scatter', 
      mode = 'lines+markers',  # Add lines and markers
      marker = list(size = 8)  # Set marker size
    ) %>%
      layout(
        title = paste("Public Sector Employment in", input$country_second, "Over Time"),
        xaxis = list(title = "Year", tickangle = 45, dtick = 2),
        yaxis = list(title = "Employment Value"),
        legend = list(title = list(text = "Indicator"))
      ) %>%
      add_annotations(
        x = ~year, 
        y = ~value,  # Add offset to place annotation above the point
        text = ~round(value, 2),  # Display value as annotation
        showarrow = FALSE,  # Remove arrows
        font = list(size = 12, color = "black"),
        xanchor = "center",  # Center annotation horizontally
        yanchor = "bottom"   # Position annotation above the point
      )
    
    plot
  })
  
  # Reactive expression to filter workforce data
  filtered_workforce_data <- reactive({
    req(input$countries_first) # Ensure input is not null
    public_sector_workforce %>%
      filter(country_name %in% input$countries_first) %>%
      group_by(country_name, indicator_name) %>%
      summarise(
        value_percentage = mean(value_percentage, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # Render the stacked bar graph
  output$stackedBarGraph <- renderPlotly({
    data_to_plot <- filtered_workforce_data()
    req(nrow(data_to_plot) > 0) # Ensure there's data to plot
    
    plot_ly(
      data = data_to_plot,
      x = ~country_name,
      y = ~value_percentage,
      color = ~indicator_name,
      type = "bar",
      text = ~paste0(round(value_percentage, 1), "%"), # Add percentage labels
      textposition = "auto",
      colors = c(
        "Public Administration workers, as a share of public total employees" = "#568340", 
        "Education workers, as a share of public total employees" = "#B3242B", 
        "Health workers, as a share of public total employees" = "#003366"
      )
    ) %>%
      layout(
        barmode = "stack",
        title = "Public Workforce Distribution by Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Workforce Distribution (%)"),
        legend = list(title = list(text = "Indicator"))
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
              "Health workers, as a share of public total employees" = "#003366"
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
  
  #Gender Workforce 
  output$employment_plot <- renderPlotly({
    # Filter data based on selected countries
    filtered_data <- gender_workforce %>%
      filter(country_name %in% input$countries_workforce)
    
    # Calculate the mean for the public sector for each country
    public_means <- filtered_data %>%
      filter(indicator_name == "Females, as a share of public paid employees") %>%
      group_by(country_name) %>%
      summarize(public_mean = mean(value_percentage, na.rm = TRUE)) %>%
      ungroup()
    
    # Calculate the mean for the private sector for each country
    private_means <- filtered_data %>%
      filter(indicator_name == "Females, as a share of private paid employees") %>%
      group_by(country_name) %>%
      summarize(private_mean = mean(value_percentage, na.rm = TRUE)) %>%
      ungroup()
    
    # Create the plot
    plot <- plot_ly(
      data = public_means,
      x = ~country_name,
      y = ~public_mean,
      type = 'bar',
      color = I("#003366"),  # Color for public sector mean
      text = ~paste("Public Mean: ", round(public_mean, 2), "%"), # Add hover text for the public mean
      hoverinfo = "text",  # Show hover info
      name = "Public Sector Mean",
      showlegend = TRUE
    ) %>%
      add_trace(
        data = private_means,
        x = ~country_name,
        y = ~private_mean,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, color = "#B3242B"),
        name = "Private Sector Mean",
        text = ~paste("Private Mean: ", round(private_mean, 2), "%"),
        hoverinfo = "text",  # Show hover info for the private sector mean
        showlegend = FALSE # Hide the legend for the mean dots
      ) %>%
      layout(
        barmode = "group",
        title = "Female Employment by Sector (Mean)",
        xaxis = list(
          title = "Country",
          tickmode = 'array', 
          tickvals = public_means$country_name,  # Explicitly set the tick values
          ticktext = public_means$country_name   # Match tick values to countries
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
 
  # Define the initial world map render
  output$worldMap <- renderLeaflet({
    leaflet(world_spdf) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2)  # Adjust view to show the world
  })
  # Reactive expression to filter data based on selected countries
  filtered_workforce_data <- reactive({
    req(input$countries_workforce)  # Ensure countries are selected
    public_sector_workforce %>%
      filter(country_name %in% input$countries_workforce) %>%
      group_by(country_name, indicator_name) %>%
      summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")
  })
  
  # Update the map based on indicator selection
  observe({
    req(input$indicatorSelect)
    
    # Filter the dataset to get countries that reported data
    reported_countries <- data_wwbi %>%
      filter(!is.na(.data[[paste0("year_", 2022)]])) %>%
      pull(country_name)
    
    leafletProxy("worldMap") %>%
      clearShapes() %>%
      addPolygons(
        data = world_spdf,
        fillColor = ~ifelse(world_spdf$country_name %in% reported_countries, "#28a745", "#CCCCCC"),
        fillOpacity = 0.7,
        color = "#FFFFFF",
        weight = 1,
        highlightOptions = highlightOptions(color = "#FFD700", weight = 2, fillOpacity = 0.9),
        label = ~name,
        labelOptions = labelOptions(style = list("font-weight" = "bold"), textsize = "12px", direction = "auto"),
        popup = ~paste("<strong>Country:</strong>", name)
      )
  })
  
  output$variableTable <- renderDT({
    data_wwbi %>%
      select(country_name, indicator_name, matches("^year_20(1[0-9]|2[0-2])"))
  })
  # Render Data Table of variables
  output$variableTable <- renderDT({
    data_wwbi %>%
      select(country_name, indicator_name, matches("^year_20(1[0-9]|2[0-2])"))
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
