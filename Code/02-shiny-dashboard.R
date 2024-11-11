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

# View the reshaped data
print(selected_data_long)



# Filter the data for the specific indicator "Wage bill as a percentage of Public Expenditure"

filtered_data <- data_wwbi[data_wwbi$indicator_name == "Wage bill as a percentage of Public Expenditure", ]


filtered_data <- filtered_data %>%
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
                                                               "Public sector employment, as a share of paid employment"), ]

public_sector_emp <- public_sector_emp %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #2015 obs

# Keep the last year available for each country
public_sector_emp <- public_sector_emp %>%
  filter(!is.na(value)) %>%                      # Keep rows where `value` is not NA
  group_by(country_name) %>%                      # Group by country_name (or any other variable)
  filter(year == max(year[!is.na(value)])) %>%   # Get the last available year for each country
  ungroup()                                      # Ungroup the data

# Define UI ----


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "WWB Indicators"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
      menuItem("Variable List", tabName = "variableList", icon = icon("table")),
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-line")), 
      menuItem("Indicators Status", tabName = "indicators", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Dashboard Description", status = "primary", solidHeader = TRUE, width = 12,
                    "Welcome to the World Bank Indicators Dashboard!")
              )
      ),
      tabItem(tabName = "widgets",
              fluidRow(
                infoBoxOutput("numberIndicatorsBox", width = 6),
                infoBoxOutput("numberCountriesBox", width = 6),
                infoBoxOutput("temporalCoverageAnnualBox", width = 6),
                infoBoxOutput("temporalCoverageYearsBox", width = 6),
                infoBoxOutput("lastUpdatedBox", width = 6)
              )
      ),
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
      tabItem(tabName = "graphs",
              fluidRow(
                box(selectInput("indicator", "Select a WWB Indicator", 
                                choices = c("Wage bill (as % of public expenditure) over time", "Wage bill as a percentage of GDP")),
                    title = "Worldwide Bureaucracy Indicators", status = "primary", solidHeader = TRUE, width = 4),
                box(selectInput('countries', 'Countries', 
                                choices = unique(filtered_data$country_name), 
                                selected = unique(filtered_data$country_name)[1], 
                                multiple = TRUE),
                    width = 8)
              ),
              mainPanel(
                plotlyOutput('plot')
              )
      ),
      tabItem(tabName = "indicators",
              fluidRow(
                box(title = "Indicator Status Across Countries", status = "primary", solidHeader = TRUE, width = 12,
                    "This map shows which countries have reported data for the selected indicator."
                ),
                box(title = "Select Indicator", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("indicatorSelect", "Choose Indicator", 
                                choices = c("Gender", "Education Level", "Age", "Labor Status", "Wage", "Industry"))
                ),
                box(title = "World Map", status = "primary", solidHeader = TRUE, width = 12,
                    leafletOutput("worldMap", height = 500)
                )
              )
      )
    )
  )
)


## Define Server ----

server <- function(input, output, session) {
  
  # Reactive expression to select appropriate dataset based on indicator
  selected_data <- reactive({
    if (input$indicator == "Wage bill as a percentage of GDP") {
      # Assuming wage_bill_gdp is a data frame that contains the relevant data
      data <- wage_bill_gdp[wage_bill_gdp$country_name %in% input$countries, ]
    } else {
      # Assuming filtered_data is a data frame that contains the relevant data
      data <- filtered_data[filtered_data$country_name %in% input$countries, ]
    }
    return(data)
  })
  
  # Render Plotly plot based on selected indicator
  output$plot <- renderPlotly({
    data_to_plot <- selected_data()
    
    # Get the final value (last year data) for each country
    last_year_data <- data_to_plot %>%
      group_by(country_name) %>%
      filter(year == max(year)) %>%
      ungroup() %>%
      select(country_name, year, value)
    
    # Set title and mode based on selected indicator
    if (input$indicator == "Wage bill as a percentage of GDP") {
      title_text <- "Wage Bill as % of GDP Over Time"
      plot_mode <- 'markers'  # Dot plot for GDP indicator
    } else {
      title_text <- "Wage Bill as % of Public Expenditure Over Time"
      plot_mode <- 'lines+markers'  # Line plot for public expenditure indicator
    }
    
    # Create the plot
    plot <- plot_ly(data = data_to_plot, 
                    x = ~year, 
                    y = ~value, 
                    color = ~country_name, 
                    type = 'scatter', 
                    mode = plot_mode,
                    marker = list(size = 8)) %>%
      layout(title = title_text,
             xaxis = list(title = "Year", dtick = 5),
             yaxis = list(title = ifelse(input$indicator == "Wage bill as a percentage of GDP", 
                                         "Wage Bill (% of GDP)", "Wage Bill (%)")),
             legend = list(title = list(text = "Country")))
    
    # Add annotations for the last year's value for each country
    for (i in 1:nrow(last_year_data)) {
      plot <- plot %>%
        add_annotations(
          x = last_year_data$year[i], 
          y = last_year_data$value[i],
          text = paste(round(last_year_data$value[i], 2)),
          showarrow = FALSE,  # No arrow for annotation
          font = list(size = 12, color = "black"),
          bgcolor = "white",
          xanchor = "center",
          yanchor = "bottom"
        )
    }
    
    plot
  })
  
  # Update world map based on selected indicator
  observe({
    req(input$indicatorSelect)
    
    reported_countries <- data_wwbi %>%
      filter(!is.na(.data[[paste0("year_", 2022)]])) %>%
      pull(country_name)
    
    leafletProxy("worldMap") %>%
      clearShapes() %>%
      addPolygons(data = world_spdf,
                  fillColor = ~ifelse(world_spdf$name %in% reported_countries, "#28a745", "#CCCCCC"),
                  fillOpacity = 0.7,
                  color = "#FFFFFF",
                  weight = 1,
                  highlightOptions = highlightOptions(color = "#FFD700", weight = 2, fillOpacity = 0.9),
                  label = ~name,
                  labelOptions = labelOptions(style = list("font-weight" = "bold"), textsize = "12px", direction = "auto"),
                  popup = ~paste("<strong>Country:</strong>", name)
      )
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


shinyApp(ui, server)








###############################################################################



ui <- fluidPage(
  headerPanel('Example'),
  sidebarPanel(
    selectInput('countries', 'Countries', 
                choices = unique(public_sector_emp$country_name), 
                selected = unique(public_sector_emp$country_name)[1],  # Default to the first country
                multiple = TRUE)  # Allow multiple countries to be selected
  ),
  mainPanel(
    plotlyOutput('plot')
  )
)



server <- function(input, output) {
  
  # Reactive expression to filter the data based on selected countries
  filtered_data <- reactive({
    public_sector_emp %>%
      filter(country_name %in% input$countries)
  })
  
  # Render Plotly plot
  output$plot <- renderPlotly({
    data_to_plot <- filtered_data()  # Get filtered data
    
    # Ensure the data is in the correct format (long format)
    data_to_plot_long <- data_to_plot %>%
      select(country_name, indicator_name, year, value) %>%
      mutate(indicator_name = factor(indicator_name))
    
    # Plotly: scatter plot with different colors for each indicator
    plot <- plot_ly(data = data_to_plot_long, 
                    x = ~country_name, 
                    y = ~value, 
                    color = ~indicator_name,  # Different color for each indicator
                    type = 'scatter',
                    mode = 'markers',  # Scatter plot (dots)
                    marker = list(size = 8)) %>%
      layout(title = "Public sector employment as a share of",
             xaxis = list(title = "Country", tickangle = 45),  # Rotate x-axis labels if needed
             yaxis = list(title = "Value"),
             legend = list(title = list(text = "Indicator")))
    
    plot
  })
}


shinyApp(ui, server)






# Define UI ----


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "WWB Indicators"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
      menuItem("Variable List", tabName = "variableList", icon = icon("table")),
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-line")), 
      menuItem("Indicators Status", tabName = "indicators", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Dashboard Description", status = "primary", solidHeader = TRUE, width = 12,
                    "Welcome to the World Bank Indicators Dashboard!")
              )
      ),
      tabItem(tabName = "widgets",
              fluidRow(
                infoBoxOutput("numberIndicatorsBox", width = 6),
                infoBoxOutput("numberCountriesBox", width = 6),
                infoBoxOutput("temporalCoverageAnnualBox", width = 6),
                infoBoxOutput("temporalCoverageYearsBox", width = 6),
                infoBoxOutput("lastUpdatedBox", width = 6)
              )
      ),
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
      tabItem(tabName = "graphs",
              fluidRow(
                box(selectInput("indicator", "Select a WWB Indicator", 
                                choices = c("Wage bill (as % of public expenditure) over time", "Wage bill as a percentage of GDP", "Public sector employment as a share of")),
                    title = "Worldwide Bureaucracy Indicators", status = "primary", solidHeader = TRUE, width = 4),
                box(selectInput('countries', 'Countries', 
                                choices = unique(filtered_data$country_name), 
                                selected = unique(filtered_data$country_name)[1], 
                                multiple = TRUE),
                    width = 8)
              ),
              mainPanel(
                plotlyOutput('plot')
              )
      ),
      tabItem(tabName = "indicators",
              fluidRow(
                box(title = "Indicator Status Across Countries", status = "primary", solidHeader = TRUE, width = 12,
                    "This map shows which countries have reported data for the selected indicator."
                ),
                box(title = "Select Indicator", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("indicatorSelect", "Choose Indicator", 
                                choices = c("Gender", "Education Level", "Age", "Labor Status", "Wage", "Industry"))
                ),
                box(title = "World Map", status = "primary", solidHeader = TRUE, width = 12,
                    leafletOutput("worldMap", height = 500)
                )
              )
      )
    )
  )
)


## Define Server ----

server <- function(input, output, session) {
  
  # Reactive expression to select the appropriate dataset based on the indicator
  selected_data <- reactive({
    req(input$countries)  # Ensure 'countries' input is available
    
    if (input$indicator == "Wage bill as a percentage of GDP") {
      # Filter wage_bill_gdp dataset based on selected countries
      data <- wage_bill_gdp %>% filter(country_name %in% input$countries)
    } else {
      # Filter filtered_data dataset based on selected countries
      data <- filtered_data() %>% filter(country_name %in% input$countries)
    }
    return(data)
  })
  
  # Render Plotly plot for the main indicator
  output$plot <- renderPlotly({
    data_to_plot <- selected_data()
    
    # Calculate the maximum year to get the latest data points
    max_year <- max(data_to_plot$year, na.rm = TRUE)
    
    # Filter data to the latest year for annotation
    last_year_data <- data_to_plot %>%
      group_by(country_name) %>%
      filter(year == max_year) %>%
      ungroup() %>%
      select(country_name, year, value)
    
    # Set plot title and mode based on the selected indicator
    title_text <- ifelse(input$indicator == "Wage bill as a percentage of GDP",
                         "Wage Bill as % of GDP Over Time",
                         "Wage Bill as % of Public Expenditure Over Time")
    plot_mode <- ifelse(input$indicator == "Wage bill as a percentage of GDP", 'markers', 'lines+markers')
    
    # Create the Plotly plot
    plot <- plot_ly(data = data_to_plot, 
                    x = ~year, 
                    y = ~value, 
                    color = ~country_name, 
                    type = 'scatter', 
                    mode = plot_mode,
                    marker = list(size = 8)) %>%
      layout(title = title_text,
             xaxis = list(title = "Year", dtick = 5),
             yaxis = list(title = ifelse(input$indicator == "Wage bill as a percentage of GDP", 
                                         "Wage Bill (% of GDP)", "Wage Bill (%)")),
             legend = list(title = list(text = "Country")))
    
    # Add annotations for each country's last year's value
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
  
  # Render third graph: Public Sector Employment Indicators by Country
  output$thirdGraph <- renderPlotly({
    data_to_plot <- filtered_data()  # Call filtered_data as a reactive expression
    
    # Reshape data to long format
    data_to_plot_long <- data_to_plot %>%
      select(country_name, indicator_name, year, value) %>%
      mutate(indicator_name = factor(indicator_name))
    
    # Create scatter plot for employment indicators
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
  
  # Update the world map based on the selected indicator
  observe({
    req(input$indicatorSelect)  # Ensure indicatorSelect is available
    
    reported_countries <- data_wwbi %>%
      filter(!is.na(.data[[paste0("year_", 2022)]])) %>%
      pull(country_name)
    
    leafletProxy("worldMap") %>%
      clearShapes() %>%
      addPolygons(data = world_spdf,
                  fillColor = ~ifelse(world_spdf$name %in% reported_countries, "#28a745", "#CCCCCC"),
                  fillOpacity = 0.7,
                  color = "#FFFFFF",
                  weight = 1,
                  highlightOptions = highlightOptions(color = "#FFD700", weight = 2, fillOpacity = 0.9),
                  label = ~name,
                  labelOptions = labelOptions(style = list("font-weight" = "bold"), textsize = "12px", direction = "auto"),
                  popup = ~paste("<strong>Country:</strong>", name)
      )
  })
  
  # Render Data Table of variables
  output$variableTable <- renderDT({
    data_wwbi %>%
      select(country_name, indicator_name, matches("^year_20(1[0-9]|2[0-2])"))
  })
  
  # Dummy outputs for info boxes
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
    infoBox("Years Available", "2022", icon = icon("calendar"), color = "blue")
  })
  
  output$lastUpdatedBox <- renderInfoBox({
    infoBox("Last Updated", "November 2024", icon = icon("clock"), color = "blue")
  })
}



shinyApp(ui, server)



