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

## Define UI ----

# Define UI
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
                                choices = c("Wage bill (as % of public expenditure) over time")), # Select indicator
                    title = "Worldwide Bureaucracy Indicators", status = "primary", solidHeader = TRUE, width = 4),
                box(selectInput("countryFilter", "Select Country", 
                                choices = unique(filtered_data$country_name), selected = NULL, multiple = TRUE),
                    width = 4),
                mainPanel(
                  plotlyOutput('plot')  # Render plotly output
                )
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

# Server ----
server <- function(input, output, session) {
  
  # Filter data based on indicator and selected countries
  filtered_data <- reactive({
    selected_data_long %>%
      filter(indicator_name == "Wage bill as a percentage of Public Expenditure",
             country_name %in% input$countryFilter)
  })
  
  # Render the plotly plot
  output$plot <- renderPlotly({
    req(filtered_data())
    
    plot_ly(filtered_data(), 
            x = ~year, 
            y = ~value, 
            color = ~country_name, 
            type = 'scatter', 
            mode = 'lines+markers',
            line = list(width = 2), 
            marker = list(size = 6)) %>%
      layout(title = "Wage Bill as a Percentage of Public Expenditure: Trend Over Time",
             xaxis = list(title = "Year", dtick = 5),  # 5-year intervals
             yaxis = list(title = "Wage Bill (%)"),
             legend = list(title = list(text = "Country")))
  })
  
  # Update world map based on selected indicator
  observe({
    req(input$indicatorSelect)
    
    # Filter countries with reported data for the selected indicator
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
  
  # Dummy outputs for widgets to prevent errors (define actual values in your code)
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

# Run the app
shinyApp(ui, server)

# Run the app
shinyApp(ui, server)



# Filter the data for the specific indicator "Wage bill as a percentage of Public Expenditure"

filtered_data <- data_wwbi[data_wwbi$indicator_name == "Wage bill as a percentage of Public Expenditure", ]


filtered_data <- filtered_data %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) 


# Create the plot
plot <- plot_ly(filtered_data, 
                x = ~year, 
                y = ~value, 
                color = ~country_name, 
                type = 'scatter', 
                mode = 'lines+markers',
                line = list(width = 2), 
                marker = list(size = 6)) %>%
  layout(title = "Wage Bill as a Percentage of Public Expenditure: Trend Over Time",
         xaxis = list(title = "Year", dtick = 5),  # 5-year intervals
         yaxis = list(title = "Wage Bill (%)"),
         legend = list(title = list(text = "Country")))

print(plot)




library(shiny)
library(plotly)

ui <- fluidPage(
  headerPanel('Example'),
  sidebarPanel(
    selectInput('countries', 'Countries', 
                choices = unique(filtered_data$country_name), 
                selected = unique(filtered_data$country_name)[1],  # Default to the first country
                multiple = TRUE)  # Allow multiple countries to be selected
  ),
  mainPanel(
    plotlyOutput('plot')
  )
)

server <- function(input, output) {
  
  # Reactive expression to filter data based on selected countries
  filtered_countries_data <- reactive({
    filtered_data[filtered_data$country_name %in% input$countries, ]
  })
  
  # Render Plotly plot based on selected countries
  output$plot <- renderPlotly({
    data_to_plot <- filtered_countries_data()  # Get filtered data based on selected countries
    
    # Extract the most recent year and its value for each country
    last_year_data <- data_to_plot %>%
      group_by(country_name) %>%
      filter(year == max(year)) %>%
      ungroup() %>%
      select(country_name, year, value)
    
    # Create the plot
    plot <- plot_ly(data_to_plot, 
                    x = ~year, 
                    y = ~value, 
                    color = ~country_name,  # Different colors for each country
                    type = 'scatter', 
                    mode = 'lines+markers',
                    line = list(width = 2), 
                    marker = list(size = 6)) %>%
      layout(title = paste("Wage Bill as a Percentage of Public Expenditure for Selected Countries Over Time"),
             xaxis = list(title = "Year", dtick = 5),  # 5-year intervals
             yaxis = list(title = "Wage Bill (%)"),
             legend = list(title = list(text = "Country")))
    
    # Add annotations for the last year values for each country without arrows
    for(i in 1:nrow(last_year_data)) {
      plot <- plot %>%
        add_annotations(
          x = last_year_data$year[i], 
          y = last_year_data$value[i],
          text = paste(round(last_year_data$value[i], 2)),
          showarrow = FALSE,  # Remove the arrow
          font = list(size = 12, color = "black"),
          bgcolor = "white",
          xanchor = "center",
          yanchor = "bottom"
        )
    }
    
    plot
  })
}

shinyApp(ui, server)


