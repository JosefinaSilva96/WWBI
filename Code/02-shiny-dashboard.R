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

public_sector_emp_temp <- public_sector_emp %>%
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




# Filter the data for the specific indicator "Characteristics of the public sector workforce"

public_sector_workforce <- data_wwbi[data_wwbi$indicator_name %in% c("Education workers, as a share of public total employees", 
                                                               "Health workers, as a share of public total employees", 
                                                               "Core Public Administration workers, as a share of public total employees"), ]

public_sector_workforce <- public_sector_workforce %>%
  pivot_longer(cols = starts_with("year_"), 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.numeric(gsub("year_", "", year))) %>%  # Clean the 'year' column
  filter(!is.na(value)) #1043 obs


public_sector_workforce <- public_sector_workforce %>%
  mutate(value_percentage = value * 100)


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
      menuItem("Gender Workforce Graphs", tabName = "genderWorkforceGraphs", icon = icon("chart-line"))
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
                                choices = unique(public_sector_emp$country_name), 
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
                box(
                  box(title = "Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                      selectInput("countries_workforce", 
                                  "Select Countries for Workforce Graph", 
                                  choices = unique(public_sector_workforce$country_name), 
                                  selected = unique(public_sector_workforce$country_name)[1], 
                                  multiple = TRUE)
                  )
                  
                )
              ),
              fluidRow(
                box(title = "Stacked Bar Chart", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("stackedBarGraph", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "Select Country (Second Graph)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("selected_country", 
                              "Select Country for Second Graph", 
                              choices = unique(public_sector_workforce$country_name), 
                              selected = NULL, 
                              multiple = FALSE)
                )
              ),
              fluidRow(
                box(
                  title = "Second Graph: Horizontal Stacked Bar (Single Country)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("messageOutput"),
                  plotlyOutput("horizontalStackedBar")
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
    data_to_plot <- public_sector_emp_temp %>%
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
    data_to_plot <- public_sector_emp %>%
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
        "Core Public Administration workers, as a share of public total employees" = "#568340", 
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
              "Core Public Administration workers, as a share of public total employees" = "#568340",
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
  titlePanel("Gender Workforce Distribution"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_country", 
        "Select Country", 
        choices = unique(gender_workforce$country_name), 
        selected = unique(gender_workforce$country_name)[1], 
        multiple = TRUE # Allow multiple country selection
      )
    ),
    mainPanel(
      plotlyOutput("employment_plot")
    )
  )
)

# Define Server ----
server <- function(input, output, session) {
  # Reactive value to store clicked point info
  clicked_point <- reactiveVal(NULL)
  
  output$employment_plot <- renderPlotly({
    # Filter data based on selected countries
    filtered_data <- gender_workforce %>%
      filter(country_name %in% input$selected_country)
    
    # Create the plot
    plot <- plot_ly(
      data = filtered_data %>% filter(indicator_name == "Females, as a share of public paid employees"),
      x = ~country_name,
      y = ~value_percentage,
      type = 'bar',
      color = I("#003366"),  # Color for public sector
      text = ~paste(indicator_name, ": ", value_percentage, "%"), # Add hover text
      hoverinfo = "text",  # Show hover info
      name = "Public Sector",
      textinfo = "none"  # Hide text by default above the bar
    ) %>%
      add_trace(
        data = filtered_data %>% filter(indicator_name == "Females, as a share of private paid employees"),
        x = ~country_name,
        y = ~value_percentage,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, color = "#B3242B"),
        name = "Private Sector",
        text = ~paste(indicator_name, ": ", value_percentage, "%"), # Add hover text for private sector
        hoverinfo = "text",  # Show hover info
        showlegend = FALSE # Hide the legend for scatter markers
      ) %>%
      layout(
        barmode = "group",
        title = "Female Employment by Sector",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Employment (%)"),
        legend = list(title = list(text = "Sector"))
      )
    
    # Update text for clicked point
    if (!is.null(clicked_point())) {
      plot <- plot %>%
        layout(
          annotations = list(
            x = clicked_point()$x,
            y = clicked_point()$y,
            text = clicked_point()$text,
            showarrow = TRUE,
            arrowhead = 7,
            ax = 0,
            ay = -40
          )
        )
    }
    
    return(plot)
  })
  
  # Capture the click event
  observeEvent(event_data("plotly_click"), {
    # Extract clicked data point
    click_data <- event_data("plotly_click")
    
    # Store the clicked point's info
    clicked_point(list(
      x = click_data$x,
      y = click_data$y,
      text = click_data$text
    ))
  })
}

# Run the App ----
shinyApp(ui, server)

 ###########################################################

#Test 2 ----

# Define UI ----

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "WWB Indicators"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
      menuItem("Variable List", tabName = "variableList", icon = icon("table")),
      menuItem("Wage Bill Graphs", tabName = "wageBillGraphs", icon = icon("chart-line")), 
      menuItem("Public Sector Graphs", tabName = "publicSectorGraphs", icon = icon("chart-line")), 
      menuItem("Public Sector Workforce Graphs", tabName = "publicSectorWorkforceGraphs", icon = icon("chart-line")), 
      menuItem("Indicators Status", tabName = "indicators", icon = icon("globe"))
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
                                choices = unique(public_sector_emp$country_name), 
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
                box(
                  box(title = "Country Selection", status = "primary", solidHeader = TRUE, width = 12,
                      selectInput("countries_workforce", 
                                  "Select Countries for Workforce Graph", 
                                  choices = unique(public_sector_workforce$country_name), 
                                  selected = unique(public_sector_workforce$country_name)[1], 
                                  multiple = TRUE)
                )
               
                )
              ),
              fluidRow(
                box(title = "Stacked Bar Chart", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("stackedBarGraph", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "Select Country (Second Graph)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("selected_country", 
                              "Select Country for Second Graph", 
                              choices = unique(public_sector_workforce$country_name), 
                              selected = NULL, 
                              multiple = FALSE)
                )
              ),
              fluidRow(
                box(
                  title = "Second Graph: Horizontal Stacked Bar (Single Country)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("messageOutput"),
                  plotlyOutput("horizontalStackedBar")
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
    data_to_plot <- public_sector_emp_temp %>%
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
    data_to_plot <- public_sector_emp %>%
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
        "Core Public Administration workers, as a share of public total employees" = "#568340", 
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
              "Core Public Administration workers, as a share of public total employees" = "#568340",
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
