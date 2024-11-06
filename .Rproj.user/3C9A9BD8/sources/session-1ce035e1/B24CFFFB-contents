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


### INITIAL COMMANDS ----

#Set data path 

data_path <- "C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/Data/Intermediate"

#Load indicators data set 

wwbi <- read_dta(file.path(data_path, "data_wwbi.dta"))


# Load world spatial data

world_spdf <- ne_countries(scale = "medium", returnclass = "sf")

# Create a color palette for countries
color_palette <- colorFactor(c("lightgreen", "lightgray"), domain = c("reported", "not_reported"))



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
countries <- unique(data_wwbi$country_name)  # Extract unique country names from the dataset

# Define UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "WWB Indicators"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
      menuItem("Variable List", tabName = "variableList", icon = icon("table")),
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-simple")), 
      menuItem("Indicators Status", tabName = "indicators", icon = icon("chart-simple"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Dashboard Description", status = "primary", solidHeader = TRUE, width = 12,
                    "Welcome to the World Bank Indicators Dashboard!"
                )
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
                box(title = "Available Variables", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("variableTable")
                )
              )
      ),
      
      tabItem(tabName = "graphs",
              fluidRow(
                box(selectInput("indicator", "Select a WWB Indicator", 
                                choices = c("Wage bill (as % of public expenditure) over time", "Wage bill (% of public expenditures) and GDP per capita", "Public sector employment", "Public sector employment over time", "Distribution of public sector workforce", "Sectoral distribution of public sector workforce", 
                                            "Female employment by sector", "Gender distribution in the public sector workforce", "Individuals with tertiary education by sector of employment", 
                                            "Formality levels", "Public sector wage premium (compared to all private sector workers)", 
                                            "Public sector wage premium (compared to all private sector workers)", 
                                            "Gender wage premium in the public sector", "Pay compression ratios (ratio of 90th/10th percentile earners)")),
                    title = "Worldwide Bureaucracy Indicators", status = "primary", solidHeader = TRUE, width = 4),
                box(selectInput("yearSelect", "Select Year", choices = years), width = 4),
                box(selectInput("countrySelect", "Select Country", choices = countries), width = 4)
              ),
              conditionalPanel(
                condition = "input.indicator == 'Wage Bill'",
                box(title = "Wage bill (as % of public expenditure) over time", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("genderPlot", width = "100%"),
                    downloadButton("downloadgenderPlot", "Download Plot", class = "btn btn-primary")
                )
              ),
              conditionalPanel(
                condition = "input.indicator == 'Wage Bill and GDP per capita'",
                box(title = "Wage Bill and GDP per capita", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("industryPlot", width = "100%"),
                    downloadButton("downloadindustryPlot", "Download Plot", class = "btn btn-primary")
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

# Define Server ----
server <- function(input, output, session) {
  
  output$numberIndicatorsBox <- renderInfoBox({
    infoBox("Number of Indicators", "300", icon = icon("flag"), color = "light-blue")
  })
  
  output$numberCountriesBox <- renderInfoBox({
    infoBox("Number of Countries", "200", icon = icon("earth-americas"), color = "light-blue")
  })
  
  output$temporalCoverageAnnualBox <- renderInfoBox({
    infoBox("Temporal Coverage", "Annual", icon = icon("check"), color = "light-blue")
  })
  
  output$temporalCoverageYearsBox <- renderInfoBox({
    infoBox("Temporal Coverage", "2000 to 2022", icon = icon("timeline"), color = "light-blue")
  })
  
  output$lastUpdatedBox <- renderInfoBox({
    infoBox("Last Updated on", "Sep 15, 2022", icon = icon("calendar"), color = "light-blue")
  })
  
  output$variableTable <- renderDT({
    datatable(data_wwbi, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  output$worldMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  observe({
    req(input$indicatorSelect, input$yearSelect)
    
    selected_year_column <- paste0("year_", input$yearSelect)
    
    reported_countries <- data_wwbi[!is.na(data_wwbi[[selected_year_column]]), "country_name"]
    
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
}

# Run the app ----
shinyApp(ui, server)
