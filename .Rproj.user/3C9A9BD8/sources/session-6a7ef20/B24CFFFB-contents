# Shiny Dashboard for Worldwide Bureaucracy Indicators

### Libraries

# List of required packages
packages <- c(
  "haven", "dplyr", "tidyr", "stringr", "labelled", "data.table",
  "ggplot2", "shiny", "shinythemes", "DT", "maps", "mapdata",
  "leaflet", "rnaturalearth", "sf", "plotly", "officer", "flextable",
  "viridis", "here", "glue", "colourpicker", "wbstats", "htmlwidgets",
  "bs4Dash", "countrycode", "bslib"
)

# Function to check and install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Install missing packages
sapply(packages, install_if_missing)

# Load all libraries
lapply(packages, library, character.only = TRUE)




### Load data sets ----

# Load the data sets

data_path <- file.path(getwd()) 

# Automatically detect the correct root folder (without "Code/")
if (basename(getwd()) == "Code") {
  data_path <- dirname(getwd())  # Move one level up if inside "Code/"
} else {
  data_path <- getwd()  # Use current directory if already correct
}

# Debugging: Print the detected data path to check correctness
print(paste("Using data path:", data_path))


data_wwbi     <- read_dta(file.path(data_path, "Data/data_wwbi.dta"))


data_gdp     <- read_dta(file.path(data_path, "Data/data_gdp.dta"))

gdp_2015     <- read_dta(file.path(data_path, "Data/gdp_2015.dta"))


world_spdf <- st_read(file.path(data_path, "Data/world_spatial.gpkg"))


selected_data_long     <- read_dta(file.path(data_path, "Data/selected_data_long.dta"))


data_wwbi_long     <- read_dta(file.path(data_path, "Data/data_wwbi_long.dta"))


wage_bill_publicexp     <- read_dta(file.path(data_path, "Data/wage_bill_publicexp.dta"))


wage_bill_gdp     <- read_dta(file.path(data_path, "Data/wage_bill_gdp.dta"))

public_sector_emp_temp <- readRDS(file.path(data_path, "Data", "public_sector_emp_temp.rds"))

public_sector_emp_temp <- readRDS(file.path(data_path, "Data", "public_sector_emp_temp.rds")) %>%
  mutate(
    across(where(~inherits(.x, "haven_labelled")), as_factor)  # Convert haven-labelled to factors
  )

public_sector_emp <- readRDS(file.path(data_path, "Data", "public_sector_emp.rds"))

public_sector_emp_temp_last <- readRDS(file.path(data_path, "Data", "public_sector_emp_temp_last.rds")) %>%
  mutate(
    across(where(~inherits(.x, "haven_labelled")), as_factor)  # Convert haven-labelled to factors
  )

public_sector_workforce     <- read_dta(file.path(data_path, "Data/public_sector_workforce.dta"))

public_sector_workforce_first_last     <- read_dta(file.path(data_path, "Data/public_sector_workforce_first_last.dta"))

gender_workforce     <- read_dta(file.path(data_path, "Data/gender_workforce.dta"))


data_indicator_wb     <- read_dta(file.path(data_path, "Data/data_indicator_wb.dta"))

merged_data     <- read_dta(file.path(data_path, "Data/merged_data.dta"))

tertiary_education     <- read_dta(file.path(data_path, "Data/tertiary_education.dta"))

public_wage_premium     <- read_dta(file.path(data_path, "Data/public_wage_premium.dta"))

public_wage_premium_educ     <- read_dta(file.path(data_path, "Data/public_wage_premium_educ.dta"))


gender_wage_premium <- readRDS(file.path(data_path, "Data", "gender_wage_premium.rds"))


gender_wage_premium_last <- readRDS(file.path(data_path, "Data", "gender_wage_premium_last.rds"))


gender_leadership <- readRDS(file.path(data_path, "Data", "gender_leadership.rds"))


gender_wage_premiumpublic <- readRDS(file.path(data_path, "Data", "gender_wage_premiumpublic.rds"))


pay_compression <- readRDS(file.path(data_path, "Data", "pay_compression.rds"))



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
      div(class = "nav-item", actionLink("nav_instructions", "Instructions")),
      div(class = "nav-item", actionLink("nav_metadata", "Metadata")),
      div(class = "nav-item", actionLink("nav_publications", "Publications")),
      
      # Collapsible Section - The Macro Fundamentals
      div(class = "nav-section", onclick = "toggleSection('macro_section')", "The Macro Fundamentals"),
      div(id = "macro_section", style = "display: none;",
          div(class = "nav-sub-item", actionLink("nav_wagebill", "Wage Bill Graphs")),
          div(class = "nav-sub-item", actionLink("nav_wagebill_gdp", "Wage Bill & GDP Graphs"))
      ),
      
      # Collapsible Section - The Size of the Public Sector
      div(class = "nav-section", onclick = "toggleSection('public_sector_section')", "The Size of the Public Sector"),
      div(id = "public_sector_section", style = "display: none;",
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
          div(class = "nav-sub-item", actionLink("nav_wagepremium", "Public Sector Wage Premium")),
          div(class = "nav-sub-item", actionLink("nav_wagepremium_gender", "Wage Premium Gender Graphs")), 
          div(class = "nav-sub-item", actionLink("nav_pay_compression", "Pay Compression Graphs"))
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
  observeEvent(input$nav_instructions,         { active_tab("instructions") })
  observeEvent(input$nav_metadata,          { active_tab("metadata") })
  observeEvent(input$nav_publications,          { active_tab("publications") })
  observeEvent(input$nav_wagebill,          { active_tab("wagebill") })
  observeEvent(input$nav_wagebill_gdp,      { active_tab("wagebill_gdp") })
  observeEvent(input$nav_public_workforce,  { active_tab("public_workforce") })
  observeEvent(input$nav_gender_workforce,  { active_tab("gender_workforce") })
  observeEvent(input$nav_education,         { active_tab("education") })
  observeEvent(input$nav_public_educ,       { active_tab("public_educ") })
  observeEvent(input$nav_public_graphs,     { active_tab("public_graphs") })
  observeEvent(input$nav_wagepremium_gender,{ active_tab("wagepremium_gender") })
  observeEvent(input$nav_female_leadership, { active_tab("female_leadership") })
  observeEvent(input$nav_wagepremium,       { active_tab("wagepremium") })
  observeEvent(input$nav_gender_wage_premium, { active_tab("gender_wage_premium") })
  observeEvent(input$nav_pay_compression, { active_tab("pay_compression") })
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
        ), 
        fluidRow(
          div(style = "border: 2px solid white; padding: 15px; border-radius: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              "Contact Information: Zahid Hasnain-zhasnain@worldbank.org and
                                    Daniel Rogger-drogger@worldbank.org")
        )
      )
    } else if (tab == "instructions") {
      tagList(
        h3("Instructions Manual"),
        fluidRow(
          div(style = "border: 2px solid white; padding: 15px; border-radius: 10px; 
                      background: linear-gradient(to right, #4A90E2, #D4145A);
                      color: white; font-size: 16px; text-align: center;",
              "The Worldwide Bureaucracy Indicators (WWBI), developed by the Bureaucracy Lab at the World Bank, provide a unique cross-national dataset on public sector employment and wages. Designed to fill critical data gaps, the WWBI support researchers, development practitioners, and policymakers in understanding the size, structure, and fiscal impact of the public sector. The dataset, derived from household surveys and administrative data, offers insights into public-sector employment demographics, wage competitiveness, and the overall wage bill. Version 3.1 expands the dataset with 129 new surveys, two additional countries, and updated wage bill data for 195 countries (2022), strengthening its role as a key tool for comparative and quantitative research on state capacity and public-sector labor markets worldwide.")
        ),
        
        # Button to download the PDF
        downloadButton("download_pdf", "Download Codebook")
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
    } else if(tab == "pay_compression") {
      tagList(
        h3("Pay Compression in the Public and Private Sectors"),
        
        # Section Description
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                  background: linear-gradient(to right, #4A90E2, #D4145A);
                  color: white; font-size: 16px; text-align: center;",
              "This visualization explores pay compression in the public and private sectors across selected countries.")
        ),
        
        # Country Selection
        fluidRow(
          selectInput("selected_countries", "Select Countries", 
                      choices = unique(pay_compression$country_name), 
                      multiple = TRUE,
                      selected = unique(pay_compression$country_name)[1])  # Default country selected
        ),
        
        # Scatter Plot Output (Fix: Use plotlyOutput instead of plotOutput)
        fluidRow(
          plotlyOutput("paycompression_plot", height = "600px")
        ),
        
        # Note/Explanation
        fluidRow(
          div(style = "border: 2px solid white; padding: 10px; 
                  background: linear-gradient(to right, #4A90E2, #D4145A);
                  color: white; font-size: 16px; text-align: center;",
              textOutput("note_dotplot"))
        ),
        
        # Download Button for Report
        fluidRow(
          downloadButton("downloadPayCompressionDoc", "Download Pay Compression Report")
        )
      )
    } else if(tab == "download_all") {
      tagList(
        
        # Title
        h3("Download Graph Reports"),
        
        # Description box
        fluidRow(
          div(style = "border: 2px solid white; padding: 15px; border-radius: 10px; 
              background: linear-gradient(to right, #4A90E2, #D4145A);
                                          color: white; font-size: 16px; text-align: center;",
              p("You can download a comprehensive report with all graphs or select specific graphs to include in your report.",
                style = "font-size: 16px; color: #333;"))
        ),
        
        br(),
        
        # ✅ Download Full Report Button
        fluidRow(
          column(12, align = "center",
                 downloadButton("downloadAllGraphsDoc", "Download Full Report", 
                                style = "padding: 10px 20px; font-size: 16px;")
          )
        ),
        
        hr(),
        
        # ✅ Select Graphs to Download
        h4("Download a Custom Report"),
        
        checkboxGroupInput(
          inputId = "selected_graphs",
          label = "Select Graphs to Include:",
          choices = list(
            "Wage Bill" = "wagebill",
            "Wage Bill as % of GDP" = "wagebill_gdp",
            "Tertiary Education" = "tertiaryeducation",
            "Gender Wage Premium" = "genderwagepremium",
            "Wage Premium by Education" = "wagepremiumeducation",
            "Public Employment" = "public_employment",
            "Wage Premium by Gender" = "wagepremiumgender",
            "Public Sector Workforce" = "public_workforce",
            "Gender Workforce" = "gender_workforce",
            "Female Occupation Groups" = "femaleoccupation",
            "Wage Premium" = "wagepremium",
            "Gender Wage Premium Report" = "gender_wage_premium", 
            "Pay Compression Report" = "pay_compression"
          ),
          selected = c("wagebill", "public_employment") # Default selections
        ),
        
        br(),
        
        # ✅ Download Selected Graphs Button
        fluidRow(
          column(12, align = "center",
                 downloadButton("downloadSelectedGraphsDoc", "Download Selected Report",
                                style = "padding: 10px 20px; font-size: 16px;")
          )
        )
      )
    }
  }
)

  # ---------------------------
  
    output$download_pdf <- downloadHandler(
      filename = "Codebook and Explanatory Note.pdf",
      content = function(file) {
        file.copy(file.path(data_path, "Files", "WWBI Codebook v3.1.pdf"), file)
      }
    )

  
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
        color = "Country"
      ) +
      theme_minimal()
    
    # Create Public Expenditure Graph
    graph_exp <- ggplot(graph_data_exp, aes(x = year, y = value, color = country_name)) +
      geom_line(size = 1.2) + 
      geom_point(size = 3) +
      labs(
        title = "Wage Bill as % of Public Expenditure Over Time",
        x = "Year",
        y = "Wage Bill (% of Public Expenditure)", 
        color = "Country"
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
    # Extract wage bill for the first selected country in 2010
    first_country_2010_wage_bill <- wage_bill_gdp %>%
      filter(country_name == first_country, year == 2010) %>%
      summarise(wage_bill_2010 = first(value)) %>%
      pull(wage_bill_2010) %>%
      coalesce(NA)
    
    # Extract wage bill for the first selected country in the last available year
    latest_year <- wage_bill_gdp %>%
      filter(country_name == first_country) %>%
      summarise(last_year = max(year, na.rm = TRUE)) %>%
      pull(last_year)
    
    first_country_latest_wage_bill <- wage_bill_gdp %>%
      filter(country_name == first_country, year == latest_year) %>%
      summarise(wage_bill_latest = first(value)) %>%
      pull(wage_bill_latest) %>%
      coalesce(NA)
    
    # Determine increase or decrease
    trend_text <- if (!is.na(first_country_2010_wage_bill) && !is.na(first_country_latest_wage_bill)) {
      if (first_country_latest_wage_bill > first_country_2010_wage_bill) {
        paste0("an increase from ", round(first_country_2010_wage_bill, 0), "% in 2010 to ", 
               round(first_country_latest_wage_bill, 0), "% in ", latest_year, ".")
      } else if (first_country_latest_wage_bill < first_country_2010_wage_bill) {
        paste0("a decrease from ", round(first_country_2010_wage_bill, 0), "% in 2010 to ", 
               round(first_country_latest_wage_bill, 0), "% in ", latest_year, ".")
      } else {
        paste0("no significant change, remaining at ", round(first_country_2010_wage_bill, 0), "% from 2010 to ", latest_year, ".")
      }
    } else {
      "insufficient data to determine the trend."
    }
    
    # Construct dynamic interpretation text

    gdp_interpretation_text <- paste0(
      "Figure 1.1 illustrates the Wage bill as a percentage of GDP for the selected countries, showing ", relationship_text, 
      " between a country’s level of economic development and the size of its public sector in the ", first_region, " region. ",
      "This means that as GDP per capita increases, the public sector wage bill tends to ",
      ifelse(regional_trend < -0.2, "decrease.", ifelse(regional_trend > 0.2, "increase.", "remain relatively stable.")), 
      " Given ", first_country, "’s position in this trend, it indicates that ", first_country, 
      " spends ", spending_pattern, " on its public sector wage bill compared to other countries in ", first_region, ".\n\n",
      "For ", first_country, ", the wage bill as a percentage of GDP was ", trend_text
    )
    
    doc <- doc %>% 
      body_add_par("Wage Bill as % of GDP Over Time", style = "heading 2") %>% 
      body_add_img(src = img_path_gdp, width = 6, height = 4) %>% 
      body_add_par(gdp_interpretation_text, style = "Normal")
    
    # Add Public Expenditure Graph to the Document
    
    # Extract wage bill as % of public expenditure for the selected country
    wage_bill_exp_trend <- wage_bill_publicexp %>%
      filter(country_name == first_country)
    
    # ✅ Get the wage bill values for 2010 and latest available year (Rounded)
    wage_bill_exp_2010 <- wage_bill_exp_trend %>%
      filter(year == 2010) %>%
      summarise(value_2010 = max(value, na.rm = TRUE)) %>%
      pull(value_2010) %>%
      coalesce(NA) %>%
      round(0)
    
    latest_year <- wage_bill_exp_trend %>%
      summarise(last_year = max(year, na.rm = TRUE)) %>%
      pull(last_year)
    
    wage_bill_exp_latest <- wage_bill_exp_trend %>%
      filter(year == latest_year) %>%
      summarise(value_latest = max(value, na.rm = TRUE)) %>%
      pull(value_latest) %>%
      coalesce(NA) %>%
      round(0)
    
    # ✅ Compute volatility (standard deviation) of the selected country
    country_volatility <- wage_bill_exp_trend %>%
      summarise(volatility = sd(value, na.rm = TRUE)) %>%
      pull(volatility) %>%
      coalesce(NA)
    
    # ✅ Compute average volatility for the region
    regional_volatility <- wage_bill_publicexp %>%
      filter(wb_region == first_region) %>%
      group_by(country_name) %>%
      summarise(volatility = sd(value, na.rm = TRUE)) %>%
      ungroup() %>%
      summarise(mean_volatility = mean(volatility, na.rm = TRUE)) %>%
      pull(mean_volatility) %>%
      coalesce(NA)
    
    # ✅ Compare country to regional volatility
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
    
    # ✅ Construct dynamic interpretation text with 2010 and latest year values
    public_exp_interpretation_text <- paste0(
      "The wage bill as a share of public expenditures in ", first_country, " was ",
      if (!is.na(wage_bill_exp_2010) && !is.na(wage_bill_exp_latest)) {
        paste0(wage_bill_exp_2010, "% in 2010 and has ",
               if (wage_bill_exp_latest > wage_bill_exp_2010) {
                 "increased"
               } else if (wage_bill_exp_latest < wage_bill_exp_2010) {
                 "decreased"
               } else {
                 "remained the same"
               },
               " to ", wage_bill_exp_latest, "% in ", latest_year, ", as shown in Figure 1.2.")
      } else {
        "varied over time, as shown in Figure 1.2."
      },
      " The public sector wage bill in ", first_country, " has exhibited ", stability_text,
      " compared to the volatility observed in regional peers."
    )
    
    # ✅ Add Interpretation to Document
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
  generate_gdp_analysis_section <- function(doc, selected_countries) {
    # Filter data for selected countries
    filtered_data_df <- merged_data %>% filter(country_name %in% selected_countries)
    
    # Ensure the dataset is not empty
    if (nrow(filtered_data_df) == 0) {
      doc <- doc %>% body_add_par("No data available for Wage Bill vs. GDP analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Extract the first selected country
    first_country <- selected_countries[1]
    
    # ✅ Extract values for selected countries
    country_summary <- filtered_data_df %>%
      group_by(country_name) %>%
      summarise(
        wage_bill = round(mean(indicator_value, na.rm = TRUE), 0),  
        gdp_per_capita = round(exp(mean(log_gdp, na.rm = TRUE)), 0) 
      )
    
    # ✅ Extract values for the first selected country
    first_country_values <- country_summary %>%
      filter(country_name == first_country)
    
    first_country_wage_bill <- first_country_values %>%
      pull(wage_bill) %>%
      coalesce(NA)
    
    first_country_gdp <- first_country_values %>%
      pull(gdp_per_capita) %>%
      coalesce(NA)
    
    # ✅ Extract regional averages for comparison
    regional_avg <- filtered_data_df %>%
      summarise(
        avg_wage_bill = round(mean(indicator_value, na.rm = TRUE), 0),
        avg_gdp_per_capita = round(exp(mean(log_gdp, na.rm = TRUE)), 0)
      )
    
    # ✅ Construct interpretation text incorporating first-country values
    interpretation_text <- paste0(
      "Figure 1.3 illustrates the relationship between the wage bill as a percentage of public expenditure ",
      "and GDP per capita across selected countries. The selected countries have an average wage bill of ",
      regional_avg$avg_wage_bill, "%, with a GDP per capita of $",
      format(regional_avg$avg_gdp_per_capita, big.mark = ","), ".\n\n",
      "For ", first_country, ", the wage bill represents ", first_country_wage_bill, 
      "% of public expenditure, with a GDP per capita of $",
      format(first_country_gdp, big.mark = ","), "."
    )
    
    # ✅ Add section header and introduction
    doc <- doc %>% 
      body_add_par("Wage bill (% of public expenditure) and GDP per capita in the region", style = "heading 2") %>%
      body_add_par("This note presents evidence on public sector employment and compensation practices in relation to GDP per capita.", style = "Normal")
    
    # ✅ Create and Save Plot
    plot <- ggplot(filtered_data_df, aes(x = log_gdp, y = indicator_value, color = country_name)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
      labs(
        title = "Wage Bill vs. Log(GDP per Capita)", 
        x = "Log(GDP per Capita, 2015)", 
        y = "Wage Bill",
        color = "Country"  
      ) +
      theme_minimal()
    
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = plot, width = 8, height = 6, dpi = 300)
    
    # ✅ Add Image and Interpretation
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>%
      body_add_par("Note: This indicator represents the relationship between wage bill and log(GDP per capita). The trendline provides a visual reference for the overall pattern.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
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
    
    # ✅ Ensure at least one country is selected
    if (is.null(input$countries_workforce) || length(input$countries_workforce) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Extract the first selected country
    first_country <- input$countries_workforce[1]
    
    # ✅ Filter data for selected countries
    first_graph_data <- filtered_workforce_data() %>% filter(country_name %in% input$countries_workforce)
    
    if (nrow(first_graph_data) == 0) {
      doc <- doc %>% body_add_par("No data available for Public Workforce Distribution by Country.", style = "Normal")
      return(doc)
    }
    
    # ✅ Generate the first graph
    first_graph_ggplot <- ggplot(first_graph_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Public Administration" = "#568340", "Education" = "#B3242B", 
                                   "Health" = "#003366", "Other" = "#A9A9A9")) +
      labs(
        title = "Public Workforce Distribution by Country", 
        x = "Country", 
        y = "Workforce Distribution (%)", 
        fill = "Sector"  # ✅ Renames the legend title for indicator_name
      ) +
      theme_minimal()
    
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph_ggplot, width = 8, height = 4)
    
    # ✅ Extract workforce distribution for the first country
    sector_distribution <- public_sector_workforce %>%
      filter(country_name == first_country) %>%
      group_by(indicator_name) %>%
      summarise(share = mean(value_percentage, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = indicator_name, values_from = share, values_fill = list(share = 0))
    
    # ✅ Extract workforce distribution for other selected countries (excluding the first country)
    comparison_distribution <- public_sector_workforce %>%
      filter(country_name %in% input$countries_workforce & country_name != first_country) %>%
      group_by(indicator_name) %>%
      summarise(avg_share = mean(value_percentage, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = indicator_name, values_from = avg_share, values_fill = list(avg_share = 0))
    
    # ✅ Extract values safely
    public_admin_share <- sector_distribution$`Public Administration` %||% 0
    education_share <- sector_distribution$Education %||% 0
    health_share <- sector_distribution$Health %||% 0
    
    comparison_admin_share <- comparison_distribution$`Public Administration` %||% 0
    comparison_education_share <- comparison_distribution$Education %||% 0
    comparison_health_share <- comparison_distribution$Health %||% 0
    
    # ✅ Handle the case where all values are zero (missing)
    if (public_admin_share == 0 && education_share == 0 && health_share == 0) {
      doc <- doc %>% body_add_par(
        paste0("No public sector employment data available for ", first_country, "."),
        style = "Normal"
      )
      return(doc)
    }
    
    # ✅ Construct sector ranking for the first country
    sector_shares <- c(public_admin_share, education_share, health_share)
    sector_names <- c("Public Administration", "Education", "Health")
    largest_sector <- sector_names[which.max(sector_shares)]
    largest_sector_share <- max(sector_shares)
    
    # ✅ Compare first country with others
    education_comparison <- if (education_share > comparison_education_share) {
      paste0("This is higher than the average of ", round(comparison_education_share, 1), "% among the other selected countries.")
    } else {
      paste0("This is lower than the average of", round(comparison_education_share, 1), "% among the other selected countries.")
    }
    
    health_comparison <- if (health_share > comparison_health_share) {
      paste0("The health sector, while representing a smaller segment at ", 
             round(health_share, 1), "%, still surpasses the average of ", 
             round(comparison_health_share, 1), "% in other selected countries.")
    } else {
      paste0("The health sector accounts for ", round(health_share, 1), "%, aligning closely with the average of", 
             round(comparison_health_share, 1), "% in other selected countries.")
    }
    
    # ✅ Construct dynamic interpretation text
    sector_interpretation_text <- paste0(
      first_country, " has the highest proportion of public sector employees in ", largest_sector,
      ", with ", round(largest_sector_share, 1), "% of paid public sector workers employed in this area. ",
      "The education sector represents ", round(education_share, 1), "% of the workforce. ", education_comparison, " ",
      "The health sector accounts for ", round(health_share, 1), "%, ", health_comparison
    )
    
    doc <- doc %>% 
      body_add_par("Public Workforce Distribution by Country", style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4) %>% 
      body_add_par(sector_interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  
  #Tertiary Education 
  
  output$barPlot <- renderPlotly({
    req(input$selected_countries)
    
    # Filter Data
    filtered_data <- tertiary_education %>% 
      filter(country_name %in% input$selected_countries)
    
    # Define Colors
    custom_colors <- c("as a share of private paid employees" = "#B3242B", 
                       "as a share of public paid employees" = "#003366")
    
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
        scale_fill_manual(values = c("as a share of private paid employees" = "#B3242B", 
                                     "as a share of public paid employees" = "#003366")) +
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
    
    # ✅ Ensure at least one country is selected
    if (is.null(input$selected_countries) || length(input$selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Filter data for selected countries
    filtered_data <- tertiary_education %>% filter(country_name %in% input$selected_countries)
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Generate Tertiary Education ggplot
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("as a share of private paid employees" = "#B3242B", 
                                   "as a share of public paid employees" = "#003366")) +
      labs(title = "Tertiary Education by Sector and Country", x = "Country", y = "Tertiary Education (%)", fill = "Sector") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # ✅ Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # ✅ Extract summary statistics
    avg_public <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of public paid employees"], na.rm = TRUE), 1)
    avg_private <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of private paid employees"], na.rm = TRUE), 1)
    
    highest_public_country <- filtered_data %>%
      filter(indicator_name == "as a share of public paid employees") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_public_country <- filtered_data %>%
      filter(indicator_name == "as a share of public paid employees") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    highest_private_country <- filtered_data %>%
      filter(indicator_name == "as a share of private paid employees") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_private_country <- filtered_data %>%
      filter(indicator_name == "as a share of private paid employees") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    # ✅ Construct interpretation text
    interpretation_text <- paste0(
      "This graph compares tertiary education attainment among employees in the public and private sectors across selected countries. ",
      "On average,",  avg_public, "% of public sector employees have completed tertiary education, while in the private sector, the share is ", avg_private, "%. ",
      "The country with the highest share of tertiary-educated public sector employees is ", highest_public_country, ", whereas ", lowest_public_country, "has the lowest proportion. ",
      "In the private sector, ", highest_private_country, "has the highest tertiary education level among employees, while ", lowest_private_country, " has the lowest."
    )
    
    # ✅ Add image and interpretation text to the document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the proportion of individuals with tertiary education working in public and private sector employment.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
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
    
    # ✅ Ensure at least one country is selected
    if (is.null(input$countries_wage_premium) || length(input$countries_wage_premium) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Filter data for selected countries
    filtered_data <- public_wage_premium %>% 
      filter(country_name %in% input$countries_wage_premium) %>%
      drop_na(value_percentage)  # Remove NA values
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Convert value_percentage to numeric safely
    filtered_data$value_percentage <- as.numeric(filtered_data$value_percentage)
    
    # ✅ Extract the first selected country
    first_country <- input$countries_wage_premium[1]
    
    # ✅ Calculate key statistics
    avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
    
    highest_country <- filtered_data %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_country <- filtered_data %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    # ✅ Extract wage premium for the first country and comparison
    first_country_premium <- filtered_data %>%
      filter(country_name == first_country) %>%
      pull(value_percentage) %>%
      coalesce(0)  # ✅ Defaults to 0 if missing
    
    comparison_premium <- filtered_data %>%
      filter(country_name != first_country) %>%
      summarise(avg_other_countries = mean(value_percentage, na.rm = TRUE)) %>%
      pull(avg_other_countries) %>%
      coalesce(0)  # ✅ Defaults to 0 if missing
    
    # ✅ Prevent errors when comparing values
    if (!is.na(first_country_premium) && !is.na(comparison_premium) && first_country_premium > comparison_premium) {
      comparison_statement <- paste0("This is higher than the average of", round(comparison_premium, 1), "% across the other selected countries.")
    } else if (!is.na(first_country_premium) && !is.na(comparison_premium)) {
      comparison_statement <- paste0("This is lower than the average of", round(comparison_premium, 1), "% across the other selected countries.")
    } else {
      comparison_statement <- "Comparison data is not available."
    }
    
    # ✅ Create the Dot Plot with Different Colors
    filtered_data <- filtered_data %>%
      mutate(color = ifelse(country_name == first_country, "#B3242B", "#003366"))
    
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = color)) +
      geom_point(size = 5) +
      scale_color_identity() +  # Use assigned colors directly
      labs(title = "Public Sector Wage Premium by Country", x = "Country", y = "Wage Premium (%)") +
      theme_minimal()
    
    # ✅ Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # ✅ Construct comparative interpretation text
    interpretation_text <- paste0(
      "This graph compares public sector wage premiums across selected countries. ",
      "On average, public sector employees earn ",avg_wage_premium, "% more than private sector employees. ",
      "The country with the highest wage premium is ",  highest_country, ", while ",  lowest_country, " has the lowest wage premium.\n\n",
      "In ", first_country, ", the public sector wage premium is", round(first_country_premium, 1), "%. ", 
      comparison_statement
    )
    
    # ✅ Add image and interpretation text to the document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the wage premium in the public sector relative to private sector employees across selected countries.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
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
                                     y = data_to_plot_long$value_percentage, 
                                     text = round(data_to_plot_long$value_percentage, 2),
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
        data_to_plot_long <- data_to_plot %>% select(country_name, indicator_label, year, value, value_percentage) %>% 
          mutate(indicator_label = factor(indicator_label))
        first_graph_wage_premium_gender <- ggplot(data_to_plot_long, aes(x = country_name, y = value_percentage, color = indicator_label)) +
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
        data_to_plot_long <- data_to_plot %>% select(year, indicator_name, value, value_percentage) %>% 
          mutate(indicator_name = factor(indicator_name))
        second_graph_wage_premium_gender <- ggplot(data_to_plot_long, aes(x = year, y = value_percentage, color = indicator_name)) +
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
    
    # ✅ First Graph - Wage Premium by Gender (Multi-Country)
    if ("firstGraphgender" %in% input$graphs_to_download && length(input$countries_first) > 0) {
      data_to_plot <- gender_wage_premium_last %>% filter(country_name %in% input$countries_first)
      
      if (nrow(data_to_plot) > 0) {
        data_to_plot_long <- data_to_plot %>% 
          select(country_name, indicator_label, year, value, value_percentage) %>% 
          mutate(indicator_label = factor(indicator_label))
        
        first_graph_wage_premium_gender <- ggplot(data_to_plot_long, aes(x = country_name, y = value_percentage, color = indicator_label)) +
          geom_point(size = 3) +
          labs(title = "Wage Premium Gender (Multi-Country)", x = "Country", y = "Value") +
          theme_minimal()
        
        # Save the plot as an image
        img_path1 <- tempfile(fileext = ".png")
        ggsave(img_path1, plot = first_graph_wage_premium_gender, width = 8, height = 6)
        
        # ✅ Extract summary statistics
        first_country <- input$countries_first[1]
        
        avg_wage_premium <- round(mean(data_to_plot$value_percentage, na.rm = TRUE), 1)
        
        highest_country <- data_to_plot %>%
          filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
          pull(country_name) %>%
          first()
        
        lowest_country <- data_to_plot %>%
          filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
          pull(country_name) %>%
          first()
        
        first_country_premium <- data_to_plot %>%
          filter(country_name == first_country) %>%
          pull(value_percentage) %>%
          coalesce(0)
        
        comparison_premium <- data_to_plot %>%
          filter(country_name != first_country) %>%
          summarise(avg_other_countries = mean(value_percentage, na.rm = TRUE)) %>%
          pull(avg_other_countries) %>%
          coalesce(0)
        
        # ✅ Compare first country with others
        comparison_statement <- if (first_country_premium > comparison_premium) {
          paste0("This is higher than the average of ", round(comparison_premium, 1), "% across the other selected countries.")
        } else {
          paste0("This is lower than the average of ", round(comparison_premium, 1), "% across the other selected countries.")
        }
        
        interpretation_text1 <- paste0(
          "This graph compares public sector wage premiums by gender across selected countries. ",
          "On average, the gender wage premium in the public sector is ",  avg_wage_premium, "%. ",
          "The highest wage premium is observed in ", highest_country, ", while the lowest is in", lowest_country, ".\n\n",
          "In ", first_country, ", the gender wage premium is ", round(first_country_premium, 1), "%. ", 
          comparison_statement
        )
        
        # ✅ Add Image and Description
        doc <- doc %>% 
          body_add_par("First Graph: Wage Premium Gender (Multi-Country)", style = "heading 2") %>% 
          body_add_img(src = img_path1, width = 6, height = 4) %>% 
          body_add_par("This graph shows the wage premium by gender across multiple countries.", style = "Normal") %>%
          body_add_par(interpretation_text1, style = "Normal")
      } else {
        doc <- doc %>% body_add_par("No data available for Wage Premium by Gender (Multi-Country).", style = "Normal")
      }
    }
    
    # ✅ Second Graph - Wage Premium by Gender (Single Country Over Time)
    if ("secondGraphgender" %in% input$graphs_to_download && !is.null(input$country_second)) {
      data_to_plot <- gender_wage_premium %>% filter(country_name == input$country_second)
      
      if (nrow(data_to_plot) > 0) {
        data_to_plot_long <- data_to_plot %>% 
          select(year, indicator_name, value, value_percentage) %>% 
          mutate(indicator_name = factor(indicator_name))
        
        second_graph_wage_premium_gender <- ggplot(data_to_plot_long, aes(x = year, y = value_percentage, color = indicator_name)) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          labs(title = paste("Public Sector Wage Premium by Gender in", input$country_second, "Over Time"), 
               x = "Year", y = "Employment Value") +
          theme_minimal()
        
        # Save the plot as an image
        img_path2 <- tempfile(fileext = ".png")
        ggsave(img_path2, plot = second_graph_wage_premium_gender, width = 8, height = 6)
        
        # ✅ Extract wage premium trend for selected country
        first_year <- min(data_to_plot$year, na.rm = TRUE)
        last_year <- max(data_to_plot$year, na.rm = TRUE)
        
        wage_premium_first_year <- data_to_plot %>%
          filter(year == first_year) %>%
          pull(value) %>%
          coalesce(NA)
        
        wage_premium_last_year <- data_to_plot %>%
          filter(year == last_year) %>%
          pull(value) %>%
          coalesce(NA)
        
        trend_direction <- if (wage_premium_last_year > wage_premium_first_year) {
          "increased"
        } else if (wage_premium_last_year < wage_premium_first_year) {
          "decreased"
        } else {
          "remained stable"
        }
        
        interpretation_text2 <- paste0(
          "In ", input$country_second, ", the gender wage premium in the public sector has", trend_direction, "from", 
          round(wage_premium_first_year, 1), "% in", first_year, "to", 
          round(wage_premium_last_year, 1), "%in", last_year, "."
        )
        
        # ✅ Add Image and Description
        doc <- doc %>% 
          body_add_par("Second Graph: Public Sector Employment (Single Country)", style = "heading 2") %>% 
          body_add_img(src = img_path2, width = 6, height = 4) %>% 
          body_add_par("This graph shows the wage premium by gender trends over time for the selected country.", style = "Normal") %>%
          body_add_par(interpretation_text2, style = "Normal")
      } else {
        doc <- doc %>% body_add_par("No data available for Wage Premium by Gender (Over Time).", style = "Normal")
      }
    }
    
    return(doc)
  }
  
  
  
  # Wage premium by Education Level 
  
  # Render the Public Sector Wage Premium by Education Level Graph
  output$education_wage_premium_plot <- renderPlotly({
    
    req(input$selected_country)  # Ensure a country is selected
    
    # Filter the data set for the selected country
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
    
    # ✅ Ensure at least one country is selected
    if (is.null(input$selected_countries) || length(input$selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Extract the first selected country
    first_country <- input$selected_countries[1]
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      paste0("This section presents an analysis of public sector wage premiums based on different education levels for ", 
             first_country, ". The comparison is made against private sector formal workers."), 
      style = "Normal"
    )
    
    # ✅ Filter data for selected countries
    filtered_data <- public_wage_premium_educ %>%
      filter(country_name %in% input$selected_countries) %>%
      drop_na(value_percentage)
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Generate Wage Premium by Education Level ggplot
    ggplot_obj <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "No Education" = "#003366",
        "Primary Education" = "#B3242B",
        "Secondary Education" = "#3B3B3B",
        "Tertiary Education" = "#006400"
      )) +
      labs(
        title = "Public Sector Wage Premium by Education Level (Compared to Private Formal Workers)",
        x = "Education Level",
        y = "Wage Premium (%)",
        fill = "Education Level"
      ) +
      theme_minimal()
    
    # ✅ Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # ✅ Extract summary statistics
    highest_education <- filtered_data %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(indicator_name) %>%
      first()
    
    lowest_education <- filtered_data %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(indicator_name) %>%
      first()
    
    avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
    
    # ✅ Extract wage premium for the first country and comparison
    first_country_premium <- filtered_data %>%
      filter(country_name == first_country) %>%
      summarise(avg_premium = mean(value_percentage, na.rm = TRUE)) %>%
      pull(avg_premium) %>%
      coalesce(0)
    
    comparison_premium <- filtered_data %>%
      filter(country_name != first_country) %>%
      summarise(avg_other_countries = mean(value_percentage, na.rm = TRUE)) %>%
      pull(avg_other_countries) %>%
      coalesce(0)
    
    # ✅ Compare first country with others
    comparison_statement <- if (first_country_premium > comparison_premium) {
      paste0("This is higher than the average of", round(comparison_premium, 1), "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of", round(comparison_premium, 1), "% across the other selected countries.")
    }
    
    interpretation_text <- paste0(
      "This graph illustrates public sector wage premiums by education level in ", first_country, 
      ", comparing earnings with private sector formal workers. ",
      "On average, the public sector wage premium in ", first_country, " is ", round(first_country_premium, 0), "%. ",
      "The highest wage premium is observed for those with ", highest_education, ", while the lowest wage premium is for those with ", lowest_education, "."
    )
    
    # ✅ Add image and interpretation text to the document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the public sector wage premium by education level, comparing earnings with private sector formal workers.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  

  #Public Sector Graphs 
  
  # First Graph - Multi-Country Dot Plot
  output$firstGraphpublic <- renderPlotly({
    filtered_data <- public_sector_emp_temp_last %>% 
      filter(country_name %in% input$countries_first)
    
    ggplotly(
      ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
        geom_point(size = 4) +
        labs(title = "Public Sector Employment (Last Year Available)", 
             x = "Country", y = "Value") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
      ggplot(filtered_data, aes(x = year, y = value_percentage, color = indicator_label)) +
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
                            aes(x = country_name, y = value_percentage, color = indicator_label)) +
        geom_point(size = 4) +
        labs(title = "Public Sector Employment (Last Year Available)", x = "Country", y = "Value") +
        theme_minimal()
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = first_graph, width = 8, height = 6)
      
      doc <- doc %>% body_add_par("Public Sector Employment - Last Year Available", style = "heading 2")
      doc <- doc %>% body_add_img(src = img_path1, width = 6, height = 4)
      
      # Second Graph - Save as Image
      second_graph <- ggplot(public_sector_emp_temp %>% filter(country_name == input$country_second), 
                             aes(x = year, y = value_percentage, color = indicator_label)) +
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
    
    # ✅ Ensure at least one country is selected safely
    if (is.null(input$countries_first) || length(input$countries_first) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    selected_countries <- input$countries_first
    first_country <- if (length(selected_countries) > 0) selected_countries[1] else NA
    
    if (is.na(first_country)) {
      doc <- doc %>% body_add_par("Invalid country selection.", style = "Normal")
      return(doc)
    }
    
    # ✅ Filter data for selected countries (last available year)
    filtered_data <- public_sector_emp_temp_last %>% 
      filter(country_name %in% selected_countries)
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Generate First Graph - Public Sector Employment (Last Year Available)
    first_graph <- ggplot(filtered_data, 
                          aes(x = country_name, y = value_percentage, color = indicator_label)) +
      geom_point(size = 4) +
      labs(title = "Public Sector Employment (Last Year Available)", 
           x = "Country", 
           y = "Employment (%)", 
           color = "Sector") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # ✅ Extract employment levels per indicator for the first country
    get_value <- function(df, country, indicator) {
      val <- df %>% filter(country_name == country, indicator_label == indicator) %>% 
        pull(value_percentage) %>% first()
      ifelse(length(val) == 0 || is.na(val), "Data not available", round(val, 0))
    }
    
    formal_first <- get_value(filtered_data, first_country, "as a share of formal employment")
    paid_first <- get_value(filtered_data, first_country, "as a share of paid employment")
    total_first <- get_value(filtered_data, first_country, "as a share of total employment")
    
    # ✅ Get highest and lowest countries per indicator
    get_extreme_country <- function(df, indicator, func) {
      row <- df %>% filter(indicator_label == indicator) %>%
        filter(value_percentage == func(value_percentage, na.rm = TRUE)) %>%
        select(country_name, value_percentage) %>% first()
      
      if (nrow(row) == 0) return(list(country = "N/A", value = "Data not available"))
      return(list(country = row$country_name, value = round(row$value_percentage, 0)))
    }
    
    formal_highest <- get_extreme_country(filtered_data, "as a share of formal employment", max)
    formal_lowest <- get_extreme_country(filtered_data, "as a share of formal employment", min)
    
    paid_highest <- get_extreme_country(filtered_data, "as a share of paid employment", max)
    paid_lowest <- get_extreme_country(filtered_data, "as a share of paid employment", min)
    
    total_highest <- get_extreme_country(filtered_data, "as a share of total employment", max)
    total_lowest <- get_extreme_country(filtered_data, "as a share of total employment", min)
    
    # ✅ Compare first country with other countries
    compare_value <- function(value, high, low) {
      if (value == "Data not available") return("data not available")
      if (value > high) {
        return("This is the highest employment rate among the selected countries.")
      } else if (value < low) {
        return("This is the lowest employment rate among the selected countries.")
      } else {
        return("This falls within the range observed across the selected countries.")
      }
    }
    
    formal_comparison <- compare_value(formal_first, formal_highest$value, formal_lowest$value)
    paid_comparison <- compare_value(paid_first, paid_highest$value, paid_lowest$value)
    total_comparison <- compare_value(total_first, total_highest$value, total_lowest$value)
    
    # ✅ Interpretation for first graph
    interpretation_text1 <- paste0(
      "This graph compares public sector employment across selected countries. ",
      "For employment as a share of formal employment, the highest level is in ", formal_highest$country, 
      " at ", formal_highest$value, "%, while the lowest is in ", formal_lowest$country, 
      " at ", formal_lowest$value, "%.\n\n",
      "For employment as a share of paid employment, the highest level is in ", paid_highest$country, 
      " at ", paid_highest$value, "%, while the lowest is in ", paid_lowest$country, 
      " at ", paid_lowest$value, "%.\n\n",
      "For employment as a share of total employment, the highest level is in ", total_highest$country, 
      " at ", total_highest$value, "%, while the lowest is in ", total_lowest$country, 
      " at ", total_lowest$value, "%.\n\n",
      "In ", first_country, ", public sector employment as a share of formal employment is ", formal_first, "%. ", formal_comparison, "\n",
      "As a share of paid employment, it is ", paid_first, "%. ", paid_comparison, "\n",
      "As a share of total employment, it is ", total_first, "%. ", total_comparison
    )
    
    # ✅ Add First Graph and Interpretation
    doc <- doc %>% 
      body_add_par("Public Sector Employment - Last Year Available", style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4) %>% 
      body_add_par(interpretation_text1, style = "Normal")
    
    return(doc)
  }
  

#Gender Wage premium 
  
  # First Graph - Multi-Country Dot Plot for Wage Premium by Gender
  output$firstGraphGenderWagePremium <- renderPlotly({
    filtered_data <- gender_wage_premium_last %>% 
      filter(country_name %in% input$countries_first)
    
    ggplotly(
      ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
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
      ggplot(filtered_data, aes(x = year, y = value_percentage, color = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(title = "Public Sector Wage Premium by Gender Over Time", 
             x = "Year", 
             y = "Wage Premium (%)",
             color = "Indicator") +  # Updated label for legend
        theme_minimal() +
        annotate("text", x = Inf, y = min(filtered_data$value_percentage) - 5,  # Adjusted variable name
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
                            aes(x = country_name, y = value_percentage, color = indicator_label)) +
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
                             aes(x = year, y = value_percentage, color = indicator_label)) +
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
      "This section presents an analysis of the public sector wage premium by gender across selected countries.", 
      style = "Normal"
    )
    
    # ✅ Ensure at least one country is selected and not NA
    if (is.null(input$countries_first) || length(na.omit(input$countries_first)) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    selected_countries <- input$countries_first
    first_country <- selected_countries[1]
    
    # ✅ Filter data for selected countries (last available year)
    filtered_data <- gender_wage_premium_last %>% 
      filter(country_name %in% selected_countries) %>%
      drop_na(value_percentage)
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Function to get rounded values safely
    get_rounded_value <- function(df, country, indicator) {
      val <- df %>% filter(country_name == country, indicator_label == indicator) %>%
        pull(value_percentage) %>% first()
      ifelse(is.na(val), "Data not available", round(val, 0))
    }
    
    # ✅ Values for the first selected country
    male_first_country <- get_rounded_value(filtered_data, first_country, "Male")
    female_first_country <- get_rounded_value(filtered_data, first_country, "Female")
    
    # ✅ Get the country with the highest wage premium for each indicator
    get_max_country <- function(df, indicator) {
      max_row <- df %>% filter(indicator_label == indicator) %>%
        filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
        select(country_name, value_percentage) %>% first()
      
      if (nrow(max_row) == 0) return(list(country = "N/A", value = "Data not available"))
      return(list(country = max_row$country_name, value = round(max_row$value_percentage, 0)))
    }
    
    max_male <- get_max_country(filtered_data, "Male")
    max_female <- get_max_country(filtered_data, "Female")
    
    # ✅ Generate **Dot Plot** for Public Sector Wage Premium by Gender (Last Year Available)
    first_graph <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
      geom_point(size = 4) +
      labs(title = "Public Sector Wage Premium by Gender (Last Year Available)", 
           x = "Country", 
           y = "Wage Premium (%)",
           color = "Indicator") +  
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # ✅ Interpretation for the first graph
    interpretation_text1 <- paste0(
      "This graph displays the public sector wage premium by gender for the last available year across the selected countries. ",
      "In ", first_country, ", the wage premium for male employees is ", male_first_country, 
      "%, while for female employees, it is ", female_first_country, "%. ",
      "The country with the highest male wage premium is ", max_male$country, " at ", max_male$value, 
      "%, while the highest female wage premium is in ", max_female$country, " at ", max_female$value, "%."
    )
    
    # ✅ Add First Graph and Interpretation
    doc <- doc %>% 
      body_add_par("Public Sector Wage Premium by Gender (Last Year Available)", style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4) %>% 
      body_add_par(interpretation_text1, style = "Normal")
    
    # ✅ Second Graph - Public Sector Wage Premium by Gender Over Time
    time_series_data <- gender_wage_premium %>% 
      filter(country_name == first_country)  # Only keep the first selected country
    
    if (nrow(time_series_data) > 0) {
      second_graph <- ggplot(time_series_data, aes(x = year, y = value_percentage, color = indicator_label, group = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(title = paste("Public Sector Wage Premium by Gender Over Time in", first_country), 
             x = "Year", 
             y = "Wage Premium (%)",
             color = "Gender") +
        theme_minimal()
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph, width = 8, height = 6)
      
      # ✅ Extract wage premium values for 2010 and last year available
      first_year <- 2010
      last_year <- max(time_series_data$year, na.rm = TRUE)
      
      get_year_value <- function(df, country, year, indicator) {
        val <- df %>% filter(year == year, country_name == country, indicator_label == indicator) %>%
          pull(value_percentage) %>% first()
        ifelse(is.na(val), "Data not available", round(val, 0))
      }
      
      male_2010 <- get_year_value(time_series_data, first_country, first_year, "Male")
      female_2010 <- get_year_value(time_series_data, first_country, first_year, "Female")
      male_last <- get_year_value(time_series_data, first_country, last_year, "Male")
      female_last <- get_year_value(time_series_data, first_country, last_year, "Female")
      
      # ✅ Interpretation for the second graph
      interpretation_text2 <- paste0(
        "This graph illustrates how the public sector wage premium by gender has evolved over time across selected countries. ",
        "For ", first_country, ", in ", first_year, ", the male wage premium was ", male_2010, 
        "% and the female wage premium was ", female_2010, "%.** In **", last_year, 
        ", the male wage premium is ", male_last, "% and the female wage premium is ", female_last, "%."
      )
      
      # ✅ Add Second Graph and Interpretation
      doc <- doc %>% 
        body_add_par("Public Sector Wage Premium by Gender Over Time", style = "heading 2") %>% 
        body_add_img(src = img_path2, width = 6, height = 4) %>% 
        body_add_par(interpretation_text2, style = "Normal")
      
    } else {
      doc <- doc %>% body_add_par("No data available for Public Sector Wage Premium by Gender trends over time.", style = "Normal")
    }
    
    return(doc)
  }
  
  
  # Gender Workforce Graphs
  
  # First Graph - Multi-Country Bar Plot
  output$firstGraphGenderWorkforce <- renderPlotly({
    filtered_data <- gender_workforce %>% 
      filter(country_name %in% input$countries_gender)
    
    # Ensure factor levels match color scale
    filtered_data$indicator_name <- factor(filtered_data$indicator_name, 
                                           levels = c("as a share of private paid employees", 
                                                      "as a share of public paid employees"))
    
    ggplotly(
      ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("as a share of private paid employees" = "#B3242B", 
                                     "as a share of public paid employees" = "#003366")) +
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
                                           levels = c("as a share of private paid employees", 
                                                      "as a share of public paid employees"))
    
    ggplotly(
      ggplot(filtered_data, aes(x = year, y = value_percentage, color = indicator_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c("as a share of private paid employees" = "#B3242B", 
                                      "as a share of public paid employees" = "#003366")) +
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
        scale_fill_manual(values = c("as a share of private paid employees" = "#B3242B", 
                                     "as a share of public paid employees" = "#003366")) +
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
        scale_color_manual(values = c("as a share of private paid employees" = "#B3242B", 
                                      "as a share of public paid employees" = "#003366")) +
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
    
    # ✅ Ensure at least one country is selected
    if (is.null(input$countries_gender) || length(input$countries_gender) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Extract the first selected country
    first_country <- input$countries_gender[1]
    
    # ✅ Filter data for selected countries (last available year)
    filtered_data <- gender_workforce %>% 
      filter(country_name %in% input$countries_gender)
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Generate First Graph - Female Employment by Sector (Last Year Available)
    first_graph <- ggplot(filtered_data, 
                          aes(x = country_name, y = round(value_percentage, 0), fill = indicator_name)) +  # ✅ Ensure rounded values in the graph
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("as a share of private paid employees" = "#B3242B", 
                                   "as a share of public paid employees" = "#003366")) +
      labs(title = "Female Employment by Sector (Last Year Available)", 
           x = "Country", y = "Employment (%)", fill = "Sector") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # ✅ Rotate labels to avoid overlap
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # ✅ Extract summary statistics (Rounded to Whole Numbers)
    avg_public <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of public paid employees"], na.rm = TRUE), 0)
    avg_private <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of private paid employees"], na.rm = TRUE), 0)
    
    highest_public_country <- filtered_data %>%
      filter(indicator_name == "as a share of public paid employees") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_public_country <- filtered_data %>%
      filter(indicator_name == "as a share of public paid employees") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    highest_private_country <- filtered_data %>%
      filter(indicator_name == "as a share of private paid employees") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_private_country <- filtered_data %>%
      filter(indicator_name == "as a share of private paid employees") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    # ✅ Extract and round employment values for the first selected country
    first_country_public <- filtered_data %>%
      filter(country_name == first_country, indicator_name == "as a share of public paid employees") %>%
      pull(value_percentage) %>%
      first() %>%
      coalesce(0) %>%
      round(0)
    
    first_country_private <- filtered_data %>%
      filter(country_name == first_country, indicator_name == "as a share of private paid employees") %>%
      pull(value_percentage) %>%
      first() %>%
      coalesce(0) %>%
      round(0)
    
    # ✅ Compare first country with others using rounded numbers
    comparison_public <- if (first_country_public > avg_public) {
      paste0("This is higher than the average of ", avg_public, "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", avg_public, "% across the other selected countries.")
    }
    
    comparison_private <- if (first_country_private > avg_private) {
      paste0("This is higher than the average of ", avg_private, "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", avg_private, "% across the other selected countries.")
    }
    
    # ✅ Ensure all numbers in interpretation_text1 are rounded integers
    interpretation_text1 <- paste0(
      "This graph compares female employment in the public and private sectors across selected countries. ",
      "On average, ", avg_public, "% of public sector employees are female, while in the private sector, the share is ", avg_private, "%. ",
      "The highest female employment in the public sector is in ", highest_public_country, ", while the lowest is in ", lowest_public_country, ". ",
      "In the private sector, ", highest_private_country, " has the highest share of female employees, whereas ", lowest_private_country, " has the lowest.\n\n",
      "In ", first_country, ", female representation in the public sector is ", first_country_public, "%.", 
      comparison_public, "\n",
      "In the private sector, female representation in ", first_country, " is ", first_country_private, "%. ",
      comparison_private
    )
    
    # ✅ Add First Graph and Interpretation
    doc <- doc %>% 
      body_add_par("Female Employment by Sector (Last Year Available)", style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4) %>% 
      body_add_par(interpretation_text1, style = "Normal")
    
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
    
    # ✅ Ensure at least one country is selected
    if (is.null(input$selected_countries) || length(input$selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Extract the first selected country
    first_country <- input$selected_countries[1]
    
    # ✅ Filter data for selected countries
    filtered_data <- gender_leadership %>% filter(country_name %in% input$selected_countries)
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Generate ggplot for Female Occupational Groups
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Clerks-Public" = "#003366", "Managers-Public" = "#ADD8E6",
                                   "Clerks-Private" = "#006400", "Managers-Private" = "#90EE90")) +
      labs(title = "Females by Occupational Group and Sector", x = "Country", y = "Female Share (%)", fill = "Occupation") +
      theme_minimal()
    
    # ✅ Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # ✅ Extract summary statistics
    highest_public_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Public") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_public_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Public") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    highest_private_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Private") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_private_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Private") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    highest_public_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Public") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_public_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Public") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    highest_private_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Private") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_private_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Private") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    # ✅ Round averages to 1 decimal place
    avg_public_managers <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Managers-Public"], na.rm = TRUE), 1)
    avg_private_managers <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Managers-Private"], na.rm = TRUE), 1)
    avg_public_clerks <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Clerks-Public"], na.rm = TRUE), 1)
    avg_private_clerks <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Clerks-Private"], na.rm = TRUE), 1)
    
    # ✅ Extract employment levels for the first country and comparison
    first_country_public_managers <- filtered_data %>%
      filter(country_name == first_country, indicator_label == "Managers-Public") %>%
      pull(value_percentage) %>%
      first() %>%
      coalesce(0) %>%
      round(1)  # ✅ Round to 1 decimal place
    
    first_country_private_managers <- filtered_data %>%
      filter(country_name == first_country, indicator_label == "Managers-Private") %>%
      pull(value_percentage) %>%
      first() %>%
      coalesce(0) %>%
      round(1)  # ✅ Round to 1 decimal place
    
    # ✅ Compare first country with others
    comparison_public_managers <- if (first_country_public_managers > avg_public_managers) {
      paste0("This is higher than the average of ", format(avg_public_managers, nsmall = 1), "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", format(avg_public_managers, nsmall = 1), "% across the other selected countries.")
    }
    
    comparison_private_managers <- if (first_country_private_managers > avg_private_managers) {
      paste0("This is higher than the average of ", format(avg_private_managers, nsmall = 1), "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", format(avg_private_managers, nsmall = 1), "% across the other selected countries.")
    }
    
    interpretation_text <- paste0(
      "This graph compares female representation in different occupational groups across selected countries. ",
      "On average,", avg_public_managers, "% of public sector managers are female, while in the private sector, female managers account for ", avg_private_managers, "%. ",
      "The highest female representation among public sector managers is in ", highest_public_managers, ", whereas the lowest is in  ", lowest_public_managers, ". ",
      "In the private sector, the highest female manager share is in ", highest_private_managers, "**, while the lowest is in", lowest_private_managers, ".\n\n",
      "In ", first_country, ", female managers account for ", first_country_public_managers, "% in the public sector. ", 
      comparison_public_managers, "\n",
      "In the private sector, female managers in ", first_country, "represent", first_country_private_managers, "%. ",
      comparison_private_managers
    )
    
    # ✅ Add Image and Interpretation to the Document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the share of females in various occupational groups (Managers/Clerks) in the public and private sectors for the selected countries.", style = "Normal") %>%
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
    # Add Section Title
    doc <- doc %>% body_add_par("Gender Wage Premium in Public Sector by Industry", style = "heading 1")
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of gender wage premiums in the public sector by industry 
      (Public Administration, Education, and Health) across selected countries.", 
      style = "Normal"
    )
    
    # ✅ Ensure at least one country is selected and not NA
    if (is.null(input$selected_countries) || length(na.omit(input$selected_countries)) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- input$selected_countries[1]
    if (is.na(first_country)) {
      doc <- doc %>% body_add_par("Invalid country selection.", style = "Normal")
      return(doc)
    }
    
    # ✅ Filter data for selected countries
    filtered_data <- gender_wage_premiumpublic %>%
      filter(country_name %in% input$selected_countries, 
             indicator_name %in% c("Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)", 
                                   "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)", 
                                   "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"))
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0 || all(is.na(filtered_data$value_percentage))) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Rename indicators for readability
    filtered_data$indicator_label <- recode(filtered_data$indicator_name,
                                            "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" = "Public Administration",
                                            "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)" = "Education",
                                            "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)" = "Health")
    
    # ✅ Generate Gender Wage Premium ggplot
    gender_wage_plot <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Blues") +  
      labs(title = "Gender Wage Premium in Public Sector by Industry",
           x = "Country", y = "Wage Premium (%)", fill = "Industry") +
      theme_minimal()
    
    # ✅ Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = gender_wage_plot, width = 8, height = 6, dpi = 300)
    
    # ✅ Extract summary statistics safely
    safe_max <- function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
    safe_min <- function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))
    safe_mean <- function(x) ifelse(all(is.na(x)), 0, round(mean(x, na.rm = TRUE), 0))
    
    highest_admin <- filtered_data %>%
      filter(indicator_label == "Public Administration", value_percentage == safe_max(value_percentage)) %>%
      pull(country_name) %>%
      first() %>%
      coalesce("N/A")
    
    lowest_admin <- filtered_data %>%
      filter(indicator_label == "Public Administration", value_percentage == safe_min(value_percentage)) %>%
      pull(country_name) %>%
      first() %>%
      coalesce("N/A")
    
    highest_education <- filtered_data %>%
      filter(indicator_label == "Education", value_percentage == safe_max(value_percentage)) %>%
      pull(country_name) %>%
      first() %>%
      coalesce("N/A")
    
    lowest_education <- filtered_data %>%
      filter(indicator_label == "Education", value_percentage == safe_min(value_percentage)) %>%
      pull(country_name) %>%
      first() %>%
      coalesce("N/A")
    
    highest_health <- filtered_data %>%
      filter(indicator_label == "Health", value_percentage == safe_max(value_percentage)) %>%
      pull(country_name) %>%
      first() %>%
      coalesce("N/A")
    
    lowest_health <- filtered_data %>%
      filter(indicator_label == "Health", value_percentage == safe_min(value_percentage)) %>%
      pull(country_name) %>%
      first() %>%
      coalesce("N/A")
    
    avg_admin <- safe_mean(filtered_data$value_percentage[filtered_data$indicator_label == "Public Administration"])
    avg_education <- safe_mean(filtered_data$value_percentage[filtered_data$indicator_label == "Education"])
    avg_health <- safe_mean(filtered_data$value_percentage[filtered_data$indicator_label == "Health"])
    
    first_country_admin <- filtered_data %>%
      filter(country_name == first_country, indicator_label == "Public Administration") %>%
      pull(value_percentage) %>%
      first() %>%
      coalesce(0) %>%
      round(0)
    
    first_country_education <- filtered_data %>%
      filter(country_name == first_country, indicator_label == "Education") %>%
      pull(value_percentage) %>%
      first() %>%
      coalesce(0) %>%
      round(0)
    
    # ✅ Compare first country with others using rounded numbers
    comparison_admin <- if (first_country_admin > avg_admin) {
      paste0("This is higher than the average of ", avg_admin, "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", avg_admin, "% across the other selected countries.")
    }
    
    comparison_education <- if (first_country_education > avg_education) {
      paste0("This is higher than the average of ", avg_education, "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", avg_education, "% across the other selected countries.")
    }
    
    # ✅ Ensure all numbers in interpretation_text are rounded integers
    interpretation_text <- paste0(
      "This graph compares the gender wage premium in the public sector across different industries. ",
      "On average, the wage premium in Public Administration is ", avg_admin, "%, in Education it is ", avg_education, "%, ",
      "and in Health it is ", avg_health, "%.\n\n",
      "The highest wage premium in Public Administration is in ", highest_admin, ", while the lowest is in ", lowest_admin, ". ",
      "In Education, the highest wage premium is observed in ", highest_education, ", whereas the lowest is in ", lowest_education, ". ",
      "For Health, the highest gender wage premium is in ", highest_health, ", while the lowest is in ", lowest_health, ".\n\n",
      "In ", first_country, ", the wage premium in Public Administration is ", first_country_admin, "%. ", 
      comparison_admin, "\n",
      "In Education, the wage premium in ", first_country, " is ", first_country_education, "%. ",
      comparison_education
    )
    
    # ✅ Add Image and Interpretation to the Document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the gender wage premium in the public sector across different industries.", style = "Normal") %>% 
      body_add_par(interpretation_text, style = "Normal")
    
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
      " using the Worldwide Bureaucracy Indicators (WWBI). The primary data source is the Labor Force Survey , conducted by the National Statistics Office. ",
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
  
#Pay compression 
  
  output$paycompression_plot <- renderPlotly({
    req(input$selected_countries)  # Ensure input is selected
    
    # **Ensure indicator_label is Character (Not Factor)**
    pay_compression <- pay_compression %>%
      mutate(indicator_label = as.character(indicator_label),
             country_name = as.character(country_name))  # Convert country names too
    
    # **Debugging: Print Key Information**
    print("Unique indicator_name values before filtering:")
    print(unique(pay_compression$indicator_name))
    print("Unique indicator_label values before filtering:")
    print(unique(pay_compression$indicator_label))
    print("Column names before filtering:")
    print(colnames(pay_compression))
    
    # **Filter for selected countries**
    filtered_data <- pay_compression %>%
      filter(country_name %in% input$selected_countries) %>%
      filter(indicator_label %in% c("Public Sector", "Private Sector")) %>%
      pivot_wider(names_from = indicator_label, values_from = value) %>%
      drop_na()
    
    # **Debugging: Check if filtering worked**
    if (nrow(filtered_data) == 0) {
      print("🚨 No data available after filtering! Check input selections.")
      print("Available country_name values:")
      print(unique(pay_compression$country_name))  # Debug available countries
      return(NULL)  # Stop execution if no valid data
    }
    
    # **Debugging: Check column names after pivot**
    print("Column names after pivot:")
    print(colnames(filtered_data))
    
    # **Ensure Public and Private Sector columns exist**
    if (!("Public Sector" %in% colnames(filtered_data)) || !("Private Sector" %in% colnames(filtered_data))) {
      print("🚨 Missing expected columns after pivoting!")
      return(NULL)  # Prevents error if columns are missing
    }
    
    # **Check if we have enough numeric values to run regression**
    if (sum(!is.na(filtered_data$`Public Sector`)) < 2 || sum(!is.na(filtered_data$`Private Sector`)) < 2) {
      print("🚨 Not enough non-NA values for regression!")
      return(NULL)  # Stops execution if insufficient data
    }
    
    # **Fit a linear trend line safely**
    trend_model <- lm(`Public Sector` ~ `Private Sector`, data = filtered_data)
    trend_line <- predict(trend_model, newdata = data.frame(`Private Sector` = filtered_data$`Private Sector`))
    
    # **Generate plot**
    plot_ly(data = filtered_data, 
            x = ~`Private Sector`, 
            y = ~`Public Sector`, 
            type = 'scatter', 
            mode = 'markers+text',
            text = ~country_name,
            textposition = "top center",
            marker = list(size = 10, color = "#003366", opacity = 0.7)) %>%
      add_trace(
        x = filtered_data$`Private Sector`,
        y = trend_line,
        type = "scatter",
        mode = "lines",
        line = list(color = "gray", dash = "dash"),
        name = "Trendline"
      ) %>%
      add_trace(
        x = c(min(filtered_data$`Private Sector`, na.rm = TRUE)), 
        y = c(min(filtered_data$`Private Sector`, na.rm = TRUE)), 
        type = "scatter",
        mode = "lines",
        line = list(color = "gold", width = 2),
        name = "45-degree line"
      ) %>%
      layout(title = "Pay Compression: Public vs. Private Sector",
             xaxis = list(title = "Private Sector Pay Compression"),
             yaxis = list(title = "Public Sector Pay Compression"),
             showlegend = FALSE, 
             plot_bgcolor = "white",
             paper_bgcolor = "white")
  })
  
    output$note_dotplot <- renderText({
      "Note: This graph compares pay compression ratios in the public and private sectors. The 45-degree line represents equal compression in both sectors, while the trendline provides a visual reference for overall patterns across countries."
    })
    output$downloadPayCompressionDoc <- downloadHandler(
      filename = function() { paste0("Pay_Compression_Report_", Sys.Date(), ".docx") },
      content = function(file) {
        # Filter data for selected countries and pay compression indicators
        filtered_data_df <- pay_compression %>%
          filter(country_name %in% input$countries_first) %>%
          filter(indicator_name %in% c("Pay compression ratio in public sector (ratio of 90th/10th percentile)",
                                       "Pay compression ratio in private sector (ratio of 90th/10th percentile)")) %>%
          pivot_wider(names_from = indicator_name, values_from = value) %>%
          rename(Public_Sector = `Pay compression ratio in public sector (ratio of 90th/10th percentile)`,
                 Private_Sector = `Pay compression ratio in private sector (ratio of 90th/10th percentile)`) %>%
          drop_na()
        
        req(nrow(filtered_data_df) > 0) # Ensure there is data before proceeding
        
        # Get the first selected country for the report title
        countries <- if (!is.null(input$countries) & length(input$countries) > 0) input$countries[1] else "Selected Countries"
        report_title <- paste("Pay Compression Analysis Report -", countries)
        
        # Create Word document
        doc <- read_docx()
        title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
        doc <- doc %>% body_add_fpar(fpar(ftext(report_title, prop = title_style)))
        
        # Add Introduction
        doc <- doc %>% body_add_par("Introduction", style = "heading 2") %>% 
          body_add_par("This report presents an analysis of pay compression ratios in the public and private sectors across selected countries. 
                    Pay compression is measured as the ratio of wages at the 90th percentile to the 10th percentile, providing insights into 
                    wage inequality within each sector. The analysis compares these ratios and examines trends across different economies.", 
                       style = "Normal")
        
        # Create scatter plot with trend line
        plot <- ggplot(filtered_data_df, aes(x = Private_Sector, y = Public_Sector, label = country_name)) +
          geom_point(color = "#003366", size = 3) +
          geom_text(vjust = -0.5, size = 3) +
          geom_smooth(method = "lm", color = "gray", linetype = "dashed") + # Trendline
          geom_abline(slope = 1, intercept = 0, color = "gold", size = 1.2) + # 45-degree reference line
          labs(title = "Pay Compression: Public vs. Private Sector",
               x = "Private Sector Pay Compression",
               y = "Public Sector Pay Compression") +
          theme_minimal()
        
        # Add plot to Word document
        doc <- doc %>% body_add_gg(value = plot, style = "centered") 
        
        # Add explanatory note
        doc <- doc %>% body_add_par("Note: This graph compares pay compression ratios in the public and private sectors. 
                                 The 45-degree line represents equal compression in both sectors, while the trendline 
                                 provides a visual reference for overall patterns across countries.", style = "Normal")
        
        # Save document
        print(doc, target = file)
      }
    )
    
  #Pay compression section  
    
    generate_pay_compression_section <- function(doc, selected_countries) {
      # Filter data for selected countries and relevant indicators
      filtered_data_df <- pay_compression %>%
        filter(country_name %in% selected_countries) %>%
        filter(indicator_label %in% c("Public Sector", "Private Sector")) %>%
        pivot_wider(names_from = indicator_label, values_from = value) %>%
        drop_na()
      
      # Ensure that the dataset is not empty
      if (nrow(filtered_data_df) == 0) {
        doc <- doc %>% body_add_par("No data available for Pay Compression Analysis.", style = "Normal")
        return(doc)
      }
      
      # ✅ Extract the first selected country
      first_country <- selected_countries[1]
      
      # ✅ Compute summary statistics for all selected countries
      country_summary <- filtered_data_df %>%
        group_by(country_name) %>%
        summarise(
          public_compression = round(mean(`Public Sector`, na.rm = TRUE), 1),  
          private_compression = round(mean(`Private Sector`, na.rm = TRUE), 1) 
        )
      
      # ✅ Find highest and lowest compression rates for both sectors
      highest_public <- country_summary %>% filter(public_compression == max(public_compression, na.rm = TRUE)) %>% pull(country_name)
      lowest_public <- country_summary %>% filter(public_compression == min(public_compression, na.rm = TRUE)) %>% pull(country_name)
      
      highest_private <- country_summary %>% filter(private_compression == max(private_compression, na.rm = TRUE)) %>% pull(country_name)
      lowest_private <- country_summary %>% filter(private_compression == min(private_compression, na.rm = TRUE)) %>% pull(country_name)
      
      # ✅ Extract values for the first selected country
      first_country_values <- country_summary %>% filter(country_name == first_country)
      
      first_public_compression <- first_country_values %>% pull(public_compression) %>% coalesce(NA)
      first_private_compression <- first_country_values %>% pull(private_compression) %>% coalesce(NA)
      
      # ✅ Compute averages for the remaining selected countries
      other_countries_avg <- country_summary %>%
        filter(country_name != first_country) %>%
        summarise(
          avg_public_compression = round(mean(public_compression, na.rm = TRUE), 1),
          avg_private_compression = round(mean(private_compression, na.rm = TRUE), 1)
        )
      
      # ✅ Determine if first country has a high or low compression rate
      rank_public <- rank(-country_summary$public_compression, ties.method = "min")[country_summary$country_name == first_country]
      rank_private <- rank(-country_summary$private_compression, ties.method = "min")[country_summary$country_name == first_country]
      
      public_position <- ifelse(rank_public == 1, "the highest", ifelse(rank_public == nrow(country_summary), "the lowest", "in the middle range"))
      private_position <- ifelse(rank_private == 1, "the highest", ifelse(rank_private == nrow(country_summary), "the lowest", "in the middle range"))
      
      # ✅ Construct interpretation text with ranking and comparisons
      interpretation_text <- paste0(
        "This figure compares pay compression ratios (90th/10th percentile) in the public and private sectors.\n\n",
        "For ", first_country, ", the pay compression ratio is ", first_public_compression, 
        " in the public sector and ", first_private_compression, " in the private sector.\n\n",
        "Among the selected countries, ", highest_public, " has the highest public sector pay compression, while ",
        lowest_public, " has the lowest public sector pay compression.\n\n",
        "In the private sector, ", highest_private, " has the highest pay compression, whereas ",
        lowest_private, " has the lowest.\n\n",
        first_country, " is ranked ", public_position, " in public sector compression and ",
        private_position, " in private sector compression compared to other selected countries.\n\n",
        "A higher compression ratio indicates greater income disparity within the sector. The trendline provides an overall pattern, and the 45-degree reference line represents equality between public and private sector compression."
      )
      
      # ✅ Add section header and introduction
      doc <- doc %>% 
        body_add_par("Pay Compression in the Public and Private Sectors", style = "heading 2") %>%
        body_add_par("This section analyzes income disparity within the public and private sectors by comparing pay compression ratios.", style = "Normal")
      
      # ✅ Create scatter plot
      plot <- ggplot(filtered_data_df, aes(x = `Private Sector`, y = `Public Sector`, label = country_name)) +
        geom_point(color = "#003366", size = 3) +
        geom_text(vjust = -0.5, size = 3) +
        geom_smooth(method = "lm", color = "gray", linetype = "dashed") + # Trendline
        geom_abline(slope = 1, intercept = 0, color = "gold", size = 1.2) + # 45-degree reference line
        labs(title = "Pay Compression: Public vs. Private Sector",
             x = "Private Sector Pay Compression",
             y = "Public Sector Pay Compression") +
        theme_minimal()
      
      # ✅ Save plot as image
      img_path <- tempfile(fileext = ".png")
      ggsave(filename = img_path, plot = plot, width = 8, height = 6, dpi = 300)
      
      # ✅ Add image and interpretation text to Word document
      doc <- doc %>% 
        body_add_img(src = img_path, width = 6, height = 4) %>%
        body_add_par("Note: The 45-degree line represents equal pay compression in both sectors, while the trendline provides a visual reference for overall patterns across countries.", style = "Normal") %>%
        body_add_par(interpretation_text, style = "Normal")
      
      return(doc)
    }
    
    
    
  
  generate_conclusion_section <- function(doc) {
    # Add Section Title
    doc <- doc %>% body_add_par("Conclusion", style = "heading 1")
    
    # Add Conclusion Text
    doc <- doc %>% body_add_par(
      "This report provides a comprehensive analysis of wage bill trends, gender employment representation, and workforce participation in the public sector. 
      The findings highlight key trends and disparities across different sectors and countries.",
      style = "Normal"
    )
    
    return(doc)
  }
  
  #Download selected graphs 
  
  output$downloadSelectedGraphsDoc <- downloadHandler(
    filename = function() { 
      paste0("Wage_bill_and_public_employment_analysis_Selected_Report_", Sys.Date(), ".docx") 
    },
    content = function(file) {
      # Get the selected countries dynamically
      selected_countries <- input$countries_first  
      
      # Initialize Word document
      doc <- read_docx() 
      
      # Add Report Title
      title_style <- fp_text(color = "#722F37", font.size = 20, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Wage_bill_and_public_employment_analysis_Selected_Report_", prop = title_style)))
      doc <- generate_intro_section(doc)  # Add the Intro First
      
      # Define Section Style 
      section_style <- fp_text(color = "#003366", font.size = 14, bold = TRUE)
      
      # ✅ Dynamically include only selected sections
      selected_sections <- input$selected_graphs
      
      # ✅ Ensure selected_sections is not NULL before checking length
      if (is.null(selected_sections) || length(selected_sections) == 0) {
        doc <- doc %>% body_add_par("No sections selected for download.", style = "Normal")
      } else {
        if ("wagebill" %in% selected_sections) {
          doc <- generate_wage_bill_analysis_section(doc)
        }
        if ("wagebill_gdp" %in% selected_sections) {
          doc <- generate_gdp_analysis_section(doc, selected_countries)
        }
        if ("tertiaryeducation" %in% selected_sections) {
          doc <- generate_tertiary_education_section(doc)
        }
        if ("genderwagepremium" %in% selected_sections) {
          doc <- generate_wage_premium_gender_section(doc)
        }
        if ("wagepremiumeducation" %in% selected_sections) {
          doc <- generate_wage_premium_education_section(doc)
        }
        if ("public_employment" %in% selected_sections) {
          doc <- generate_public_sector_employment_section(doc) 
        }
        if ("wagepremiumgender" %in% selected_sections) {
          doc <- generate_wage_premium_gender_report_section(doc)
        }
        if ("public_workforce" %in% selected_sections) {
          doc <- generate_public_sector_workforce_section(doc)
        }
        if ("gender_workforce" %in% selected_sections) {
          doc <- generate_gender_workforce_section(doc)
        }
        if ("femaleoccupation" %in% selected_sections) {  # Fixed typo from "femaleocuupation"
          doc <- generate_females_occupation_groups_section(doc)
        }
        if ("wagepremium" %in% selected_sections) {
          doc <- generate_wage_premium_report_section(doc)
        }
        if ("gender_wage_premium" %in% selected_sections) {
          doc <- generate_gender_wage_premium_section(doc)
        }
        if ("pay_compression" %in% selected_sections) {
          doc <- generate_pay_compression_section (doc)
        }
      }
      
      # ✅ Save the customized report
      print(doc, target = file)
    }
  )
  
  
  #Download one single report
 
  output$downloadAllGraphsDoc <- downloadHandler(
    filename = function() { 
      paste0("Wage_bill_and_public_employment_analysis_", Sys.Date(), ".docx") 
    },
    content = function(file) {
          
      # Get the selected countries dynamically
          selected_countries <- input$countries_first  
  
      # Initialize Word document
      doc <- read_docx() 
      
      # Add Report Title
      title_style <- fp_text(color = "#722F37", font.size = 20, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Wage bill and public employment analysis", prop = title_style)))
      doc <- generate_intro_section(doc)  # Add the Intro First
      
      #Define Section Style 
      
      section_style <- fp_text(color = "#003366", font.size = 14, bold = TRUE)
      
      # Add Sections from Each Tab
      doc <- generate_wage_bill_analysis_section(doc) #  Wage Bill Analysis
      doc <- generate_gdp_analysis_section(doc, selected_countries)
      doc <- generate_tertiary_education_section(doc) # Tertiary Education Analysis
      doc <- generate_wage_premium_gender_section(doc) #Wage Premium Gender Analysis
      doc <- generate_wage_premium_education_section(doc) #Wage Premium by Education
      
      doc <- doc %>% body_add_fpar(fpar(ftext("The size of the public sector", prop = section_style)))
      
      doc <- generate_public_sector_employment_section(doc)  #Public Sector Employment
      doc <- generate_wage_premium_gender_report_section(doc) #Wage Premium Gender Report
      
      doc <- doc %>% body_add_fpar(fpar(ftext("Characteristics of the public sector workforce", prop = section_style)))
      
      doc <- generate_gender_workforce_section(doc) #Gender Workforce Analysis
      doc <- generate_females_occupation_groups_section(doc) #Females by Occupational Groups
      
      doc <- doc %>% 
        body_add_fpar(fpar(ftext("Competitiveness of Public Sector Wages", prop = section_style))) %>%
        body_add_par(
          "Public sector compensation should theoretically be designed with an awareness of its influence on the broader labor market. According to the theory of “compensating wage differentials,” a job should pay more (or less) depending on its non-wage characteristics that are undesirable (or desirable). Therefore, the optimal design of public sector wages should account for the greater presence of both financial and non-financial benefits in public service. Ideally, public sector wages should include a slight penalty in base wages due to the higher non-monetary benefits such as job security, pension plans, and the opportunity to serve the public. This structure means that, despite lower monetary compensation, the total de facto compensation (including these non-monetary benefits) that individuals receive would be roughly equivalent to that in the private sector. Public sector wages should also track private sector wages, maintaining a small penalty while ensuring that wage rigidities in the public sector do not create a significant gap between the two sectors. This alignment would ensure that public sector compensation remains competitive without being distortionary, preventing shortages of skills or qualified applicants in either sector.",
          style = "Normal"
        )
      
      doc <- generate_wage_premium_report_section(doc) #Public Sector Wage Premium Report
      doc <- generate_gender_wage_premium_section(doc)    # Wage Premium by industry Analysis
      doc <- generate_pay_compression_section (doc) #Pay Compression section
      
      
      # ✅ Add Conclusion Section at the End
      doc <- generate_conclusion_section(doc)  
      
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


# the end##############################################


    

   