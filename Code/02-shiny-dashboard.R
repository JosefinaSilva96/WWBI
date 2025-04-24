# Shiny Dashboard for Worldwide Bureaucracy Indicators

### Libraries

# Unique list of required packages
packages <- unique(c(
  "haven", "dplyr", "tidyr", "stringr", "labelled", "data.table",
  "ggplot2", "shiny", "shinythemes", "DT", "maps", "mapdata",
  "leaflet", "rnaturalearth", "sf", "plotly", "officer",
  "viridis", "here", "glue", "colourpicker", "wbstats", "htmlwidgets",
  "bs4Dash", "countrycode", "bslib", "ggthemes"
))

# Function to check and install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing", pkg, "..."))
    install.packages(pkg, dependencies = TRUE)
  }
}

# Install missing packages
lapply(packages, install_if_missing)

# Load all libraries
lapply(packages, function(pkg) {
  message(paste("Loading", pkg, "..."))
  library(pkg, character.only = TRUE)
})



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


world_spdf$name <- countrycode(as.character(world_spdf$admin), origin = "country.name", destination = "country.name")



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

public_sector_workforce_clean     <- read_dta(file.path(data_path, "Data/public_sector_workforce_clean.dta"))


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

pay_compression_wide <- readRDS(file.path(data_path, "Data", "pay_compression_wide.rds"))



## Shiny Dashboard ----

# ---------------------------
# UI
# ---------------------------

ui <- bootstrapPage(
  theme = bs_theme(version = 5, bootswatch = 'sandstone'),
  
  tags$style(HTML("
  /* General page background and text color */
  body, .container-fluid, .main-container, .content-wrapper, .flex-grow-1 {
    background-color: #356088 !important;
    color: white !important;
  }

  /* Typography and panels */
  h1, h2, h3, h4, h5, h6, p, .well, .card, .panel, .info-box, .custom-info-box, .box {
    color: white !important;
    background-color: transparent !important;
    border: none !important;
  }

  /* Panels and wells with a border */
  .well, .panel {
    background-color: #356088 !important;
    border: 1px solid #6fa8dc !important;
    border-radius: 8px;
  }

  /* Buttons */
  .btn, .btn-primary {
    background-color: #6fa8dc !important;
    border: none !important;
  }

  .btn:hover {
    background-color: #4a90c2 !important;
  }

  /* Link styling */
  a, a:hover {
    color: #ffffff !important;
    text-decoration: underline;
  }

  /* Sidebar styles */
  #sidebar {
    height: 100vh;
     width: 280px; /* Increased width */
     min-width: 280px;
    background-color: #2b4c66; /* Adjusted to match main background */
    padding: 20px;
    color: white;
    overflow-y: auto;
  }

  .nav-item {
    display: block;
    margin: 10px 0;
    padding: 10px 15px;
    font-size: 17px;
    font-weight: bold;
    color: white;
    background-color: transparent;
    border-radius: 6px;
    text-decoration: none;
    transition: background 0.2s;
  }

  .nav-item:hover {
    background-color: rgba(255, 255, 255, 0.1);
    cursor: pointer;
  }

  .nav-sub-item {
    margin-left: 10px;
    margin-bottom: 6px;
    padding: 6px 12px;
    font-size: 15px;
    font-weight: normal;
    color: white;
    text-decoration: none;
    display: block;
    border-radius: 4px;
  }

  .nav-sub-item:hover {
    background-color: rgba(255, 255, 255, 0.1);
    cursor: pointer;
  }

  .nav-item.active, .nav-sub-item.active {
    background-color: #6fa8dc !important;
    color: white !important;
  }

  .nav-section {
    font-size: 18px;
    font-weight: bold;
    margin-top: 25px;
    margin-bottom: 10px;
    color: white;
    padding-left: 5px;
  }

  #macro_section, 
  #public_sector_section, 
  #public_sector_workforce_section, 
  #public_sector_wages_section,
  #equity_public_sector_section {
    padding-left: 15px;
    display: none;
  }
")),
  
  # JavaScript to toggle sections
  tags$script(HTML("
    function toggleSection(sectionId) {
      var section = document.getElementById(sectionId);
      section.style.display = section.style.display === 'none' ? 'block' : 'none';
    }
  ")),
  
  # Layout
  div(class = "d-flex",
      # Sidebar
      div(
        id = "sidebar",
        div(class = "nav-item", actionLink("nav_dashboard", "Overview")),
        div(class = "nav-item", actionLink("nav_instructions", "Instructions")),
        div(class = "nav-item", actionLink("nav_metadata", "Metadata")),
        
        div(class = "nav-section", onclick = "toggleSection('macro_section')", "Macro Fundamentals of the Public Sector"),
        div(id = "macro_section",
            div(class = "nav-sub-item", actionLink("nav_wagebill", "Wage Bill Graphs")),
            div(class = "nav-sub-item", actionLink("nav_wagebill_gdp", "Wage Bill & GDP Graphs"))
        ),
        
        div(class = "nav-section", onclick = "toggleSection('public_sector_section')", "Size and Characteristics of the Public Sector"),
        div(id = "public_sector_section",
            div(class = "nav-sub-item", actionLink("nav_public_graphs", "Public Employment")),
            div(class = "nav-sub-item", actionLink("nav_public_workforce", "Employment Distribution")),
            div(class = "nav-sub-item", actionLink("nav_education", "Tertiary Education"))
        ),
        
        div(class = "nav-section", onclick = "toggleSection('public_sector_wages_section')", "Competitiveness of Public Sector Wages"),
        div(id = "public_sector_wages_section",
            div(class = "nav-sub-item", actionLink("nav_wagepremium", "Wage Premium")),
            div(class = "nav-sub-item", actionLink("nav_public_educ", "Wage Premium by Education")),
            div(class = "nav-sub-item", actionLink("nav_pay_compression", "Pay Compression"))
        ),
        
        div(class = "nav-section", onclick = "toggleSection('equity_public_sector_section')", "Equity in Public Sector"),
        div(id = "equity_public_sector_section",
            div(class = "nav-sub-item", actionLink("nav_gender_workforce", "Female Employment")),
            div(class = "nav-sub-item", actionLink("nav_female_leadership", "Female Leadership")),
            div(class = "nav-sub-item", actionLink("nav_wagepremium_gender", "Wage Premium by Gender")),
            div(class = "nav-sub-item", actionLink("nav_gender_wage_premium", "Gender Wage Premium by Industry"))
        ),
        
        div(class = "nav-item", actionLink("nav_download_all", "📥 Download All Graphs"))
      ),
      
      # Main content area
      div(class = "flex-grow-1 p-4",
          h2("Worldwide Bureaucracy Indicators"),
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
        fluidRow(
          column(6, align = "center",
                 tags$img(src = "https://raw.githubusercontent.com/JosefinaSilva96/WWBI/main/www/wbg_dec_logo.png", height = "80px")
          ),
          column(6, align = "center",
                 tags$img(src = "https://raw.githubusercontent.com/JosefinaSilva96/WWBI/main/www/wbg_institutions_logo.png", height = "80px")
          )
        ),
        h3("Overview"),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "The Worldwide Bureaucracy Indicators (WWBI) database is a unique cross-national dataset on public sector employment and wages that aims to fill an information gap, thereby helping researchers, development practitioners, and policymakers gain a better understanding of the personnel dimensions of state capability, the footprint of the public sector within the overall labor market, and the fiscal implications of the public sector wage bill. The dataset is derived from administrative data and household surveys, thereby complementing existing, expert perception-based approaches.")
        ), 
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "Contact Information: Zahid Hasnain-zhasnain@worldbank.org and
                                    Daniel Rogger-drogger@worldbank.org")
        ), 
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "We kindly ask all users of the dashboard to cite it as follows: Source: Worldwide Bureaucracy Indicators (WWBI) Dashboard – World Bank.")
        ), 
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "Disclaimer:The findings, interpretations, and conclusions presented in this dashboard are those of the World Bank staff and do not necessarily reflect the views of the World Bank, its affiliated organizations, the Executive Directors of the World Bank, or the governments they represent.
              The boundaries, colors, denominations, and other information shown on this dashboard do not imply any judgment on the part of the World Bank concerning the legal status of any territory, or the endorsement or acceptance of such boundaries. The terms “country” or “economy,” as used in this dashboard, are used for statistical convenience and do not imply political independence.")
        ), 
        fluidRow(
          column(10,
                 h3("📄 Publications"),
                 wellPanel(
                   style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
                   h4("Download Team Publications:"),
                   tags$ul(
                     tags$li(
                       downloadLink("pub1", "Innovating Bureaucracy for a More Capable Government"),
                       br(), tags$small("Report")
                     ),
                     tags$li(
                       downloadLink("pub2", "Introducing the Worldwide Bureaucracy Indicators: A New Global Dataset on Public Sector Employment and Compensation"),
                       br(), tags$small("Faisal Ali Baig- World Bank Group, Xu Han- University of Maryland, Zahid Hasnain- World Bank Group, Daniel Rogger- World Bank Group ")
                     ),
                     tags$li(
                       downloadLink("pub3", "Public Sector Employment and Compensation: An Assessment Framework"),
                       br(), tags$small("Report")), 
                       tags$li(
                         downloadLink("pub4", "Worldwide Bureaucracy Indicators"),
                         br(), tags$small("Report")
                       )
                     )
                   )
                 )
          )
      )
    } else if (tab == "instructions") {
      tagList(
        h3("📘 Instruction Manual"),
        fluidRow(
          wellPanel(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px; color: white;",
            
            p("This Dashboard is a product of the Bureaucracy Lab, a joint initiative between the Governance Global Practice and the Development Impact Evaluation (DIME) Department of the Research Group at the World Bank."),
            
            p("The dashboard allows users to explore key indicators from the Worldwide Bureaucracy Indicators (WWBI) through a variety of interactive visualizations, which can also be exported into a Word report for further use and analysis."),
            
            p("Each section of the dashboard presents a set of graphs designed to facilitate benchmarking of state capacity measures across countries, regions, and income groups. Below is a brief overview of each section:"),
            
            tags$ul(
              tags$li(tags$b("Macro-Fundamentals of the Public Sector:"), 
                      "This section shows the trends in the size of the public wage bill, expressed as a percentage of both total public expenditure and GDP. It also includes cross-country comparisons of these indicators by income level (measured by GDP per capita)."),
              tags$li(tags$b("Size and Characteristics of the Public Sector:"), 
                      "In this section, users can examine the size of public sector employment within the overall labor market, the distribution of the public workforce by sector (industry), and the educational attainment of public sector workers compared to private sector employees."),
              tags$li(tags$b("Competitiveness of Public Sector Wages:"), 
                      "This section presents the public sector wage premium or the difference in wages between public and private sector workers, adjusted for characteristics such as gender, education, and location. It also shows how this premium varies by education level and compares wage compression ratios for both public and private sector across countries."),
              tags$li(tags$b("Equity in the Public Sector:"), 
                      "This section provides insights into gender equity in public employment. The visualizations compare the representation of women in the public and private sectors, examines occupational segregation, and highlights gender wage gaps across industries within the public sector."),
              tags$li(tags$b("Download Graph Report:"), 
                      "This tab allows users to download a complete report containing all visualizations from the dashboard or create a custom report by selecting specific sections and their corresponding graphs. The reports are generated in .doc format and include prefilled text to support interpretation and analysis.")
            ),
            
            h4("🧭 How to Use the Dashboard"),
            tags$ol(
              tags$li("In each tab, select a country of interest and choose comparator countries, regions, or income groups."),
              tags$li("To check the availability of each indicator by country, users can navigate to the “Metadata” section. By selecting the indicator of interest, the map will display the countries for which data is available and the corresponding value for that indicator."),
              tags$li("In all visualization sections, the dropdown menus for country, region, or income group only display options with available data for the selected indicator. If an indicator is not available for a specific country or group, it will not appear in the dropdown list."),
              tags$li("The first country selected will appear first in the graphs and will serve as the reference point for benchmark comparisons."),
              tags$li("Each tab presents different sets of graphs, which can be downloaded individually using the Download Report in Word option."),
              tags$li("Alternatively, users may choose their country and comparators across all tabs and then go to the final tab, “Download All Graphs,” to export all selected visualizations into a comprehensive, pre-formatted Word report.")
            ),
            p("For detailed information on how indicators are constructed, data sources, and the list of countries and surveys included in the dataset, please refer to the WWBI Codebook, accessible through the button below.")
          )
        ),
        
        # Button to download the PDF
        downloadButton("download_pdf", "📥 Download Codebook"), 
        
        fluidRow(
          wellPanel(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px; color: white;",
            
            p("GitHub Repository: ", 
              tags$a(href = "https://github.com/worldbank/Worldwide-Bureaucracy-Indicators", 
                     "https://github.com/worldbank/Worldwide-Bureaucracy-Indicators", target = "_blank")),
            
            p("Data Catalog: ", 
              tags$a(href = "https://datacatalog.worldbank.org/int/home", 
                     "https://datacatalog.worldbank.org/int/home", target = "_blank"))
          )
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
          column(4, div(class = "custom-infobox", infoBox("Last Updated", "2025", icon = icon("clock"))))
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This map shows which countries have reported data for the selected indicator.")
        ),
        fluidRow(
          column(6,
                 selectInput("indicatorSelect", "Select Indicator", 
                             choices = unique(data_wwbi$indicator_name), selected = NULL)
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
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization explores the wage bill over time for selected countries.")
        ),
        fluidRow(
          column(4,
                 selectInput("countries", "Select Countries:",
                             choices = unique(data_wwbi_long$country_name),
                             selected = NULL,  # No default; user must select one or more
                             multiple = TRUE)
          ),
          column(4,
                 radioButtons("graph_choice", "Choose a measure of wage bill:",
                              choices = c("Wage Bill as % of Public Expenditure" = "Public",
                                          "Wage Bill as % of GDP" = "GDP"),
                              selected = "Public")
          )
        ),
        fluidRow(
          plotlyOutput("plotwagebill", height = "500px")
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
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
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This graph shows the relationship between the size of the wage bill and GDP per capita.")
        ),
        fluidRow(
          column(4,
                 selectInput("countries_first", "Select Countries:", 
                             choices = unique(data_wwbi_long$country_name), 
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
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_dotplot")
          )
        )
      )
      
    } else if(tab == "public_workforce") {
      tagList(
        h3("Distribution of Public Sector Employment"),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization shows the distribution of the public sector workforce across the three main industries (public administration, health and education).")
        ),
        fluidRow(
          selectInput("countries_workforce", "Select Countries for Workforce Graph",
                      choices = unique(data_wwbi_long$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("stackedBarGraph", height = "600px")
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_stackedBarGraph"))
        ),
        fluidRow(
          selectInput("selected_country", "Select Country for Second Graph",
                      choices = unique(data_wwbi_long$country_name), multiple = FALSE)
        ),
        fluidRow(
          plotlyOutput("horizontalStackedBar", height = "600px")
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
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
        h3("Workers with Tertiary Education"),
        fluidRow(
          fluidRow(
            div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
                "This visualization shows the proportion of workers with tertiary education in the public and private sectors.")
          ),
          selectInput("selected_countries", "Select Countries", 
                      choices = unique(data_wwbi_long$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("barPlot", height = "600px")
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_tertiaryEducation"))
        ),
        fluidRow(
          downloadButton("downloadGraphsWordEducation", "Download Tertiary Education Report")
        )
      )
    } else if(tab == "female_leadership") {
        tagList(
          h3("Female Leadership Occupations and Sector"),
          fluidRow(
            div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
                "This visualization shows the share of females in various occupational groups (Managers/Clerks) in the public and private sectors across selected countries.")
          ),
          fluidRow(
            selectInput("selected_countries", "Select Countries", 
                        choices = unique(data_wwbi_long$country_name), multiple = TRUE)
          ),
          fluidRow(
            plotlyOutput("barPlotwomen", height = "600px")
          ),
          fluidRow(
            div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
                textOutput("note_barPlotwomen"))
          ),
          fluidRow(
            downloadButton("downloadGraphsWordfemale", "Download Female Leadership Occupations Report")
          )
        )
    } else if(tab == "wagepremium_gender") {
      tagList(
        h3("Public Sector Wage Premium by Gender"),
        
        # Description Box
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization explores the public sector wage premium by gender across selected countries and its trend over time.")
        ),
        
        # Multi-Country Selection for First Graph
        fluidRow(
          selectInput("countries_first", "Select Countries for First Graph", 
                      choices = unique(data_wwbi_long$country_name), 
                      multiple = TRUE)
        ),
        
        # First Graph Output - Multi-Country Dot Plot
        fluidRow(
          plotlyOutput("firstGraphGenderWagePremium", height = "600px")
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_firstGraphGenderWagePremium"))
        ),
        # Single-Country Selection for Second Graph
        fluidRow(
          selectInput("country_second", "Select Country for Second Graph", 
                      choices = unique(data_wwbi_long$country_name), 
                      multiple = FALSE)
        ),
        
        # Second Graph Output - Single-Country Line Plot
        fluidRow(
          plotlyOutput("secondGraphGenderWagePremium", height = "600px")
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_secondGraphGenderWagePremium"))
        ),
        
        # Download Button
        fluidRow(
          downloadButton("downloadGraphsWordGenderWagePremium", "Download Public Sector Wage Premium by Gender Report")
        )
      )
      
    } else if(tab == "wagepremium") {
      tagList(
        h3("Public Sector Wage Premium"),
        fluidRow(
          selectInput("countries_wage_premium", "Select Countries for First Graph", 
                      choices = unique(data_wwbi_long$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("dotPlot", height = "500px")
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_wage_premium"))
        ),
        fluidRow(
          downloadButton("downloadWagePremiumReport", "Download Public Sector Wage Premium Report")
        )
      )
    } else if(tab == "public_educ") {
      tagList(
        h3("Public Sector Wage Premium by Education Level"),
        
        # Description Box
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization explores the public sector wage premium by education level, compared to private formal workers.")
        ),
        
        # Country Selection
        fluidRow(
          selectInput("selected_country", "Select Country for Graph", 
                      choices = unique(data_wwbi_long$country_name), 
                      multiple = FALSE)
        ),
        
        # Bar Plot Output
        fluidRow(
          plotlyOutput("education_wage_premium_plot", height = "600px")
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_education_wage_premium"))
        ),
        # Download Button
        fluidRow(
          downloadButton("downloadEducationWagePremium", "Download Public Sector Wage premium by Education Level Report")
        )
      )
    } else if(tab == "public_graphs") {
      tagList(
        h3("Public Sector Employment Graphs"),
        
        # Description Box
        #fluidRow(
          #div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              #"Note: This indicator represents the size of public sector employment in the labor market for the most recent year available in each country.")
        #),
        
        # Multi-Country Selection for First Graph
        fluidRow(
          selectInput("countries_first", "Select Countries for First Graph", 
                      choices = unique(data_wwbi_long$country_name), multiple = TRUE)
        ),
        
        # Dot Plot Output
        fluidRow(
          plotlyOutput("firstGraphpublic", height = "600px")
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_firstGraphpublic"))
        ),
        # Single-Country Selection for Line Graph
        fluidRow(
          selectInput("country_second", "Select Country for Second Graph", 
                      choices = unique(data_wwbi_long$country_name), multiple = FALSE)
        ),
        
        # Line Plot Output
        fluidRow(
          plotlyOutput("secondGraphpublic", height = "600px")
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_secondGraphpublic"))
        ),
        # Download Button
        fluidRow(
          downloadButton("downloadGraphsWord", "Download Graphs as Word File")
        )
      )
    } else if(tab == "gender_workforce") {
      tagList(
        h3("Female share of employment"),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization explores female employment in the public and private sectors across selected countries.")
        ),
        fluidRow(
          selectInput("countries_gender", "Select Countries for First Graph", 
                      choices = unique(data_wwbi_long$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotlyOutput("firstGraphGenderWorkforce")
        ),
        fluidRow(
          selectInput("country_gender", "Select Country for Second Graph", 
                      choices = unique(data_wwbi_long$country_name), multiple = FALSE)
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_firstGraphGenderWorkforce"))
        ),
        fluidRow(
          plotlyOutput("secondGraphGenderWorkforce")
        ),
        fluidRow(
          downloadButton("downloadGraphsWordGender", "Download Female share of employment Report")
        ), 
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_secondGraphGenderWorkforce"))
        )
      )
    } else if(tab == "gender_wage_premium") {
      tagList(
        h3("Gender Wage Premium in Public Sector by Industry"),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization explores the gender wage premium in the public sector by industry across selected countries.")
        ),
        fluidRow(
          selectInput("selected_countries", "Select Countries", 
                      choices = unique(data_wwbi_long$country_name), multiple = TRUE)
        ),
        fluidRow(
          plotOutput("gender_wage_barplot", height = "600px")
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_gender_wage_barplot"))
        ),
        fluidRow(
          downloadButton("downloadGenderWagePremium", "Download Gender Wage premium in Public Sector by Industry Report")
        )
      )
    } else if(tab == "pay_compression") {
      tagList(
        h3("Pay Compression Ratios"),
        
        # Section Description
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization explores pay compression in the public and private sectors across selected countries.")
        ),
        
        # Country Selection
        fluidRow(
          selectInput(
            inputId = "countries_first",
            label = "Select countries",
            choices = unique(data_wwbi_long$country_name),
            multiple = TRUE
          )# Default country selected
        ),
        
        # Scatter Plot Output (Fix: Use plotlyOutput instead of plotOutput)
        fluidRow(
          plotlyOutput("paycompression_plot", height = "600px")
        ),
        
        # Note/Explanation
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_dotplot"))
        ),
        
        # Download Button for Report
        fluidRow(
          downloadButton("downloadPayCompressionDoc", "Download Pay Compression Ratios Report")
        )
      )
    } else if(tab == "download_all") {
      tagList(
        
        # Title
        h3("Download Graph Reports"),
        
        # Description box
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              p("You can download a comprehensive report with all graphs or select specific graphs to include in your report.",
                style = "font-size: 16px; color: #333;")
          )
        ),
        
        br(),
        
        fluidRow(
          column(6, align = "center",
                 selectInput(
                   inputId = "download_report_countries",
                   label = "Select countries for the report/slides:",
                   choices = unique(data_wwbi_long$country_name),
                   selected = c("Chile"),
                   multiple = TRUE,
                   selectize = TRUE
                 )
          )
        ),
        
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
            "Public Sector Wage Premium by Education Level" = "wagepremiumeducation",
            "Public Employment" = "public_employment",
            "Public sector Wage Premium by Gender" = "wagepremiumgender",
            "Public Sector Workforce" = "public_workforce",
            "Female share of employment" = "gender_workforce",
            "Female Leadership Occupations" = "female_leadership",
            "Wage Premium" = "wagepremium",
            "Gender Wage premium in Public Sector by Industry" = "gender_wage_premium", 
            "Pay Compression Ratios" = "pay_compression"
          ),
          selected = c("wagebill", "public_employment") # Default selections
        ),
        
        br(),
        
        # ✅ Download Buttons
        fluidRow(
          column(6, align = "center",
                 downloadButton("downloadAllGraphsDoc", "\U1F4C4 Download Full Word Report",
                                style = "padding: 10px 20px; font-size: 16px; margin-top: 10px;")
          ),
          column(6, align = "center",
                 downloadButton("downloadSelectedGraphsDoc", "\U1F4C4 Download Custom Word Report",
                                style = "padding: 10px 20px; font-size: 16px; margin-top: 10px;")
          ),
          column(6, align = "center",
                 downloadButton("downloadSelectedGraphsPPT", "\U1F4CA Download PowerPoint Slides",
                                style = "padding: 10px 20px; font-size: 16px; margin-top: 10px;")
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

#Publications 
  
  output$pub1 <- downloadHandler(
    filename = function() {
      "Innovating-Bureaucracy-for-a-More-Capable-Government.pdf"
    },
    content = function(file) {
      file.copy(
        file.path(data_path, "Files", "Innovating-Bureaucracy-for-a-More-Capable-Government.pdf"),
        file
      )
    }
  )
  output$pub2 <- downloadHandler(
    filename = function() {
      "Public Administration Review - 2021 - Baig - Introducing the Worldwide Bureaucracy Indicators  A New Global Dataset on.pdf"
    },
    content = function(file) {
      file.copy(
        file.path(data_path, "Files", "Public Administration Review - 2021 - Baig - Introducing the Worldwide Bureaucracy Indicators  A New Global Dataset on.pdf"),
        file
      )
    }
  )
  
  output$pub3 <- downloadHandler(
    filename = function() {
      "Public-Sector-Employment-and-Compensation-An-Assessment-Framework.pdf"
    },
    content = function(file) {
      file.copy(
        file.path(data_path, "Files", "Public-Sector-Employment-and-Compensation-An-Assessment-Framework.pdf"),
        file
      )
    }
  )
  
  output$pub4 <- downloadHandler(
    filename = function() {
      "Worldwide-Bureaucracy-Indicators-Methodology-Insights-and-Applications.pdf"
    },
    content = function(file) {
      file.copy(
        file.path(data_path, "Files", "Worldwide-Bureaucracy-Indicators-Methodology-Insights-and-Applications.pdf"),
        file
      )
    }
  )
  
  #Sections 
  
  
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
    
    if (nrow(d) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 )))
    }
    
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
            color = ~country_name,
            type = "scatter",
            mode = "lines+markers",
            marker = list(size = 8)) %>%
      layout(
        title = title_text,
        xaxis = list(title = "Year", dtick = 2),
        yaxis = list(title = y_label),
        legend = list(title = list(text = "Indicator"))
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
  generate_wage_bill_analysis_section <- function(doc, selected_countries) {
    # ✅ Extract first selected country
    first_country <- selected_countries[1] %||% "Unknown Country"
    
    # ✅ Get region using countrycode
    first_region <- countrycode(first_country, origin = "country.name", destination = "region") %||% "its region"
    
    # ✅ Filter data
    data_country <- wage_bill_publicexp %>% filter(country_name == first_country)
    value_2010 <- data_country %>% filter(year == 2010) %>% pull(value) %>% first() %||% NA
    value_2022 <- data_country %>% filter(year == 2022) %>% pull(value) %>% first() %||% NA
    
    last_year <- data_country %>% summarise(last_year = max(year, na.rm = TRUE)) %>% pull(last_year)
    
    # ✅ Filter for all countries selected
    graph_data_gdp <- wage_bill_gdp %>% filter(country_name %in% selected_countries)
    graph_data_exp <- wage_bill_publicexp %>% filter(country_name %in% selected_countries)
    
    # ✅ Create and save graphs
    graph_gdp <- ggplot(graph_data_gdp, aes(x = year, y = value, color = country_name)) +
      geom_line(size = 1.2) + geom_point(size = 3) +
      labs(title = "Wage Bill as % of GDP Over Time", x = "Year", y = "Wage Bill (% of GDP)", color = "Country") +
      theme_minimal()
    
    graph_exp <- ggplot(graph_data_exp, aes(x = year, y = value, color = country_name)) +
      geom_line(size = 1.2) + geom_point(size = 3) +
      labs(title = "Wage Bill as % of Public Expenditure Over Time", x = "Year", y = "Wage Bill (% of Public Expenditure)", color = "Country") +
      theme_minimal()
    
    img_path_gdp <- tempfile(fileext = ".png")
    ggsave(img_path_gdp, plot = graph_gdp, width = 8, height = 6, dpi = 300)
    
    img_path_exp <- tempfile(fileext = ".png")
    ggsave(img_path_exp, plot = graph_exp, width = 8, height = 6, dpi = 300)
    
    # ✅ GDP interpretation
    regional_trend <- wage_bill_gdp %>%
      filter(wb_region == first_region) %>%
      summarise(avg_value = mean(value, na.rm = TRUE)) %>%
      pull(avg_value)
    
    relationship_text <- case_when(
      is.na(regional_trend) ~ "an uncertain relationship",
      regional_trend < -0.2 ~ "a negative relationship",
      regional_trend > 0.2 ~ "a positive relationship",
      TRUE ~ "no strong relationship"
    )
    
    first_country_wage_bill <- wage_bill_gdp %>%
      filter(country_name == first_country) %>%
      summarise(latest = max(value, na.rm = TRUE)) %>%
      pull(latest)
    
    regional_avg_wage_bill <- wage_bill_gdp %>%
      filter(wb_region == first_region) %>%
      summarise(mean_wage = mean(value, na.rm = TRUE)) %>%
      pull(mean_wage)
    
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
    
    first_country_2010 <- wage_bill_gdp %>%
      filter(country_name == first_country, year == 2010) %>%
      summarise(value = first(value)) %>%
      pull(value) %>%
      round(0) %||% NA
    
    first_country_latest <- wage_bill_gdp %>%
      filter(country_name == first_country, year == last_year) %>%
      summarise(value = first(value)) %>%
      pull(value) %>%
      round(0) %||% NA
    
    
    trend_text <- if (!is.na(first_country_2010) && !is.na(first_country_latest)) {
      if (first_country_latest > first_country_2010) {
        paste0("an increase from ", round(first_country_2010, 0), "% in 2010 to ", round(first_country_latest, 0), "% in ", last_year, ".")
      } else if (first_country_latest < first_country_2010) {
        paste0("a decrease from ", round(first_country_2010, 0), "% in 2010 to ", round(first_country_latest, 0), "% in ", last_year, ".")
      } else {
        paste0("no significant change, remaining at ", round(first_country_2010, 0), "% from 2010 to ", last_year, ".")
      }
    } else {
      "insufficient data to determine the trend."
    }
    
    gdp_interpretation <- paste0(
      "Figure 1.1 illustrates the Wage bill as a percentage of GDP for the selected countries, showing ", relationship_text, 
      " between a country’s level of economic development and the size of its public sector in the ", first_region, " region. ",
      first_country, " spends ", spending_pattern, " on its public sector wage bill compared to peers.\n\n",
      "For ", first_country, ", the wage bill as a percentage of GDP shows ", trend_text
    )
    
    doc <- doc %>%
      body_add_par("Wage Bill as % of GDP Over Time", style = "heading 2") %>%
      body_add_img(src = img_path_gdp, width = 6, height = 4) %>%
      body_add_par(gdp_interpretation, style = "Normal")
    
    # ✅ Public Expenditure analysis
    exp_data <- wage_bill_publicexp %>% filter(country_name == first_country)
    
    exp_2010 <- exp_data %>%
      filter(year == 2010) %>%
      summarise(value = max(value, na.rm = TRUE)) %>%
      pull(value) %>%
      round(0) %||% NA
    
    exp_latest <- exp_data %>%
      filter(year == last_year) %>%
      summarise(value = max(value, na.rm = TRUE)) %>%
      pull(value) %>%
      round(0) %||% NA
    
    country_volatility <- sd(exp_data$value, na.rm = TRUE)
    regional_volatility <- wage_bill_publicexp %>%
      filter(wb_region == first_region) %>%
      group_by(country_name) %>%
      summarise(vol = sd(value, na.rm = TRUE)) %>%
      summarise(mean_vol = mean(vol, na.rm = TRUE)) %>%
      pull(mean_vol)
    
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
    
    exp_trend_text <- if (!is.na(exp_2010) && !is.na(exp_latest)) {
      paste0(exp_2010, "% in 2010 and has ",
             if (exp_latest > exp_2010) "increased" else if (exp_latest < exp_2010) "decreased" else "remained the same",
             " to ", exp_latest, "% in ", last_year, ".")
    } else {
      "varied over time, as shown in Figure 1.2."
    }
    
    doc <- doc %>%
      body_add_par("Wage Bill as % of Public Expenditure Over Time", style = "heading 2") %>%
      body_add_img(src = img_path_exp, width = 6, height = 4) %>%
      body_add_par(
        paste0(
          "The wage bill as a share of public expenditures in ", first_country, " was ", exp_trend_text,
          " The public sector wage bill in ", first_country, " has exhibited ", stability_text, "."
        ), 
        style = "Normal"
      )
    
    return(doc)
  }
  
  #Slides
  
  generate_wage_bill_analysis_slide <- function(ppt, selected_countries) {
    # Validate selected countries
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      return(ppt)
    }
    
    # Filter data for graphs
    graph_data_gdp <- wage_bill_gdp %>% filter(country_name %in% selected_countries)
    graph_data_exp <- wage_bill_publicexp %>% filter(country_name %in% selected_countries)
    
    if (nrow(graph_data_gdp) == 0 && nrow(graph_data_exp) == 0) {
      return(ppt)
    }
    
    # Graph 1: Wage Bill as % of GDP
    if (nrow(graph_data_gdp) > 0) {
      graph_gdp <- ggplot(graph_data_gdp, aes(x = year, y = value, color = country_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(
          title = "Wage Bill as % of GDP Over Time", 
          x = "Year", y = "Wage Bill (% of GDP)", color = "Country"
        ) +
        theme_minimal()
      
      img_path_gdp <- tempfile(fileext = ".png")
      ggsave(img_path_gdp, plot = graph_gdp, width = 8, height = 6, dpi = 300)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path_gdp, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    # Graph 2: Wage Bill as % of Public Expenditure
    if (nrow(graph_data_exp) > 0) {
      graph_exp <- ggplot(graph_data_exp, aes(x = year, y = value, color = country_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(
          title = "Wage Bill as % of Public Expenditure Over Time", 
          x = "Year", y = "Wage Bill (% of Public Expenditure)", color = "Country"
        ) +
        theme_minimal()
      
      img_path_exp <- tempfile(fileext = ".png")
      ggsave(img_path_exp, plot = graph_exp, width = 8, height = 6, dpi = 300)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path_exp, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    return(ppt)
  }
  
  
  output$dot_plot <- renderPlotly({
    req(input$countries_first)
    
    filtered_data_df <- merged_data %>% 
      filter(country_name %in% input$countries_first)
    
    if (nrow(filtered_data_df) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # If there's data, continue with the normal plot
    filtered_data_df <- filtered_data_df %>%
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
      layout(
        title = "Wage Bill vs. Log(GDP per Capita)",
        xaxis = list(title = "Log(GDP per Capita, 2015)"),
        yaxis = list(title = "Wage Bill"),
        showlegend = FALSE,
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
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
    # Filter data
    filtered_data_df <- merged_data %>% filter(country_name %in% selected_countries)
    
    # Return message if empty
    if (nrow(filtered_data_df) == 0) {
      doc <- doc %>% body_add_par("No data available for Wage Bill vs. GDP analysis.", style = "Normal")
      return(doc)
    }
    
    # Use first selected country
    first_country <- selected_countries[1]
    
    # Summarize country-level indicators
    country_summary <- filtered_data_df %>%
      group_by(country_name) %>%
      summarise(
        wage_bill = round(mean(indicator_value, na.rm = TRUE), 0),
        gdp_per_capita = round(exp(mean(log_gdp, na.rm = TRUE)), 0)
      )
    
    # Get values for first country
    first_country_values <- country_summary %>% filter(country_name == first_country)
    first_country_wage_bill <- first_country_values$wage_bill %||% NA
    first_country_gdp <- first_country_values$gdp_per_capita %||% NA
    
    # Get regional averages
    regional_avg <- filtered_data_df %>%
      summarise(
        avg_wage_bill = round(mean(indicator_value, na.rm = TRUE), 0),
        avg_gdp_per_capita = round(exp(mean(log_gdp, na.rm = TRUE)), 0)
      )
    
    # Text interpretation
    interpretation_text <- paste0(
      "Figure 1.3 illustrates the relationship between the wage bill as a percentage of public expenditure ",
      "and GDP per capita across selected countries. The selected countries have an average wage bill of ",
      regional_avg$avg_wage_bill, "%, with a GDP per capita of $",
      format(regional_avg$avg_gdp_per_capita, big.mark = ","), ".\n\n",
      "For ", first_country, ", the wage bill represents ", first_country_wage_bill, 
      "% of public expenditure, with a GDP per capita of $",
      format(first_country_gdp, big.mark = ","), "."
    )
    
    # Section title and intro
    doc <- doc %>%
      body_add_par("Wage bill (% of public expenditure) and GDP per capita in the region", style = "heading 2") %>%
      body_add_par("This note presents evidence on public sector employment and compensation practices in relation to GDP per capita.", style = "Normal")
    
    # Plot
    plot <- ggplot(filtered_data_df, aes(x = log_gdp, y = indicator_value, color = country_name)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
      labs(
        title = "Wage Bill vs. Log(GDP per Capita)", 
        x = "Log(GDP per Capita, 2015)", 
        y = "Wage Bill (% of Public Expenditure)",
        color = "Country"
      ) +
      theme_minimal() +
      scale_color_manual(values = scales::hue_pal()(length(unique(filtered_data_df$country_name))))
    
    # Save and insert
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = plot, width = 8, height = 6, dpi = 300)
    
    doc <- doc %>%
      body_add_img(src = img_path, width = 6, height = 4) %>%
      body_add_par("Note: This indicator represents the relationship between wage bill and log(GDP per capita). The trendline provides a visual reference for the overall pattern.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  #Slides
  
  generate_gdp_analysis_slide <- function(ppt, selected_countries) {
    # Validate input
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter data
    filtered_data_df <- merged_data %>% filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data_df) == 0) {
      return(ppt)
    }
    
    # Plot
    plot <- ggplot(filtered_data_df, aes(x = log_gdp, y = indicator_value, color = country_name)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
      labs(
        title = "Wage Bill vs. Log(GDP per Capita)", 
        x = "Log(GDP per Capita, 2015)", 
        y = "Wage Bill (% of Public Expenditure)",
        color = "Country"
      ) +
      theme_minimal() +
      scale_color_manual(values = scales::hue_pal()(length(unique(filtered_data_df$country_name))))
    
    # Save plot to image
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = plot, width = 8, height = 6, dpi = 300)
    
    # Add slide with image only
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }

  
  #Workforce graphs 
  
  filtered_workforce_data <- reactive({
    req(input$countries_workforce)
    public_sector_workforce_clean %>% group_by(country_name, indicator_name) %>% slice_max(order_by = year, n = 1) %>% ungroup()
  })
  
  output$stackedBarGraph <- renderPlotly({
    req(input$countries_workforce)
    
    data_to_plot <- filtered_workforce_data() %>% 
      filter(country_name %in% input$countries_workforce)
    
    if (nrow(data_to_plot) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Define color-blind friendly palette (Okabe-Ito)
    color_blind_palette <- c(
      "Public Administration" = "#E69F00",  # orange
      "Education" = "#56B4E9",              # sky blue
      "Health" = "#009E73",                 # bluish green
      "Other" = "#F0E442"                   # yellow
    )
    
    # Create stacked bar chart
    plot_ly(data = data_to_plot,
            x = ~country_name,
            y = ~value_percentage,
            color = ~indicator_name,
            type = "bar",
            text = ~paste("Country:", country_name,
                          "<br>Indicator:", indicator_name,
                          "<br>Value:", round(value_percentage, 1), "%"),
            textposition = "auto",
            colors = color_blind_palette) %>%
      layout(
        barmode = "stack",
        title = "Public Workforce Distribution by Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Workforce Distribution (%)", range = c(0, 100)),
        legend = list(title = list(text = "<b>Indicator</b>"))
      )
  })
  
  output$note_stackedBarGraph <- renderText({
    "Note: This indicator represents the distribution of public sector employment across different industries (Public Administration, Education, Health, and Other) as a percentage of paid public employment."
  })
  output$messageOutput <- renderText({
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
    # Color-blind friendly palette (Okabe-Ito)
    color_blind_palette <- c(
      "Public Administration" = "#E69F00",  # orange
      "Education" = "#56B4E9",              # sky blue
      "Health" = "#009E73",                 # bluish green
      "Other" = "#F0E442"                   # yellow
    )
    
    plot_ly(data = data_to_plot,
            x = ~value_percentage,
            y = ~factor(year, levels = c(last_year, first_year)),
            color = ~indicator_name,
            type = "bar",
            orientation = "h",
            text = ~paste0(round(value_percentage, 1), "%"),
            textposition = "inside",
            colors = color_blind_palette) %>%
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
        scale_fill_viridis_d(option = "D") +  # Color-blind friendly discrete palette
        labs(
          title = "Public Workforce Distribution by Country",
          x = "Country",
          y = "Workforce Distribution (%)",
          fill = "Sector"
        ) +
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
          second_graph_ggplot <- ggplot(second_graph_data, aes(
            x = value_percentage,
            y = factor(year, levels = c(last_year, first_year)),
            fill = indicator_name
          )) +
            geom_bar(stat = "identity", position = "stack", orientation = "horizontal") +
            scale_fill_viridis_d(option = "D") +  # Color-blind friendly discrete palette
            labs(
              title = paste("Sectoral Distribution of Public Sector Workforce in", input$selected_country),
              x = "Percentage (%)",
              y = "Year",
              fill = "Sector"
            ) +
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
  generate_public_sector_workforce_section <- function(doc, selected_countries) {
    doc <- doc %>% body_add_par("Public Sector Workforce Analysis", style = "heading 1")
    
    doc <- doc %>% body_add_par(
      "This section presents an analysis of public workforce distribution across different sectors and countries.", 
      style = "Normal"
    )
    
    # ✅ Validate selected countries
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- selected_countries[1]
    
    # ✅ Filter data
    first_graph_data <- public_sector_workforce %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(first_graph_data) == 0) {
      doc <- doc %>% body_add_par("No data available for Public Workforce Distribution by Country.", style = "Normal")
      return(doc)
    }
    
    # ✅ Plot graph
    first_graph_ggplot <- ggplot(first_graph_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_viridis_d(option = "D") +  # Automatically assigns different accessible colors
      labs(
        title = "Public Workforce Distribution by Country", 
        x = "Country", y = "Workforce Distribution (%)", fill = "Sector"
      ) +
      theme_minimal()
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph_ggplot, width = 8, height = 4)
    
    # ✅ Sector distribution for first country
    sector_distribution <- public_sector_workforce %>%
      filter(country_name == first_country) %>%
      group_by(indicator_name) %>%
      summarise(share = mean(value_percentage, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = indicator_name, values_from = share, values_fill = list(share = 0))
    
    # ✅ Sector averages for other countries
    comparison_distribution <- public_sector_workforce %>%
      filter(country_name %in% selected_countries & country_name != first_country) %>%
      group_by(indicator_name) %>%
      summarise(avg_share = mean(value_percentage, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = indicator_name, values_from = avg_share, values_fill = list(avg_share = 0))
    
    # ✅ Extract values
    `%||%` <- function(x, y) ifelse(is.null(x) || is.na(x), y, x)
    public_admin_share <- sector_distribution$`Public Administration` %||% 0
    education_share <- sector_distribution$Education %||% 0
    health_share <- sector_distribution$Health %||% 0
    
    comparison_education_share <- comparison_distribution$Education %||% 0
    comparison_health_share <- comparison_distribution$Health %||% 0
    
    # ✅ Check for missing values
    if (public_admin_share == 0 && education_share == 0 && health_share == 0) {
      doc <- doc %>% body_add_par(
        paste0("No public sector employment data available for ", first_country, "."),
        style = "Normal"
      )
      return(doc)
    }
    
    # ✅ Largest sector logic
    sector_shares <- c(public_admin_share, education_share, health_share)
    sector_names <- c("Public Administration", "Education", "Health")
    largest_sector <- sector_names[which.max(sector_shares)]
    largest_sector_share <- max(sector_shares)
    
    # ✅ Comparison texts
    education_comparison <- if (education_share > comparison_education_share) {
      paste0("This is higher than the average of ", round(comparison_education_share, 1), "% among the other selected countries.")
    } else {
      paste0("This is lower than the average of ", round(comparison_education_share, 1), "% among the other selected countries.")
    }
    
    health_comparison <- if (health_share > comparison_health_share) {
      paste0("The health sector, while representing a smaller segment at ", 
             round(health_share, 1), "%, still surpasses the average of ", 
             round(comparison_health_share, 1), "% in other selected countries.")
    } else {
      paste0("The health sector accounts for ", round(health_share, 1), "%, aligning closely with the average of ", 
             round(comparison_health_share, 1), "% in other selected countries.")
    }
    
    # ✅ Interpretation
    sector_interpretation_text <- paste0(
      first_country, " has the highest proportion of public sector employees in ", largest_sector,
      ", with ", round(largest_sector_share, 1), "% of paid public sector workers employed in this area. ",
      "The education sector represents ", round(education_share, 1), "% of the workforce. ", education_comparison, " ",
      "The health sector accounts for ", round(health_share, 1), "%, ", health_comparison
    )
    
    # ✅ Add to document
    doc <- doc %>% 
      body_add_par("Public Workforce Distribution by Country", style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4) %>% 
      body_add_par(sector_interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  #Slides
  
  generate_public_sector_workforce_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter data
    first_graph_data <- public_sector_workforce %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(first_graph_data) == 0) {
      return(ppt)
    }
    
    # Create the bar plot
    first_graph_ggplot <- ggplot(first_graph_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_viridis_d(option = "D") +  # Automatically assigns different accessible colors
      labs(
        title = "Public Workforce Distribution by Country", 
        x = "Country", y = "Workforce Distribution (%)", fill = "Sector"
      ) +
      theme_minimal()
    
    # Save to PNG
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = first_graph_ggplot, width = 8, height = 4)
    
    # Add slide to ppt
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  
  
  
  #Workers with tertiary education
  
  output$barPlot <- renderPlotly({
    req(input$selected_countries)
    
    # Filter Data
    filtered_data <- tertiary_education %>% 
      filter(country_name %in% input$selected_countries)
    
    # Check if filtered data is empty
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Define Colors
    custom_colors <- c("as a share of private paid employees" = "#0072B2", 
                       "as a share of public paid employees" = "#D55E00")
    
    # Generate Bar Plot
    plot <- filtered_data %>%
      plot_ly(x = ~country_name, y = ~value_percentage, 
              color = ~indicator_name, colors = custom_colors, 
              type = 'bar', barmode = 'group',
              text = ~paste0(round(value_percentage, 1), "%"),
              textposition = "auto") %>%
      layout(
        title = "Workers with Tertiary Education by Sector and Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Tertiary Education (%)"),
        legend = list(title = list(text = "<b>Sector</b>"))
      )
    
    plot
  })
  
  output$note_tertiaryEducation <- renderText({
    "Note: This indicator represents the proportion of individuals with tertiary education in the public and private sectors across selected countries. It highlights differences in educational attainment among paid employees by sector."
  })
  
  output$downloadGraphsWordEducation <- downloadHandler(
    filename = function() { paste0("Workers_Tertiary_Education_Report_", Sys.Date(), ".docx") },
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
        scale_fill_manual(values = c("as a share of private paid employees" = "#0072B2", 
                                     "as a share of public paid employees" = "#D55E00")) +
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
  generate_tertiary_education_section <- function(doc, selected_countries) {
    # Add Section Title and Introduction
    doc <- doc %>%
      body_add_par("Tertiary Education Analysis", style = "heading 1") %>%
      body_add_par(
        "This section presents an analysis of tertiary education among public and private sector employees across selected countries.", 
        style = "Normal"
      )
    
    # ✅ Ensure valid country selection
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Filter relevant data
    filtered_data <- tertiary_education %>%
      filter(country_name %in% selected_countries)
    
    # ✅ Handle empty data
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Create ggplot
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "as a share of private paid employees" = "#0072B2", 
        "as a share of public paid employees" = "#D55E00"
      )) +
      labs(
        title = "Workers with Tertiary Education by Sector and Country",
        x = "Country", y = "Tertiary Education (%)", fill = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # ✅ Save image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # ✅ Summary statistics
    avg_public <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of public paid employees"], na.rm = TRUE), 1)
    avg_private <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of private paid employees"], na.rm = TRUE), 1)
    
    highest_public_country <- filtered_data %>%
      filter(indicator_name == "as a share of public paid employees") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    lowest_public_country <- filtered_data %>%
      filter(indicator_name == "as a share of public paid employees") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    highest_private_country <- filtered_data %>%
      filter(indicator_name == "as a share of private paid employees") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    lowest_private_country <- filtered_data %>%
      filter(indicator_name == "as a share of private paid employees") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    # ✅ Interpretation
    interpretation_text <- paste0(
      "This graph compares tertiary education attainment among employees in the public and private sectors across selected countries. ",
      "On average, ", avg_public, "% of public sector employees have completed tertiary education, while in the private sector, the share is ", avg_private, "%. ",
      "The country with the highest share of tertiary-educated public sector employees is ", highest_public_country, ", whereas ", lowest_public_country, " has the lowest proportion. ",
      "In the private sector, ", highest_private_country, " has the highest tertiary education level among employees, while ", lowest_private_country, " has the lowest."
    )
    
    # ✅ Add plot and interpretation to document
    doc <- doc %>%
      body_add_img(src = img_path, width = 6, height = 4) %>%
      body_add_par("This graph shows the proportion of individuals with tertiary education working in public and private sector employment.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  #Slides
  
  generate_tertiary_education_slide <- function(ppt, selected_countries) {
    # ✅ Validate input
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # ✅ Filter relevant data
    filtered_data <- tertiary_education %>%
      filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data) == 0) {
      return(ppt)
    }
    
    # ✅ Create ggplot
    plot <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "as a share of private paid employees" = "#0072B2", 
        "as a share of public paid employees" = "#D55E00"
      )) +
      labs(
        title = "Workers with Tertiary Education by Sector and Country",
        x = "Country", y = "Tertiary Education (%)", fill = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # ✅ Save plot to image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = plot, width = 8, height = 6)
    
    # ✅ Add slide with image only
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7),
              location = ph_location_type(type = "body"))
    
    return(ppt)
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
    
    # Fallback if no data
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
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
      marker = list(size = 10, opacity = 0.8, color = ~color),
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
  generate_wage_premium_report_section <- function(doc, selected_countries) {
    # Add Section Title and Intro
    doc <- doc %>%
      body_add_par("Public Sector Wage Premium Analysis", style = "heading 1") %>%
      body_add_par(
        "This section presents an analysis of public sector wage premiums compared to private sector employees across selected countries.", 
        style = "Normal"
      )
    
    # ✅ Validate country input
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Filter data
    filtered_data <- public_wage_premium %>% 
      filter(country_name %in% selected_countries) %>%
      drop_na(value_percentage)
    
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Extract first country
    first_country <- selected_countries[1]
    
    # ✅ Summary statistics
    avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
    
    highest_country <- filtered_data %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    lowest_country <- filtered_data %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    first_country_premium <- filtered_data %>%
      filter(country_name == first_country) %>%
      pull(value_percentage) %>%
      first() %>% coalesce(0)
    
    comparison_premium <- filtered_data %>%
      filter(country_name != first_country) %>%
      summarise(avg = mean(value_percentage, na.rm = TRUE)) %>%
      pull(avg) %>% coalesce(0)
    
    comparison_statement <- if (!is.na(first_country_premium) && !is.na(comparison_premium)) {
      if (first_country_premium > comparison_premium) {
        paste0("This is higher than the average of ", round(comparison_premium, 1), "% across the other selected countries.")
      } else {
        paste0("This is lower than the average of ", round(comparison_premium, 1), "% across the other selected countries.")
      }
    } else {
      "Comparison data is not available."
    }
    
    # ✅ Plot
    filtered_data <- filtered_data %>%
      mutate(color = ifelse(country_name == first_country, "#0072B2", "#D55E00"))
    
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = color)) +
      geom_point(size = 5) +
      scale_color_identity() +
      labs(title = "Public Sector Wage Premium by Country", x = "Country", y = "Wage Premium (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # ✅ Interpretation text
    interpretation_text <- paste0(
      "This graph compares public sector wage premiums across selected countries. ",
      "On average, public sector employees earn ", avg_wage_premium, "% more than private sector employees. ",
      "The country with the highest wage premium is ", highest_country, ", while ", lowest_country, " has the lowest.\n\n",
      "In ", first_country, ", the wage premium is ", round(first_country_premium, 1), "%. ",
      comparison_statement
    )
    
    # ✅ Add to document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the wage premium in the public sector relative to private sector employees across selected countries.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  
  #Female share of employment
  
  output$employment_plot <- renderPlotly({
    filtered_data <- gender_workforce %>% 
      filter(country_name %in% input$countries_workforce)
    
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    public_latest <- filtered_data %>% 
      filter(indicator_name == "Females, as a share of public paid employees") %>% 
      group_by(country_name) %>% 
      filter(year == max(year, na.rm = TRUE)) %>% 
      ungroup()
    
    private_latest <- filtered_data %>% 
      filter(indicator_name == "Females, as a share of private paid employees") %>% 
      group_by(country_name) %>% 
      filter(year == max(year, na.rm = TRUE)) %>% 
      ungroup()
    
    if (nrow(public_latest) == 0 || nrow(private_latest) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for one or both sectors in the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    plot <- plot_ly(data = public_latest,
                    x = ~country_name,
                    y = ~value_percentage,
                    type = 'bar',
                    color = I("#0072B2"),
                    text = ~paste("Country: ", country_name, 
                                  "<br>Last year available: ", year, 
                                  "<br>Employment (%): ", round(value_percentage, 2)),
                    hoverinfo = "text",
                    name = "Public Sector",
                    showlegend = TRUE) %>%
      add_trace(data = private_latest,
                x = ~country_name,
                y = ~value_percentage,
                type = "scatter",
                mode = "markers",
                marker = list(size = 10, color = "#D55E00"),
                name = "Private Sector",
                text = ~paste("Country: ", country_name, 
                              "<br>Last year available: ", year, 
                              "<br>Employment (%): ", round(value_percentage, 2)),
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
    custom_colors <- c("Females, as a share of private paid employees" = "#0072B2", 
                       "Females, as a share of public paid employees" = "#D55E00")
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
    plot <- plot_ly(data = data_to_plot_long,
                    x = ~year,
                    y = ~value_percentage,
                    color = ~indicator_label,
                    colors = c("Male" = "#0072B2", "Female" = "#D55E00"),
                    type = "scatter",
                    mode = "lines+markers") %>%
      add_annotations(x = ~year,
                      y = ~value_percentage,
                      text = ~round(value_percentage, 2),
                      showarrow = FALSE,
                      font = list(size = 12, color = "black"),
                      xanchor = "center",
                      yanchor = "bottom") %>%
      layout(title = "Public Sector Wage Premium Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Wage Premium (%)"),
             legend = list(title = list(text = "Gender")))
    
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
  generate_wage_premium_gender_section <- function(doc, selected_countries) {
    # Add section title and intro
    doc <- doc %>%
      body_add_par("Wage Premium Gender Analysis", style = "heading 1") %>%
      body_add_par(
        "This section presents evidence on public sector employment and compensation practices by gender across selected countries.",
        style = "Normal"
      )
    
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- selected_countries[1]
    
    # ────────────────────────────────────────────────
    # ✅ Graph 1: Multi-Country Wage Premium by Gender
    # ────────────────────────────────────────────────
    data_to_plot <- gender_wage_premium_last %>% filter(country_name %in% selected_countries)
    
    if (nrow(data_to_plot) > 0) {
      # Graph
      multi_country_plot <- ggplot(data_to_plot, aes(x = country_name, y = value_percentage, color = indicator_label)) +
        geom_point(size = 3) +
        scale_color_manual(
          values = c(
            "Male" = "#0072B2",    # Colorblind-friendly blue
            "Female" = "#D55E00"   # Colorblind-friendly orange/red
          )
        ) +
        labs(
          title = "Public Sector Wage Premium by Gender (Latest Year)",
          x = "Country", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = multi_country_plot, width = 8, height = 6)
      
      # Summary statistics
      avg_premium <- round(mean(data_to_plot$value_percentage, na.rm = TRUE), 1)
      
      highest_country <- data_to_plot %>%
        filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
        pull(country_name) %>% first()
      
      lowest_country <- data_to_plot %>%
        filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
        pull(country_name) %>% first()
      
      first_country_premium <- data_to_plot %>%
        filter(country_name == first_country) %>%
        summarise(avg = mean(value_percentage, na.rm = TRUE)) %>%
        pull(avg) %>% round(1)
      
      comparison_premium <- data_to_plot %>%
        filter(country_name != first_country) %>%
        summarise(avg = mean(value_percentage, na.rm = TRUE)) %>%
        pull(avg) %>% round(1)
      
      comparison_statement <- if (first_country_premium > comparison_premium) {
        paste0("This is higher than the average of ", comparison_premium, "% across the other selected countries.")
      } else {
        paste0("This is lower than the average of ", comparison_premium, "% across the other selected countries.")
      }
      
      interpretation1 <- paste0(
        "This graph compares public sector wage premiums by gender across selected countries. ",
        "On average, the gender wage premium is ", avg_premium, "%. ",
        "The highest premium is observed in ", highest_country, ", and the lowest in ", lowest_country, ". ",
        "In ", first_country, ", the premium is ", first_country_premium, "%. ", comparison_statement
      )
      
      # Add to doc
      doc <- doc %>%
        body_add_par("Wage Premium by Gender (Multi-Country)", style = "heading 2") %>%
        body_add_img(src = img_path1, width = 6, height = 4) %>%
        body_add_par(interpretation1, style = "Normal")
    } else {
      doc <- doc %>% body_add_par("No data available for the selected countries (multi-country comparison).", style = "Normal")
    }
    
    # ─────────────────────────────────────────────
    # ✅ Graph 2: Time Series for First Country
    # ─────────────────────────────────────────────
    time_series_data <- gender_wage_premium %>% filter(country_name == first_country)
    
    if (nrow(time_series_data) > 0) {
      time_series_plot <- ggplot(time_series_data, aes(x = year, y = value_percentage, color = indicator_label, group = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(
          values = c(
            "Male" = "#0072B2",    # Blue - Colorblind-friendly
            "Female" = "#D55E00"   # Vermillion - Colorblind-friendly
          )
        ) +
        labs(
          title = paste("Wage Premium by Gender Over Time in", first_country),
          x = "Year", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal()
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = time_series_plot, width = 8, height = 6)
      
      first_year <- min(time_series_data$year, na.rm = TRUE)
      last_year  <- max(time_series_data$year, na.rm = TRUE)
      
      get_value_for_year <- function(df, year, gender) {
        val <- df %>% filter(year == year, indicator_label == gender) %>%
          pull(value_percentage) %>% first()
        ifelse(is.na(val), "Data not available", round(val, 1))
      }
      
      male_first  <- get_value_for_year(time_series_data, first_year, "Male")
      female_first <- get_value_for_year(time_series_data, first_year, "Female")
      male_last   <- get_value_for_year(time_series_data, last_year, "Male")
      female_last  <- get_value_for_year(time_series_data, last_year, "Female")
      
      interpretation2 <- paste0(
        "This graph shows how the public sector wage premium by gender evolved in ", first_country, ". ",
        "In ", first_year, ", the wage premium was ", male_first, "% for men and ", female_first, "% for women. ",
        "By ", last_year, ", it was ", male_last, "% for men and ", female_last, "% for women."
      )
      
      # Add to doc
      doc <- doc %>%
        body_add_par("Wage Premium by Gender (Over Time)", style = "heading 2") %>%
        body_add_img(src = img_path2, width = 6, height = 4) %>%
        body_add_par(interpretation2, style = "Normal")
    } else {
      doc <- doc %>% body_add_par(paste("No time series data available for", first_country), style = "Normal")
    }
    
    return(doc)
  }
  
  #Slides
  
  generate_wage_premium_gender_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    first_country <- selected_countries[1]
    
    # ────────────────────────────────────────────────
    # Graph 1: Multi-Country Wage Premium by Gender
    # ────────────────────────────────────────────────
    data_to_plot <- gender_wage_premium_last %>% filter(country_name %in% selected_countries)
    
    if (nrow(data_to_plot) > 0) {
      multi_country_plot <- ggplot(data_to_plot, aes(x = country_name, y = value_percentage, color = indicator_label)) +
        geom_point(size = 3) +
        scale_color_manual(
          values = c(
            "Male" = "#0072B2",    # Colorblind-friendly blue
            "Female" = "#D55E00"   # Colorblind-friendly orange/red
          )
        ) +
        labs(
          title = "Public Sector Wage Premium by Gender (Latest Year)",
          x = "Country", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = multi_country_plot, width = 8, height = 6)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path1, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    # ─────────────────────────────────────────────
    # Graph 2: Time Series for First Country
    # ─────────────────────────────────────────────
    time_series_data <- gender_wage_premium %>% filter(country_name == first_country)
    
    if (nrow(time_series_data) > 0) {
      time_series_plot <- ggplot(time_series_data, aes(x = year, y = value_percentage, color = indicator_label, group = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(
          values = c(
            "Male" = "#0072B2",    # Blue - Colorblind-friendly
            "Female" = "#D55E00"   # Vermillion - Colorblind-friendly
          )
        ) +
        labs(
          title = paste("Wage Premium by Gender Over Time in", first_country),
          x = "Year", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal()
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = time_series_plot, width = 8, height = 6)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path2, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    return(ppt)
  }

  # Wage premium by Education Level 
  
  # Render the Public Sector Wage Premium by Education Level Graph
  
  output$education_wage_premium_plot <- renderPlotly({
    req(input$selected_country)  # Ensure a country is selected
    
    # Filter the data set for the selected country
    filtered_data <- public_wage_premium_educ %>%
      filter(country_name == input$selected_country) %>%
      drop_na(value_percentage)  # Remove NAs
    
    # Check if there's data
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Define colorblind-friendly colors for education levels
    education_colors <- c(
      "No Education"        = "#E69F00",  # Orange
      "Primary Education"   = "#56B4E9",  # Sky Blue
      "Secondary Education" = "#009E73",  # Bluish Green
      "Tertiary Education"  = "#D55E00"   # Vermillion
    )
    
    # Create the bar plot
    p <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = education_colors) +
      labs(
        title = "Public Sector Wage Premium by Education Level (Compared to Private Formal Workers)",
        x = "Education Level",
        y = "Wage Premium (%)",
        fill = "Education Level"
      ) +
      theme_minimal()
    
    ggplotly(p)  # Convert ggplot to Plotly for interactivity
  })
  
  output$note_education_wage_premium <- renderText({
    "Note: This indicator represents the public sector wage premium across different education levels, comparing public sector wages to those of private formal workers."
  })
  
  output$downloadEducationWagePremium <- downloadHandler(
    filename = function() {
      paste0("Public_Sector_Wage_Premium_Education Level_", Sys.Date(), ".docx")
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
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c(
          "No Education"       = "#E69F00",  # Orange
          "Primary Education"  = "#56B4E9",  # Sky Blue
          "Secondary Education"= "#009E73",  # Bluish Green
          "Tertiary Education" = "#D55E00"   # Vermillion
        )) +
        labs(
          title = "Public Sector Wage Premium by Education Level",
          x = "Education Level",
          y = "Wage Premium (%)",
          fill = "Education Level"
        ) +
        theme_minimal()
      
      ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
      
      # Add Image to Word Document
      doc <- doc %>% body_add_img(src = img_path, width = 6, height = 4)
      
      # Save the Word Document
      print(doc, target = file)
    }
  )
  
  generate_wage_premium_education_section <- function(doc, selected_countries) {
    # Section Title
    doc <- doc %>% body_add_par("Public Sector Wage Premium by Education Level", style = "heading 1")
    
    # Validate selected countries
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # Extract first country
    first_country <- selected_countries[1]
    
    # Add intro paragraph
    doc <- doc %>% body_add_par(
      paste0("This section presents an analysis of public sector wage premiums based on different education levels for ", 
             first_country, ". The comparison is made against private sector formal workers."), 
      style = "Normal"
    )
    
    # Filter data
    filtered_data <- public_wage_premium_educ %>%
      filter(country_name %in% selected_countries) %>%
      drop_na(value_percentage)
    
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # Plot
    ggplot_obj <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "No Education"       = "#E69F00",  # Orange
        "Primary Education"  = "#56B4E9",  # Sky Blue
        "Secondary Education"= "#009E73",  # Bluish Green
        "Tertiary Education" = "#D55E00"   # Vermillion
      )) +
      labs(
        title = "Public Sector Wage Premium by Education Level",
        x = "Education Level",
        y = "Wage Premium (%)",
        fill = "Education Level"
      ) +
      theme_minimal()
    
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # Summary stats
    highest_education <- filtered_data %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(indicator_name) %>%
      first()
    
    lowest_education <- filtered_data %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(indicator_name) %>%
      first()
    
    avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
    
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
    
    comparison_statement <- if (first_country_premium > comparison_premium) {
      paste0("This is higher than the average of ", round(comparison_premium, 1), "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", round(comparison_premium, 1), "% across the other selected countries.")
    }
    
    # Interpretation
    interpretation_text <- paste0(
      "This graph illustrates public sector wage premiums by education level in ", first_country, 
      ", comparing earnings with private sector formal workers. ",
      "On average, the public sector wage premium in ", first_country, " is ", round(first_country_premium, 0), "%. ",
      "The highest wage premium is observed for those with ", highest_education, 
      ", while the lowest is for those with ", lowest_education, ". ", comparison_statement
    )
    
    # Add to doc
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the public sector wage premium by education level, comparing earnings with private sector formal workers.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  #Slides
  
  generate_wage_premium_education_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter data
    filtered_data <- public_wage_premium_educ %>%
      filter(country_name %in% selected_countries) %>%
      drop_na(value_percentage)
    
    if (nrow(filtered_data) == 0) {
      return(ppt)
    }
    
    # Generate plot
    ggplot_obj <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "No Education"       = "#E69F00",  # Orange
        "Primary Education"  = "#56B4E9",  # Sky Blue
        "Secondary Education"= "#009E73",  # Bluish Green
        "Tertiary Education" = "#D55E00"   # Vermillion
      )) +
      labs(
        title = "Public Sector Wage Premium by Education Level",
        x = "Education Level",
        y = "Wage Premium (%)",
        fill = "Education Level"
      ) +
      theme_minimal()
    
    # Save image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # Add slide
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  #Public Sector Graphs 
  
  # First Graph - Multi-Country Dot Plot
  
  output$firstGraphpublic <- renderPlotly({
    filtered_data <- public_sector_emp_temp_last %>% 
      filter(country_name %in% input$countries_first)
    
    # If there's no data, show a fallback message
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Otherwise, generate the plot
    ggplotly(
      ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
        geom_point(size = 4) +
        scale_color_manual(values = c(
          "as a share of formal employment" = "#E69F00",  # Orange
          "as a share of paid employment"   = "#56B4E9",  # Sky Blue
          "as a share of total employment"  = "#009E73"   # Bluish Green
        )) +
        labs(
          title = "Public Sector Employment (Last Year Available)", 
          x = "Country", 
          y = "Value", 
          color = "Indicator"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ) %>% 
      layout(legend = list(title = list(text = "Indicator")))
  })
  
  
  output$note_firstGraphpublic <- renderText({
    "Note: This indicator represents public sector employment as a percentage of total employment for the most recent year available in each country."
  })
  
  # Second Graph - Single-Country Line Plot
  
  output$secondGraphpublic <- renderPlotly({
    filtered_data <- public_sector_emp_temp %>% 
      filter(country_name == input$country_second)
    
    # Fallback if no data available
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Plot if data exists
    ggplotly(
      ggplot(filtered_data, aes(x = year, y = value_percentage, color = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c(
          "as a share of formal employment" = "#E69F00",  # Orange
          "as a share of paid employment"   = "#56B4E9",  # Sky Blue
          "as a share of total employment"  = "#009E73"   # Bluish Green
        )) +
        labs(
          title = "Public Sector Employment Over Time", 
          x = "Year", 
          y = "Value", 
          color = "Indicator"
        ) +
        theme_minimal()
    ) %>%
      layout(legend = list(title = list(text = "Indicator")))
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
        scale_color_manual(values = c(
          "as a share of formal employment" = "#E69F00",  # Orange
          "as a share of paid employment"   = "#56B4E9",  # Sky Blue
          "as a share of total employment"  = "#009E73"   # Bluish Green
        )) +
        labs(
          title = "Public Sector Employment (Last Year Available)", 
          x = "Country", 
          y = "Value", 
          color = "Sector"
        ) +
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
  
  
  generate_public_sector_employment_section <- function(doc, selected_countries) {
    # Add Section Title and Intro
    doc <- doc %>% 
      body_add_par("Public Sector Employment Analysis", style = "heading 1") %>% 
      body_add_par(
        "This section presents the analysis of public sector employment across selected countries and its trend over time.", 
        style = "Normal"
      )
    
    # Validate selected countries
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- selected_countries[1]
    if (is.na(first_country)) {
      doc <- doc %>% body_add_par("Invalid country selection.", style = "Normal")
      return(doc)
    }
    
    # Filter data for selected countries (last available year)
    filtered_data <- public_sector_emp_temp_last %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # First Graph
    first_graph <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
      geom_point(size = 4) +
      scale_color_manual(values = c(
        "as a share of formal employment" = "#E69F00",  # Orange
        "as a share of paid employment"   = "#56B4E9",  # Sky Blue
        "as a share of total employment"  = "#009E73"   # Bluish Green
      )) +
      labs(
        title = "Public Sector Employment (Last Year Available)", 
        x = "Country", 
        y = "Employment (%)", 
        color = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # Helper functions
    get_value <- function(df, country, indicator) {
      val <- df %>% filter(country_name == country, indicator_label == indicator) %>% 
        pull(value_percentage) %>% first()
      ifelse(length(val) == 0 || is.na(val), "Data not available", round(val, 0))
    }
    
    get_extreme_country <- function(df, indicator, func) {
      row <- df %>% 
        filter(indicator_label == indicator, !is.na(value_percentage)) %>%
        filter(value_percentage == func(value_percentage, na.rm = TRUE)) %>%
        slice(1)
      
      if (nrow(row) == 0) return(list(country = "N/A", value = "Data not available"))
      return(list(country = row$country_name, value = round(row$value_percentage, 0)))
    }
    
    # Extract values for first country
    formal_first <- get_value(filtered_data, first_country, "as a share of formal employment")
    paid_first   <- get_value(filtered_data, first_country, "as a share of paid employment")
    total_first  <- get_value(filtered_data, first_country, "as a share of total employment")
    
    # Extract max/min across countries
    formal_highest <- get_extreme_country(filtered_data, "as a share of formal employment", max)
    formal_lowest  <- get_extreme_country(filtered_data, "as a share of formal employment", min)
    paid_highest   <- get_extreme_country(filtered_data, "as a share of paid employment", max)
    paid_lowest    <- get_extreme_country(filtered_data, "as a share of paid employment", min)
    total_highest  <- get_extreme_country(filtered_data, "as a share of total employment", max)
    total_lowest   <- get_extreme_country(filtered_data, "as a share of total employment", min)
    
    # Comparison logic
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
    paid_comparison   <- compare_value(paid_first, paid_highest$value, paid_lowest$value)
    total_comparison  <- compare_value(total_first, total_highest$value, total_lowest$value)
    
    # Interpretation
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
    
    # Add content to doc
    doc <- doc %>%
      body_add_par("Public Sector Employment - Last Year Available", style = "heading 2") %>%
      body_add_img(src = img_path1, width = 6, height = 4) %>%
      body_add_par(interpretation_text1, style = "Normal")
    
    return(doc)
  }
  
  #Slides 
  
  generate_public_sector_employment_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter data for selected countries (last available year)
    filtered_data <- public_sector_emp_temp_last %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data) == 0) {
      return(ppt)  # No data, skip slide
    }
    
    # Plot: Public Sector Employment by Country
    first_graph <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
      geom_point(size = 4) +
      scale_color_manual(values = c(
        "as a share of formal employment" = "#E69F00",  # Orange
        "as a share of paid employment" = "#56B4E9",    # Sky Blue
        "as a share of total employment" = "#009E73"    # Bluish Green
      )) +
      labs(
        title = "Public Sector Employment (Last Year Available)", 
        x = "Country", 
        y = "Employment (%)", 
        color = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save image to temp file
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = first_graph, width = 8, height = 6)
    
    # Add slide with the plot only
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  
#Gender Wage premium 
  
  # First Graph - Multi-Country Dot Plot for Wage Premium by Gender
  output$firstGraphGenderWagePremium <- renderPlotly({
    filtered_data <- gender_wage_premium_last %>% 
      filter(country_name %in% input$countries_first)
    
    # Show fallback if no data available
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Create the actual plot
    ggplotly(
      ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
        geom_point(size = 4) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",    # Orange
          "Female" = "#56B4E9"   # Sky Blue
        )) +
        labs(
          title = "Public Sector Wage Premium by Gender (Last Year Available)", 
          x = "Country", 
          y = "Wage Premium (%)",
          color = "Indicator"
        ) +
        theme_minimal()
    )
  })
  
  
  # Second Graph - Single-Country Line Plot for Wage Premium by Gender Over Time
  
  output$secondGraphGenderWagePremium <- renderPlotly({
    filtered_data <- gender_wage_premium %>% 
      filter(country_name == input$country_second)
    
    # Show fallback if no data is available
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Create the ggplot
    min_y <- min(filtered_data$value_percentage, na.rm = TRUE) - 5
    
    ggplotly(
      ggplot(filtered_data, aes(x = year, y = value_percentage, color = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",    # Orange
          "Female" = "#56B4E9"   # Sky Blue
        )) +
        labs(
          title = "Public Sector Wage Premium by Gender Over Time", 
          x = "Year", 
          y = "Wage Premium (%)",
          color = "Indicator"
        ) +
        theme_minimal() +
        annotate("text", x = Inf, y = min_y,
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
        scale_color_manual(values = c(
          "Male" = "#E69F00",    # Orange
          "Female" = "#56B4E9"   # Sky Blue
        )) +
        labs(
          title = "Public Sector Wage Premium by Gender (Last Year Available)", 
          x = "Country", 
          y = "Wage Premium (%)",
          color = "Gender"
        ) +
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
        scale_color_manual(values = c(
          "Male" = "#E69F00",    # Orange
          "Female" = "#56B4E9"   # Sky Blue
        )) +
        labs(
          title = "Public Sector Wage Premium by Gender Over Time", 
          x = "Year", 
          y = "Wage Premium (%)",
          color = "Gender"
        ) +
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
  
  generate_wage_premium_gender_report_section <- function(doc, selected_countries) {
    # Section title and intro
    doc <- doc %>%
      body_add_par("Public Sector Wage Premium by Gender", style = "heading 1") %>%
      body_add_par(
        "This section presents an analysis of the public sector wage premium by gender across selected countries.",
        style = "Normal"
      )
    
    # Validate selection
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- selected_countries[1]
    
    # Filter for last available year data
    filtered_data <- gender_wage_premium_last %>% 
      filter(country_name %in% selected_countries) %>%
      drop_na(value_percentage)
    
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # Safe extraction helpers
    get_rounded_value <- function(df, country, indicator) {
      val <- df %>% filter(country_name == country, indicator_label == indicator) %>%
        pull(value_percentage) %>% first()
      ifelse(is.na(val), "Data not available", round(val, 0))
    }
    
    get_max_country <- function(df, indicator) {
      df_filtered <- df %>% filter(indicator_label == indicator)
      max_val <- max(df_filtered$value_percentage, na.rm = TRUE)
      max_row <- df_filtered %>% filter(value_percentage == max_val)
      if (nrow(max_row) == 0) return(list(country = "N/A", value = "Data not available"))
      return(list(country = max_row$country_name[1], value = round(max_row$value_percentage[1], 0)))
    }
    
    male_first_country <- get_rounded_value(filtered_data, first_country, "Male")
    female_first_country <- get_rounded_value(filtered_data, first_country, "Female")
    
    max_male   <- get_max_country(filtered_data, "Male")
    max_female <- get_max_country(filtered_data, "Female")
    
    # First graph
    first_graph <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
      geom_point(size = 4) +
      scale_color_manual(values = c(
        "Male" = "#E69F00",    # Orange
        "Female" = "#56B4E9"   # Sky Blue
      )) +
      labs(
        title = "Public Sector Wage Premium by Gender (Last Year Available)",
        x = "Country", y = "Wage Premium (%)", color = "Gender"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    interpretation_text1 <- paste0(
      "This graph displays the public sector wage premium by gender for the last available year across the selected countries. ",
      "In ", first_country, ", the wage premium for male employees is ", male_first_country,
      "%, while for female employees, it is ", female_first_country, "%. ",
      "The country with the highest male wage premium is ", max_male$country, " at ", max_male$value,
      "%, while the highest female wage premium is in ", max_female$country, " at ", max_female$value, "%."
    )
    
    doc <- doc %>%
      body_add_par("Public Sector Wage Premium by Gender (Last Year Available)", style = "heading 2") %>%
      body_add_img(src = img_path1, width = 6, height = 4) %>%
      body_add_par(interpretation_text1, style = "Normal")
    
    # Time series for first selected country
    time_series_data <- gender_wage_premium %>% 
      filter(country_name == first_country)
    
    if (nrow(time_series_data) > 0) {
      second_graph <- ggplot(time_series_data, aes(x = year, y = value_percentage, color = indicator_label, group = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",    # Orange
          "Female" = "#56B4E9"   # Sky Blue
        )) +
        labs(
          title = paste("Public Sector Wage Premium by Gender Over Time in", first_country),
          x = "Year", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal()
      
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph, width = 8, height = 6)
      
      first_year <- 2010
      last_year  <- max(time_series_data$year, na.rm = TRUE)
      
      get_year_value <- function(df, year, indicator) {
        val <- df %>% filter(year == year, indicator_label == indicator) %>%
          pull(value_percentage) %>% first()
        ifelse(is.na(val), "Data not available", round(val, 0))
      }
      
      male_2010    <- get_year_value(time_series_data, first_year, "Male")
      female_2010  <- get_year_value(time_series_data, first_year, "Female")
      male_last    <- get_year_value(time_series_data, last_year, "Male")
      female_last  <- get_year_value(time_series_data, last_year, "Female")
      
      interpretation_text2 <- paste0(
        "This graph illustrates how the public sector wage premium by gender has evolved over time in ", first_country, ". ",
        "In ", first_year, ", the male wage premium was ", male_2010, "% and the female wage premium was ", female_2010, "%. ",
        "By ", last_year, ", the male wage premium is ", male_last, "% and the female wage premium is ", female_last, "%."
      )
      
      doc <- doc %>%
        body_add_par("Public Sector Wage Premium by Gender Over Time", style = "heading 2") %>%
        body_add_img(src = img_path2, width = 6, height = 4) %>%
        body_add_par(interpretation_text2, style = "Normal")
      
    } else {
      doc <- doc %>% body_add_par("No time series data available for this country.", style = "Normal")
    }
    
    return(doc)
  }
  
  #Slides
  
  generate_wage_premium_gender_report_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    first_country <- selected_countries[1]
    
    # Filter for last available year (cross-country)
    filtered_data <- gender_wage_premium_last %>% 
      filter(country_name %in% selected_countries) %>%
      drop_na(value_percentage)
    
    if (nrow(filtered_data) > 0) {
      # Graph 1: Public Sector Wage Premium by Gender (Last Year Available)
      first_graph <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
        geom_point(size = 4) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",   # Orange
          "Female" = "#56B4E9"  # Sky Blue
        )) +
        labs(
          title = "Public Sector Wage Premium by Gender (Last Year Available)",
          x = "Country", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = first_graph, width = 8, height = 6)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path1, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    # Graph 2: Time series for first country
    time_series_data <- gender_wage_premium %>% 
      filter(country_name == first_country)
    
    if (nrow(time_series_data) > 0) {
      second_graph <- ggplot(time_series_data, aes(x = year, y = value_percentage, color = indicator_label, group = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",   # Orange
          "Female" = "#56B4E9"  # Sky Blue
        )) +
        labs(
          title = paste("Public Sector Wage Premium by Gender Over Time in", first_country),
          x = "Year", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal()

      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph, width = 8, height = 6)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path2, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    return(ppt)
  }
  
  
  # Gender Workforce Graphs
  
  # First Graph - Multi-Country Bar Plot
  
  output$firstGraphGenderWorkforce <- renderPlotly({
    filtered_data <- gender_workforce %>% 
      filter(country_name %in% input$countries_gender)
    
    # Show fallback if no data is available
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Ensure factor levels match color scale
    filtered_data$indicator_name <- factor(filtered_data$indicator_name, 
                                           levels = c("as a share of private paid employees", 
                                                      "as a share of public paid employees"))
    
    # Create the grouped bar chart
    ggplotly(
      ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c(
          "as a share of private paid employees" = "#E69F00",  # Orange
          "as a share of public paid employees" = "#56B4E9"    # Sky Blue
        )) +
        labs(
          title = "Female Employment by Sector (Last Year Available)", 
          x = "Country", 
          y = "Employment (%)", 
          fill = "Sector"
        ) +
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
    
    # Show fallback if no data is available
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = paste("No data available for", input$country_gender),
                 annotations = list(
                   text = "No data available for the selected country.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Ensure factor levels match color scale
    filtered_data$indicator_name <- factor(filtered_data$indicator_name, 
                                           levels = c("as a share of private paid employees", 
                                                      "as a share of public paid employees"))
    
    # Create the line chart
    ggplotly(
      ggplot(filtered_data, aes(x = year, y = value_percentage, color = indicator_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c(
          "as a share of private paid employees" = "#E69F00",  # Orange
          "as a share of public paid employees" = "#56B4E9"    # Sky Blue
        )) +
        labs(
          title = paste("Female Employment by Sector Over Time in", input$country_gender), 
          x = "Year", 
          y = "Female Employment (%)", 
          color = "Sector"
        ) +
        theme_minimal()
    )
  })
  
  output$note_secondGraphGenderWorkforce <- renderText({
    "Note: This indicator represents the trend of female employment in the public and private sectors over time, allowing for a comparison of sectoral changes."
  })
  
  # Download Handler - Save Graphs to Word Document
  
  output$downloadGraphsWordGender <- downloadHandler(
    filename = function() {
      paste0("Female share of employment_Analysis_", Sys.Date(), ".docx")
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
        scale_fill_manual(values = c(
          "as a share of private paid employees" = "#E69F00",  # Orange
          "as a share of public paid employees" = "#56B4E9"    # Sky Blue
        )) +
        labs(
          title = "Female Employment by Sector (Last Year Available)", 
          x = "Country", y = "Employment (%)", fill = "Sector"
        ) +
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
        scale_color_manual(values = c(
          "as a share of private paid employees" = "#E69F00",  # orange
          "as a share of public paid employees" = "#56B4E9"    # sky blue
        )) +
        labs(
          title = paste("Female Employment by Sector Over Time in", input$country_gender), 
          x = "Year", y = "Female Employment (%)", color = "Sector"
        ) +
        theme_minimal()
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph, width = 8, height = 6)
      doc <- doc %>% body_add_par("Female Employment by Sector Over Time", style = "heading 2")
      doc <- doc %>% body_add_img(src = img_path2, width = 6, height = 4)
      
   
      # Save the Document
      print(doc, target = file)
    }
  )
  
  generate_gender_workforce_section <- function(doc, selected_countries) {
    # Add Section Title
    doc <- doc %>% body_add_par("Gender Workforce Analysis", style = "heading 1")
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of female employment in the public and private sectors across selected countries.", 
      style = "Normal"
    )
    
    # ✅ Ensure at least one country is selected
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Extract the first selected country
    first_country <- selected_countries[1]
    
    # ✅ Filter data for selected countries (last available year)
    filtered_data <- gender_workforce %>% 
      filter(country_name %in% selected_countries)
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Generate Plot
    first_graph <- ggplot(filtered_data, 
                          aes(x = country_name, y = round(value_percentage, 0), fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "as a share of private paid employees" = "#E69F00",  # orange
        "as a share of public paid employees" = "#56B4E9"    # sky blue
      )) +
      labs(
        title = "Female Employment by Sector (Last Year Available)", 
        x = "Country", y = "Employment (%)", fill = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # ✅ Summary Statistics
    avg_public <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of public paid employees"], na.rm = TRUE), 0)
    avg_private <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of private paid employees"], na.rm = TRUE), 0)
    
    get_country_extreme <- function(data, label, func) {
      data %>%
        filter(indicator_name == label, value_percentage == func(value_percentage, na.rm = TRUE)) %>%
        pull(country_name) %>%
        first()
    }
    
    highest_public_country <- get_country_extreme(filtered_data, "as a share of public paid employees", max)
    lowest_public_country  <- get_country_extreme(filtered_data, "as a share of public paid employees", min)
    highest_private_country <- get_country_extreme(filtered_data, "as a share of private paid employees", max)
    lowest_private_country  <- get_country_extreme(filtered_data, "as a share of private paid employees", min)
    
    # ✅ Extract first country stats
    get_country_value <- function(data, country, label) {
      data %>%
        filter(country_name == country, indicator_name == label) %>%
        pull(value_percentage) %>%
        first() %>%
        coalesce(0) %>%
        round(0)
    }
    
    first_country_public  <- get_country_value(filtered_data, first_country, "as a share of public paid employees")
    first_country_private <- get_country_value(filtered_data, first_country, "as a share of private paid employees")
    
    # ✅ Comparisons
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
    
    # ✅ Interpretation Text
    interpretation_text1 <- paste0(
      "This graph compares female employment in the public and private sectors across selected countries. ",
      "On average, ", avg_public, "% of public sector employees are female, while in the private sector, the share is ", avg_private, "%. ",
      "The highest female employment in the public sector is in ", highest_public_country, ", while the lowest is in ", lowest_public_country, ". ",
      "In the private sector, ", highest_private_country, " has the highest share of female employees, whereas ", lowest_private_country, " has the lowest.\n\n",
      "In ", first_country, ", female representation in the public sector is ", first_country_public, "%.", 
      " ", comparison_public, "\n",
      "In the private sector, female representation in ", first_country, " is ", first_country_private, "%. ",
      comparison_private
    )
    
    # ✅ Add Content to Word Document
    doc <- doc %>% 
      body_add_par("Female Employment by Sector (Last Year Available)", style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4) %>% 
      body_add_par(interpretation_text1, style = "Normal")
    
    return(doc)
  }
  
  #Slides
  
  generate_gender_workforce_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter relevant data
    filtered_data <- gender_workforce %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data) == 0) {
      return(ppt)
    }
    
    # Create ggplot graph
    gender_graph <- ggplot(filtered_data, 
                           aes(x = country_name, y = round(value_percentage, 0), fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(
        values = c(
          "as a share of private paid employees" = "#E69F00",  # orange
          "as a share of public paid employees" = "#56B4E9"    # sky blue
        )
      ) +
      labs(
        title = "Female Employment by Sector (Last Year Available)", 
        x = "Country", y = "Employment (%)", fill = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save to PNG
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = gender_graph, width = 8, height = 6, dpi = 300)
    
    # Add slide with image only
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  

  # Women Leadership 
  
  output$barPlotwomen <- renderPlotly({
    if (is.null(input$selected_countries) || length(input$selected_countries) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No country selected",
                 annotations = list(
                   text = "Please select at least one country to view the data.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    filtered_data <- gender_leadership %>% 
      filter(country_name %in% input$selected_countries)
    
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    plot_ly(
      data = filtered_data,
      x = ~country_name,
      y = ~value_percentage,
      color = ~indicator_label,
      colors = c(
        "Clerks-Public" = "#E69F00",    # orange
        "Managers-Public" = "#56B4E9",  # sky blue
        "Clerks-Private" = "#009E73",   # green
        "Managers-Private" = "#F0E442"  # yellow
      ),
      type = 'bar',
      barmode = 'group'
    ) %>%
      layout(
        title = "Females by Occupational Group and Sector",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Female Share (%)"),
        bargap = 0.2
      )
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
        scale_fill_viridis_d(option = "D", name = "Occupation") +
        labs(
          title = "Females by Occupational Group and Sector",
          x = "Country", 
          y = "Female Share (%)"
        ) +
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
  
  generate_females_occupation_groups_section <- function(doc, selected_countries) {
    # Add Section Title
    doc <- doc %>% body_add_par("Females by Occupational Group and Sector", style = "heading 1")
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of female representation in different occupational groups across selected countries.", 
      style = "Normal"
    )
    
    # ✅ Ensure at least one country is selected
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Extract the first selected country
    first_country <- selected_countries[1]
    if (is.na(first_country)) first_country <- "Unknown Country"
    
    # ✅ Filter data for selected countries
    filtered_data <- gender_leadership %>% filter(country_name %in% selected_countries)
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Generate ggplot for Female Occupational Groups
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(option = "D", name = "Occupation") +
      labs(
        title = "Females by Occupational Group and Sector",
        x = "Country", 
        y = "Female Share (%)"
      ) +
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
      round(1)
    
    first_country_private_managers <- filtered_data %>%
      filter(country_name == first_country, indicator_label == "Managers-Private") %>%
      pull(value_percentage) %>%
      first() %>%
      coalesce(0) %>%
      round(1)
    
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
      "In the private sector, the highest female manager share is in ", highest_private_managers, ", while the lowest is in ", lowest_private_managers, ".\n\n",
      "In ", first_country, ", female managers account for ", first_country_public_managers, "% in the public sector. ", 
      comparison_public_managers, "\n",
      "In the private sector, female managers in ", first_country, " represent ", first_country_private_managers, "%. ",
      comparison_private_managers
    )
    
    # ✅ Add Image and Interpretation to the Document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the share of females in various occupational groups (Managers/Clerks) in the public and private sectors for the selected countries.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  generate_females_occupation_groups_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter the data
    filtered_data <- gender_leadership %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data) == 0) {
      return(ppt)  # no data to plot
    }
    
    # Plot
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(option = "D", name = "Occupation") +
      labs(
        title = "Females by Occupational Group and Sector",
        x = "Country", 
        y = "Female Share (%)"
      ) +
      theme_minimal()
    
    # Save as image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6, dpi = 300)
    
    # Add to PowerPoint
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
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
    
    # Fallback if no data
    if (nrow(filtered_data) == 0) {
      return(
        ggplot() + 
          theme_void() +
          annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected country/countries.",
                   size = 6, color = "red", hjust = 0.5, vjust = 0.5)
      )
    }
    
    # Create actual ggplot
    ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(name = "Indicator", option = "D") +
      labs(
        title = "Gender Wage Premium in Public Sector by Industry",
        x = "Country", y = "Wage Premium (%)"
      ) +
      theme_minimal() +
      annotate("text", x = Inf, y = min(filtered_data$value_percentage, na.rm = TRUE) - 5, 
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
        scale_fill_viridis_d(option = "D") +  # Options: "D", "C", "E", etc.
        labs(
          title = "Gender Wage Premium in Public Sector by Industry",
          x = "Country", y = "Wage Premium (%)", fill = "Industry"
        ) +
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
  generate_gender_wage_premiumbysector_section <- function(doc, selected_countries) {
    # Section Title
    doc <- doc %>% body_add_par("Gender Wage Premium in Public Sector by Industry", style = "heading 1")
    
    # Intro Text
    doc <- doc %>% body_add_par(
      "This section presents an analysis of gender wage premiums in the public sector by industry 
    (Public Administration, Education, and Health) across selected countries.", 
      style = "Normal"
    )
    
    # ✅ Validate selected countries
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- selected_countries[1]
    if (is.na(first_country)) {
      doc <- doc %>% body_add_par("Invalid country selection.", style = "Normal")
      return(doc)
    }
    
    # ✅ Filter and label the data
    filtered_data <- gender_wage_premiumpublic %>%
      filter(country_name %in% selected_countries,
             indicator_name %in% c(
               "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)", 
               "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)", 
               "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"
             ))
    
    if (nrow(filtered_data) == 0 || all(is.na(filtered_data$value_percentage))) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Clean labels
    filtered_data$indicator_label <- recode(filtered_data$indicator_name,
                                            "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" = "Public Administration",
                                            "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)" = "Education",
                                            "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)" = "Health"
    )
    
    # ✅ Plot
    gender_wage_plot <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(option = "D") +  # Options: "D", "C", "E", etc.
      labs(
        title = "Gender Wage Premium in Public Sector by Industry",
        x = "Country", y = "Wage Premium (%)", fill = "Industry"
      ) +
      theme_minimal()
    
    # ✅ Save plot
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = gender_wage_plot, width = 8, height = 6, dpi = 300)
    
    # ✅ Utility functions
    safe_max <- function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
    safe_min <- function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))
    safe_mean <- function(x) ifelse(all(is.na(x)), 0, round(mean(x, na.rm = TRUE), 0))
    
    # ✅ Stats by sector
    get_extreme <- function(data, label, func) {
      data %>%
        filter(indicator_label == label, value_percentage == func(value_percentage)) %>%
        pull(country_name) %>%
        first() %>%
        coalesce("N/A")
    }
    
    highest_admin    <- get_extreme(filtered_data, "Public Administration", safe_max)
    lowest_admin     <- get_extreme(filtered_data, "Public Administration", safe_min)
    highest_education <- get_extreme(filtered_data, "Education", safe_max)
    lowest_education  <- get_extreme(filtered_data, "Education", safe_min)
    highest_health    <- get_extreme(filtered_data, "Health", safe_max)
    lowest_health     <- get_extreme(filtered_data, "Health", safe_min)
    
    # ✅ Means
    avg_admin    <- safe_mean(filtered_data$value_percentage[filtered_data$indicator_label == "Public Administration"])
    avg_education <- safe_mean(filtered_data$value_percentage[filtered_data$indicator_label == "Education"])
    avg_health    <- safe_mean(filtered_data$value_percentage[filtered_data$indicator_label == "Health"])
    
    # ✅ First country values
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
    
    # ✅ Interpretation
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
    
    # ✅ Add content to doc
    doc <- doc %>%
      body_add_img(src = img_path, width = 6, height = 4) %>%
      body_add_par("This graph shows the gender wage premium in the public sector across different industries.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  
  generate_gender_wage_premiumbysector_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter relevant indicators
    filtered_data <- gender_wage_premiumpublic %>%
      filter(
        country_name %in% selected_countries,
        indicator_name %in% c(
          "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)",
          "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)",
          "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"
        )
      )
    
    if (nrow(filtered_data) == 0 || all(is.na(filtered_data$value_percentage))) {
      return(ppt)  # no data to plot
    }
    
    # Rename for labels
    filtered_data$indicator_label <- recode(
      filtered_data$indicator_name,
      "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" = "Public Administration",
      "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)" = "Education",
      "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)" = "Health"
    )
    
    # Create ggplot graph
    gender_wage_plot <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(option = "D") +  # Discrete viridis scale
      labs(
        title = "Gender Wage Premium in Public Sector by Industry",
        x = "Country", y = "Wage Premium (%)", fill = "Industry"
      ) +
      theme_minimal()
    
    # Save to PNG
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = gender_wage_plot, width = 8, height = 6, dpi = 300)
    
    # Add slide with image only
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  
  generate_intro_section <- function(doc, selected_countries) {
    # ✅ Use first selected country safely
    first_country <- if (!is.null(selected_countries) && length(selected_countries) > 0 && !is.na(selected_countries[1])) {
      selected_countries[1]
    } else {
      "Unknown Country"
    }
    
    # ✅ Try to detect World Bank region
    first_region <- countrycode(first_country, origin = "country.name", destination = "region")
    if (is.na(first_region)) {
      first_region <- "its respective region"
    }
    
    # ✅ Define styles
    title_style <- fp_text(color = "#722F37", font.size = 20, bold = TRUE)
    subtitle_style <- fp_text(color = "black", font.size = 16, bold = TRUE)
    
    # ✅ Add formatted title
    doc <- doc %>% 
      body_add_fpar(fpar(ftext(first_country, prop = title_style))) %>% 
      body_add_fpar(fpar(ftext("Wage Bill and Public Employment Analysis", prop = subtitle_style)))
    
    # ✅ Construct intro text
    intro_text <- paste0(
      "This note presents evidence on public sector employment and compensation practices in ", first_country,
      " using the Worldwide Bureaucracy Indicators (WWBI). The primary data source is the Labor Force Survey, conducted by the National Statistics Office. ",
      "For international comparisons, peer countries from ", first_region, " are included.",
      "\n\n",
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
    
    # ✅ Add intro text to the document
    doc <- doc %>% body_add_par(intro_text, style = "Normal")
    
    return(doc)
  }
  
  
#Pay compression 
  
  output$paycompression_plot <- renderPlotly({
    req(input$countries_first)  # Ensure at least one country is selected
    
    filtered_data_df <- pay_compression_wide %>%
      filter(country_name %in% input$countries_first)
    
    # Fallback if no data
    if (nrow(filtered_data_df) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper", yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5, y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Fallback if required columns are missing
    if (!all(c("Public_Sector", "Private_Sector") %in% colnames(filtered_data_df))) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "Data error",
                 annotations = list(
                   text = "Required columns (Public_Sector, Private_Sector) are missing.",
                   xref = "paper", yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16, color = "red"),
                   x = 0.5, y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Prepare data and trendline
    filtered_data_df <- filtered_data_df %>%
      mutate(color = ifelse(country_name == input$countries_first[1], "#B3242B", "#003366"))
    
    trendline_model <- lm(Public_Sector ~ Private_Sector, data = filtered_data_df)
    trendline_values <- predict(trendline_model, newdata = filtered_data_df)
    
    # Build the plot
    plot_ly() %>%
      add_trace(
        data = filtered_data_df,
        x = ~Private_Sector,
        y = ~Public_Sector,
        type = "scatter",
        mode = "markers+text",
        text = ~country_name,
        textposition = "top center",
        marker = list(size = 10, color = ~color, opacity = 0.7)
      ) %>%
      add_trace(
        x = filtered_data_df$Private_Sector,
        y = trendline_values,
        type = "scatter",
        mode = "lines",
        line = list(color = "gray", dash = "dash"),
        name = "Trendline"
      ) %>%
      layout(
        title = "Pay Compression: Public vs. Private Sector (Latest Year)",
        xaxis = list(title = "Private Sector Pay Compression"),
        yaxis = list(title = "Public Sector Pay Compression"),
        showlegend = TRUE,
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  
    output$note_dotplot <- renderText({
      "Note: This graph compares pay compression ratios in the public and private sectors. The trendline provides a visual reference for overall patterns across countries."
    })
    output$downloadPayCompressionDoc <- downloadHandler(
      filename = function() { paste0("Pay_Compression_Ratios_Report_", Sys.Date(), ".docx") },
      
      content = function(file) {
        # **Filter data for selected countries**
        filtered_data_df <- pay_compression_wide %>%
          filter(country_name %in% input$countries_first)
        
        req(nrow(filtered_data_df) > 0) # **Ensure there is data before proceeding**
        
        # **Get the first selected country for the report title**
        countries <- if (!is.null(input$countries_first) & length(input$countries_first) > 0) {
          paste(input$countries_first, collapse = ", ")
        } else {
          "Selected Countries"
        }
        
        report_title <- paste("Pay Compression Ratios Analysis Report -", countries)
        
        # **Create Word document**
        doc <- read_docx()
        title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
        doc <- doc %>% body_add_fpar(fpar(ftext(report_title, prop = title_style)))
        
        # **Add Introduction**
        doc <- doc %>% body_add_par("Introduction", style = "heading 2") %>% 
          body_add_par("This report presents an analysis of pay compression ratios in the public and private sectors across selected countries. 
                    Pay compression is measured as the ratio of wages at the 90th percentile to the 10th percentile, providing insights into 
                    wage inequality within each sector. The analysis compares these ratios and examines trends across different economies.", 
                       style = "Normal")
        
        # **Create scatter plot with trend line**
        plot <- ggplot(filtered_data_df, aes(x = Private_Sector, y = Public_Sector, label = country_name)) +  # or group variable
          geom_point(size = 3) +
          geom_text(vjust = -0.5, size = 3) +
          geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
          labs(
            title = "Pay Compression: Public vs. Private Sector",
            x = "Private Sector Pay Compression",
            y = "Public Sector Pay Compression"
          ) +
          scale_color_viridis(discrete = TRUE, option = "D") +
          theme_minimal()
        
        # **Add plot to Word document**
        doc <- doc %>% body_add_gg(value = plot, style = "centered") 
        
        # **Add explanatory note**
        doc <- doc %>% body_add_par("Note: This graph compares pay compression ratios in the public and private sectors. 
                                  The trendline provides a visual reference for overall patterns across countries.", style = "Normal")
        
        # **Save document**
        print(doc, target = file)
      }
    )
    
  #Pay compression section  
    
    generate_pay_compression_section <- function(doc, selected_countries) {
      doc <- doc %>% body_add_par("Pay Compression in the Private and Public Sector", style = "heading 1")
      
      doc <- doc %>% body_add_par(
        "This section presents an analysis of the pay compression for the private and public sector 
     across selected countries.", 
        style = "Normal"
      )
      
      if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
        doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
        return(doc)
      }
      
      first_country <- selected_countries[1]
      
      filtered_data_df <- pay_compression_wide %>%
        filter(country_name %in% selected_countries)
      
      req(nrow(filtered_data_df) > 0)
      
      country_summary <- filtered_data_df %>%
        group_by(country_name) %>%
        summarise(
          public_compression = round(mean(Public_Sector, na.rm = TRUE), 1),  
          private_compression = round(mean(Private_Sector, na.rm = TRUE), 1) 
        )
      
      highest_public <- country_summary %>%
        filter(public_compression == max(public_compression, na.rm = TRUE)) %>% pull(country_name)
      lowest_public <- country_summary %>%
        filter(public_compression == min(public_compression, na.rm = TRUE)) %>% pull(country_name)
      
      highest_private <- country_summary %>%
        filter(private_compression == max(private_compression, na.rm = TRUE)) %>% pull(country_name)
      lowest_private <- country_summary %>%
        filter(private_compression == min(private_compression, na.rm = TRUE)) %>% pull(country_name)
      
      first_country_values <- country_summary %>% filter(country_name == first_country)
      first_public_compression <- first_country_values %>% pull(public_compression) %>% coalesce(NA)
      first_private_compression <- first_country_values %>% pull(private_compression) %>% coalesce(NA)
      
      other_countries_avg <- country_summary %>%
        filter(country_name != first_country) %>%
        summarise(
          avg_public_compression = round(mean(public_compression, na.rm = TRUE), 1),
          avg_private_compression = round(mean(private_compression, na.rm = TRUE), 1)
        )
      
      if (first_country %in% country_summary$country_name) {
        rank_public <- rank(-country_summary$public_compression, ties.method = "min")[country_summary$country_name == first_country]
        rank_private <- rank(-country_summary$private_compression, ties.method = "min")[country_summary$country_name == first_country]
        
        public_position <- dplyr::case_when(
          rank_public == 1 ~ "the highest",
          rank_public == nrow(country_summary) ~ "the lowest",
          TRUE ~ "in the middle range"
        )
        
        private_position <- dplyr::case_when(
          rank_private == 1 ~ "the highest",
          rank_private == nrow(country_summary) ~ "the lowest",
          TRUE ~ "in the middle range"
        )
      } else {
        public_position <- "unranked"
        private_position <- "unranked"
      }
      
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
      
      plot <- ggplot(filtered_data_df, aes(x = Private_Sector, y = Public_Sector, label = country_name)) +  # or group variable
        geom_point(size = 3) +
        geom_text(vjust = -0.5, size = 3) +
        geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
        labs(
          title = "Pay Compression: Public vs. Private Sector",
          x = "Private Sector Pay Compression",
          y = "Public Sector Pay Compression"
        ) +
        scale_color_viridis(discrete = TRUE, option = "D") +
        theme_minimal()
      
      img_path <- tempfile(fileext = ".png")
      ggsave(filename = img_path, plot = plot, width = 8, height = 6, dpi = 300)
      
      doc <- doc %>% 
        body_add_img(src = img_path, width = 6, height = 4) %>%
        body_add_par("Note: The trendline provides a visual reference for overall patterns across countries.", style = "Normal") %>%
        body_add_par(interpretation_text, style = "Normal")
      
      return(doc)
    }
    
    generate_pay_compression_slide <- function(ppt, selected_countries) {
      if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
        return(ppt)
      }
      
      filtered_data_df <- pay_compression_wide %>%
        filter(country_name %in% selected_countries)
      
      req(nrow(filtered_data_df) > 0)
      
      # Create summary for plot
      plot <- ggplot(filtered_data_df, aes(x = Private_Sector, y = Public_Sector, label = country_name)) +  # or group variable
        geom_point(size = 3) +
        geom_text(vjust = -0.5, size = 3) +
        geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
        labs(
          title = "Pay Compression: Public vs. Private Sector",
          x = "Private Sector Pay Compression",
          y = "Public Sector Pay Compression"
        ) +
        scale_color_viridis(discrete = TRUE, option = "D") +
        theme_minimal()
      
      # Save plot as image
      img_path <- tempfile(fileext = ".png")
      ggsave(filename = img_path, plot = plot, width = 6, height = 6, dpi = 300)
      
      # Add slide with only the image
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path, height = 4, width = 4),
                location = ph_location_type(type = "body"))
      
      return(ppt)
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
  
  add_section_slide <- function(ppt, title) {
    ppt %>%
      add_slide(layout = "Title Slide", master = "Office Theme") %>%
      ph_with(
        value = fpar(
          ftext(title, prop = fp_text(color = "#003366", font.size = 40, bold = TRUE))
        ),
        location = ph_location_type(type = "ctrTitle")
      )
  }
  
  
  #Download selected graphs 
  
  output$downloadSelectedGraphsDoc <- downloadHandler(
    filename = function() { 
      paste0("Wage_bill_and_public_employment_analysis_Selected_Report_", Sys.Date(), ".docx") 
    },
    content = function(file) {
      # Get the selected countries dynamically
      selected_countries <- input$download_report_countries
      
      # Initialize Word document
      doc <- read_docx() 
      
      # Add Report Title
      title_style <- fp_text(color = "#722F37", font.size = 20, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Wage bill and public employment analysis", prop = title_style)))
      doc <- generate_intro_section(doc, selected_countries)  # Add the Intro First
      
      # Define Section Style 
      section_style <- fp_text(color = "#003366", font.size = 14, bold = TRUE)
      
      # ✅ Dynamically include only selected sections
      selected_sections <- input$selected_graphs
      
      # ✅ Ensure selected_sections is not NULL before checking length
      if (is.null(selected_sections) || length(selected_sections) == 0) {
        doc <- doc %>% body_add_par("No sections selected for download.", style = "Normal")
      } else {
        if ("wagebill" %in% selected_sections) {
          doc <- generate_wage_bill_analysis_section(doc, selected_countries)
        }
        if ("wagebill_gdp" %in% selected_sections) {
          doc <- generate_gdp_analysis_section(doc, selected_countries)
        }
        if ("tertiaryeducation" %in% selected_sections) {
          doc <- generate_tertiary_education_section(doc, selected_countries)
        }
        if ("genderwagepremium" %in% selected_sections) {
          doc <- generate_wage_premium_gender_section(doc, selected_countries)
        }
        if ("wagepremiumeducation" %in% selected_sections) {
          doc <- generate_wage_premium_education_section(doc, selected_countries)
        }
        if ("public_employment" %in% selected_sections) {
          doc <- generate_public_sector_employment_section(doc, selected_countries) 
        }
        if ("wagepremiumgender" %in% selected_sections) {
          doc <- generate_wage_premium_gender_report_section(doc, selected_countries)
        }
        if ("public_workforce" %in% selected_sections) {
          doc <- generate_public_sector_workforce_section(doc, selected_countries)
        }
        if ("gender_workforce" %in% selected_sections) {
          doc <- generate_gender_workforce_section(doc, selected_countries)
        }
        if ("female_leadership" %in% selected_sections) {  # Fixed typo from "femaleocuupation"
          doc <- generate_females_occupation_groups_section(doc, selected_countries)
        }
        if ("wagepremium" %in% selected_sections) {
          doc <- generate_wage_premium_report_section(doc, selected_countries)
        }
        if ("gender_wage_premium" %in% selected_sections) {
          doc <- generate_gender_wage_premiumbysector_section(doc, selected_countries)
        }
        if ("pay_compression" %in% selected_sections) {
          doc <- generate_pay_compression_section(doc, selected_countries = selected_countries)
          
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
      selected_countries <- input$download_report_countries # ✅ Use country selector from the download tab
      
      # Initialize Word document
      doc <- read_docx() 
      
      # Title
      
      title_style <- fp_text(color = "#722F37", font.size = 20, bold = TRUE)
      doc <- doc %>%
        body_add_fpar(fpar(ftext("", prop = title_style))) %>%
        generate_intro_section(selected_countries)
      
      # Section Header Style
      
      section_style <- fp_text(color = "#003366", font.size = 14, bold = TRUE)
      
      # ──────────────────────────────────────
      # 📘 Section 1: Macro-Fundamentals
      # ──────────────────────────────────────
      doc <- doc %>%
        body_add_fpar(fpar(ftext("Macro-Fundamentals of the Public Sector", prop = section_style))) %>%
        generate_wage_bill_analysis_section(selected_countries) %>%
        generate_gdp_analysis_section(selected_countries)
      
      # ──────────────────────────────────────
      # 📘 Section 2: Size and Characteristics
      # ──────────────────────────────────────
      doc <- doc %>%
        body_add_fpar(fpar(ftext("Size and Characteristics of the Public Sector", prop = section_style))) %>%
        generate_public_sector_employment_section(selected_countries) %>%
        generate_tertiary_education_section(selected_countries)
      
      # ──────────────────────────────────────
      # 📘 Section 3: Competitiveness of Public Sector Wages
      # ──────────────────────────────────────
      doc <- doc %>%
        body_add_fpar(fpar(ftext("Competitiveness of Public Sector Wages", prop = section_style))) %>%
        body_add_par(
          "Public sector compensation should theoretically be designed with an awareness of its influence on the broader labor market. According to the theory of “compensating wage differentials,” a job should pay more (or less) depending on its non-wage characteristics that are undesirable (or desirable)...",
          style = "Normal"
        ) %>%
        generate_wage_premium_report_section(selected_countries) %>%
        generate_wage_premium_education_section(selected_countries) %>%
        generate_pay_compression_section(selected_countries = selected_countries)
      
      # ──────────────────────────────────────
      # 📘 Section 4: Equity in the Public Sector
      # ──────────────────────────────────────
      doc <- doc %>%
        body_add_fpar(fpar(ftext("Equity in the Public Sector", prop = section_style))) %>%
        generate_gender_workforce_section(selected_countries) %>%
        generate_females_occupation_groups_section(selected_countries) %>%
        generate_gender_wage_premiumbysector_section(selected_countries) %>%
        generate_wage_premium_gender_report_section(selected_countries)
      
      # ──────────────────────────────────────
      # 📘 Conclusion
      # ──────────────────────────────────────
      doc <- generate_conclusion_section(doc)
      
      # Save final report
      print(doc, target = file)
    }
  )
  
  
  #Power Point Slides 
  
  output$downloadSelectedGraphsPPT <- downloadHandler(
    filename = function() { 
      paste0("Wage_bill_and_public_employment_analysis_Selected_Presentation_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      selected_countries <- input$download_report_countries
      selected_sections <- input$selected_graphs
      
      # Initialize PowerPoint
      ppt <- read_pptx()
      
      # Add a title slide
      ppt <- ppt %>%
        add_slide(layout = "Title Slide", master = "Office Theme") %>%
        ph_with("Wage bill and public employment analysis", location = ph_location_type(type = "ctrTitle")) %>%
        ph_with(paste("Generated on", Sys.Date()), location = ph_location_type(type = "subTitle"))
      
      # Only include selected graphs
      if (!is.null(selected_sections) && length(selected_sections) > 0) {
      
        if ("wagebill" %in% selected_sections || "wagebill_gdp" %in% selected_sections) {
          ppt <- add_section_slide(ppt, "Macro-Fundamentals of the Public Sector")
        }
        if ("wagebill" %in% selected_sections) {
          ppt <- generate_wage_bill_analysis_slide(ppt, selected_countries)
        }
        if ("wagebill_gdp" %in% selected_sections) {
          ppt <- generate_gdp_analysis_slide(ppt, selected_countries)
        }
        if ("public_employment" %in% selected_sections || "public_workforce" %in% selected_sections || "tertiaryeducation" %in% selected_sections) {
          ppt <- add_section_slide(ppt, "Size and Characteristics of the Public Sector")
        }
        if ("public_employment" %in% selected_sections) {
          ppt <- generate_public_sector_employment_slide(ppt, selected_countries)
        }
        if ("public_workforce" %in% selected_sections) {
          ppt <- generate_public_sector_workforce_slide(ppt, selected_countries)
        }
        if ("tertiaryeducation" %in% selected_sections) {
          ppt <- generate_tertiary_education_slide(ppt, selected_countries)
        }
        if ("genderwagepremium" %in% selected_sections || "wagepremiumeducation" %in% selected_sections || "pay_compression" %in% selected_sections) {
          ppt <- add_section_slide(ppt, "Competiiveness of public sector wages")
        }
        
        if ("genderwagepremium" %in% selected_sections) {
          ppt <- generate_wage_premium_gender_slide(ppt, selected_countries)
        }
        if ("wagepremiumeducation" %in% selected_sections) {
          ppt <-generate_wage_premium_education_slide(ppt, selected_countries)
        }
        
        if ("pay_compression" %in% selected_sections) {
          ppt <- generate_pay_compression_slide(ppt, selected_countries)
        }
        
        if ("wagepremiumgender" %in% selected_sections || "gender_workforce" %in% selected_sections || "gender_wage_premium" %in% selected_sections
            || "female_leadership" %in% selected_sections) {
          ppt <- add_section_slide(ppt, "Equity in public sector")
        }
        
        if ("wagepremiumgender" %in% selected_sections) {
          ppt <- generate_wage_premium_gender_report_slide(ppt, selected_countries)
        }
        
        
        if ("gender_workforce" %in% selected_sections) {
          ppt <- generate_gender_workforce_slide(ppt, selected_countries)
        }
        if ("gender_wage_premium" %in% selected_sections) {
          ppt <- generate_gender_wage_premiumbysector_slide(ppt, selected_countries)
        }
        
        if ("female_leadership" %in% selected_sections) {
          ppt <- generate_females_occupation_groups_slide(ppt, selected_countries)
        }
  
        # add more slide generators here as needed
      } else {
        # If no selection: optionally include a placeholder slide
        ppt <- ppt %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with_text(type = "title", str = "No graphs selected") %>%
          ph_with_text(type = "body", str = "Please select at least one graph to download.")
      }
      
      # Save the PowerPoint
      print(ppt, target = file)
    }
  )
  
  
  # ---------------------
  # Clean base map with legend
  output$worldMap <- renderLeaflet({
    leaflet(world_spdf) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addLegend(
        position = "bottomright", 
        colors = c("gray", "#6DA96F"),
        labels = c("No Data", "Reported"),
        title = "Indicator Availability",
        opacity = 1
      )
  })
  
  # Create reactive that flags countries that have ANY data for the selected indicator
  filtered_data_for_map <- reactive({
    req(input$indicatorSelect)
    
    data_wwbi %>%
      filter(indicator_name == input$indicatorSelect) %>%
      mutate(
        any_data = apply(select(., starts_with("year_")), 1, function(x) any(!is.na(x)))
      ) %>%
      filter(any_data) %>%
      transmute(country_name, indicator_name, has_data = 1)  # use 'has_data' instead of value_percentage
  })
  
  # Update the map
  observe({
    req(input$indicatorSelect)
    
    reported_countries <- filtered_data_for_map()
    
    if (nrow(reported_countries) == 0) return()
    
    # Match the country names to the shapefile column
    world_data_merged <- world_spdf %>%
      left_join(reported_countries, by = c("name_long" = "country_name"))
    
    # Use a factor color palette based on 0 (missing) and 1 (has data)
    color_pal <- colorFactor(palette = c("gray", "#6DA96F"), domain = c(0, 1))
    
    leafletProxy("worldMap") %>% clearShapes() %>%
      addPolygons(
        data = world_data_merged,
        fillColor = ~color_pal(ifelse(is.na(has_data), 0, has_data)),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        highlightOptions = highlightOptions(color = "#FFD700", weight = 2, fillOpacity = 0.9),
        label = ~paste0("Country: ", name_long, "-", 
                        ifelse(!is.na(has_data), "Reported", "No Data")),
        popup = ~paste(
          "Country:", name_long, "-",
          "Indicator:", ifelse(!is.na(indicator_name), indicator_name, "None"), "-",
          ifelse(!is.na(has_data), "Reported", "No Data Available")
        )
      )
    
    output$countryCount <- renderText({
      paste("Total Countries with Data:", nrow(reported_countries))
    })
  })
  
  
  # Change info Box colors to "purple" to match the quartz theme
  
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
    infoBox("Last Updated", "2025", icon = icon("clock"), color = "purple")
  })
  
}


shinyApp(ui = ui, server = server)



# the end ##############################################




