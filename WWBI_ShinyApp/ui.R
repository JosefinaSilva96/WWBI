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
library(rnaturalearth)
library(sf)
library(plotly)
library(officer)
library(flextable)
library(viridis)
library(here)
library(glue)
library(colourpicker)
library(wbstats)
library(htmlwidgets)
library(bs4Dash)
library(countrycode)
library(bslib)
library(lubridate)
library(scales)

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