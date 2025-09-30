# Shiny Dashboard for Worldwide Bureaucracy Indicators

### Libraries

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(data.table)
library(ggplot2)
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
library(viridis)
library(here)
library(glue)
library(colourpicker)
library(htmlwidgets)
library(bs4Dash)
library(countrycode)
library(bslib)
library(ggthemes)
library(shinyBS)

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


library(shiny)
library(bslib)

ui <- bootstrapPage(
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  
  # Put global CSS/JS in <head>
  tags$head(
    # ------- Your styles (unchanged) -------
    tags$style(HTML("
      :root { --accent: #6fa8dc; }

      /* General page background and text color */
      html, body { height: 100%; }
      body, .container-fluid, .main-container, .content-wrapper, .flex-grow-1 {
        background-color: #002244 !important;
        color: #ffffff !important;
      }

      /* Typography and panels */
      h1, h2, h3, h4, h5, h6, p, .well, .card, .panel, .info-box, .custom-info-box, .box {
        color: #ffffff !important;
        background-color: transparent !important;
        border: none !important;
      }

      /* Panels and wells with a border */
      .well, .panel {
        background-color: #002244 !important;
        border: 1px solid var(--accent) !important;
        border-radius: 8px;
      }

      /* Buttons */
      .btn, .btn-primary {
        background-color: var(--accent) !important;
        border: none !important;
      }
      .btn:hover { background-color: #4a90c2 !important; }

      /* Link styling */
      a { color: #ffffff !important; text-decoration: none; }
      a:hover { text-decoration: underline; }

      /* --- Sidebar container --- */
      #sidebar {
        height: 100vh;
        width: 290px; min-width: 290px;
        background: linear-gradient(180deg, #2b4c66 0%, #253f57 100%);
        padding: 18px 16px;
        color: #e8f0fb;
        overflow-y: auto;
        border-right: 1px solid rgba(255,255,255,0.08);
        box-shadow: inset 0 0 12px rgba(0,0,0,.25);
        position: sticky; top: 0;
      }

      /* subtle custom scrollbar */
      #sidebar::-webkit-scrollbar { width: 8px; }
      #sidebar::-webkit-scrollbar-thumb {
        background: rgba(255,255,255,.25);
        border-radius: 8px;
      }
      #sidebar::-webkit-scrollbar-track { background: transparent; }

      /* brand/title area (optional) */
      .sidebar-brand {
        display: flex; align-items: center; gap: 10px;
        margin: 2px 6px 14px;
        font-weight: 700; letter-spacing: .3px;
        color: #fff;
      }
      .sidebar-brand .brand-dot {
        width: 10px; height: 10px; border-radius: 50%;
        background: var(--accent); display: inline-block;
      }

      /* section headings (click to expand) */
      .nav-section {
        display: flex; align-items: center; justify-content: space-between;
        font-size: 16px; font-weight: 700;
        padding: 10px 10px; margin: 12px 6px 4px;
        color: #dbe7ff; border-radius: 8px;
        transition: background .2s, color .2s;
        cursor: pointer;
      }
      .nav-section:hover { background: rgba(255,255,255,.06); }
      .nav-section::after {
        content: '▾'; font-size: 14px; opacity: .8; margin-left: 8px;
      }
      .section-open::after { transform: rotate(180deg); }

      /* links */
      .nav-item a, .nav-sub-item a { color: inherit; text-decoration: none; }

      /* top-level items */
      .nav-item {
        display: flex; align-items: center; gap: 10px;
        margin: 6px 6px; padding: 10px 12px;
        font-size: 16px; font-weight: 600; color: #eef5ff;
        border-radius: 10px; transition: transform .08s, background .2s;
      }
      .nav-item:hover { background: rgba(255,255,255,.08); transform: translateX(2px); }

      /* active item with accent bar */
      .nav-item.active {
        background: rgba(111,168,220,.25);
        box-shadow: inset 0 0 0 1px rgba(111,168,220,.5);
        position: relative;
      }
      .nav-item.active::before {
        content: ''; position: absolute; left: -6px; top: 10px; bottom: 10px;
        width: 4px; border-radius: 4px; background: var(--accent);
      }

      /* sub-items (include all expandable sections) */
      #macro_section,
      #public_sector_section,
      #public_sector_workforce_section,
      #public_sector_wages_section,
      #equity_public_sector_section {
        padding: 4px 6px 6px 12px; display: none;
        border-left: 1px dashed rgba(255,255,255,.15);
        margin-left: 10px;
      }

      .nav-sub-item {
        display: flex; align-items: center; gap: 8px;
        margin: 4px 0; padding: 8px 10px;
        font-size: 15px; color: #eaf3ff; border-radius: 8px;
        transition: background .2s, transform .08s;
      }
      .nav-sub-item:hover { background: rgba(255,255,255,.06); transform: translateX(2px); }
      .nav-sub-item.active { background: rgba(111,168,220,.22); }

      /* =========================
         Accordion (Bootstrap 5)
         ========================= */
      .accordion-item{
        background-color:#2b4c66;
        border:1px solid #6fa8dc;
        border-radius:12px !important;
        margin-bottom:14px;
        overflow:hidden;
        color:#fff;
      }
      .accordion-button{
        background-color:#2b4c66;
        color:#fff;
        box-shadow:none !important;
        font-size:18px;
        padding:16px 20px;
      }
      .accordion-button:not(.collapsed){
        background-color:#356088;
        color:#fff;
      }
      .accordion-button:focus{
        box-shadow:none !important;
      }
      .accordion-body{
        background-color:#356088;
        color:#fff;
        padding:18px 22px;
        border-top:1px solid #6fa8dc;
      }
      .accordion-button::after{ filter: invert(1); }  /* white chevron */

      /* Keep all three logos on one line and evenly spaced */
      .logos-row { display:flex; align-items:center; justify-content:space-between; gap:12px; flex-wrap:nowrap; }
      .logo-wrap { flex:1 1 0; display:flex; justify-content:center; }
      .logos-row img { max-width:100%; height:auto; object-fit:contain; display:inline-block; vertical-align:middle; }
      img.bl-logo, img.wb-logo { max-height:64px; }
      @media (min-width: 992px) { img.wb-logo.wb-logo--right { max-height:80px; } }
      .wb-logo.wb-logo--right.padfix { transform: scale(1.12); transform-origin: center; }
img.wb-logo.wb-logo--dec { max-height: 60px !important; }
      /* Info boxes (optional) */
      .custom-infobox .info-box-icon{
        flex: 0 0 var(--tile);
        height: var(--tile);
        border-radius: 12px;
        background-color: #00BFE5 !important;
        color: #fff !important;
        display:flex; align-items:center; justify-content:center;
        float: none !important;
      }
      .custom-infobox .info-box-content{ margin:0 !important; padding:0; }
      .custom-infobox .info-box-text{ font-size:15px !important; line-height:1.2; letter-spacing:.2px; margin:0; }
      .custom-infobox .info-box-number{ font-size:22px !important; font-weight:600; line-height:1.1; margin-top:2px; }

      @media (max-width: 992px){
        .custom-infobox .info-box{ --tile:64px; }
        .custom-infobox .info-box-icon i{ font-size:22px !important; }
        .custom-infobox .info-box-text{ font-size:14px !important; }
        .custom-infobox .info-box-number{ font-size:20px !important; }
      }
      @media (max-width: 768px){
        .custom-infobox .info-box{ --tile:56px; --gap:10px; padding:6px 4px; }
        .custom-infobox .info-box-icon i{ font-size:20px !important; }
        .custom-infobox .info-box-text{ font-size:13px !important; }
        .custom-infobox .info-box-number{ font-size:18px !important; }
      }

      #graph_choice .form-check { margin-bottom: .25rem; }
    ")),
    
    # ------- Accordion styles to match your palette -------
    tags$style(HTML("
      .accordion-item{
        background-color:#2b4c66;
        border:1px solid #6fa8dc;
        border-radius:12px !important;
        margin-bottom:14px;
        overflow:hidden;
        color:#fff;
      }
      .accordion-button{
        background-color:#2b4c66;
        color:#fff;
        box-shadow:none !important;
        font-size:18px;
        padding:16px 20px;
      }
      .accordion-button:not(.collapsed){
        background-color:#356088;
        color:#fff;
      }
      .accordion-button:focus{ box-shadow:none !important; }
      .accordion-body{
        background-color:#356088;
        color:#fff;
        padding:18px 22px;
        border-top:1px solid #6fa8dc;
      }
      .accordion-button::after{ filter: invert(1); }

      /* Logo size (base) */
      .wb-logo{
        max-height:60px;
        width:auto;
        height:auto;
        display:inline-block;
      }

      /* Download buttons */
      .dl-btn {
        font-size: 18px;
        padding: 14px 22px;
        border-radius: 12px;
        background-color: #76A9D6;
        border-color: #76A9D6;
        color: #fff;
      }
      .dl-btn:hover {
        background-color: #669bd0;
        border-color: #669bd0;
        color:#fff;
      }

      /* Only shrink the DEC logo */
      img.wb-logo.wb-logo--dec { max-height: 45px !important; }

      li i.fa, li i.fas, li i.fa-solid { margin: 0 6px; color: #fff; }

      /* Keep the group left-aligned */
      #graph_choice { text-align: left; }

      /* Title aligned with the radio dots (fixed small typo: removed stray space in -1.9rem) */
      #graph_choice .rb-title{
        display: block;
        font-weight: 700;
        font-size: 1.1rem;
        line-height: 1.2;
        margin: 0 0 .25rem 0;
        padding-left: 0;
        margin-left: -1.9rem;
      }

      @media (max-width: 768px){
        #graph_choice .rb-title{
          font-size: 1.0rem;
          padding-left: 1.9rem;
        }
      }

      #graph_choice .form-check { margin-bottom: .3rem; }
    ")),
    
    # ------- JS to toggle sidebar submenus (yours) -------
    tags$script(HTML("
      function toggleSection(id){
        var section = document.getElementById(id);
        section.style.display = (section.style.display === 'none' || section.style.display === '') ? 'block' : 'none';
        var header = document.querySelector('[onclick=\"toggleSection(\\''+id+'\\')\"]');
        if(header){ header.classList.toggle('section-open'); }
      }
      // highlight clicked items
      document.addEventListener('click', function(e){
        if(e.target.closest('.nav-item')){
          document.querySelectorAll('#sidebar .nav-item').forEach(n=>n.classList.remove('active'));
          e.target.closest('.nav-item').classList.add('active');
        }
        if(e.target.closest('.nav-sub-item')){
          document.querySelectorAll('#sidebar .nav-sub-item').forEach(n=>n.classList.remove('active'));
          e.target.closest('.nav-sub-item').classList.add('active');
        }
      }, true);
    "))
  ),
  
  # ------- Layout -------
  div(class = "d-flex",
      # --- Sidebar ---
      div(
        id = "sidebar",
        div(class = "nav-item", actionLink("nav_dashboard", "Overview")),
        div(class = "nav-item", actionLink("nav_instructions", "Instructions")),
        div(class = "nav-item", actionLink("nav_metadata", "Metadata")),
        
        div(class = "nav-section", onclick = "toggleSection('macro_section')",
            "Macro Fundamentals of the Public Sector"),
        div(id = "macro_section",
            div(class = "nav-sub-item", actionLink("nav_wagebill", "Wage Bill Graphs")),
            div(class = "nav-sub-item", actionLink("nav_wagebill_gdp", "Wage Bill & GDP Graphs"))
        ),
        
        div(class = "nav-section", onclick = "toggleSection('public_sector_section')",
            "Size and Characteristics of the Public Sector"),
        div(id = "public_sector_section",
            div(class = "nav-sub-item", actionLink("nav_public_graphs", "Public Employment")),
            div(class = "nav-sub-item", actionLink("nav_public_workforce", "Employment Distribution")),
            div(class = "nav-sub-item", actionLink("nav_education", "Tertiary Education"))
        ),
        
        div(class = "nav-section", onclick = "toggleSection('public_sector_wages_section')",
            "Competitiveness of Public Sector Wages"),
        div(id = "public_sector_wages_section",
            div(class = "nav-sub-item", actionLink("nav_wagepremium", "Wage Premium")),
            div(class = "nav-sub-item", actionLink("nav_public_educ", "Wage Premium by Education")),
            div(class = "nav-sub-item", actionLink("nav_pay_compression", "Pay Compression"))
        ),
        
        div(class = "nav-section", onclick = "toggleSection('equity_public_sector_section')",
            "Equity in Public Sector"),
        div(id = "equity_public_sector_section",
            div(class = "nav-sub-item", actionLink("nav_gender_workforce", "Female Employment")),
            div(class = "nav-sub-item", actionLink("nav_female_leadership", "Female Leadership")),
            div(class = "nav-sub-item", actionLink("nav_wagepremium_gender", "Wage Premium by Gender")),
            div(class = "nav-sub-item", actionLink("nav_gender_wage_premium", "Gender Wage Premium by Industry"))
        ),
        
        div(class = "nav-item", actionLink("nav_download_all", "📥 Download All Graphs"))
      ),
      
      # --- Main content with collapsible tabs (accordion) ---
      div(class = "flex-grow-1 p-4",
          h2("Worldwide Bureaucracy Indicators"),
          uiOutput("main_content")
      )
  )
)
