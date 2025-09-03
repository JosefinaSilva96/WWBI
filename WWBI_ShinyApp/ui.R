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
library(shinydashboard)

ui <- dashboardPage(
  skin = "blue",  # AdminLTE skin (names only)
  dashboardHeader(title = "WWBI Dashboard", titleWidth = 280),
  
  dashboardSidebar(
    width = 280,
    
    # ====== Global styles & scripts (put in <head>) ======
    tags$head(
      tags$style(HTML("
        :root{
          --bg:#0f3352;         /* content background */
          --sidebar1:#2b4c66;   /* sidebar gradient top */
          --sidebar2:#243d55;   /* sidebar gradient bottom */
          --accent:#6fa8dc;     /* brand accent */
          --header:#183a5a;     /* header/nav bar */
        }

        /* Header */
        .main-header .logo,
        .main-header .navbar { background-color: var(--header) !important; }
        .main-header .logo { color:#fff !important; font-weight:700; }
        .main-header .navbar .sidebar-toggle { color:#fff !important; }

        /* Content wrapper (AdminLTE) */
        .content-wrapper, .right-side {
          background: var(--bg) !important;
          color: #fff !important;
        }
        /* Add padding because BS3 doesn't ship p-classes */
        .content-padding { padding: 22px 24px; }

        /* Typography & containers */
        h1,h2,h3,h4,h5,h6,p,.well,.panel,.box {
          color:#fff !important; background:transparent !important; border:none !important;
        }
        .well,.panel{
          border:1px solid var(--accent) !important; border-radius:10px;
          background-color:#002244 !important;
        }

        /* Buttons & links */
        .btn,.btn-primary{ background:var(--accent) !important; border:none !important; }
        .btn:hover{ background:#4a90c2 !important; }
        a{ color:#fff !important; text-decoration:none; }
        a:hover{ text-decoration:underline; }

        /* ------- Sidebar look (AdminLTE) ------- */
        .main-sidebar{
          background:linear-gradient(180deg,var(--sidebar1) 0%, var(--sidebar2) 100%) !important;
          color:#e8f0fb !important;
          border-right:1px solid rgba(255,255,255,.08);
          box-shadow:inset 0 0 12px rgba(0,0,0,.25);
        }
        .sidebar{ padding:14px 10px; }

        /* custom scrollbar */
        .main-sidebar::-webkit-scrollbar{ width:8px; }
        .main-sidebar::-webkit-scrollbar-thumb{ background:rgba(255,255,255,.25); border-radius:8px; }
        .main-sidebar::-webkit-scrollbar-track{ background:transparent; }

        /* Top-level items (custom) */
        .nav-item{
          display:flex; align-items:center; gap:10px;
          margin:6px; padding:10px 12px; border-radius:10px;
          font-size:16px; font-weight:600; color:#eef5ff;
          transition:transform .08s, background .2s;
        }
        .nav-item a{ color:inherit; text-decoration:none; width:100%; display:block; }
        .nav-item:hover{ background:rgba(255,255,255,.08); transform:translateX(2px); }
        .nav-item.active{
          background:rgba(111,168,220,.22);
          box-shadow:inset 0 0 0 1px rgba(111,168,220,.5);
          position:relative;
        }
        .nav-item.active::before{
          content:''; position:absolute; left:-6px; top:10px; bottom:10px;
          width:4px; border-radius:4px; background:var(--accent);
        }

        /* Section headers (expand/collapse) */
        .nav-section{
          display:flex; align-items:center; gap:8px;
          margin:14px 6px 6px; padding:10px; border-radius:8px;
          font-weight:700; color:#dbe7ff; cursor:pointer;
          transition:background .2s;
        }
        .nav-section:hover{ background:rgba(255,255,255,.06); }
        .nav-section .caret{ margin-left:auto; transition:transform .2s; opacity:.9; }
        .nav-section.open .caret{ transform:rotate(180deg); }

        /* Sub-items & container */
        .nav-sub{
          padding:4px 6px 6px 14px; margin-left:6px;
          border-left:1px dashed rgba(255,255,255,.15); display:none;
        }
        .nav-sub-item{
          display:flex; align-items:center; gap:8px;
          margin:4px 0; padding:8px 10px; border-radius:8px;
          font-size:15px; color:#eaf3ff; transition:background .2s, transform .08s;
        }
        .nav-sub-item a{ color:inherit; text-decoration:none; width:100%; display:block; }
        .nav-sub-item:hover{ background:rgba(255,255,255,.06); transform:translateX(2px); }
        .nav-sub-item.active{ background:rgba(111,168,220,.20); }

        /* Optional: shinydashboard boxes with your palette */
        .box.box-solid>.box-header{ background-color:#2b4c66; color:#fff; }
        .box.box-solid{ border:1px solid var(--accent); border-radius:12px; }
        .box { box-shadow:none; }
      ")),
      tags$script(HTML("
        function toggleSection(id){
          const sec = document.getElementById(id);
          const hdr = document.querySelector('[data-target=\"'+id+'\"]');
          if(!sec || !hdr) return;
          const show = (sec.style.display === 'none' || sec.style.display === '');
          sec.style.display = show ? 'block' : 'none';
          hdr.classList.toggle('open', show);
          // remember state
          if(window.localStorage){
            const openIds = JSON.parse(localStorage.getItem('openSections')||'[]');
            const idx = openIds.indexOf(id);
            if(show && idx === -1) openIds.push(id);
            if(!show && idx > -1) openIds.splice(idx,1);
            localStorage.setItem('openSections', JSON.stringify(openIds));
          }
        }
        // active highlight
        document.addEventListener('click', function(e){
          const top = e.target.closest('.nav-item');
          const sub = e.target.closest('.nav-sub-item');
          if(top){
            document.querySelectorAll('.main-sidebar .nav-item').forEach(n=>n.classList.remove('active'));
            top.classList.add('active');
          }
          if(sub){
            document.querySelectorAll('.main-sidebar .nav-sub-item').forEach(n=>n.classList.remove('active'));
            sub.classList.add('active');
          }
        }, true);

        // restore on load: open sections & set 'Overview' active
        document.addEventListener('DOMContentLoaded', function(){
          try{
            const openIds = JSON.parse(localStorage.getItem('openSections')||'[]');
            openIds.forEach(id=>{
              const sec = document.getElementById(id);
              const hdr = document.querySelector('[data-target=\"'+id+'\"]');
              if(sec && hdr){ sec.style.display='block'; hdr.classList.add('open'); }
            });
          }catch(e){}
          const first = document.querySelector('.nav-item');
          if(first) first.classList.add('active');
        });
      "))
    ),
    
    # ====== Custom sidebar content ======
    div(class = "nav-item", actionLink("nav_dashboard", "Overview")),
    div(class = "nav-item", actionLink("nav_instructions", "Instructions")),
    div(class = "nav-item", actionLink("nav_metadata", "Metadata")),
    
    div(class = "nav-section", `data-target`="macro_section",
        span("Macro Fundamentals of the Public Sector"), span(class="caret","▾"),
        onclick = "toggleSection('macro_section')"),
    div(id = "macro_section", class="nav-sub",
        div(class = "nav-sub-item", actionLink("nav_wagebill", "Wage Bill Graphs")),
        div(class = "nav-sub-item", actionLink("nav_wagebill_gdp", "Wage Bill & GDP Graphs"))
    ),
    
    div(class = "nav-section", `data-target`="public_sector_section",
        span("Size and Characteristics of the Public Sector"), span(class="caret","▾"),
        onclick = "toggleSection('public_sector_section')"),
    div(id = "public_sector_section", class="nav-sub",
        div(class = "nav-sub-item", actionLink("nav_public_graphs", "Public Employment")),
        div(class = "nav-sub-item", actionLink("nav_public_workforce", "Employment Distribution")),
        div(class = "nav-sub-item", actionLink("nav_education", "Tertiary Education"))
    ),
    
    div(class = "nav-section", `data-target`="public_sector_wages_section",
        span("Competitiveness of Public Sector Wages"), span(class="caret","▾"),
        onclick = "toggleSection('public_sector_wages_section')"),
    div(id = "public_sector_wages_section", class="nav-sub",
        div(class = "nav-sub-item", actionLink("nav_wagepremium", "Wage Premium")),
        div(class = "nav-sub-item", actionLink("nav_public_educ", "Wage Premium by Education")),
        div(class = "nav-sub-item", actionLink("nav_pay_compression", "Pay Compression"))
    ),
    
    div(class = "nav-section", `data-target`="equity_public_sector_section",
        span("Equity in Public Sector"), span(class="caret","▾"),
        onclick = "toggleSection('equity_public_sector_section')"),
    div(id = "equity_public_sector_section", class="nav-sub",
        div(class = "nav-sub-item", actionLink("nav_gender_workforce", "Female Employment")),
        div(class = "nav-sub-item", actionLink("nav_female_leadership", "Female Leadership")),
        div(class = "nav-sub-item", actionLink("nav_wagepremium_gender", "Wage Premium by Gender")),
        div(class = "nav-sub-item", actionLink("nav_gender_wage_premium", "Gender Wage Premium by Industry"))
    ),
    
    div(class = "nav-item", actionLink("nav_download_all", "📥 Download All Graphs"))
  ),
  
  dashboardBody(
    div(class = "content-padding",
        h2("Worldwide Bureaucracy Indicators"),
        uiOutput("main_content")
    )
  )
)

