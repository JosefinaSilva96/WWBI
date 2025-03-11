### Worldwide Bureaucracy Indicators -  Main R Script

# Load libraries ---- 

library(here)

# Debugging: Print the working directory to check paths
print(getwd())

# Run the scripts correctly
source(here("Code", "02-shiny-dashboard.R"))

# Run the app
shinyApp(ui = ui, server = server)

