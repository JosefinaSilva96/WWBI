## App.R

library(shiny)

# Source your main app script

source("02-shiny-dashboard.R")

# Run the app

shinyApp(ui = ui, server = server)