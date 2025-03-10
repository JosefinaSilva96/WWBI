## App.R

library(shiny)

# Source the dashboard script from the ShinyApp folder


source("ShinyApp/02-shiny-dashboard.R")

# Run the app
shinyApp(ui = ui, server = server)
