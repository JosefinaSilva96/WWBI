#Shiny App WWBI-Main do file

# Install Shiny if not installed

if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")

# Load the Shiny package

library(shiny)

# Run the app directly from GitHub

runGitHub("WWBI", "JosefinaSilva96", subdir = "Code")


