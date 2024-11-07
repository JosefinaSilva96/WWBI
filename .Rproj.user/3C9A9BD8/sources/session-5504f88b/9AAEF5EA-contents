# Worldwide Bureaucracy Indicators
# 01. Data processing

### Libraries

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(readxl)
library(data.table)
library(lubridate)

### Loading data ----

# Load the data sets

data <- read_excel("C:/Users/wb631166/OneDrive - WBG/Desktop/Bureaucracy Lab/WWBI/Worldwide Bureaucracy Indicators (WWBI) Version 3.1.xlsx", 
                   sheet = "Data") #61004 obs


#Transform the data sets into a data.table

data_table <- as.data.table(data)

#View data

View(data_table)
head(data_table)
n_distinct(data_table)
nrow(data_table) # 61004 observations 
glimpse(data_table)

# Rename columns with small letters

setnames(data_table, tolower(names(data_table)))

# Rename all columns that start with a number by adding a "year_" prefix

setnames(data_table, old = names(data_table), new = ifelse(grepl("^[0-9]", names(data_table)), paste0("year_", names(data_table)), names(data_table)))


# Replace special characters columns

setnames(data_table, gsub("[^[:alnum:]_]", "_", names(data_table)))

#Transform data.table to a data.frame

data_wwbi<- as.data.frame(data_table)

# Save the project data 

write_dta(data_wwbi, file.path(data_path, "Data/Intermediate/data_wwbi.dta"))

