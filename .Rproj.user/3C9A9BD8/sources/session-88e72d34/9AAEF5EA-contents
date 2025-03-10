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

data_path1 <- file.path(getwd(), "data_wwbi.dta") 

data_wwbi <- read_dta(data_path1)


#data <- read_excel("Data/Worldwide Bureaucracy Indicators (WWBI) Version 3.1.xlsx", 
                   #sheet = "Data") #61004 obs


#Load GDP pc IMF for countries

data_path2 <- file.path(getwd(), "data_gdp.dta") 

gdp_pc <- read_dta(data_path2)


#gdp_pc <-  read_excel("Data/IMF_GDP.xls", 
                      #sheet = "gdp") #228 obs


#Transform the data sets into a data.table

data_table <- as.data.table(data)

data_table_gdp <- as.data.table(gdp_pc)

#View data

View(data_table)
head(data_table)
n_distinct(data_table)
nrow(data_table) # 61004 observations 
glimpse(data_table)


#View data gdp

View(data_table_gdp)
head(data_table_gdp)
n_distinct(data_table_gdp)
nrow(data_table_gdp) # 228 observations 
glimpse(data_table_gdp)

# Rename columns with small letters

setnames(data_table, tolower(names(data_table)))


setnames(data_table_gdp, tolower(names(data_table_gdp)))


# Rename all columns that start with a number by adding a "year_" prefix

setnames(data_table, old = names(data_table), new = ifelse(grepl("^[0-9]", names(data_table)), paste0("year_", names(data_table)), names(data_table)))


setnames(data_table_gdp, old = names(data_table_gdp), new = ifelse(grepl("^[0-9]", names(data_table_gdp)), paste0("year_", names(data_table_gdp)), names(data_table_gdp)))

# Replace special characters columns

setnames(data_table, gsub("[^[:alnum:]_]", "_", names(data_table)))

setnames(data_table_gdp, gsub("[^[:alnum:]_]", "_", names(data_table_gdp)))

#Rename column for gdp data base 

setnames(data_table_gdp, old = "gdp_per_capita__current_prices___u_s__dollars_per_capita_", new = "country_name")


# Identify columns that start with "year"

year_cols <- grep("^year", names(data_table_gdp), value = TRUE)

# Convert "year" columns to numeric

data_table_gdp[, (year_cols) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = year_cols]

# Round the numeric "year" columns to 2 decimal places

data_table_gdp[, (year_cols) := lapply(.SD, round, 2), .SDcols = year_cols]


#Transform data.table to a data.frame

data_wwbi<- as.data.frame(data_table)

data_gdp<- as.data.frame(data_table_gdp)

# Save the project data 

write_dta(data_wwbi, file.path(data_path, "Data/Intermediate/data_wwbi.dta"))


write_dta(data_gdp, file.path(data_path, "Data/Intermediate/data_gdp.dta"))

