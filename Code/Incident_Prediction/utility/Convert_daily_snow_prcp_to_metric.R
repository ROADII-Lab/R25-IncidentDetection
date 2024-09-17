# Info --------------------------------------------------------------------
# Purpose: Convert daily snow and precipitation amounts from inches to millimeters.

# Prep --------------------------------------------------------------------
source('utility/get_packages.R') # installs necessary packages

# Load packages 
library(data.table) # contains FREAD
library(dplyr)
library(stringr)

inputdir <- file.path(getwd(),"Input")

years <- c(2019, 2020, 2021, 2022)

convert_to_mm <- function(df){
  df$SNOW <- df$SNOW * 25.4
  df$PRCP <- df$PRCP * 25.4
  return(df)
}

load_convert_save <- function(fp){
  temp = fread(fp)
  temp = convert_to_mm(temp)
  write.csv(x = temp, file = fp)
  cat("Converted ", fp, " to millimeters.  \n")
  rm(temp); gc()
}

for(year in years){
  folder = file.path(inputdir, "Weather","GHCN", "Daily", year)
  file_list = list.files(path = folder)
  if(length(file_list)>0){
    cat("Converting ", year, " files to millimeters. \n")
    for(i in file_list){load_convert_save(file.path(folder,i))}
    cat("Completed conversions for ", year, ". \n")
  }
}