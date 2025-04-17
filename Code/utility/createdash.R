library(readr)
library(openxlsx)
library(zip)

create_dashboard <- function(RoadNetwork, CrashPrediction, DateInfo) {
  # Define paths
  zip_file <- "TableauDashboard.zip"
  unzip_folder <- "UnzippedFiles"
  data_folder <- file.path(unzip_folder, "Data")
  clean_dash_folder <- file.path(data_folder, "CleanDash")
  
  # Unzip the Tableau dashboard files
  unzip(zip_file, exdir = unzip_folder)
  
  # Overwrite the CrashPrediction.csv
  write_csv(CrashPrediction, file.path(clean_dash_folder, "CrashPrediction.csv"))
  
  # Overwrite the DateInfo.xlsx
  write.xlsx(DateInfo, file.path(clean_dash_folder, "DateInfo.xlsx"))
  
  # Define the path for the shapefiles to be created
  file.copy(RoadNetwork, clean_dash_folder, overwrite = TRUE)
  
  # Repackage the files into a .twbx
  zipr("TableauDashboard.twbx", c(unzip_folder, "TableauDashboard.twbx"), recurse = TRUE)
}

create_dashboard(##RoadNetworkDF, ##CrashPredictionDF, ##DateInfoDF)