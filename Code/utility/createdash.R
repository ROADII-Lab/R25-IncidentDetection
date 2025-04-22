# This script is sourced by PredictWeek.R

config_dir <- file.path(getwd(), "config")

create_dashboard <- function(RoadNetwork, CrashPredictions, DateInfo) {
  
  # Define paths
  zip_file <- file.path(config_dir, "TableauDashboard.zip")
  unzip_folder <- file.path(config_dir, "UnzippedFiles")
  data_folder <- file.path(unzip_folder, "Data")
  clean_dash_folder <- file.path(data_folder, "CleanDash")
  
  # Define paths for output files
  prediction_path <- file.path(clean_dash_folder, "CrashPrediction.csv")
  dateinfo_path <- file.path(clean_dash_folder, "DateInfo.csv")
  network_path <- file.path(clean_dash_folder, "RoadNetwork.shp") 

  # Unzip the Tableau dashboard files
  unzip(zip_file, exdir = unzip_folder)
  
  # Overwrite the CrashPrediction.csv
  write_csv(CrashPredictions, prediction_path)
  
  # Overwrite the DateInfo.csv
  write_csv(DateInfo, dateinfo_path)
  
  # Overwrite the road network shapefile
  write_sf(RoadNetwork, network_path, delete_layer = TRUE)
  
  template_path <- file.path(unzip_folder, 'TableauDashboard.twb')
  
  # Repackage the files into a .twbx
  zipr(file.path(unzip_folder, "TableauDashboard.twbx"), c(data_folder, template_path), recurse = TRUE)
  
}

# Call the function with the relevant arguments
create_dashboard(RoadNetwork = state_network, CrashPredictions = CrashPredictions, DateInfo = DateInfo)