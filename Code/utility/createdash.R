# This script is sourced by PredictWeek.R

config_dir <- file.path(getwd(), "config")

create_dashboard <- function(RoadNetwork, CrashPredictions, DateInfo, normalized) {
  
  # Define paths
  zip_file <- file.path(config_dir, "TableauDashboard.zip")
  unzip_folder <- file.path(config_dir, "UnzippedFiles")
  data_folder <- file.path(unzip_folder, "Data")
  clean_dash_folder <- file.path(data_folder, "CleanDash")
  
  # Define paths for output files
  prediction_path <- file.path(clean_dash_folder, "CrashPredictions.csv")
  dateinfo_path <- file.path(clean_dash_folder, "DateInfo.csv")
  network_path <- file.path(clean_dash_folder, "RoadNetwork.shp") 

  # Unzip the Tableau dashboard files
  unzip(zip_file, exdir = unzip_folder)
  
  # Overwrite the CrashPredictions.csv
  write_csv(CrashPredictions, prediction_path)
  
  # Overwrite the DateInfo.csv
  write_csv(DateInfo, dateinfo_path)
  
  # Overwrite the road network shapefile
  write_sf(RoadNetwork, network_path, delete_layer = TRUE)
  
  template_path <- file.path(unzip_folder, 'TableauDashboard.twb')
  
  # Repackage the files into a .twbx
  zipr(file.path(predict_week_out_dir, paste0("Dashboard_", modelno, "_", today, normalized, ".twbx")), c(data_folder, template_path), recurse = TRUE)
  
  # Clean up the temporary files from the config directory
  unlink(unzip_folder, recursive = TRUE)
  
}

# Filter the state network to the road segments of interest
matches <- as.character(state_network$osm_id) %in% as.character(next_week$osm_id)
state_network <- state_network[matches, ]

##########################

# Call the function with the relevant arguments
create_dashboard(RoadNetwork = state_network, CrashPredictions = CrashPredictions, DateInfo = DateInfo, normalized = "Normalized")

# Now create another version that is not normalized

next_week_out <- data.frame(next_week, Crash_pred = predict_next_week, prob_next_week)

next_week_out <- next_week_out %>%
  group_by(Hour, DayOfWeek) %>%
  mutate(Hourly_CrashRisk = Prob_Crash) %>%
  ungroup()

source(file.path("utility", "pivotdash.R"))

create_dashboard(RoadNetwork = state_network, CrashPredictions = CrashPredictions, DateInfo = DateInfo, normalized = "")

