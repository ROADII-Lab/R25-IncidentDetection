library(dplyr)
library(tidyr)
library(readr)

# Load the dataset
####################################################
#########UPDATE WITH ACTUAL ROADII PATH#############
####################################################
datatall <- read_csv("~/GitHub/MACVSM/Dashboard/MN.csv")

# Define the necessary columns
datatall <- datatall %>% select(osm_id, date, Year, Month, Hour, DayOfWeek, weekday, Hourly_CrashRisk)

# Determine the minimum day of the week
min_day <- min(datatall$DayOfWeek)
min_hour <- min(datatall$Hour[datatall$DayOfWeek == min_day])
datatall <- mutate(datatall, AdjustedHourOfWeek = ((DayOfWeek - min_day) %% 7) * 24 + Hour - min_hour)

# Filter out incomplete first day
#first_day_data <- filter(datatall, DayOfWeek == min_day)
#first_day_hours <- first_day_data$Hour
#if (min(first_day_hours) > 0) {
#  datatall <- filter(datatall, DayOfWeek > min_day)
#  datatall <- mutate(datatall, DayOfWeek = DayOfWeek - 1)
#}

# Pivot the data
pivoted_data <- datatall %>%
  select(osm_id, AdjustedHourOfWeek, Hourly_CrashRisk) %>%
  group_by(osm_id, AdjustedHourOfWeek) %>%
  summarise(Hourly_CrashRisk = mean(Hourly_CrashRisk, na.rm = TRUE)) %>%
  pivot_wider(names_from = AdjustedHourOfWeek, values_from = Hourly_CrashRisk)

# Rename columns
colnames(pivoted_data)[-1] <- paste0("D", (as.numeric(colnames(pivoted_data)[-1]) %/% 24) + 1, "H", (as.numeric(colnames(pivoted_data)[-1]) %% 24) + 1)

library(sf)
library(dplyr)
#######################################################
##########PULL IN STATE NETWORK FILE SCRIPT############
#######################################################
state_network <- #state network filepath
state_network$osm_id <- as.integer(gdf$osm_id)
pivoted_data$osm_id <- as.integer(na.omit(as.numeric(df$osm_id)))
state_geo <- merge(x = state_network, 
                   y = pivoted_data, 
                   by.x = "osm_id", 
                   by.y = "osm_id", all.x = TRUE)

output_shapefile_path <- "TableauShapefile.shp"
st_write(state_geo, output_shapefile_path)




######Prepare Tableau Assets code converted from RDR  
prepare_tableau_assets <- function(report_file, output_folder, cfg, logger) {
    library(lubridate)
    library(fs)
    library(zip)
    
  
    ####### Use ROADII file paths
    logger$info("Start: prepare_tableau_assets")
    tab_dir_name <- paste0("tableau_report_", cfg[['run_id']], "_", format(Sys.time(), "%Y_%m_%d_%H-%M-%S"))
    tableau_directory <- file.path(output_folder, 'Reports', tab_dir_name)
    dir_create(tableau_directory)
    
    # Define config directory and template locations
    config_directory <- file.path(path_abs(path("..")), 'config')
    root_twb_location <- file.path(config_directory, 'template_dashboard.twb')
    
    check_file <- function(file) {
      if (!file_exists(file)) {
        logger$error(paste("TABLEAU REPORT INPUT FILE ERROR:", file, "could not be found"))
        stop(paste("TABLEAU REPORT INPUT FILE ERROR:", file, "could not be found"))
      }
    }
    
    check_file(root_twb_location)
    
    #image_files <- c(
      #"dictionary_noBackground.png",
      #"images.png",
      #"Picture4.png",
      #"Picture2.png"
    #)
    
    #lapply(image_files, function(img) {
    #  check_file(file.path(config_directory, 'tableau_images', img))
    #})
    
    file_copy(root_twb_location, file.path(tableau_directory, 'tableau_dashboard.twb'))
    
    #lapply(image_files, function(img) {
     # file_copy(file.path(config_directory, 'tableau_images', img), tableau_directory)
    #})
    
    # Copy the report file
    logger$debug("Copying the tableau report xlsx file to the tableau report directory")
    file_copy(report_file, file.path(tableau_directory, 'tableau_input_file.xlsx'))
    ## check if this can work with .csv, otherwise convert output to xlsx
    
    # Create packaged workbook
    twbx_dashboard_filename <- file.path(tableau_directory, 'tableau_dashboard.twbx')
    
    zip(zipfile = twbx_dashboard_filename, 
        files = c(
          file.path(tableau_directory, 'tableau_dashboard.twb'),
          file.path(tableau_directory, 'tableau_input_file.xlsx'),
          #file.path(tableau_directory, image_files)
        ),
        flags = "-j")
    
    # Delete the original files for cleanup
    file_delete(file.path(tableau_directory, c('tableau_dashboard.twb', 'tableau_input_file.xlsx'))) #add , image_files if using
    
    # Open the Tableau dashboard
    system(paste("open", shQuote(twbx_dashboard_filename)))
    
    logger$info("Finished: prepare_tableau_assets")
    return(tableau_directory)
  }

