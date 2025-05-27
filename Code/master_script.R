# Master Script
# This script allows users to adjust the parameters and individually run the random forest training scripts and the prediction script.

# Mandatory Parameters ----------------------------------------------------
# The user must define the below parameters according to their use case. 

# Define a unique identifier for this model run.
# Use this to create a separate identifier to distinguish when multiple models are attempted for a given state and year.
# Note: the full model name gets created by combining the num parameter below with other tags, including the state, 
# year, whether or not time_bins is TRUE and whether or not imputed_waze is TRUE. Output files are named in this way.
num <- "10to1_majorroads"

# Define the state, using abbreviation.
state <- "WA"

# Define the year.
year <- 2021

# Indicate whether to aggregate data into multi-hour bins, 
# versus training and generating predictions based on individual hours.
time_bins <- T

# Indicate the size of the bin when 'time_bins' above is set to T for true.
# When using hours, the size of the interval must be less than 24 hours.
# Note: if the number of hours is not a factor of 24 then the final interval will
# not be the same size as the others (it will be smaller, i.e. the remainder).
# As an example, if "6 hours" is entered for the time_interval, then the data will be 
# aggregated into the following bins: bins (Midnight-6AM, 6AM-12PM, 12-6PM, 6PM-Midnight).
time_interval <- "6 hours"

# Indicate if imputed Waze values will be used. 
# TRUE will use the imputed Waze files, FALSE will not. 
imputed_waze <- T

# Optional Parameters -----------------------------------------------------
# These parameters do not need to be changed but can be if desired. 

# Define the geographic projection. 
projection <- 5070

# Define the percentage of the population to use in the test sample. 
test_percentage <- 0.03

##Identify road types you'd like to use for analysis. You can pick from: c('motorway', 'trunk', 'primary', 'secondary', 'tertiary')
# Note: regardless of which subset you pick, all five of the above road types will be queried and added to the state_network file.
# The road types will later be filtered down to the subset you identify below - this is to avoid errant spatial matches during data processing.
# Open street maps also has two additional road types ('unclassified' and 'residential'). These are not options to analyze with this tool and 
# cannot be selected.
# This page explains more about the road types, which are in the 'highway' attribute: https://wiki.openstreetmap.org/wiki/Key:highway
road_types <- c('motorway', 'trunk', 'primary')

# Indicate whether to subset the analysis to only certain road segments. 
# If filter_osm is set to T or TRUE, you must put a .csv file in the Input folder that specifies a subset of osm_ids to use.
# The first row should be the column header, 'osm_id', and the remaining rows the osm_id values that should be included.
# File name should be 'osm_subset.csv'
filter_osm <- F
# if the above is true, then for AOI_shp_path specify the path (within the Input folder) that leads to the shapefile that defines the area of interest.
AOI_shp_path <- paste('Shapefiles', 'MN_Metro', 'MN_Metro.shp', sep='/')

# Indicate whether to include events as a predictor.
# If include_events is set to T or TRUE, you must put a .csv file in the Input folder that specifies special events, such as holidays.
# It should contain a column called "Date", with dates specified in the following format: %m/%d/%Y - 
# for example, January 6, 2026 would be 1/6/2026 in that format.
# File name should be 'events.csv'
include_events <- T

# confirm the name of the crash column that is used as the target, or response variable.
response.var <- "crash" # binary indicator of whether crash occurred, based on processing above. random forest function can also accept numeric target. 

# ratio of non-crash observations to crash observations to use in sampling. actual ratio in natural population is typically 10,000-to-1.
# try 5 (which means 5-to-1) or 10 (which means 10-to-1)
noncrashratio <- 10


# RandomForest_Train.R ----------------------------------------------------
# Run this source code to train the random forest model.
# ENSURE TO RUN THE PARAMETERS FIRST! 

source(file.path("analysis", "RandomForest_Train.R"))

# PredictWeek.R -----------------------------------------------------------
# Run this source code to generate the predicted week Tableau file.
# ENSURE TO RUN THE PARAMETERS FIRST! 

source(file.path("analysis", "PredictWeek.R"))


