# Master Script
# This script allows users to adjust the parameters and individually run the random forest training scripts and the prediction script.

# Mandatory Parameters ----------------------------------------------------
# The user must define the below parameters according to their use case. 

# Define a unique identifier for this model run.
# Use this to create a separate identifier to distinguish when multiple models are attempted for a given state and year.
# Note: the full model name gets created by combining the num parameter below with other tags, including the state, 
# year, whether or not time_bins is TRUE and whether or not imputed_waze is TRUE. Output files are named in this way.
num <- "avg_CAD" 

# Define the state, using abbreviation.
state <- "MN"

# Define the year.
year <- 2020

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

##Identify road types you'd like to query for; can pick from c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential').
# Note: if including 'residential it may take a long time to query all the data from open street maps.
road_types <- c('motorway', 'trunk', 'primary', 'secondary', 'tertiary')

# RandomForest_Train.R ----------------------------------------------------
# Run this source code to train the random forest model.
# ENSURE TO RUN THE PARAMETERS FIRST! 

source(file.path("analysis", "RandomForest_Train.R"))

# PredictWeek.R -----------------------------------------------------------
# Run this source code to generate the predicted week Tableau file.
# ENSURE TO RUN THE PARAMETERS FIRST! 

source(file.path("analysis", "PredictWeek.R"))


