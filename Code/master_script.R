# Master Script
# This script allows users to adjust the parameters and individually run the random forest training scripts and the prediction script.

# Mandatory Parameters ----------------------------------------------------
# These parameters need to be defined specifically to user's use case. 

# Define a unique identifier for this run.
num <- "temp_agg_1" 

# Define the state.
state <- "WA"

# Define the year.
year <- 2021

# Indicate whether to aggregate into 6-hour bins (Midnight-6AM, 6AM-12PM, 12-6PM, 6PM-Midnight), 
# versus training and generating predictions based on individual hours. 
# If time_bins is set to TRUE, the tool will aggregate the data
# in 6-hour bins and train on that.
time_bins <- F

# Indicate if imputed Waze values exist or not. 
# TRUE will use the imputed Waze files, FALSE will not. 
imputed_waze <- T

# Optional Parameters -----------------------------------------------------
# These parameters do not need to be changed but can be if desired. 

# Define the geographic projection. 
projection <- 5070

# Define the percentage of the population to use in the test sample. 
test_percentage <- 0.03

##identify road types you'd like to query for; can pick from c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential')
road_types <- c('motorway', 'trunk', 'primary', 'secondary', 'tertiary')

# Compute Model Name ------------------------------------------------------
if(train_imputed == TRUE){
  modelno = paste(state, year, "imputed", ifelse(time_bins, "tbins",""), num, sep = "_")
}else{
  modelno = paste(state, year, "NOTimputed", ifelse(time_bins, "tbins",""), num, sep = "_")
}

# RandomForest_Train.R ----------------------------------------------------
# Run this source code to train the random forest model.
# ENSURE TO RUN THE PARAMETERS FIRST! 

source(file.path("analysis", "RandomForest_Train.R"))

# PredictWeek.R -----------------------------------------------------------
# Run this source code to generate the predicted week Tableau file.
# ENSURE TO RUN THE PARAMETERS FIRST! 

source(file.path("analysis", "PredictWeek.R"))


