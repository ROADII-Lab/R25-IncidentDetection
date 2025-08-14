
####################### Parameters from master script###################################################################

# Define a unique identifier for this model run.
# Use this to create a separate identifier to distinguish when multiple models are attempted for a given state and year.
# Note: the full model name gets created by combining the num parameter below with other tags, including the state, 
# year, whether or not time_bins is TRUE and whether or not imputed_waze is TRUE. Output files are named in this way.
num <- "5to1_metro"

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

##Identify road types you'd like to use for analysis. You can pick from: c('motorway', 'trunk', 'primary', 'secondary', 'tertiary')
# Note: regardless of which subset you pick, all five of the above road types will be queried and added to the state_network file.
# The road types will later be filtered down to the subset you identify below - this is to avoid errant spatial matches during data processing.
# Open street maps also has two additional road types ('unclassified' and 'residential'). These are not options to analyze with this tool and 
# cannot be selected.
# This page explains more about the road types, which are in the 'highway' attribute: https://wiki.openstreetmap.org/wiki/Key:highway
road_types <- c('motorway', 'trunk', 'primary')

# Indicate whether to subset the analysis to only a certain area of interest.
# If filter_osm is set to T or TRUE, you must prepare a shapefile or other geospatial file defining the area of interest and 
# specify the full path to that shapefile in the next parameter (AOI_shp_path)
filter_osm <- T
# if the above is true, then for AOI_shp_path specify the path (within the Input folder) that leads to the shapefile that defines the area of interest.
AOI_shp_path <- paste('Shapefiles', 'MN_Metro', 'MN_Metro.shp', sep='/')

# Indicate whether to include events as a predictor.
# If include_events is set to T or TRUE, you must put a .csv file in the Input folder that specifies special events, such as holidays.
# It should contain a column called "Date", with dates specified in the following format: %m/%d/%Y - 
# for example, January 6, 2026 would be 1/6/2026 in that format.
# File name should be 'events.csv'
include_events <- T

# confirm the name of the crash column that is used as the target, or response variable.
response.var <- "CAD_CRASH" # binary indicator of whether crash occurred, based on processing above. random forest function can also accept numeric target. 

# ratio of non-crash observations to crash observations to use in sampling. actual ratio in natural population is typically 10,000-to-1.
# try 5 (which means 5-to-1) or 10 (which means 10-to-1)
noncrashratio <- 5

#####################################################################################################################

inputdir <- file.path(getwd(),"Input")
intermediatedir <- file.path(getwd(),"Intermediate")
outputdir<- file.path(getwd(),"Output")
predict_week_out_dir <- file.path(outputdir, "Predict_Week_Outputs")
pilot_dir <- file.path(outputdir, "Pilot_Results")
pilot_results_dir <- file.path(pilot_dir, "Analysis")

if(!dir.exists(pilot_results_dir)) { dir.create(pilot_results_dir) }

source(file.path("utility", "get_packages.R")) # installs necessary package

# read helper functions (e.g. fill_na())
source(file.path("analysis", "RandomForest_Fx.R"))

# read in road network (needed to load CAD data)
source(file.path("utility", "OpenStreetMap_pull.R"))

# read in functions to load predictions and actual crash data and present results
source(file.path("utility", "MN_pilot_helper_functions.R"))

# The full model identifier gets created in this next step
modelno = paste(state,
                year, 
                ifelse(imputed_waze, "imputed", "NOTimputed"),
                ifelse(time_bins, "tbins", "hourly"), 
                num, 
                sep = "_")

# establish the dates of relevant runs
May_runs <- as.Date(c("2025-05-20", "2025-05-21", "2025-05-23"))
June_runs <- as.Date(c("2025-06-09", "2025-06-11", "2025-06-13"))
July_runs <- as.Date(c("2025-07-03"))

# create data frames that combine predictions with actual CAD crashes
May <- load_combine(runs = May_runs, CAD_fn = "250520 Metro CAD data.csv")

May_20_23 <- May %>% filter(date <= as.Date("2025-05-23"))

May_24_26 <- May %>% filter(date > as.Date("2025-05-23"))

June_9_15 <- load_combine(runs = June_runs, CAD_fn = "250609 Metro CAD data.csv")

July_3_6 <- load_combine(runs = July_runs, CAD_fn = "250703 Metro CAD data.csv")

# make a list of all data frames and then plot a comparison of risk scores for crash versus non-crash for all time periods
df_list <- list(Normal_May_Week = May_20_23, Memorial_Day_Weekend = May_24_26, Normal_June_Week = June_9_15, July_4_Weekend = July_3_6)

plot_compare_risk_list(df_list, file.path(pilot_results_dir, "risk_comparison.png"))

# Run additional analyses for various time periods

May_20_23_results <- run_analysis(May_20_23, "May_20_23")

May_24_26_results <- run_analysis(May_24_26, "May_24_26")

June_9_15_results <- run_analysis(June_9_15, "June_9_15")

July_3_6_results <- run_analysis(July_3_6, "July_3_6")

# Run additional analyses for all periods merged together
all_periods <- do.call(bind_rows, df_list) 

all_periods_results <- run_analysis(all_periods, "all_periods")

# Combine a subset of results across all into one data frame for comparison

overall_summary <- data.frame(
  Period = character(),
  PR_AUC = numeric(),
  multiplier = numeric(),
  random_baseline = numeric(),
  crashes_per_day = numeric(),
  stringsAsFactors = FALSE
)

results_list <- list(
  Normal_May_Week = May_20_23_results,
  Memorial_Day_Weekend = May_24_26_results,
  Normal_June_Week = June_9_15_results,
  July_4_Weekend = July_3_6_results,
  All_Periods = all_periods_results
)

for(i in seq_along(results_list)) {
  # Extract the sublist for convenience
  res <- results_list[[i]]
  
  # Construct a named list
  new_row <- list(
    Period = names(results_list)[i],
    PR_AUC = res$overall_PR$pr_auc,
    multiplier = res$overall_PR$multiplier,
    random_baseline = res$overall_PR$random_baseline,
    crashes_per_day = res$overall_PR$crashes_per_day
  )
  
  # Add the new row to overall_results
  overall_summary <- bind_rows(overall_summary, new_row)
}

write.csv(overall_summary, file = file.path(pilot_results_dir, "overall_summary.csv"), row.names = F)

####################################################################

test <- May_20_23$osm_id %>% filter(pos_fraction > 0)

test2 <- test %>% filter(f1_score > 0)

####################################################################
