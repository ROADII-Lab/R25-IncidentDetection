
inputdir <- file.path(getwd(),"Input")
intermediatedir <- file.path(getwd(),"Intermediate")
outputdir<- file.path(getwd(),"Output")
predict_week_out_dir <- file.path(outputdir, "Predict_Week_Outputs")
pilot_results_dir <- file.path(outputdir, "Pilot_Results")

if(!dir.exists(pilot_results_dir)) { dir.create(pilot_results_dir) }

source(file.path("utility", "get_packages.R")) # installs necessary package

# read helper functions (e.g. fill_na())
source(file.path("analysis", "RandomForest_Fx.R"))

# read in road network (needed to load CAD data)
source(file.path("utility", "OpenStreetMap_pull.R"))

# read in functions to load predictions and actual crash data and present results
source(file.path("utility", "MN_CAD_load_pilot_data.R"))

# The full model identifier gets created in this next step
modelno = paste(state,
                year, 
                ifelse(imputed_waze, "imputed", "NOTimputed"),
                ifelse(time_bins, "tbins", "hourly"), 
                num, 
                sep = "_")

# establish the dates of relevant runs
runs <- as.Date(c("2025-05-20", "2025-05-21", "2025-05-23"))

# load predictions
predictions <- load_and_combine_csv(runs)

# load actual CAD_CRASH data
CAD_result <- load_CAD_pilot_results(file.path(inputdir,"Crash","250520 Metro CAD data.csv"))

# trim off predictions that extend beyond the time frame for which we have actual results
predictions <- predictions %>% filter(date <= max(CAD_result$latest_record))

# join actual results to predictions
merge_result <- left_join(predictions, CAD_result$df, by = c('osm_id','Month', 'day', 'Hour')) %>%
  fill_na() %>%
  mutate(CAD_CRASH = ifelse(CAD_CRASH>0, 1, 0))

# Call main function disaggregating by different variables with static threshold
results_hour_stat <- performance_by_group(merge_result, "Hour", use_dynamic_threshold = FALSE, fixed_threshold = 0.85)
results_weekday_stat <- performance_by_group(merge_result, "weekday", use_dynamic_threshold = FALSE, fixed_threshold = 0.85)
results_roadtype_stat <- performance_by_group(merge_result, "highway", use_dynamic_threshold = FALSE, fixed_threshold = 0.85)
# lower threshold for this one
results_osm_id_stat <- performance_by_group(merge_result, "osm_id", use_dynamic_threshold = FALSE, fixed_threshold = 0.5)

# Generate some plots
plot_pr_auc_with_baseline(results_hour_stat, "Hour")
plot_multiplier(results_hour_stat, "Hour")
plot_accuracy(results_hour_stat, "Hour")
plot_f1(results_hour_stat, "Hour")

####################################################################
results_osm_id_stat <- results_osm_id_stat %>%
  mutate()

test <- results_osm_id_stat %>% filter(pos_fraction > 0)

test2 <- test %>% filter(f1_score > 0)

####################################################################

# Calculate precision-recall results for May 20-26
May20to26_PR_result <- calculate_PR(dataframe = merge_result)

# Calculate precision-recall results for May 20-23 (normal week)
May_20to23 <- merge_result %>% filter(date <= as.Date("2025-05-23"))
May20to23_PR_result <- 

# Calculate precision-recall results for May 24-26 (Memorial Day holiday weekend)
May24to26_PR_result <- merge_result %>% filter(date > as.Date("2025-05-23"))
  
# Save as interactive HTML
htmlwidgets::saveWidget(
  fig,
  file = file.path(pilot_results_dir, paste0("PR_curve_", date(min(predictions$date)), "_to_", date(max(predictions$date)), ".html"))
)