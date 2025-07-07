
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

# function to run analysis for a given time period
run_analysis <- function(df, save_name){
  
  # Determine precision-recall curve for the entire set (not disaggregated)
  # returns fig, pr_auc, multiplier, random_baseline
  overall_PR = calculate_PR(dataframe = df)
  
  # Save as interactive HTML
  htmlwidgets::saveWidget(
    overall_PR$fig,
    file = file.path(pilot_results_dir, paste0("Overall_PR_curve_", save_name, ".html"))
  )
  
  # Then calculate performance disaggregated by different variables with static threshold
  results_hour_stat = performance_by_group(df, "Hour", use_dynamic_threshold = FALSE, fixed_threshold = 0.85)
  results_weekday_stat = performance_by_group(df, "weekday", use_dynamic_threshold = FALSE, fixed_threshold = 0.85)
  results_roadtype_stat = performance_by_group(df, "highway", use_dynamic_threshold = FALSE, fixed_threshold = 0.85)
  # lower threshold for this one
  results_osm_id_stat = performance_by_group(df, "osm_id", use_dynamic_threshold = FALSE, fixed_threshold = 0.5)
  
  write.csv(results_hour_stat, file = file.path(pilot_results_dir, paste0(save_name, "results_hour_stat.csv")))
  write.csv(results_weekday_stat, file = file.path(pilot_results_dir, paste0(save_name, "results_weekday_stat.csv")))
  write.csv(results_roadtype_stat, file = file.path(pilot_results_dir, paste0(save_name, "results_roadtype_stat.csv")))
  write.csv(results_osm_id_stat, file = file.path(pilot_results_dir, paste0(save_name, "results_osm_id_stat.csv")))
  
  generate_plots(result_df = results_hour_stat, group_var = "Hour", save_name = save_name)
  generate_plots(result_df = results_weekday_stat, group_var = "weekday", save_name = save_name)
  generate_plots(result_df = results_roadtype_stat, group_var = "highway", save_name = save_name)
  
  return(list(overall_PR = overall_PR, hour = results_hour_stat, weekday = results_weekday_stat, highway = results_roadtype_stat, osm_id = results_osm_id_stat))
}

May_20_23 <- merge_result %>% filter(date <= as.Date("2025-05-23"))

May24_to_26 <- merge_result %>% filter(date > as.Date("2025-05-23"))

May20_to_23_results <- run_analysis(May_20_23, "May_20_23")

May24_to_26_results <- run_analysis(May24_to_26, "May24_to_26")


####################################################################

test <- May_20_23$results_osm_id_stat %>% filter(pos_fraction > 0)

test2 <- test %>% filter(f1_score > 0)

####################################################################
