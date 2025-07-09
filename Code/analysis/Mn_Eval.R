
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


May <- load_combine(runs = May_runs, CAD_fn = "250520 Metro CAD data.csv")

May_20_23 <- merge_result %>% filter(date <= as.Date("2025-05-23"))

May_24_26 <- merge_result %>% filter(date > as.Date("2025-05-23"))

June_9_15 <- load_combine(runs = June_runs, CAD_fn = "250609 Metro CAD data.csv")

July_3_6 <- load_combine(runs = July_runs, CAD_fn = "250703 Metro CAD data.csv")

df_list <- list(Normal_May_Week = May_20_23, Memorial_Day_Weekend = May_24_to_26, Normal_June_Week = June_9_15, July_4_Weekend = July_3_6)

plot_compare_risk_list(df_list, file.path(pilot_results_dir, "risk_comparison.png"))


# Run additional analyses for various time periods

May_20_23_results <- run_analysis(May_20_23, "May_20_23")

May_24_26_results <- run_analysis(May_24_26, "May_24_26")

June_9_15_results <- run_analysis(June_9_15, "June_9_15")

July_3_6_results <- run_analysis(July_3_6, "July_3_6")

all_periods <- do.call(bind_rows, df_list) 

all_periods_results <- run_analysis(all_periods, "all_periods")

# Combine a subset of results across all into one data frame

overall_summary <- data.frame(
  Period = character(),
  PR_AUC = numeric(),
  multiplier = numeric(),
  random_baseline = numeric(),
  stringsAsFactors = FALSE
)

results_list <- list(
  Normal_May_Week = May_20_23_results,
  Memorial_Day_Weekend = May_24_to_26_results,
  Normal_June_Week = June_9_15_results,
  July_4_Weekend = July_3_6_results
)

for(i in seq_along(results_list)) {
  # Extract the sublist for convenience
  res <- results_list[[i]]
  
  # Construct a named list (preferred over named vector here)
  new_row <- list(
    Period = names(results_list)[i],
    PR_AUC = res$overall_PR$pr_auc,
    multiplier = res$overall_PR$multiplier,
    random_baseline = res$overall_PR$random_baseline
  )
  
  # Add the new row to overall_results
  overall_summary <- bind_rows(overall_results, new_row)
}



####################################################################

test <- May_20_23$results_osm_id_stat %>% filter(pos_fraction > 0)

test2 <- test %>% filter(f1_score > 0)

####################################################################
