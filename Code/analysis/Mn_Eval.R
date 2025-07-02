
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

max(CAD_result$latest_record)
max(predictions$date)

# trim off predictions that extend beyond the time frame for which we have actual results
predictions <- predictions %>% filter(date <= max(CAD_result$latest_record))

# join actual results to predictions
merge_result <- left_join(predictions, CAD_result$df, by = c('osm_id','Month', 'day', 'Hour')) %>%
  fill_na() %>%
  mutate(CAD_CRASH = ifelse(CAD_CRASH>0, 1, 0))

# Generate visual of precision-recall curve and its area under the curve (AUC) 

# Prepare data
true_labels <- merge_result$CAD_CRASH
probs <- merge_result$Prob_Crash
pr <- pr.curve(scores.class0 = probs[true_labels == 1], 
               scores.class1 = probs[true_labels == 0], curve = TRUE)
pr_auc <- pr$auc.integral
random_baseline <- mean(true_labels)
multiplier <- if (random_baseline > 0) pr_auc / random_baseline else NA

pr_curve_df <- as.data.frame(pr$curve)
colnames(pr_curve_df) <- c("Recall", "Precision", "Threshold")

# Set up annotation text
annotation_text <- if (!is.na(multiplier)) {
  sprintf("PRAUC = %.4f<br>PRAUC is %.2f times random baseline<br>Target Variable: %s", pr_auc, multiplier, response.var)
} else {
  sprintf("PRAUC = %.4f<br>Random baseline is zero<br>Target Variable: %s", pr_auc, response.var)
}

# Create the PR curve plot
fig <- plot_ly() %>%
  # Main curve: all hover options fine
  add_trace(
    data = pr_curve_df,
    x = ~Recall,
    y = ~Precision,
    type = 'scatter',
    mode = 'lines+markers',
    name = "Model",
    text = ~paste("Recall:", round(Recall, 3), "<br>Precision:", round(Precision, 3)),
    hoverinfo = 'text'
  ) %>%
  # Baseline: NO text argument, no hover!
  add_trace(
    x = c(0, 1),
    y = rep(random_baseline, 2),
    type = 'scatter',
    mode = 'lines',
    line = list(dash = 'dash', color = 'red'),
    name = 'Random baseline',
    hoverinfo = 'none' # *crucial!*
  ) %>%
  layout(
    title = paste0("Precision-Recall Curve (Model ", modelno, ")"),
    xaxis = list(title = "Recall"),
    yaxis = list(title = "Precision"),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0,  # far left of plot area
        y = 1.05,  # just above the plot area
        text = annotation_text,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 14, color = "blue", family = "Arial"),
        align = "left"
      )
    )
  )

# Save as interactive HTML
htmlwidgets::saveWidget(
  fig,
  file = file.path(pilot_results_dir, paste0("PR_curve_", date(min(predictions$date)), "_to_", date(max(predictions$date)), ".html"))
)