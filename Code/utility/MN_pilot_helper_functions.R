
##### function to load predictions
load_and_combine_csv <- function(runs) {
  # Initialize an empty list to store data frames
  df_list <- vector("list", length(runs))
  
  for (i in seq_along(runs)) {
    filename = file.path(predict_week_out_dir, paste0(modelno,'_', runs[i], '.csv'))
    df = read_csv(filename, col_types = cols(osm_id = col_character(),
                                             date = col_datetime(format = ""))) %>%
      mutate(date = lubridate::force_tz(date, tzone = "America/Chicago"))
    
    if(i == length(runs)){
      df_list[[i]] = df
    } else {
      df = df %>% dplyr::filter(date<runs[i+1])
      df_list[[i]] = df
    } # end of if - else
    
    combined_df = bind_rows(df_list)
  } # end of for loop
  return(combined_df)
} # end of load_and_combine_csv function

##### function to load CAD pilot results
lookup <- c(CAD_CRASH = "CRASH")

load_CAD_pilot_results <- function(fp) {
  
  df <- read.csv(fp)
  
  colnames(df) <- tolower(colnames(df))
  
  df <- df %>% 
    filter(lon != "None" & lat != "None") %>%
    mutate(lat=as.numeric(lat),
           lon=as.numeric(lon),
           centraltime=lubridate::ymd_hms(created, tz = "America/Chicago"),
           close=lubridate::mdy_hm(tmc.closed, tz = "America/Chicago"),
           time_open = difftime(close, centraltime, units = "mins")) %>%
    # some of the lat and lon values had the string "None," so above function resulted in NAs by coercion.
    # filter them out now
    filter(!is.na(lon)&!is.na(lat)) %>%
    # -- Note: could not filter based on p_disposition or t_disposition because columns not present
    # filter out also the rows where the time_open is only a few minutes. According to MnDOT,  
    # those are likely erroneous because they were closed so soon after being opened.
    filter( (time_open > 5) & ( !is.na(time_open) ) ) %>% 
    # select columns of interest
    dplyr::select(eventtype, lat, lon, centraltime) %>%
    dplyr::rename(class = eventtype) %>%
    # convert to sf object
    st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
    st_transform(projection) %>%
    # associated each record with the road segment it is nearest to
    st_join(state_network %>% select(osm_id), join = st_nearest_feature) %>%
    st_drop_geometry() %>%
    dplyr::select(osm_id, centraltime, class)
  
  df <- df %>% 
    mutate(n = 1) %>%
    group_by(osm_id, centraltime, class) %>% 
    summarize(n = sum(n), .groups = "drop") %>%
    pivot_wider(names_from = class, values_from = n)
  
  df <- df %>% 
    as_tsibble(index = centraltime, key = osm_id) %>%
    arrange(osm_id, centraltime) %>%
    group_by(osm_id) %>%
    # time_interval is defined in RandomForest_Train.R script. 
    # if time bins are not being used then it just aggregates by hour.
    index_by(interval = floor_date(x = centraltime, unit = ifelse(time_bins, time_interval, "hours"))) %>%
    summarise(CRASH = sum(CRASH),
              .groups = "drop") %>%
    rename(centraltime = interval) %>%
    replace_na(list(CRASH = 0)) %>%
    rename(all_of(lookup)) %>%
    as.data.frame() %>% 
    mutate(Month = lubridate::month(centraltime),
           day = lubridate::day(centraltime),
           Hour = lubridate::hour(centraltime)) 
  
  latest_record <- max(df$centraltime)
  
  df <- df %>% select(!centraltime)  
  
  return(list(df = df, latest_record = latest_record))
  
}

# function to load and combine predictions and CAD data into one merged result
load_combine <- function(runs, CAD_fn){
  
  # load predictions
  predictions <- load_and_combine_csv(runs)
  
  # load actual CAD_CRASH data
  CAD_result <- load_CAD_pilot_results(file.path(inputdir,"Crash",CAD_fn))
  
  # trim off predictions that extend beyond the time frame for which we have actual results
  predictions <- predictions %>% filter(date <= max(CAD_result$latest_record))
  
  # join actual results to predictions
  merge_result <- left_join(predictions, CAD_result$df, by = c('osm_id','Month', 'day', 'Hour')) %>%
    fill_na() %>%
    mutate(CAD_CRASH = ifelse(CAD_CRASH>0, 1, 0))
  
  return(merge_result)
  
}

##### function to calculate precision-recall curve and return figure and metrics

# Generate visual of precision-recall curve and its area under the curve (AUC) 

calculate_PR <- function(dataframe){
  # Prepare data
  true_labels <- dataframe$CAD_CRASH
  probs <- dataframe$Prob_Crash
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
  return(list(fig = fig, pr_auc = pr_auc, multiplier = multiplier, random_baseline = random_baseline))
}

############## Functions to calculate PR AUC, Accuracy, and F1-score, #########
############## disaggregated by category                              #########

# Calculate PR AUC given data frame with CAD_CRASH and Prob_Crash
calculate_pr_auc <- function(df) {
  true_labels <- df$CAD_CRASH
  probs <- df$Prob_Crash
  if(length(unique(true_labels)) < 2) {
    return(NA_real_)
  }
  pr <- pr.curve(scores.class0 = probs[true_labels == 1],
                 scores.class1 = probs[true_labels == 0], curve = FALSE)
  return(pr$auc.integral)
}

# Helper to find threshold maximizing accuracy, returning acc, f1, prec, rec
find_best_threshold <- function(true_labels, probs) {
  thresholds <- seq(0, 1, by = 0.01)
  
  # For each threshold, compute metrics
  metrics <- sapply(thresholds, function(thresh) {
    preds <- ifelse(probs >= thresh, 1, 0)
    TP <- sum(preds == 1 & true_labels == 1)
    FP <- sum(preds == 1 & true_labels == 0)
    FN <- sum(preds == 0 & true_labels == 1)
    TN <- sum(preds == 0 & true_labels == 0)
    
    acc  <- (TP + TN) / (TP + TN + FP + FN)
    prec <- if (TP + FP > 0) TP / (TP + FP) else 0
    rec  <- if (TP + FN > 0) TP / (TP + FN) else 0
    f1   <- if (prec + rec > 0) 2 * (prec * rec) / (prec + rec) else 0
    
    c(acc = acc, f1 = f1, precision = prec, recall = rec)
  })
  
  best <- which.max(metrics["acc", ])
  list(
    threshold = thresholds[best],
    accuracy  = metrics["acc", best],
    f1        = metrics["f1", best],
    precision = metrics["precision", best],
    recall    = metrics["recall", best]
  )
}

# Main function: PR AUC + baseline + threshold metrics by group
performance_by_group <- function(data, group_var,
                                 use_dynamic_threshold = TRUE,
                                 fixed_threshold = 0.5) {
  data %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      n              = n(),
      pos_fraction   = mean(CAD_CRASH),
      pr_auc         = {
        df_sub <- pick(CAD_CRASH, Prob_Crash)
        calculate_pr_auc(df_sub)
      },
      random_baseline = mean(CAD_CRASH),
      multiplier     = ifelse(random_baseline > 0, pr_auc / random_baseline, NA_real_),
      
      # Compute a small list-column of performance metrics:
      perf_list = list(
        if (use_dynamic_threshold) {
          find_best_threshold(CAD_CRASH, Prob_Crash)
        } else {
          # fixed threshold branch
          preds <- ifelse(Prob_Crash >= fixed_threshold, 1, 0)
          TP <- sum(preds == 1 & CAD_CRASH == 1)
          FP <- sum(preds == 1 & CAD_CRASH == 0)
          FN <- sum(preds == 0 & CAD_CRASH == 1)
          TN <- sum(preds == 0 & CAD_CRASH == 0)
          
          acc  <- (TP + TN) / (TP + TN + FP + FN)
          prec <- if (TP + FP > 0) TP / (TP + FP) else 0
          rec  <- if (TP + FN > 0) TP / (TP + FN) else 0
          f1   <- if (prec + rec > 0) 2 * (prec * rec) / (prec + rec) else 0
          
          list(
            threshold = fixed_threshold,
            accuracy  = acc,
            f1        = f1,
            precision = prec,
            recall    = rec
          )
        }
      ),
      .groups = "drop"
    ) %>%
    # Unnest the perf_list into separate columns
    mutate(
      threshold_used = map_dbl(perf_list, "threshold"),
      accuracy       = map_dbl(perf_list, "accuracy"),
      f1_score       = map_dbl(perf_list, "f1"),
      precision      = map_dbl(perf_list, "precision"),
      recall         = map_dbl(perf_list, "recall")
    ) %>%
    select(-perf_list)
}

## plotting
# PR AUC and Random Baseline grouped bar plot
plot_pr_auc_with_baseline <- function(df, group_var) {
  
  # Convert grouping variable to character (discrete) for plotting bars with equal widths
  df <- df %>%
    mutate(!!group_var := as.factor(.data[[group_var]]))
  
  plot_df <- df %>%
    pivot_longer(cols = c("pr_auc", "random_baseline"),
                 names_to = "Metric", values_to = "Value") %>%
    mutate(
      Metric = recode(Metric,
                      pr_auc = "PR AUC",
                      random_baseline = "Random Baseline")
    )
  
  # Extract levels to keep all labels
  x_labels <- levels(df[[group_var]])
  
  ggplot(plot_df, aes(x = .data[[group_var]], y = Value, fill = Metric)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = sprintf("%.4f", Value)),
              position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
    labs(title = paste("PR AUC and Random Baseline by", group_var),
         x = group_var,
         y = "Value",
         fill = NULL) +
    scale_fill_manual(values = c(
      "PR AUC" = "steelblue",
      "Random Baseline" = "gray50"
    )) +
    scale_x_discrete(breaks = x_labels) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Multiplier plot (PR AUC / baseline)
plot_multiplier <- function(df, group_var) {
  
  # Convert group_var to factor to ensure x-axis discrete labels
  df <- df %>%
    mutate(!!group_var := as.factor(.data[[group_var]]))
  
  # Extract levels to keep all labels
  x_labels <- levels(df[[group_var]])
  
  ggplot(df, aes(x = .data[[group_var]], y = multiplier)) +
    geom_col(fill = "forestgreen") +
    geom_text(aes(label = round(multiplier, 2)), vjust = -0.3, size = 3) +
    labs(title = paste("Model Improvement over Baseline by", group_var),
         x = group_var,
         y = "PR AUC / Baseline") +
    scale_x_discrete(breaks = x_labels) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Accuracy plot
plot_accuracy <- function(df, group_var) {
  
  # Convert group_var to factor to ensure x-axis discrete labels
  df <- df %>%
    mutate(!!group_var := as.factor(.data[[group_var]]))
  
  # Extract levels to keep all labels
  x_labels <- levels(df[[group_var]])
  
  ggplot(df, aes(x = .data[[group_var]], y = accuracy)) +
    geom_col(fill = "coral") +
    geom_text(aes(label = sprintf("%.4f", accuracy)), vjust = -0.3, size = 3) +
    labs(title = paste("Accuracy by", group_var),
         x = group_var,
         y = "Accuracy") +
    scale_x_discrete(breaks = x_labels) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# F1 Score plot
plot_f1 <- function(df, group_var) {
  
  # Convert group_var to factor to ensure x-axis discrete labels
  df <- df %>%
    mutate(!!group_var := as.factor(.data[[group_var]]))
  
  # Extract levels to keep all labels
  x_labels <- levels(df[[group_var]])
  
  ggplot(df, aes(x = .data[[group_var]], y = f1_score)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = sprintf("%.4f", f1_score)), vjust = -0.3, size = 3) +
    labs(title = paste("F1 Score by", group_var),
         x = group_var,
         y = "F1 Score") +
    scale_x_discrete(breaks = x_labels) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Function to generate and write all plots
generate_plots <- function(result_df, group_var, save_name){
  pr_auc_with_baseline = plot_pr_auc_with_baseline(result_df, group_var)
  multiplier = plot_multiplier(result_df, group_var)
  accuracy = plot_accuracy(result_df, group_var)
  F1 = plot_f1(result_df, group_var)
  
  ggsave(
    plot = pr_auc_with_baseline,
    filename = paste0(save_name, "_pr_auc_with_baseline_by_", group_var,".png"),
    path = pilot_results_dir,
    device = "png",
    create.dir = TRUE,
    height = 6,
    width = 8,
    units = "in"
  )
  
  ggsave(
    plot = multiplier,
    filename = paste0(save_name, "_multiplier_by", group_var, ".png"),
    path = pilot_results_dir,
    device = "png",
    create.dir = TRUE,
    height = 6,
    width = 8,
    units = "in"
  )
  
  ggsave(
    plot = accuracy,
    filename = paste0(save_name, "_accuracy_by", group_var, ".png"),
    path = pilot_results_dir,
    device = "png",
    create.dir = TRUE,
    height = 6,
    width = 8,
    units = "in"
  )
  
  ggsave(
    plot = F1,
    filename = paste0(save_name, "_F1_by", group_var, ".png"),
    path = pilot_results_dir,
    device = "png",
    create.dir = TRUE,
    height = 6,
    width = 8,
    units = "in"
  )
  
}

# Function to generate and save all plots in a grid
generate_plots <- function(result_df, group_var, save_name){
  pr_auc_with_baseline <- plot_pr_auc_with_baseline(result_df, group_var)
  multiplier <- plot_multiplier(result_df, group_var)
  accuracy <- plot_accuracy(result_df, group_var)
  F1 <- plot_f1(result_df, group_var)
  
  # Arrange plots in a 2x2 grid
  combined_plot <- (pr_auc_with_baseline | multiplier) / (accuracy | F1)
  # Alternatively: combined_plot <- pr_auc_with_baseline + multiplier + accuracy + F1 + plot_layout(ncol = 2)
  
  ggsave(
    plot = combined_plot,
    filename = paste0(save_name, "_all_plots_by_", group_var, ".png"),
    path = pilot_results_dir,
    device = "png",
    create.dir = TRUE,
    height = 10,  # adjust height and width as needed
    width = 12,
    units = "in"
  )
}

# function to run analysis for a given time period
run_analysis <- function(df, save_name){
  
  # Determine precision-recall curve for the entire set (not disaggregated)
  # returns fig, pr_auc, multiplier, random_baseline
  overall_PR = calculate_PR(dataframe = df)
  
  # Compute average predicted risk for actual crashes versus actual non-crashes
  risk_comparison <- compare_risk(df)
  
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
  
  write.csv(risk_comparison, file = file.path(pilot_results_dir, paste0(save_name, "risk_comparison.csv")))
  write.csv(results_hour_stat, file = file.path(pilot_results_dir, paste0(save_name, "results_hour_stat.csv")))
  write.csv(results_weekday_stat, file = file.path(pilot_results_dir, paste0(save_name, "results_weekday_stat.csv")))
  write.csv(results_roadtype_stat, file = file.path(pilot_results_dir, paste0(save_name, "results_roadtype_stat.csv")))
  write.csv(results_osm_id_stat, file = file.path(pilot_results_dir, paste0(save_name, "results_osm_id_stat.csv")))
  
  generate_plots(result_df = results_hour_stat, group_var = "Hour", save_name = save_name)
  generate_plots(result_df = results_weekday_stat, group_var = "weekday", save_name = save_name)
  generate_plots(result_df = results_roadtype_stat, group_var = "highway", save_name = save_name)
  
  return(list(overall_PR = overall_PR, hour = results_hour_stat, weekday = results_weekday_stat, highway = results_roadtype_stat, osm_id = results_osm_id_stat))
}

# Function to calculate the average predicted risk for actual crashes and the average predicted risk for non-crashes
compare_risk <- function(df){
  risk_comparison <- df %>%
    group_by(CAD_CRASH) %>%
    summarise(Average_Risk = mean(Prob_Crash, na.rm = T)) %>%
    ungroup()
}

library(dplyr)
library(ggplot2)

# Your compare_risk function
compare_risk <- function(df){
  df %>%
    group_by(CAD_CRASH) %>%
    summarise(Average_Risk = mean(Prob_Crash, na.rm = TRUE)) %>%
    ungroup()
}

plot_compare_risk_list <- function(df_list, output_file = "compare_risk_plot.png") {
  # Check if the list has names
  if (is.null(names(df_list))) {
    stop("Input list must have names for each data frame")
  }
  
  # Apply compare_risk to each data frame and keep the names
  results_list <- lapply(names(df_list), function(name) {
    compare_risk(df_list[[name]]) %>%
      mutate(DataFrame = name)
  })
  
  # Combine all results into one data frame
  combined_results <- bind_rows(results_list)
  
  # Replace CAD_CRASH values with descriptive labels
  combined_results <- combined_results %>%
    mutate(CAD_CRASH = factor(CAD_CRASH, levels = c(0, 1), labels = c("Non-Crash", "Crash")))
  
  # Plot
  p <- ggplot(combined_results, aes(x = DataFrame, y = Average_Risk, fill = CAD_CRASH)) +
    geom_col(position = "dodge") +
    labs(title = "Comparison of Average Predicted Risk by Crash Status",
         x = "Data Frame",
         y = "Mean Predicted Risk",
         fill = "Crash Status") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the plot
  ggsave(filename = output_file, plot = p, width = 8, height = 5)
  
  # Also print the plot
  print(p)
}





