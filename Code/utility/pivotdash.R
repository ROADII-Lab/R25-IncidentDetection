# This script is called by PredictWeek.R
# next_week_out is a data frame that is already in memory because PredictWeek.R created it

# Select relevant columns from the next_week_out data frame
next_week_out <- next_week_out %>% select(osm_id, date, Hourly_CrashRisk)

# Get distinct dates
DateInfo <- next_week_out %>%
  select(date) %>%
  distinct() %>%
  arrange(date) %>%
  mutate(Date = format(DateInfo$Date, format = "%Y-%m-%d %H:%M")) %>%
  select(-date)

# Reshape crash predictions
CrashPredictions <- next_week_out %>%
  select(osm_id, date, Hourly_CrashRisk) %>%
  group_by(osm_id, date) %>%
  summarise(Hourly_CrashRisk = mean(Hourly_CrashRisk, na.rm = TRUE)) %>%
  pivot_wider(names_from = date, values_from = Hourly_CrashRisk)

add_hour_if_missing <- function(date_string) {
  if (grepl(" ", date_string)) {
    return(date_string)
  } else {
    return(paste0(date_string, " 00:00")) 
  }
}

# Apply the function to the column names of CrashPredictions
new_colnames <- sapply(colnames(CrashPredictions)[-1], add_hour_if_missing)
colnames(CrashPredictions)[-1] <- new_colnames

# Pull date info from columns
date_columns <- as.POSIXct(names(CrashPredictions)[-1], format="%Y-%m-%d %H:%M")
min_date <- min(date_columns)

# Rename columns based on the difference in hours from the minimum date
new_col_names <- c("osm_id")
for (col in date_columns) {
  time_difference <- as.numeric(difftime(col, min_date, units = "hours"))
  new_col_names <- c(new_col_names, time_difference)
}

# Assign the new column names back to CrashPredictions
names(CrashPredictions) <- new_col_names

# the csv/tableau expect columns osm_id, 0-111
expected_columns <- c("osm_id", 0:111)

# Ensure missing columns are filled with 0s
for (col in expected_columns) {
  if (!col %in% names(CrashPredictions)) {
    CrashPredictions[[as.character(col)]] <- 0 # Create new column and fill with 0s
  }
}

# Reorder columns in numerical order (after osm_id)
CrashPredictions <- CrashPredictions[, c("osm_id", sort(as.numeric(names(CrashPredictions)[-1])))]
