# Generate expected Waze events by grid cell by: month of year, day of week, and hour of day
# We will use the last 12 months from today of Waze data

# Start from PredictWeek_TN.R, where w.allmonths is already loaded

w.allmonths$date <- as.numeric(w.allmonths$date)
waze_date_range <- paste(range(w.allmonths$date), collapse = "_to_")

prepname =  paste0("TN_Waze_Expected_", g, "_", waze_date_range, ".RData")

if(!file.exists(file.path(inputdir, 'Waze', prepname))){

w.expected <- w.allmonths %>%
  group_by(GRID_ID, mo, DayOfWeek, hour) %>%
  summarize(nWazeAccident = median(nWazeAccident, na.rm = T),
            nWazeWeatherOrHazard = median(nWazeWeatherOrHazard, na.rm = T),
            nWazeJam = median(nWazeJam, na.rm = T),
            nWazeRoadClosed = median(nWazeRoadClosed, na.rm = T)
            )

save(list = c("w.expected"), 
     file = file.path(inputdir,"Waze", prepname))

} else {
  load(file.path(inputdir,"Waze", prepname))
}

