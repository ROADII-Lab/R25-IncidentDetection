
inputdir <- file.path(getwd(),"Input")
intermediatedir <- file.path(getwd(),"Intermediate")
outputdir<- file.path(getwd(),"Output")
predict_week_out_dir <- file.path(outputdir, "Predict_Week_Outputs")

# historical crash data

crash_filepath <- file.path(file.path(inputdir,"Crash", state)) # define the location of the crash variables

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# for testing purposes
if(state == "WI"){
  date_time_col <- NA
  date_col <- "CRSHDATE"
  time_col <- "CRSHTIME" 
  time_format <- "mdy_HM" # must be in a format that lubridate can recognize within the time
}

if(state == "WA"){ # MN will require some preprocessing to work with this new format
  date_time_col <- NA
  date_col <- "ACC_DATE"
  time_col <- "TIME" # date/time definition, must be in the same order as time format, will concatenate automatically
  time_format <- "ymd_HM" # must be in a format that lubridate can recognize within the time  
}

if(state == "MN"){
  date_time_col <- "time_local"
  date_col <- NA
  time_col <- NA # date/time definition, must be in the same order as time format, will concatenate automatically
  time_format <- "ymd_HMS" # must be in a format that lubridate can recognize within the time  
}

lat_col <- NA # if uploading xlsx, or csv's define the latitude variable name (if left NA, it will choose the best fit)
lon_col <- NA # if uploading xlsx, or csv's define the longitude variable name (if left NA, it will choose the best fit)

# Make outputdir and intermediatedir if not already there
if(!dir.exists(intermediatedir)) { dir.create(intermediatedir) }
if(!dir.exists(outputdir)) { dir.create(outputdir) }
if(!dir.exists(predict_week_out_dir)) { dir.create(predict_week_out_dir) }

source(file.path("utility", "get_packages.R")) # installs necessary package

# source("utility/wazefunctions.R") 

# Run timezone script 
source(file.path("utility", "timezone_adj.R"))

# read random forest function
source(file.path("analysis", "RandomForest_Fx.R"))

# The full model identifier gets created in this next step
modelno = paste(state,
                year, 
                ifelse(imputed_waze, "imputed", "NOTimputed"),
                ifelse(time_bins, "tbins", "hourly"), 
                num, 
                sep = "_")


# Load a fitted model from local machine -- run RandomForest_WazeGrids_TN.R to generate models
# Loads rf.out, the model object, which we will feed new data to predict on.
# New data will need the same structure as the data used in the model fitting.
# This script is based on model 05, which performed the best of the models we tested.

load(file.path(outputdir, 'Random_Forest_Output', paste("Model", modelno, "RandomForest_Output.RData", sep= "_")))

# Create week ----
# Create osm_id by time variables for the next week, to join everything else into 
today <- Sys.Date()

day_seq <- seq(today, today+4, by = 1)

if(time_bins){
  hour_seq <- c(0, 6, 12, 18)
} else {
  hour_seq <- 0:23
}

day_x_hour <- expand.grid(Day = day_seq, Hour = hour_seq)

day_hour_seq <- as.POSIXct(paste(day_x_hour$Day, day_x_hour$Hour), 
                           format = '%Y-%m-%d %H', tz = time_zone_name)

months <- unique(as.integer(format(day_hour_seq, "%m")))

day_hour_seq <- format(day_hour_seq, '%Y-%j %H')

#Load imputed waze data for creating OSM network

imputed_waze <- data.frame()

for (m in months){
  load(file.path(intermediatedir,'Month_Frames',paste(state, year, ifelse(time_bins, "tbins", ""), "month_frame_imputed_waze", m,".RData", sep = "_")))
  imputed_waze <- rbind(imputed_waze, waze_averages)
  rm(waze_averages)
  gc()
}

link_seq <- sort(unique(imputed_waze$osm_id))

link_x_day <- expand.grid(osm_id = link_seq,
                          date = day_hour_seq)

link_x_day <- link_x_day %>%
  mutate(date = as.POSIXct(date, format = '%Y-%j %H', tz = time_zone_name),
         Year = lubridate::year(date),
         month = as.numeric(lubridate::month(date)),
         day = as.numeric(lubridate::day(date)),
         hour = as.numeric(lubridate::hour(date)),
         DayOfWeek = format(date, '%u'), # Monday = 1,
         weekday = ifelse(DayOfWeek == 6 | DayOfWeek == 7, TRUE, FALSE))         
                  

# Add in alert averages

link_x_day <- left_join(link_x_day, imputed_waze, by = c("osm_id", "month", "hour", "weekday"))
rm(imputed_waze)

# Load Road Network

source(file.path("utility", "OpenStreetMap_pull.R"))

# If applicable, load CAD data for MN so as to join it in
if((year %in% c(2018,2019,2020)) & (state == "MN")){
  source(file.path("utility", "MN_CAD_load.R"))
  link_x_day <- left_join(link_x_day, CAD, by = c("osm_id", "month", "day", "hour"))
}

# Get weather for next week ----

source(file.path("utility", "Prep_ForecastWeather.R"))

link_x_day <- left_join(link_x_day, weather_forecast, by=c("osm_id", "date"))
rm(weather_forecast)

# hist_crashes

source(file.path("utility", "load_crashes.R"))

source(file.path("utility", "prep_hist_crash2.R"))

if(state == "MN"){
  source(file.path("utility", "MN_CAD_load_historical.R"))
  link_x_day <- left_join(link_x_day, CAD_hist, by = "osm_id") %>% fill_na()
  rm(CAD_hist)
  gc()
}

# Next Week ---------------------------------------------------------------

next_week <- link_x_day %>%
  rename(Month = month, 
         Hour = hour,
         Average_jams = average_jams,
         Average_weather = average_weather,
         Average_closure = average_closure,
         Average_accident = average_accident) %>%
  left_join(state_network %>% st_drop_geometry(), by = "osm_id") %>%
  left_join(hist_crashes, by = "osm_id") %>%
  mutate(Month = factor(Month, levels = rf.out$forest$xlevels$Month),
         Hour = factor(Hour, levels = rf.out$forest$xlevels$Hour),
         weekday = factor(weekday, levels = rf.out$forest$xlevels$weekday),
         SNOW = as.numeric(SNOW),
         highway = factor(highway, levels = rf.out$forest$xlevels$highway))

if("average_jam_level" %in% colnames(next_week)){
  next_week <- next_week %>% 
    mutate(Average_jam_level = average_jam_level)
}

new_order = sort(colnames(next_week))
next_week <- next_week[, new_order]

next_week <- fill_na(next_week) # function defined in RandomForest_Fx.R

predictors <- names(rf.out$forest$xlevels)  # predictors used in training, aka "fitvars"
next_week_clean <- next_week[, predictors, drop = FALSE]

# Make predictions ----

fitvars <- read.csv(file.path(outputdir, "Random_Forest_Output", paste0('Fitvars_', modelno, '.csv')))

# see Precision-recall tradeoff plots from re-fit local
cutoff = 0.05

predict_next_week <- predict(rf.out, next_week_clean, type = "response",
                             cutoff = c(1-cutoff, cutoff)) 

prob_next_week <- predict(rf.out, next_week_clean, type = "prob",
                          cutoff = c(1-cutoff, cutoff)) 

colnames(prob_next_week) = c('Prob_NoCrash', 'Prob_Crash')

next_week_out <- data.frame(next_week, Crash_pred = predict_next_week, prob_next_week)


next_week_out <- next_week_out %>%
  group_by(Hour, DayOfWeek) %>%
  mutate(Hourly_CrashRisk = (Prob_Crash-min(Prob_Crash))/(max(Prob_Crash)-min(Prob_Crash))) %>%
  ungroup()


write.csv(next_week_out, file = file.path(predict_week_out_dir, paste0(modelno,'_', Sys.Date(), '.csv')), row.names = F)

## Save some plots of the results in the Figures folder ##
save_charts <- function(results_df, # the dataframe object with the results
                        name_of_results # some name that will help distinguish from other results - will be used in filename for outputs
){
  
  unfaceted_plot = ggplot(data=results_df, mapping=aes(x=Hour, y=Prob_Crash, group = Hour)) + 
    geom_boxplot(fill = "green") + 
    labs(title = "Boxplot of Crash Risk by Hour",
         y = "Crash Risk",
         x = "Hour")
  
  ggsave(plot = unfaceted_plot, 
         filename = paste0("unfaceted_boxplot", name_of_results, ".png"),
         path = file.path(outputdir, "Figures"),
         device = "png",
         create.dir = T,
         height = 6, width = 5, units = "in")
  
  faceted_plot = ggplot(data=results_df, mapping=aes(x=Hour, y=Prob_Crash, group = Hour)) + 
    geom_boxplot(fill = "green") + 
    facet_wrap(~DayOfWeek) + 
    labs(title = "Boxplot of Crash Risk by Hour (Faceted by Day)",
         y = "Crash Risk",
         x = "Hour")
  
  ggsave(plot = faceted_plot, 
         filename = paste0("faceted_boxplot", name_of_results, ".png"),
         path = file.path(outputdir, "Figures"),
         device = "png",
         create.dir = T,
         height = 6, width = 5, units = "in")
  
  by_hour = results_df %>%
    group_by(Hour) %>%
    summarize(Median_Prob_Crash = median(Prob_Crash),
              Mean_Prob_Crash = mean(Prob_Crash))
  
  mean_by_hour = ggplot(data=by_hour, mapping=aes(x=Hour, y=Mean_Prob_Crash)) + 
    geom_point() + 
    labs(title = "Mean Crash Risk by Hour",
         y = "Mean Crash Risk",
         x = "Hour")
  
  ggsave(plot = mean_by_hour, 
         filename = paste0("mean_by_hour", name_of_results, ".png"),
         path = file.path(outputdir, "Figures"),
         device = "png",
         create.dir = T,
         height = 6, width = 5, units = "in")
  
  by_road_type = results_df %>%
    group_by(highway) %>%
    summarize(Median_Prob_Crash = median(Prob_Crash),
              Mean_Prob_Crash = mean(Prob_Crash))
  
  mean_by_roadtype = ggplot(data=by_road_type, mapping=aes(x=highway, y=Mean_Prob_Crash)) + 
    geom_point() + 
    labs(title = "Mean Crash Risk by Road Type",
         y = "Mean Crash Risk",
         x = "Hour")
  
  ggsave(plot = mean_by_roadtype, 
         filename = paste0("mean_by_roadtype", name_of_results, ".png"),
         path = file.path(outputdir, "Figures"),
         device = "png",
         create.dir = T,
         height = 6, width = 5, units = "in")
  
}

save_charts(results_df = next_week_out, 
            name_of_results = paste0(modelno,'_', Sys.Date())
)

# Create and display the dashboard
source(file.path("utility", "pivotdash.R"))

source(file.path("utility", "createdash.R"))