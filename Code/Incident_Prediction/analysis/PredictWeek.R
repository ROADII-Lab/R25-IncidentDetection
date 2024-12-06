rm(list=ls()) # Start fresh

gc()

inputdir <- file.path(getwd(),"Input")
interdir <- file.path(getwd(),"Intermediate")
outputdir<- file.path(getwd(),"Output")

# Make outputdir and intermediatedir if not already there
if(!dir.exists(interdir)) { dir.create(interdir) }
if(!dir.exists(outputdir)) { dir.create(outputdir) }

source('utility/get_packages.R') # installs necessary packages

library(randomForest)
library(foreach) # for parallel implementation
library(doParallel) # includes iterators and parallel
library(tidyverse)
library(sf)
library(ggplot2)

# source("utility/wazefunctions.R") 

# read random forest function
source("analysis/RandomForest_Fx.R")

# <><><><><>
state <- "WA"
#state <- "MN"
train_year <- 2021
train_imputed <- TRUE
num <- "10"


# <><><><><>

# The full model identifier gets created in this next step
if(train_imputed == TRUE){
  model.no = paste(state, train_year, "imputed", num, sep = "_")
}else{
  model.no = paste(state, train_year, "NOTimputed", num, sep = "_")
}

# Load a fitted model from local machine -- run RandomForest_WazeGrids_TN.R to generate models
# Loads rf.out, the model object, which we will feed new data to predict on.
# New data will need the same structure as the data used in the model fitting.
# This script is based on model 05, which performed the best of the models we tested.

load(file.path(outputdir, 'Random_Forest_Output', paste("Model", model.no, "RandomForest_Output.RData", sep= "_")))


# Create week ----
# Create osm_id by time variables for the next week, to join everything else into 
today <- Sys.Date()

day_seq <- seq(today, today+4, by = 1)
hour_seq <- 0:23

day_x_hour <- expand.grid(Day = day_seq, Hour = hour_seq)

day_hour_seq <- as.POSIXct(paste(day_x_hour$Day, day_x_hour$Hour), 
                           format = '%Y-%m-%d %H', tz = 'America/Chicago')

months <- unique(as.integer(format(day_hour_seq, "%m")))

day_hour_seq <- format(day_hour_seq, '%Y-%j %H')


#Load imputed waze data for creating OSM network

imputed_waze <- data.frame()

for (m in months){
  load(file.path(interdir, "Month_Frames", paste0("WA_2021_month_frame_imputed_waze_",m,"_.RData")))
  load(file.path(interdir, "Month_Frames", paste(state, train_year, "month_frame_imputed_waze",m,".RData", sep = "_")))
  imputed_waze <- rbind(imputed_waze, waze_averages)
  rm(waze_averages)
  gc()
}


link_seq <- sort(unique(imputed_waze$osm_id))

link_x_day <- expand.grid(osm_id = link_seq,
                          date = day_hour_seq)

link_x_day <- link_x_day %>%
  mutate(date = as.POSIXct(date, format = '%Y-%j %H'),
         Year = format(date, '%Y'),
         month = as.integer(format(date, '%m')),
         hour = as.numeric(format(date, '%H')),
         DayOfWeek = format(date, '%u'), # Monday = 1,
         weekday = ifelse(DayOfWeek == 6 | DayOfWeek == 7, TRUE, FALSE))

#Added in alert averages

link_x_day <- left_join(link_x_day, imputed_waze, by = c("osm_id", "month", "hour", "weekday"))
rm(imputed_waze)

# Get weather for next week ----

source('utility/Prep_ForecastWeather.R')

link_x_day <- left_join(link_x_day, weather_forecast, by=c("osm_id", "date")) %>% 
  filter(!is.na(SNOW)) # filter for times we have weather forecasts for 
rm(weather_forecast)

next_week <- link_x_day %>%
  rename(Month = month, 
         Hour = hour,
         Average_jams = average_jams,
         Average_weather = average_weather,
         Average_closure = average_closure,

         Average_accident = average_accident,
         Average_jam_level = average_jam_level) %>%
  left_join(state_network %>% st_drop_geometry(), by = "osm_id") %>%
  mutate(Month = factor(Month, levels = rf.out$forest$xlevels$Month),
         Hour = factor(Hour, levels = rf.out$forest$xlevels$Hour),
         weekday = factor(weekday, levels = rf.out$forest$xlevels$weekday),
         SNOW = as.numeric(SNOW),
         highway = factor(highway, levels = rf.out$forest$xlevels$highway))

new_order = sort(colnames(next_week))
next_week <- next_week[, new_order]

next_week <- next_week %>% replace_na(list(maxspeed = median(state_network$maxspeed, na.rm = T)))

# Make predictions ----

# Use rf.out from Model 5 for this grid size
# Make sure we have factor variables, with same levels as in the fitted data.
# Model 05
fitvars <- read.csv(file.path(outputdir, paste0('Fitvars_', model.no, '.csv')))

# see Precision-recall tradeoff plots from re-fit local
cutoff = 0.05

predict_next_week <- predict(rf.out, next_week, type = "response",
                             cutoff = c(1-cutoff, cutoff)) 

prob_next_week <- predict(rf.out, next_week, type = "prob",
                          cutoff = c(1-cutoff, cutoff)) 

colnames(prob_next_week) = c('Prob_NoCrash', 'Prob_Crash')

next_week_out <- data.frame(next_week, Crash_pred = predict_next_week, prob_next_week)


next_week_out <- next_week_out %>%
  group_by(Hour, DayOfWeek) %>%
  mutate(Hourly_CrashRisk = (Prob_Crash-min(Prob_Crash))/(max(Prob_Crash)-min(Prob_Crash))) %>%
  ungroup()


write.csv(next_week_out, file = file.path(outputdir, paste0(model.no,'_', Sys.Date(), '.csv')), row.names = F)

## Save some plots of the results in the Figures folder ##
save_charts <- function(results_df, # the dataframe object with the results
                        name_of_results # some name that will help distinguish from other results - will be used in filename for outputs
){
  
  unfaceted_plot = ggplot(data=results_df, mapping=aes(x=Hour, y=Prob_Crash, group = Hour)) + 
    geom_boxplot(fill = "green") + 
    labs(title = "Boxplot of Crash Probability by Hour",
         y = "Crash Probability",
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
    labs(title = "Boxplot of Crash Probability by Hour (Faceted by Day)",
         y = "Crash Probability",
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
    labs(title = "Mean Crash Probability by Hour",
         y = "Mean Crash Probability",
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
    labs(title = "Mean Crash Probability by Road Type",
         y = "Mean Crash Probability",
         x = "Hour")
  
  ggsave(plot = mean_by_roadtype, 
         filename = paste0("mean_by_roadtype", name_of_results, ".png"),
         path = file.path(outputdir, "Figures"),
         device = "png",
         create.dir = T,
         height = 6, width = 5, units = "in")
  
}

save_charts(results_df = next_week_out, 
            name_of_results = paste0(model.no,'_', Sys.Date())
            )


# Visualize predictions -----
# use the following objects to make visualizations
# next_week_out

#next_week_out <- read.csv(file.path(outputdir,paste0('TN_Model_05_Predictions', g, Sys.Date(), '.csv')))
#VIZ = T
#if(VIZ){
#  source('analysis/Visualize_Next_Week.R')
#}