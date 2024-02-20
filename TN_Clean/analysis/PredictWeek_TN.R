# Using best random forest models of crash estimation for TN, predict the number of crashes for the next week by grid ID and time.
# Inputs:
# - Time variables for today to next 10 days, for each Grid ID
# - Special events values prepped by Grid ID 
# - Weather forecast for by Grid ID
# - Waze input variables for model 05 (all TN crashes, Base Waze inputs): 
# - Join with all other predictors which are not time-varying, namely urban area, TotalHistCrash, TotalFatalCrash.

# Goal should be to source one script of each type fo input to prep, then join them all into a `next_week` data table. Then run predict(rf_05, next_week) to generate predictions for each grid cell, each hour of next week.

# Setup ---- 
rm(list=ls()) # Start fresh

inputdir <- file.path(getwd(),"Input")
outputdir<- file.path(getwd(),"Output")

source('utility/get_packages.R') # installs necessary packages

library(randomForest)
library(foreach) # for parallel implementation
library(doParallel) # includes iterators and parallel
library(tidyverse)
library(sf)

grids = c("TN_01dd_fishnet",
          "TN_1sqmile_hexagons")

source("utility/wazefunctions_TN.R") 

# read random forest function
source("analysis/RandomForest_WazeGrid_Fx.R")

# <><><><><>
g = grids[1] # start with square grids, now running hex also. Change between 1 and 2.
state = "TN"
# <><><><><>

# Load a fitted model from local machine -- run RandomForest_WazeGrids_TN.R to generate models
# Loads rf.out, the model object, which we will feed new data to predict on.
# New data will need the same structure as the data used in the model fitting.
# This script is based on model 05, which performed the best of the models we tested.

load(file.path(outputdir, 'Random_Forest_Output', paste0('TN_Model_05_', g, '_RandomForest_Output.RData')))

# Load data used for fitting - prepared also in RandomForest_wazeGrids_TN.R
Waze_Prepared_Data = dir(outputdir)[grep(paste0('^', state, '_\\d{4}-\\d{2}_to_\\d{4}-\\d{2}_', g, '.csv'), dir(outputdir))]

w.allmonths = read.csv(file.path(outputdir, Waze_Prepared_Data))

w.allmonths$MatchTN_buffer <- as.factor(w.allmonths$MatchTN_buffer)
w.allmonths$MatchTN_buffer_Acc <- as.factor(w.allmonths$MatchTN_buffer_Acc)
w.allmonths$TN_crash <- as.factor(w.allmonths$TN_crash)
w.allmonths$date <- as.Date(w.allmonths$date)
w.allmonths$weekday <- as.factor(w.allmonths$weekday)
w.allmonths$DayOfWeek <- as.factor(w.allmonths$DayOfWeek)
w.allmonths$GRID_ID <- as.character(w.allmonths$GRID_ID)

# Create week ----
# Create GRID_ID by time variables for the next week, to join everything else into 
today <- Sys.Date()


day_seq <- seq(today, today+7, by = 1)
hour_seq <- 0:23

day_x_hour <- expand.grid(Day = day_seq, Hour = hour_seq)

day_hour_seq <- as.POSIXct(paste(day_x_hour$Day, day_x_hour$Hour), 
                           format = '%Y-%m-%d %H', tz = 'America/Chicago')

day_hour_seq <- format(day_hour_seq, '%Y-%j %H')

grid_seq <- sort(unique(w.allmonths$GRID_ID))

grid_x_day <- expand.grid(GRID_ID = grid_seq,
                          hextime = day_hour_seq)

grid_x_day <- grid_x_day %>%
  mutate(hextime = as.POSIXct(hextime, format = '%Y-%j %H'),
         Year = format(hextime, '%Y'),
         mo = format(hextime, '%m'),
         day = format(hextime, '%j'),
         hour = as.numeric(format(hextime, '%H')),
         DayOfWeek = format(hextime, '%u'), # Monday = 1
         date = format(hextime, '%Y-%m-%d'))

na.action = "fill0" # This is an argument in append.hex, below. Other options are 'omit' or 'keep'.

# Get special events for next week ----

# Start with last week of 2018; need to get 2019. This is created by Prep_SpecialEvents.R
load(file.path(inputdir, 'SpecialEvents', paste0('Prepared_TN_SpecialEvent_', g, '.RData')))

# Join with append.hex
next_week <- append.hex(hexname = 'grid_x_day',
                        data.to.add = "TN_SpecialEvent", state = state, na.action = na.action)

# Get weather for next week ----

#source('utility/Prep_ForecastWeather.R')

# Placeholder wx.grd.day file
next_week <- next_week %>%
  mutate(PRCP = 0,
         TMIN = 60,
         TMAX = 85,
         SNOW = 0) %>%
  mutate(day = as.Date(hextime))


#wx.grd.day$day <- as.Date(wx.grd.day$day)

# next_week <- append.hex(hexname = 'next_week',
#                        data.to.add = "wx.grd.day", state = state, na.action = na.action)

# Generate Waze events for next week ----

source('utility/Prep_ExpectedWaze.R')

w.expected$mo <- as.character(w.expected$mo)
w.expected$DayOfWeek <- as.character(w.expected$DayOfWeek)

next_week <- left_join(next_week, w.expected,
                        by = c('GRID_ID', 'mo', 'DayOfWeek', 'hour'))
  
  
# Add in static variables: Historical crashes, historical FARS, and urban/rural. Grab these from w.allmonths by GRID_ID

w.staticvars <- w.allmonths %>% 
  filter(!duplicated(GRID_ID)) %>%
  dplyr::select(GRID_ID, 
                TN_UA_C, TN_UA_U,
                TotalHistCrashsum, TotalFatalCrashsum)

next_week <- left_join(next_week, w.staticvars,
                        by = 'GRID_ID')


# Make predictions ----

# Change urban cluster names 
next_week <- next_week %>%
  rename(UA_Cluster = TN_UA_C) %>%
  rename(UA_Urban = TN_UA_U) 

next_week <- next_week %>%
  mutate(UA_Rural = UA_Urban == 0 & UA_Cluster == 0)

# Use rf.out from Model 5 for this grid size
# Make sure we have factor variables, with same levels as in the fitted data.
# Model 05
fitvars <- read.csv(file.path(outputdir,'Fitvars_05_TN_01dd_fishnet.csv'))

# Fill NA with 0
next_week[,sapply(next_week,notDate)][is.na(next_week[,sapply(next_week,notDate)])] = 0

next_week$DayOfWeek <- as.factor(next_week$DayOfWeek)
levels(next_week$DayOfWeek) = c(levels(next_week$DayOfWeek), '0')
next_week_pred <- next_week[,names(next_week) %in% fitvars$fitvars]


# see Precision-recall tradeoff plots from re-fit local
cutoff = 0.05

predict_next_week <- predict(rf.out, next_week_pred, type = "response",
                             cutoff = c(1-cutoff, cutoff)) 

prob_next_week <- predict(rf.out, next_week_pred, type = "prob",
                             cutoff = c(1-cutoff, cutoff)) 

colnames(prob_next_week) = c('Prob_NoCrash', 'Prob_Crash')

next_week_out <- data.frame(next_week, Crash_pred = predict_next_week, prob_next_week)

write.csv(next_week_out, file = file.path(outputdir,paste0('TN_Model_05_Predictions', g, Sys.Date(), '.csv')), row.names = F)

# Visualize predictions -----
# use the following objects to make visualizations
# next_week_out

next_week_out <- read.csv(file.path(outputdir,paste0('TN_Model_05_Predictions', g, Sys.Date(), '.csv')))
VIZ = T
if(VIZ){
  source('analysis/Visualize_Next_Week.R')
}
