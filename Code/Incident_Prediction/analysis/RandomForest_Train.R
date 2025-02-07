# Random forest models of crash estimation for a given state. 

## Parameters to set before running#################################
num <- "temp_agg_1" # Use this to create a separate identifier to distinguish when multiple models are attempted for a given state and year.

state <- "WA"

# Indicate whether the state has a unified time zone
one_zone <-TRUE
# If one_zone is set to T or TRUE, meaning that the state has one time zone, specify the name of the time zone, selecting from 
# among the options provided after running the first line below (OlsonNames())

# OlsonNames()
#time_zone_name <- "US/Central"
time_zone_name <- "US/Pacific"

# Year
year <- 2021

# Indicate whether to aggregate into 6-hour bins (Midnight-6AM, 6AM-12PM, 12-6PM, 6PM-Midnight), 
# versus training and generating predictions based on individual hours. 
# If time_bins is set to True, the tool will aggregate the data
# in 6-hour bins and train on that.
time_bins <- F

### Optional parameters to set, or accept default.#############
# Projection 
projection <- 5070

##Use Imputed Waze?
imputed_waze <- T

test_percentage <- 0.03
##############################################################

# Setup ---- 
gc()

source('utility/get_packages.R') # installs necessary packages

library(randomForest)
library(foreach) # for parallel implementation
library(doParallel) # includes iterators and parallel
library(tidyverse)
library(dplyr)
library(ROSE)
library(performanceEstimation)
library(caret)
library(stringr)
#library(PRROC)
library(sf)


inputdir <- file.path(getwd(),"Input")
outputdir <-file.path(getwd(),"Output")
intermediatedir <- file.path(getwd(), "Intermediate")

# Make outputdir and intermediatedir if not already there
if(!dir.exists(intermediatedir)) { dir.create(intermediatedir) }
if(!dir.exists(outputdir)) { dir.create(outputdir) }

# Timezones --------------------------------------------------------------

US_timezones <- st_read(file.path("Shapefiles","Time_Zones","time_zones_ds_timezone_polygons.shp"))

tz_adj_to_names <- data.frame(tz_adj = c(-5,-6,-7,-8,-9,-10,-11), tz_name = c("US/Eastern","US/Central","US/Mountain","US/Pacific", "US/Alaska", "US/Hawaii", "US/Samoa"))

timezone_adj <- US_timezones %>% st_transform(crs=projection) %>% 
  mutate(adjustment = as.numeric(paste0(str_sub(utc, 1, 1), str_sub(utc, 2, 3)))) %>% 
  select(adjustment) %>% 
  left_join(tz_adj_to_names,by = join_by(adjustment==tz_adj))

rm(tz_adj_to_names)
# -------------------------------------------------------------------------

# The full model identifier gets created in this next step
if(imputed_waze == TRUE){
  modelno = paste(state, year, "imputed", ifelse(time_bins, "tbins",""), num, sep = "_")
}else{
  modelno = paste(state, year, "NOTimputed", ifelse(time_bins, "tbins",""), num, sep = "_")
}

train_fp <- file.path(intermediatedir,paste(state, year, "train_test_frames.RData", sep = "_"))

# check whether there is already a consolidated and prepped training frame
# at the expected file path. If not, prepare one. If so, notify the user and load the 
# existing file.
if(!file.exists(train_fp)){
prepstarttime <- Sys.time()
# Given that the consolidated training frame does not exist, initiate process
# to generate one. First generate a vector of all file paths for monthly data frames.
monthframe_fps <- file.path(intermediatedir, 'Month_Frames', paste(state, year, 1:12, 'month_frame_full.Rdata', sep = "_"))

# if there are 12 .Rdata monthly files at the expected paths then notify the user that we will re-use
# those files. Otherwise run the osm_query.R script to generate them from scratch.
if(!all(file.exists(monthframe_fps))){
  source('utility/osm_query.R') # creates training frames
} else {
  # Make sure the user is aware of the situation with respect to existing month frames
  
  load(monthframe_fps[1])
  
  if(length(unique(temp_train$Hour))>4 & time_bins){
    cat("Month frames already exist, but they were created with time_bins set to FALSE, meaning that the data \n")
    cat("are not aggregated. However, time_bins is currently set to TRUE in this script. \nIf you wish to cancel press 'c' to exit the script. You can then move or rename the month frames \nin the Intermediate folder or adjust the value for time_bins, and re-run. \nOtherwise, press 'p' to proceed, which will overwrite the existing month frames.\n")
    proceed <- readline("Press 'p' to proceed or 'c' to cancel>>")
    if(proceed == 'p'){
      source('utility/osm_query.R') # creates training frames
    } else if(proceed == 'c'){stop("User chose to exit script.")}
  } else if(length(unique(temp_train$Hour))<5 & !time_bins){
    cat("Month frames already exist, but they were created with time_bins set to TRUE, meaning that the data \n")
    cat("are aggregated. However, time_bins is currently set to FALSE in this script. \nIf you wish to cancel press 'c' to exit the script. You can then move or rename the month frames \nin the Intermediate folder or adjust the value for time_bins, and re-run. \nOtherwise, press 'p' to proceed, which will overwrite the existing month frames.\n")
    proceed <- readline("Press 'p' to proceed or 'c' to cancel>>")
    if(proceed == 'p'){
      source('utility/osm_query.R') # creates training frames
    } else if(proceed == 'c'){stop("User chose to exit script.")}
  } else {
    cat("Month frames already exist. If you wish to generate the data from scratch exit the script; delete, move, or rename the files; and re-run. This is an example file path for the month of January: ")
    cat(monthframe_fps[1], '\n\n')
  }
  
}

training_frame <- test_frame <-  data.frame(osm_id = character(),
                                            Month = numeric(),
                                            Day = numeric(),
                                            Hour = numeric(),
                                            day_of_week = factor(),
                                            weekday = logical(),
                                            ACCIDENT = numeric(),
                                            JAM = numeric(),
                                            ROAD_CLOSED = numeric(),
                                            WEATHERHAZARD = numeric(),
                                            precipitation = numeric(),
                                            temperature = numeric(),
                                            SNOW = numeric(),
                                            crash = factor())


m <- 1
for(m in 1:12){
#for(m in 1:12){
  starttime <- Sys.time()
  
  load(monthframe_fps[m])
  
  # if(time_bins){
  #   temp_train <- temp_train %>% 
  #     as_tbl_time(index = time_local) %>%
  #     collapse_by("6 hours", side = "start", clean = TRUE) %>%
  #     group_by(osm_id, Month, Day, Hour, day_of_week, weekday) %>%
  #     summarize(crash = sum(crash),
  #               WEATHERHAZARD = mean(WEATHERHAZARD, na.rm = T),
  #               JAM = mean(JAM, na.rm = T),
  #               ROAD_CLOSED = mean(ROAD_CLOSED, na.rm = T),
  #               ACCIDENT = mean(ACCIDENT, na.rm = T),
  #               jam_level = mean(jam_level, na.rm = T),
  #               precipitation = mean(precipitation, na.rm = T),
  #               temperature = mean(temperature, na.rm = T),
  #               SNOW = mean(SNOW, na.rm = T))
  # }
  
  temp_train <- temp_train %>% 
    mutate(crash = ifelse(crash >= 1, 1, 0),
           crash = factor(crash))
  
  ##### set aside validation set before down-sampling to address class imbalance. #####
  
  trainrows <- sort(sample(1:nrow(temp_train), size = nrow(temp_train)*(1-test_percentage), replace = F))
  
  testrows <- (1:nrow(temp_train))[!1:nrow(temp_train) %in% trainrows]
  
  temp_test <- temp_train[testrows,]
  
  temp_train <- temp_train[trainrows,]
  
  ############


  # temp_train <- downSample(x = temp_train %>% select(-crash),
  #                           y = temp_train$crash) %>%
  #               rename(crash = Class)

  
  # change from downSample function to a method that results in 100 to 1 non-crash
  # to crash ratio.
  # temp_train <-
  

  #training_frame <- training_frame %>% bind_rows(temp_train)
  
  #test_frame <- test_frame %>% bind_rows(temp_test)

  #rm(temp_train, temp_test)

  # training_frame <- training_frame %>% bind_rows(temp_train)
  # 
  # test_frame <- test_frame %>% bind_rows(temp_test)
  
  is_crash <- temp_train[,"crash"] == 1 
  crash_indices <- which(is_crash)
  non_crash_indices <- which (!is_crash) 

  crash_sample_size <- length(crash_indices) * 0.5
  crash_sample <- sample(crash_indices, size = crash_sample_size, replace = FALSE)

  non_crash_sample_size <- min(length(crash_sample), length(crash_indices))
  non_crash_sample <- sample(non_crash_indices, size = non_crash_sample_size, replace = FALSE)
  combined_data <- temp_train[c(crash_sample, non_crash_sample), ]

  training_frame <- training_frame %>% bind_rows(combined_data)

  test_frame <- test_frame %>% bind_rows(temp_test)
  
  rm(temp_train, temp_test, combined_data)

  
  gc()
  
  timediff <- Sys.time() - starttime
  cat(round(timediff, 2), attr(timediff, "units"), " to append training and test data for month ", m, ".\n")
  
}

# load road network in order to join in the historical crash data, road type ("highway"), and speed limit ("maxspeed")

cat("Preparing to join data on historical crashes ('hist_crashes'), road type ('highway'), and speed limit ('maxspeed').\n")
source("utility/Prep_OSMNetwork.R")

source("utility/prep_hist_crash.R")

prep_data <- function(training_frame){
  if(time_bins){
    training_frame <- training_frame %>%
      mutate(Hour = paste(as.character(Hour), as.character(Hour + 6), sep = "-"))
  }
  training_frame <- training_frame %>%
    replace_na(list(precipitation = 0, SNOW = 0)) %>%
    left_join(state_network %>% st_drop_geometry(), by = "osm_id") %>%
    left_join(hist_crashes, by = "osm_id") %>%
    mutate(Month = factor(Month, ordered = F),
           Day = factor(Day, ordered = F),
           Hour = factor(Hour, ordered = F),
           weekday = factor(weekday, ordered = F),
           osm_id = factor(osm_id, ordered = F),
           highway = factor(highway, ordered = F))

  return(training_frame)
}

training_frame <- prep_data((training_frame))
test_frame <- prep_data(test_frame)
timediff <- Sys.time() - prepstarttime
cat(round(timediff, 2), attr(timediff, "units"), "to prep data.")

save(list = c("training_frame", "test_frame"), file = file.path(intermediatedir,paste(state, year, "train_test_frames.RData", sep = "_")))

} else {
  cat("Training and test frame already exist at: ", train_fp, ". \nIf you wish to generate the data from scratch exit the script; delete, move, or rename the file; and re-run.")
  load(train_fp)
}

bin.mod.diagnostics <- function(predtab){
  
  accuracy = (predtab[1,1] + predtab[2,2] )/ sum(predtab) # true positives and true negatives divided by all observations
  precision = (predtab[1,1] )/ sum(predtab[1,]) # true positives divided by all predicted positives
  recall = (predtab[1,1] )/ sum(predtab[,1]) # true positives divided by all observed positives
  false.positive.rate = (predtab[1,2] )/ sum(predtab[,2]) # false positives divided by all observed negatives
  
  round(t(data.frame(accuracy, precision, recall, false.positive.rate)), 4)  
}

i <- 1

# read random forest function, do.rf()
source("analysis/RandomForest_Fx.R")

if(imputed_waze == TRUE){

imputed_values <- list.files(file.path(intermediatedir, "Month_Frames"), 
                             pattern = paste(state, year, "month_frame_imputed_waze", sep = "_"), 
                             full.names = TRUE)

imputed_waze_data <- list()

for(i in seq_along(imputed_values)){
  load(imputed_values[i])
  colnames(waze_averages)[2:ncol(waze_averages)] <- str_to_title(colnames(waze_averages)[2:ncol(waze_averages)])
  waze_averages$osm_id <- factor(waze_averages$osm_id)
  waze_averages$Month <- factor(waze_averages$Month)
  waze_averages$Weekday <- factor(waze_averages$Weekday)
  waze_averages$Hour <- factor(waze_averages$Hour)
  waze_averages <- waze_averages %>% rename(weekday = Weekday)
  imputed_waze_data[[i]] <- waze_averages
  gc()
}

imputed_waze_frame <- do.call(rbind, imputed_waze_data)

# the imputed waze data were calculated by grouping by osm_id, month, weekday, hour and then getting the mean.
# shouldn't the left_join operations below specify all those join fields?
training_frame <- left_join(training_frame, imputed_waze_frame)
test_frame <- left_join(test_frame, imputed_waze_frame)

rm(imputed_waze_frame, imputed_waze_data)



if((year %in% c(2018,2019,2020)) & (state == "MN")){
  source('utility/MN_CAD_load.R')
  training_frame <- left_join(training_frame, CAD)
  test_frame <- left_join(test_frame, CAD)
}

gc()
}

# sort column positions in alphabetical order
new_order = sort(colnames(training_frame))
training_frame <- training_frame[, new_order]
test_frame <- test_frame[, new_order]


# <><><><><><><><><><><><><><><><><><><><><><><><>
# End data prep 
# <><><><><><><><><><><><><><><><><><><><><><><><>

avail.cores = parallel::detectCores()

if(avail.cores > 8) avail.cores = 12 # Limit usage below max if on r4.4xlarge instance. Comment this out to run largest models.

# Use this to set number of decision trees to use, and key RF parameters. mtry is especially important, should consider tuning this with caret package
# For now use same parameters for all models for comparision; tune parameters after models are selected
rf.inputs = list(ntree.use = avail.cores * 50, avail.cores = avail.cores, mtry = NULL, maxnodes = 1000, nodesize = 100)

keyoutputs = redo_outputs = list() # to store model diagnostics

# Omit as predictors in this vector:
if(imputed_waze == TRUE){
  alwaysomit = c("crash", "Day", "osm_id", "ACCIDENT", "JAM", "ROAD_CLOSED", "WEATHERHAZARD", "jam_level", "day_of_week")
}else{
  alwaysomit = c("crash", "Day", "osm_id", "day_of_week", "average_jams", "average_weather", "average_closure", "average_accident", "average_jam_level")
}

response.var = "crash" # binary indicator of whether crash occurred, based on processing above. random forest function can also accept numeric target. 

starttime = Sys.time()

omits = c(alwaysomit)

# Check to see what we are passing as predictors
cat('Predictors to use in model', modelno, ': \n\n',
    paste(names(training_frame)[is.na(match(names(training_frame), omits))], collapse = '\n'))

fitvars = names(training_frame)[is.na(match(names(training_frame), omits))]
class_fit = n_lev_fit = levs_fit = vector()

for(f in fitvars){
  class_fit = c(class_fit, class(training_frame[,f]))
  n_lev_fit = c(n_lev_fit, ifelse(is.factor(training_frame[,f]),
                                  length(levels((training_frame[,f]))),
                                  NA))
  levs_fit = c(levs_fit, ifelse(is.factor(training_frame[,f]),
                                  paste(levels((training_frame[,f])), collapse = ", "),
                                  NA))
}


fitvar_df <- data.frame(fitvars, class_fit, n_lev_fit, levs_fit)
write.csv(fitvar_df, file = file.path(outputdir,paste0('Fitvars_', modelno, ".csv")))

# Run the Random Forest model using `do.rf()` function.
keyoutputs[[modelno]] = do.rf(train.dat = training_frame, 
                              test.dat = test_frame,
                              #thin.dat = 0.2,
                              omits, response.var = "crash", 
                              model.no = modelno, rf.inputs = rf.inputs,
                              cutoff = c(0.9, 0.1))  

save("keyoutputs", file = file.path(outputdir,paste0("Output_to_", modelno)))
#keyoutputs$"08"

timediff <- Sys.time() - starttime
cat(round(timediff, 2), attr(timediff, "units"), "to train model.")

fn = paste("Model", modelno, "RandomForest_Output.RData", sep= "_")
load(file.path(outputdir, 'Random_Forest_Output', fn))
importance(rf.out)

library(ggplot2)
import_df <- as.data.frame(importance(rf.out)) %>%
  arrange(desc(MeanDecreaseGini))

# Barplot
importance_plot <- ggplot(import_df, aes(x=reorder(row.names(import_df), -MeanDecreaseGini), y=MeanDecreaseGini)) + 
  geom_bar(stat = "identity", fill = 'purple') +
  labs(x = 'Predictor Variable',
       y = 'Importance (Mean Decrease in Gini Impurity)',
       title = 'Importance of Predictors in Crash Prediction Model') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(plot = importance_plot, 
       filename = paste0("importance_barplot", modelno, '_', Sys.Date(), ".png"),
       path = file.path(outputdir, "Figures"),
       device = "png",
       create.dir = T,
       height = 6, width = 8, units = "in")

gc()
gc()
# 

# # In case the factor levels are different between the provided training and test frames, bind the first row of one 
# # to the other and then delete it. This will equalize them.
# test_frame = rbind(training_frame[1,],test_frame)
# test_frame = test_frame[-1,]
# # Do the same in reverse as well.
# training_frame = rbind(test_frame[1,], training_frame)
# training_frame = training_frame[-1,]


# if(!(exists("training_frame"))){
#   print("Training frame not found in environment")
#   load('Intermediate/training_frame.Rdata')
# }
# 
# if(!(exists("test_frame"))){
#   print("Test frame not found in environment")
#   load('Intermediate/test_frame.Rdata')
# }

# if(any(colnames(training_frame) != colnames(test_frame))){
#   stop("Test Frame and Training Frame do not match. Check:")
#   print(setdiff(names(training_frame),names(test_frame)))
# }else(print("Training and Test Match"))
# 
# 
# for(i in colnames(training_frame)){
#   if(class(training_frame[,i]) != class(test_frame[,i])){
#     stop("class of ", i, " does not match")}else{
#       print(paste0(i, " is good to go!", sep = " "))
#     }
# }

#source('utility/wazefunctions.R')

# starttime <- Sys.time()
# rose_train <- ROSE(crash ~ ., data = temp_train)$data
# timediff <- Sys.time() - starttime
# cat(round(timediff, 2), attr(timediff, "units"), "to complete ROSE sample.")

# table(rose_train$crash)

# # function to sample data to avoid memory limitations
# thin_data <- function(dataframe){
#   dataframe = dataframe[sample(1:nrow(dataframe), size = nrow(dataframe)*sample_percentage),]
#   return(dataframe)
# }
# 
# # apply the function, if applicable
# if(!is.null(sample_percentage)) {
#   temp_train <- thin_data(temp_train)
# }

###### testing #####  
# test.dat = NULL
# test.split = .30
# split.by = NULL
# 
# train.dat = training_frame
# thin.dat = 0.2
# response.var = "crash"
# model.no = modelno
# rf.inputs = rf.inputs
# cutoff = c(0.9, 0.1)
################
# 
# # if there are 12 .Rdata monthly files at the expected paths then notify the user that we will re-use
# # those files. Otherwise run the osm_query.R script to generate them from scratch.
# train_test_fps <- file.path(intermediatedir, 'Month_Frames', paste(state, year, 1:12, 'month_frame_full.Rdata', sep = "_"))
# 
# if(!all(file.exists(train_test_fps))){
#   source('utility/osm_query.R') # creates training frames
# } else {
#   cat("Training files already exist. If you wish to generate the data from scratch exit the script; delete, move, or rename the files; and re-run. This is an example file path for the month of January: ")
#   cat(train_test_fps[1])
# }