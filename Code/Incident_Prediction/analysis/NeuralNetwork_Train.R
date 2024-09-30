# Random forest models of crash estimation for a given state. 

# Setup ---- 
gc()

source('utility/get_packages.R') # installs necessary packages

library(randomForest)
library(foreach) # for parallel implementation
library(doParallel) # includes iterators and parallel
library(tidyverse)
library(ROSE)
library(performanceEstimation)
library(caret)
library(neuralnet)

inputdir <- file.path(getwd(),"Input")
outputdir <-file.path(getwd(),"Output")
intermediatedir <- file.path(getwd(), "Intermediate")

# Make outputdir and intermediatedir if not already there
if(!dir.exists(intermediatedir)) { dir.create(intermediatedir) }
if(!dir.exists(outputdir)) { dir.create(outputdir) }

test_percentage <- 0.03

state <- "WA"

# Indicate whether the state has a unified time zone
one_zone <-TRUE
# If one_zone is set to T or TRUE, meaning that the state has one time zone, specify the name of the time zone, selecting from 
# among the options provided after running the first line below (OlsonNames())

# OlsonNames()
# time_zone_name <- "US/Central"
time_zone_name <- "US/Pacific"

# Year
year <- 2021

# Projection 
projection <- 5070 

### uncomment for temporary testing ####
m <- 1

##Use Imputed Waze?
imputed_waze <- T


train_fp <- file.path(intermediatedir,paste(state, year, "train_test_frames.RData", sep = "_"))

# check whether there is already a consolidated and prepped training frame
# at the expected file path. If not, prepare one. If so, notify the user and load the 
# existing file.
if(!file.exists(train_fp)){

# Given that the consolidated training frame does not exist, initiate process
# to generate one. First generate a vector of all file paths for monthly data frames.
monthframe_fps <- file.path(intermediatedir, 'Month_Frames', paste(state, year, 1:12, 'month_frame_full.Rdata', sep = "_"))

# if there are 12 .Rdata monthly files at the expected paths then notify the user that we will re-use
# those files. Otherwise run the osm_query.R script to generate them from scratch.
if(!all(file.exists(monthframe_fps))){
  source('utility/osm_query.R') # creates training frames
} else {
  cat("Month frames already exist. If you wish to generate the data from scratch exit the script; delete, move, or rename the files; and re-run. This is an example file path for the month of January: ")
  cat(monthframe_fps[1], '\n\n')
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
  #temp_train <-
  
  training_frame <- training_frame %>% bind_rows(temp_train)
  
  test_frame <- test_frame %>% bind_rows(temp_test)
  
  # w.expected <- training_frame %>%
  #   group_by(osm_id, Month, day_of_week, Hour) %>%
  #   summarize(nWazeAccident = median(nWazeAccident, na.rm = T),
  #             nWazeWeatherOrHazard = median(nWazeWeatherOrHazard, na.rm = T),
  #             nWazeJam = median(nWazeJam, na.rm = T),
  #             nWazeRoadClosed = median(nWazeRoadClosed, na.rm = T)
  
  rm(temp_train, temp_test)
  
  gc()
  
  timediff <- Sys.time() - starttime
  cat(round(timediff, 2), attr(timediff, "units"), " to append training and test data for month ", m, ".\n")
  
}

prep_data <- function(training_frame){
  training_frame <- training_frame %>%
    replace_na(list(precipitation = 0, SNOW = 0)) %>%
    mutate(Month = factor(Month),
           Day = factor(Day),
           Hour = factor(Hour),
           weekday = factor(weekday),
           osm_id = factor(osm_id)) 
  return(training_frame)
}

starttime <- Sys.time()
training_frame <- prep_data((training_frame))
test_frame <- prep_data(test_frame)
timediff <- Sys.time() - starttime
cat(round(timediff, 2), attr(timediff, "units"), "to prep data.")

save(list = c("training_frame", "test_frame"), file = file.path(intermediatedir,paste(state, year, "train_test_frames.RData", sep = "_")))

} else {
  cat("Training and test frame already exist at: ", train_fp, ". \nIf you wish to generate the data from scratch exit the script; delete, move, or rename the file; and re-run.")
  load(train_fp)
}

# model1 = lm(crash ~ ., data = temp_train)
# summary(model1)

# bin.mod.diagnostics <- function(predtab){
#   
#   accuracy = (predtab[1,1] + predtab[2,2] )/ sum(predtab) # true positives and true negatives divided by all observations
#   precision = (predtab[1,1] )/ sum(predtab[1,]) # true positives divided by all predicted positives
#   recall = (predtab[1,1] )/ sum(predtab[,1]) # true positives divided by all observed positives
#   false.positive.rate = (predtab[1,2] )/ sum(predtab[,2]) # false positives divided by all observed negatives
#   
#   round(t(data.frame(accuracy, precision, recall, false.positive.rate)), 4)  
# }
# 
# i <- 1

# read random forest function, do.rf()
# source("analysis/NeuralNetwork_Fx.R")

if(imputed_waze == TRUE){

imputed_values <- list.files(file.path(intermediatedir, "Month_Frames"), 
                             pattern = "imputed", 
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

training_frame <- left_join(training_frame, imputed_waze_frame)
test_frame <- left_join(test_frame, imputed_waze_frame)

rm(imputed_waze_frame, imputed_waze_data)

gc()
}

nnet_data_prep <- function(data){
  data <- data %>% 
    mutate(Month = as.numeric(Month),
           Hour = as.numeric(Hour),
           weekday = as.numeric(weekday),
           crash = as.numeric(crash))
  return(data)
}

test_frame <- nnet_data_prep(test_frame)
training_frame <- nnet_data_prep(training_frame)

# <><><><><><><><><><><><><><><><><><><><><><><><>
# End data prep 
# <><><><><><><><><><><><><><><><><><><><><><><><>

avail.cores = parallel::detectCores()

if(avail.cores > 8) avail.cores = 12 # Limit usage below max if on r4.4xlarge instance. Comment this out to run largest models.

if(imputed_waze){
  formula <- crash ~ Month + Hour + weekday + precipitation + temperature + SNOW + Average_jams + Average_weather + Average_closure + Average_accident
} else{
  formula <- crash ~ Month + Hour + weekday + ACCIDENT + JAM + ROAD_CLOSED + WEATHERHAZARD + precipitation + temperature + SNOW
}



str(test_frame)

# Omit as predictors in this vector:
# if(imputed_waze == TRUE){
#   alwaysomit = c("crash", "Day", "osm_id", "ACCIDENT", "JAM", "ROAD_CLOSED", "WEATHERHAZARD", "jam_level", "day_of_week")
# }else{
#   alwaysomit = c("crash", "Day", "osm_id", "day_of_week", "average_jams", "average_weather", "average_closure", "average_accident", "average_jam_level")
# }

neurons <- c(5, 3) # number of layers and neurons used in NN

starttime = Sys.time()

num <- "01" # Use this to create a separate identifier to distinguish when multiple models are attempted for a given state and year.

# The full model identifier gets created in this next step
if(imputed_waze == TRUE){
modelno = paste(state, year, "imputed", num, sep = "_")
}else{
  modelno = paste(state, year, "NOTimputed", num, sep = "_")
}

# train model
model <- neuralnet(formula, data = test_frame, linear.output = FALSE)

# predict model
predictions <- predict(model, training_frame)

# Convert predictions to class labels
predicted_classes <- ifelse(predictions[,1] > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(predicted_classes == training_frame$crash)
print(paste("Accuracy: ", accuracy))
