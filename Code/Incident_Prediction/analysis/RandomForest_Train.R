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

  temp_train <- downSample(x = temp_train %>% select(-crash),
                            y = temp_train$crash) %>% 
                rename(crash = Class)
  
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

bin.mod.diagnostics <- function(predtab){
  
  accuracy = (predtab[1,1] + predtab[2,2] )/ sum(predtab) # true positives and true negatives divided by all observations
  precision = (predtab[1,1] )/ sum(predtab[1,]) # true positives divided by all predicted positives
  recall = (predtab[1,1] )/ sum(predtab[,1]) # true positives divided by all observed positives
  false.positive.rate = (predtab[1,2] )/ sum(predtab[,2]) # false positives divided by all observed negatives
  
  round(t(data.frame(accuracy, precision, recall, false.positive.rate)), 4)  
}

# read random forest function, do.rf()
source("analysis/RandomForest_Fx.R")


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
alwaysomit = c("crash", "Month", "Day", "Hour", "osm_id")

alert_types = unique(training_frame$alert_type)

alert_subtypes = unique(training_frame$sub_type)

response.var = "crash" # now could use nTN_total, for all TN crashes in this grid cell, this hour. Or TN_crash, binary indicator of if any TN crash occurred in this grid cell/hours 

starttime = Sys.time()

# Best Model: 'Model 05' with main waze alert types but not sub-types included

# 05, add base Waze features
modelno = paste("07")

omits = c(alwaysomit)
# 05, add base Waze features

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
keyoutputs$"07"

timediff <- Sys.time() - starttime
cat(round(timediff, 2), attr(timediff, "units"), "to train model.")

# fn = paste(state, "Model", modelno, "RandomForest_Output.RData", sep= "_")
# load(file.path(outputdir, 'Random_Forest_Output', fn))
# importance(rf.out)

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