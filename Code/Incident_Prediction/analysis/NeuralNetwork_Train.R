# Title: Neural Network Training and Prediction Script
# Last Edited: 10/24/2024
# Purpose: Train and predict crashes for ROADII Incident Detection using TensorFlow

# Setup ---- 
gc()

source('utility/get_packages.R') # installs necessary packages

library(dplyr)
library(stringr)
library(torch)
library(brulee)
library(recipes)
library(ggplot2)
library(yardstick)


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

##Use Imputed Waze?
imputed_waze <- T

train_fp <- file.path(intermediatedir,paste(state, year, "train_test_frames.RData", sep = "_"))

# Data Prep ---------------------------------------------------------------

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
  
  # set aside validation set before down-sampling to address class imbalance.
  
  trainrows <- sort(sample(1:nrow(temp_train), size = nrow(temp_train)*(1-test_percentage), replace = F))
  
  testrows <- (1:nrow(temp_train))[!1:nrow(temp_train) %in% trainrows]
  
  temp_test <- temp_train[testrows,]
  
  temp_train <- temp_train[trainrows,]
  

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
    mutate(temperature = temperature + abs(min(temperature)), # ensure no negative input which seems to give a warning
           Month = as.factor(Month),
           Hour = as.factor(Hour),
           weekday = as.factor(as.numeric(weekday)-1), # should be in 0 or 1, not 1 or 2
           crash = as.factor(as.numeric(crash)-1)) # should be in 0 or 1, not 1 or 2
  return(data)
}

test_frame <- nnet_data_prep(test_frame)
training_frame <- nnet_data_prep(training_frame)

# Train -------------------------------------------------------------------

set.seed(1)

if(imputed_waze){
  
  formula <- crash ~ Month + Hour + weekday + precipitation + temperature + SNOW + Average_jams + Average_weather + Average_closure + Average_accident
  #boxcox_vars <- c(precipitation, SNOW, Average_jams, Average_weather, Average_closure, Average_accident)
  # interactions <- c(~ starts_with("Month"):temperature + 
  #                     SNOW:temperature + 
  #                     Average_jams:Average_closure:Average_accident)

} else{
  # will do this later, testing for now on imputed waze
  formula <- crash ~ Month + Hour + weekday + ACCIDENT + JAM + ROAD_CLOSED + WEATHERHAZARD + precipitation + temperature + SNOW
  
  }

inciditent_pred <- 
  recipe(formula, data = training_frame) %>% 
  #step_log(c(precipitation, SNOW, Average_jams, Average_weather, Average_closure, Average_accident)) %>% # normalize variables that are on the extremes
  step_dummy(all_nominal_predictors(), one_hot = T) %>% # makes dummy variales for nominal predictors (i.e Month_1, Month_2)
  step_interact(~ starts_with("Month"):temperature + 
                  SNOW:temperature + 
                  Average_jams:Average_closure:Average_accident) %>% # creates interaction variables 
  step_zv(all_predictors()) %>% # get's rid of varilbles with 0 variance 
  step_normalize(all_numeric_predictors(), na_rm = TRUE) # normalizes all numeric predictors 

fit <- brulee_mlp(inciditent_pred, 
                  data = training_frame, 
                  hidden_units = 20, # number of nodes in each layer 
                  dropout = 0.05, # proportion of layers set to zero 
                  rate_schedule = "cyclic", # maximization technique 
                  step_size = 4)
fit

autoplot(fit)

predictions <- predict(fit, test_frame) %>%
               bind_cols(test_frame)


table(predictions$crash, predictions$.pred_class)

predictions %>%
  mutate(crash_num = as.numeric(crash),
         .pred_class_num = as.numeric(.pred_class)) %>%
  rmse(crash_num, .pred_class_num)

# seeing what variables look like 
ggplot(data=training_frame, aes(x=ACCIDENT)) +
  geom_histogram(bins=30)

summary(training_frame$temperature)

sum(is.na())


# shape_size <- 784
# 
# inputs <- layer_input(shape = shape(shape_size), # this can be empty I believe? 
#                       name = "crashes") # layer input creates the first layer of a NN (the entry point)
# 
# x <- inputs %>% # this constructs the layers of the NN
#   layer_dense(units = 64, # number of neurons in the layer
#               activation = "relu", # activation function, relu helps prevent the vanishing gradient problem (https://medium.com/@ssiddharth408/the-most-commonly-used-activation-functions-in-tensorflow-v2-11-2132107a440)
#               name = "dense_1") %>% # name of layer, should not repeat
#   layer_dense(units = 64, activation = "relu", name = "dense_2")
# 
# outputs <- x %>% 
#   layer_dense(units = 1, # think we just need 1 neuron as the probability of a crash or not
#               activation = "softmax", # often used at the output layer to produce probability
#               name = "prediction")
# 
# model <- keras_model(inputs = inputs, outputs = outputs)
# 
# c(c(x_train, y_train), c(x_test, y_test)) %<-% keras::dataset_mnist()

# avail.cores = parallel::detectCores()
# 
# if(avail.cores > 8) avail.cores = 12 # Limit usage below max if on r4.4xlarge instance. Comment this out to run largest models.
# 
# if(imputed_waze){
#   formula <- crash ~ Month + Hour + weekday + precipitation + temperature + SNOW + Average_jams + Average_weather + Average_closure + Average_accident
# } else{
#   formula <- crash ~ Month + Hour + weekday + ACCIDENT + JAM + ROAD_CLOSED + WEATHERHAZARD + precipitation + temperature + SNOW
# }
# 
# str(test_frame)
# 
# INSTALL_TF = F # Change to TRUE for first time installation
# 
# if(INSTALL_TF){
#   
#   library(remotes)
#   library(reticulate)
#   
#   remotes::install_github("rstudio/tensorflow")
#   
#   reticulate::install_python()
#   
#   library(tensorflow)
#   install_tensorflow(envname = "r-tensorflow") # suggested command, works on my GFE laptop
#   #install_tensorflow(envname = "C:/Documents/.virtualenvs/r-tensorflow/Scripts") # attempt to make it work on VM - placing it where it's asking for it when 
#   
#   # Keras is not needed (for now) it also installs TensorFlow so it will actually give an error
#   # library(keras)
#   # install_keras()
#   
# }
# 
# library(tensorflow) 
# # library(keras) 
# 
# tf$constant("Hello TensorFlow!") # testing if tensorflow is working
# 
# # this is where I've got to in getting it to work 