# Random forest models of crash estimation for a given state. 

inputdir <- file.path(getwd(),"Input")
outputdir <-file.path(getwd(),"Output")
intermediatedir <- file.path(getwd(), "Intermediate")

# Make outputdir and intermediatedir if not already there
if(!dir.exists(intermediatedir)) { dir.create(intermediatedir) }
if(!dir.exists(outputdir)) { dir.create(outputdir) }
if(!dir.exists(file.path(outputdir, "Random_Forest_Output"))){dir.create(file.path(outputdir, "Random_Forest_Output"))}
if(!dir.exists(file.path(outputdir, "Figures"))){dir.create(file.path(outputdir, "Figures"))}

# historical crash data

crash_filepath <- file.path(file.path(inputdir,"Crash", state)) # define the location of the crash variables

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

##############################################################

# Setup ---- 
gc()

source(file.path("utility", "get_packages.R")) # installs necessary package

# Timezones --------------------------------------------------------------

source(file.path("utility", "timezone_adj.R"))

# -------------------------------------------------------------------------

# read random forest function, do.rf() and other helper functions
source(file.path("analysis", "RandomForest_Fx.R"))

# -------------------------------------------------------------------------

# The full model identifier gets created in this next step

modelno = paste(state,
                year, 
                ifelse(imputed_waze, "imputed", "NOTimputed"),
                ifelse(time_bins, "tbins", "hourly"), 
                num, 
                sep = "_")
  
train_fp <- file.path(intermediatedir, paste(modelno, "train_test_frames.RData", sep = "_"))

# check whether there is already a consolidated and prepped training frame at the expected file path. If not, prepare one. If so, notify the user and load the existing file.
if(!file.exists(train_fp)){
  
prepstarttime <- Sys.time()

# Given that the consolidated training frame does not exist, initiate process to generate one. First generate a vector of all file paths for monthly data frames.

monthframe_fps <- file.path(intermediatedir, 'Month_Frames', paste(state, year, 1:12, ifelse(time_bins, "tbins", ""), 'month_frame_full.Rdata', sep = "_"))

# if there are 12 .Rdata monthly files at the expected paths then notify the user that we will re-use those files. Otherwise run the osm_query.R script to generate them from scratch.

if(!all(file.exists(monthframe_fps))){
  
  source(file.path("utility", "osm_query.R"))  # creates training frames
  
} else { # Make sure the user is aware of the situation with respect to existing month frames
 
  load(monthframe_fps[1])
  
  if(length(unique(temp_train$Hour))==24 & time_bins){
    
    cat("Month frames already exist, but they were created with time_bins set to FALSE, meaning that the data \n")
    cat("are not aggregated. However, time_bins is currently set to TRUE in this script. \nIf you wish to cancel press 'c' to exit the script. You can then move or rename the month frames \nin the Intermediate folder or adjust the value for time_bins, and re-run. \nOtherwise, press 'p' to proceed, which will overwrite the existing month frames.\n")
    
    proceed <- readline("Press 'p' to proceed or 'c' to cancel>>")
    
    if(proceed == 'p'){
      
      source(file.path("utility", "osm_query.R"))  # creates training frames
      
      } 
    
    if(proceed == 'c'){
      
      stop("User chose to exit script.")
      
    }
    
  } else{ 
  
  if(length(unique(temp_train$Hour)) < 24 & !time_bins){
    
    cat("Month frames already exist, but they were created with time_bins set to TRUE, meaning that the data \n")
    cat("are aggregated. However, time_bins is currently set to FALSE in this script. \nIf you wish to cancel press 'c' to exit the script. You can then move or rename the month frames \nin the Intermediate folder or adjust the value for time_bins, and re-run. \nOtherwise, press 'p' to proceed, which will overwrite the existing month frames.\n")
    
    proceed <- readline("Press 'p' to proceed or 'c' to cancel>>")
    
    if(proceed == 'p'){
      
      source(file.path("utility", "osm_query.R"))  # creates training frames
      
    }
  
    if(proceed == 'c'){
      
      stop("User chose to exit script.")
      
      }
  } 
    else {
      
    cat("Month frames already exist. If you wish to generate the data from scratch exit the script; delete, move, or rename the files; and re-run. This is an example file path for the month of January: ")
    cat(monthframe_fps[1], '\n\n')
    
    }
    
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

#m <- 1
for(m in 1:12){
  
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

  crash_sample_size <- length(crash_indices)
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

# Load Road Network --------------------------------------------------------------

source(file.path("utility", "OpenStreetMap_pull.R"))

# Prep Hist Crashes -----------------------------------------------------------

source(file.path("utility", "load_crashes.R")) 

source(file.path("utility", "prep_hist_crash2.R"))

prep_data <- function(training_frame){

  training_frame <- training_frame %>%
    left_join(state_network %>% st_drop_geometry(), by = "osm_id") %>%
    left_join(hist_crashes, by = "osm_id") %>%
    mutate(Month = factor(Month, ordered = F),
           Day = factor(Day, ordered = F),
           Hour = factor(Hour, ordered = F),
           weekday = factor(weekday, ordered = F),
           osm_id = factor(osm_id, ordered = F),
           highway = factor(highway, ordered = F)) %>%
    fill_na() # function defined in RandomForest_Fx.R

  return(training_frame)
}

training_frame <- prep_data((training_frame))
test_frame <- prep_data(test_frame)

# add imputed waze data, if applicable
if(imputed_waze == TRUE){
  
  imputed_values <- list.files(file.path(intermediatedir, "Month_Frames"), 
                               pattern = paste(state, year, ifelse(time_bins, "tbins", ""), "month_frame_imputed_waze", sep = "_"), 
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
  
  if((year %in% c(2018,2019,2020)) & (state == "MN")){
    source(file.path("utility", "MN_CAD_load.R"))
    training_frame <- left_join(training_frame, CAD)
    test_frame <- left_join(test_frame, CAD)
  }
  
  gc()
}

# sort column positions in alphabetical order
new_order = sort(colnames(training_frame))
training_frame <- training_frame[, new_order]
test_frame <- test_frame[, new_order]

timediff <- Sys.time() - prepstarttime
cat(round(timediff, 2), attr(timediff, "units"), "to prep data.")

save(list = c("training_frame", "test_frame"), file = file.path(intermediatedir,paste(modelno, "train_test_frames.RData", sep = "_")))

} else {
  cat("Training and test frame already exist at: ", train_fp, ". \nIf you wish to generate the data from scratch exit the script; delete, move, or rename the file; and re-run.")
  load(train_fp)
}

# <><><><><><><><><><><><><><><><><><><><><><><><>
# End data prep 
# <><><><><><><><><><><><><><><><><><><><><><><><>

bin.mod.diagnostics <- function(predtab){
  
  accuracy = (predtab[1,1] + predtab[2,2] )/ sum(predtab) # true positives and true negatives divided by all observations
  precision = (predtab[1,1] )/ sum(predtab[1,]) # true positives divided by all predicted positives
  recall = (predtab[1,1] )/ sum(predtab[,1]) # true positives divided by all observed positives
  false.positive.rate = (predtab[1,2] )/ sum(predtab[,2]) # false positives divided by all observed negatives
  
  round(t(data.frame(accuracy, precision, recall, false.positive.rate)), 4)  
}

i <- 1

avail.cores = parallelly::availableCores(omit = 1)

# Use this to set number of decision trees to use, and key RF parameters. mtry is especially important, should consider tuning this with caret package
# For now use same parameters for all models for comparision; tune parameters after models are selected
rf.inputs = list(ntree.use = avail.cores * 50, avail.cores = avail.cores, mtry = NULL, maxnodes = 1000, nodesize = 100)

keyoutputs = redo_outputs = list() # to store model diagnostics

# Omit as predictors in this vector:
if(imputed_waze == TRUE){
  alwaysomit = c("crash", "Day", "osm_id", "ACCIDENT", "JAM", "ROAD_CLOSED", "WEATHERHAZARD", "jam_level", "day_of_week", "ref")
}else{
  alwaysomit = c("crash", "Day", "osm_id", "day_of_week", "average_jams", "average_weather", "average_closure", "average_accident", "average_jam_level", "ref")
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
write.csv(fitvar_df, file = file.path(outputdir, "Random_Forest_Output", paste0('Fitvars_', modelno, ".csv")))

# Run the Random Forest model using `do.rf()` function.
keyoutputs[[modelno]] = do.rf(train.dat = training_frame, 
                              test.dat = test_frame,
                              #thin.dat = 0.2,
                              omits, response.var = "crash", 
                              model.no = modelno, rf.inputs = rf.inputs,
                              cutoff = c(0.9, 0.1))  

save("keyoutputs", file = file.path(outputdir, "Random_Forest_Output", paste0("Output_to_", modelno)))
#keyoutputs$"08"

timediff <- Sys.time() - starttime
cat(round(timediff, 2), attr(timediff, "units"), "to train model.")

fn = paste("Model", modelno, "RandomForest_Output.RData", sep= "_")
load(file.path(outputdir, 'Random_Forest_Output', fn))
importance(rf.out)

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