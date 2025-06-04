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

# Double check that AOI shapefile is there if it is expected.
if(filter_osm){
  if(!file.exists(file.path(inputdir, AOI_shp_path))){
    stop('The filter_osm parameter it set to T or TRUE, but the subpath specified at AOI_shp_path cannot be found in Input folder. Run halted.')
  }
}

# Double check that events.csv file is there if it is expected.
if(include_events){
  if(!file.exists(file.path(inputdir, "events.csv"))){
    stop('The include_events parameter it set to T or TRUE, but the events.csv file cannot be found in Input folder. Run halted.')
  }
}

# Setup ---- 
gc()

source(file.path("utility", "get_packages.R")) # installs necessary package

# Timezones --------------------------------------------------------------

source(file.path("utility", "timezone_adj.R"))

# -------------------------------------------------------------------------

# read random forest function, do.rf() and other helper functions
source(file.path("analysis", "RandomForest_Fx.R"))

# Load Road Network --------------------------------------------------------------

source(file.path("utility", "OpenStreetMap_pull.R"))

# -------------------------------------------------------------------------
# Define the osm_ids to analyze based on area of interest, if applicable
if(filter_osm){
  
  AOI <- read_sf(file.path(inputdir, AOI_shp_path)) %>% st_transform(crs = projection)
  
  intersecting_roads <- st_intersection(state_network, AOI)
  
  # pull out the osm_ids
  osm_subset <- intersecting_roads %>%
    st_drop_geometry() %>%
    select(osm_id)
}

#---------------------------------------------------------------------------------

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

# --------------------------------------------------------------------------------

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
  
  if(state == "MN"){
    temp_train <- temp_train %>% 
      mutate(CAD_CRASH = ifelse(CAD_CRASH >= 1, 1, 0),
             CAD_CRASH = factor(CAD_CRASH))
  }
  ### Subset to the osm_ids and road types of interest
  if(filter_osm){
    matches <- as.character(temp_train$osm_id) %in% osm_subset$osm_id
    temp_train <- temp_train[matches, ]
  }
  
  temp_train <- temp_train %>% 
    left_join(state_network %>% st_drop_geometry() %>% select(osm_id, highway, maxspeed, ref), by = "osm_id") %>%
    filter(highway %in% road_types)
  
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

  non_crash_sample_size <- length(crash_sample) * noncrashratio
  non_crash_sample <- sample(non_crash_indices, size = non_crash_sample_size, replace = FALSE)
  combined_data <- temp_train[c(crash_sample, non_crash_sample), ]

  training_frame <- training_frame %>% bind_rows(combined_data)

  test_frame <- test_frame %>% bind_rows(temp_test)
  
  rm(temp_train, temp_test, combined_data)

  
  gc()
  
  timediff <- Sys.time() - starttime
  cat(round(timediff, 2), attr(timediff, "units"), " to append training and test data for month ", m, ".\n")
  
}

# join in the historical crash data, road type ("highway"), and speed limit ("maxspeed")

cat("Preparing to join data on historical crashes ('hist_crashes'), road type ('highway'), and speed limit ('maxspeed').\n")

# Prep Hist Crashes -----------------------------------------------------------

source(file.path("utility", "load_crashes.R")) 

source(file.path("utility", "prep_hist_crash2.R"))

if(state == "MN"){
  source(file.path("utility", "MN_CAD_load_historical.R"))
  training_frame <- left_join(training_frame, CAD_hist, by = "osm_id") %>% fill_na()
  test_frame <- left_join(test_frame, CAD_hist, by =  "osm_id") %>% fill_na()
  rm(CAD_hist)
  gc()
}

prep_data <- function(training_frame){

  training_frame <- training_frame %>%
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
  
  training_frame <- left_join(training_frame, imputed_waze_frame) %>% fill_na()
  test_frame <- left_join(test_frame, imputed_waze_frame)  %>% fill_na()
  
  rm(imputed_waze_frame, imputed_waze_data)
  
  gc()
}

# Load events data, if applicable and add event attribute
if(include_events){
  events <- read.csv(file = file.path(inputdir, "events.csv")) %>%
    mutate(Date = lubridate::as_date(Date))
  
  training_frame <- training_frame %>%
    mutate(Date = lubridate::as_date(paste(year, Month, Day, sep = "-")),
           event = Date %in% events$Date,
           event = factor(event, ordered = F)) %>%
    select(-Date)
  
  test_frame <- test_frame %>%
    mutate(Date = lubridate::as_date(paste(year, Month, Day, sep = "-")),
           event = Date %in% events$Date,
           event = factor(event, ordered = F)) %>%
    select(-Date)  
}

######  Make sure factor levels are the same in training and test frames ############

# Identify factor columns
factor_cols <- names(training_frame)[sapply(training_frame, is.factor)]

for(col in factor_cols) {
    training_frame[[col]] <- factor(training_frame[[col]], levels = levels(test_frame[[col]]))
}

training_frame <- training_frame %>% fill_na()

####################################################################################

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

gc()
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

avail.cores <- parallelly::availableCores(omit = 1)

# Use this to set number of decision trees to use, and key RF parameters. mtry is especially important, should consider tuning this with caret package
# For now use same parameters for all models for comparision; tune parameters after models are selected
rf.inputs = list(ntree.use = avail.cores * 50, avail.cores = avail.cores, mtry = NULL, maxnodes = 1000, nodesize = 100)

keyoutputs = redo_outputs = list() # to store model diagnostics

# Omit as predictors in this vector:
if(imputed_waze == TRUE){
  alwaysomit = c("crash", "Day", "osm_id", "ACCIDENT", "JAM", "ROAD_CLOSED", "WEATHERHAZARD", "jam_level", "day_of_week", "ref", "CAD_CRASH", "CAD_HAZARD", "CAD_None", "CAD_ROADWORK", "CAD_STALL")
}else{
  alwaysomit = c("crash", "Day", "osm_id", "day_of_week", "average_jams", "average_weather", "average_closure", "average_accident", "average_jam_level", "ref", "CAD_CRASH", "CAD_HAZARD", "CAD_None", "CAD_ROADWORK", "CAD_STALL")
}

starttime = Sys.time()

#omits = c(alwaysomit, c('Avg_cad_stall', 'Avg_cad_crash', 'Avg_cad_hazard', 'Avg_cad_none', 'Avg_cad_roadwork'))
omits = alwaysomit

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

# Update names to be more presentable in importance plot
new_names <- c(Average_accident = "Waze Accident Alert",  
               Average_closure = "Waze Closure Alert",   
               Average_jams = "Waze Jams Alert",
               Average_jam_level = "Waze Jams (Automated)",
               Average_weather = "Waze Weather Alert", 
               event = "Event", 
               highway = "Highway (Road Type)", 
               hist_CAD_CRASH = "Historical CAD Crash", 
               hist_CAD_HAZARD = "Historical CAD Hazard",
               hist_CAD_None = "Historical CAD None", 
               hist_CAD_ROADWORK = "Historical CAD Roadwork", 
               hist_CAD_STALL =  "Historical CAD Stall", 
               hist_crashes = "Historical Crashes", 
               Hour = "Hour", 
               maxspeed = "Max Speed", 
               Month = "Month",
               precipitation = "Precipitation",    
               SNOW = "Snow",
               temperature = "Temperature",
               weekday = "Weekday"
               )

old_rownames <- rownames(import_df)

updated_rownames <- ifelse(old_rownames %in% names(new_names),
                           new_names[old_rownames],
                           old_rownames)

rownames(import_df) <- updated_rownames

# Barplot
importance_plot <- ggplot(import_df, aes(
  x = reorder(row.names(import_df), -MeanDecreaseGini), 
  y = MeanDecreaseGini
)) +
  geom_bar(stat = "identity", fill = 'purple') +
  labs(
    x = 'Predictor Variable',
    y = 'Importance (Decrease in Gini Impurity)',
    title = 'Importance of Predictors in Crash Prediction Model'
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("Target Variable: ", response.var), 
           hjust = 1.1, vjust = 1.5, size = 5, color = "blue", fontface = "bold") +
  theme(
    axis.text.x  = element_text(angle = 60, vjust = 1, hjust = 1, size = 16),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title   = element_text(size = 14, face = "bold", hjust = 0.5)
  )

ggsave(
  plot = importance_plot,
  filename = paste0("importance_barplot", modelno, '_', Sys.Date(), ".png"),
  path = file.path(outputdir, "Figures"),
  device = "png",
  create.dir = TRUE,
  height = 6,
  width = 8,
  units = "in"
)

rm(list = ls())
gc()
gc()