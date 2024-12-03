
# Background ----------------------------
library(dplyr)
library(tidyr)
library(osmdata)
library(sf)
library(ggplot2)
library(tigris)
library(doParallel)
library(lubridate)
library(stringr)

top_time <- Sys.time()

#source('utility/wazefunctions.R') # contains find_matches function

##Parameters to enter ------------------------------------------------------------------

keep_prop <- NULL # proportion of data to keep when thinning

# Directories ------------------------------------------------------------------

waze_dir <- file.path(inputdir, "Waze", state)

waze_jams_dir <- file.path(waze_dir, "jams")

if(!dir.exists(waze_dir)){dir.create(waze_dir, recursive = T)}

if(!dir.exists(waze_jams_dir)){dir.create(waze_jams_dir, recursive = T)}

# Timezones --------------------------------------------------------------
onSDC <- F

if(onSDC){
  US_timezones <- st_read(file.path(inputdir,"Shapefiles","Time_Zones","time_zones_ds_timezone_polygons.shp"))
}else{
  US_timezones <- st_read("https://geo.dot.gov/server/rest/services/Hosted/Time_Zones_DS/FeatureServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson")
}

tz_adj_to_names <- data.frame(tz_adj = c(-5,-6,-7,-8,-9,-10,-11), tz_name = c("US/Eastern","US/Central","US/Mountain","US/Pacific", "US/Alaska", "US/Hawaii", "US/Samoa"))

timezone_adj <- US_timezones %>% st_transform(crs=projection) %>% 
  mutate(adjustment = as.numeric(paste0(str_sub(utc, 1, 1), str_sub(utc, 2, 3)))) %>% 
  select(adjustment) %>% 
  left_join(tz_adj_to_names,by = join_by(adjustment==tz_adj))

rm(tz_adj_to_names)

# Load OSM Data ------------------------------

source(file.path("Utility", "Prep_OSMNetwork.R"))
state_network <- state_network %>% select(osm_id)

# Load Crash Data ------------------------------

# load raw file 

crash_files <- list.files(file.path(inputdir,"Crash", state_osm), pattern = 'crash.shp$', full.names = TRUE)

n = as.numeric(length(crash_files)) 
datalist <- list()
datalist = vector("list", length = n)

l = 0

#i <- "Input/Crash/Minnesota/mn21crash.shp"

if(state == "MN"){
  timestamps <- read.csv(file.path(inputdir,'Crash', state_osm, paste0(state_osm, '_timestamps.csv'))) %>%
    rename(INCIDEN = INCIDENT_ID) %>%
    mutate(DATE_TIME_OF_INCIDENT = as.POSIXct(DATE_TIME_OF_INCIDENT, format = "%m/%d/%Y %H:%M"))
  
  for (i in crash_files){
    starttime = Sys.time()
    l <- l + 1 
    
    temp <- read_sf(i) %>%
      left_join(timestamps, by = 'INCIDEN') %>%
      mutate(crashID = INCIDEN) %>%
      select(DATE_TIME_OF_INCIDENT, crashID) %>%
      st_transform(projection) %>%
      drop_na() 
    
    if(!one_zone){
      temp <- temp %>% st_join(timezone_adj, join = st_nearest_feature) %>% 
        mutate(time_local = as.POSIXct(DATE_TIME_OF_INCIDENT, tz = tz_name))
    } else {temp <- temp %>% mutate(time_local = as.POSIXct(DATE_TIME_OF_INCIDENT, tz = time_zone_name))}
    
    temp <- temp %>% select(time_local, crashID)
    
    datalist[[l]] <- temp
    
    timediff = Sys.time() - starttime
    cat("Loaded ",i,"... ")
    cat(round(timediff,2), attr(timediff, "unit"), "elapsed", "\n")
    rm(temp)
  }
}
#tz(temp$time_local)

#i <- "Input/Crash/Washington_State/wa21crash.shp" 
#sum(duplicated(temp$CASENO))

if(state == "WA"){
  for (i in crash_files){
    starttime = Sys.time()
    l <- l + 1 
    temp <- read_sf(i) %>%
      mutate(TIME = ifelse(nchar(TIME) == 3, paste0(0, TIME), TIME),
             ACC_DATE = paste(ACC_DATE, " ", TIME),
             crashID = CASENO) %>%
      select(ACC_DATE, crashID) %>%
      st_transform(projection) %>%
      drop_na()
    
    if(!one_zone){
      temp <- temp %>% st_join(timezone_adj, join = st_nearest_feature) %>% 
        mutate(time_local = as.POSIXct(ACC_DATE, format = '%Y-%m-%d %H%M', tz = tz_name))
    } else {temp <- temp %>% mutate(time_local = as.POSIXct(ACC_DATE, format = '%Y-%m-%d %H%M', tz = time_zone_name))}

    temp <- temp %>% select(time_local, crashID)
    
    datalist[[l]] <- temp
    
    timediff = Sys.time() - starttime
    cat("Loaded ",i,"... ")
    cat(round(timediff,2), attr(timediff, "unit"), "elapsed", "\n")
    rm(temp)
  }
}

## create total crashes for the year
starttime = Sys.time()
total_crashes <- do.call(bind_rows, datalist) %>%
  st_transform(projection) %>%
  mutate(YEAR = as.integer(lubridate::year(time_local)),
         month = as.integer(lubridate::month(time_local)),
         day = as.integer(lubridate::day(time_local)),
         hour = as.integer(lubridate::hour(time_local)),
         # storing coordinate information as numbers rather than sf geometry to reduce the size of the object
         # and speed subsequent operations.
         lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2],
         crash = 1,
         zone=tz(time_local)) %>%
  filter(YEAR == year) %>%
  st_join(state_network %>% select(osm_id), join = st_nearest_feature) %>%
  # removing geometry to speed group by operation and limit use of memory. Will add geometry back in later, but it will be the geometry for
  # the road segment, not the geometry for the crash point, because there are sometimes multiple crashes per observation (row)
  st_drop_geometry() %>%
  select(osm_id, month, day, hour, crash) %>%
  group_by(osm_id, month, day, hour) %>%
  summarise(crash = sum(crash)) %>%
  ungroup()
timediff = Sys.time() - starttime
cat("Created total_crashes ")
cat(round(timediff,2), attr(timediff, "unit"), "elapsed", "\n")

#------------------------------------------------------------------------------------

## Set up monthly training and test dataframes ----------------------

# Switching to operate month-by-month to avoid running out of memory 
# when trying to work with an entire year all at the same time.
monthIDs <- formatC(1:12, width = 2, flag = "0")
yearmonths <- c(paste(year, monthIDs, sep="-"))
yearmonths.1 <- paste(yearmonths, "01", sep="-")
#lastdays <- days_in_month(as.POSIXct(yearmonths.1)) # from lubridate
#yearmonths.end <- paste(yearmonths, lastdays, sep="-")

is_weekday <- function(timestamps){lubridate::wday(timestamps, week_start = 1) < 6}

# uncomment for testing
m <- 1

for (m in 1:12){
  starttime = Sys.time()
  # create sequence of hours for the month
  if(m<12){
    dates <- seq(from = as.POSIXct(yearmonths.1[m]), to = as.POSIXct(yearmonths.1[m+1]), by = "hour")
  } else {
    dates <- seq(from = as.POSIXct(yearmonths.1[m]), to = as.POSIXct(c(paste(year+1, "01-01", sep="-"))), by = "hour")
  }
  # trim off the last hour, which is for the following month
  dates <- head(dates, -1)
  # convert into a dataframe with separate columns for month, day, hour, and day of week.
  dates <- data.frame(dates = dates, 
                      month = lubridate::month(dates), 
                      day = lubridate::day(dates), 
                      hour = lubridate::hour(dates), 
                      day_of_week = lubridate::wday(dates, label = TRUE),
                      weekday = is_weekday(dates))
  # convert day_of_week into unordered since we want to treat it as a nominal variable. 
  dates$day_of_week <- factor(dates$day_of_week, ordered = FALSE)
  
  # replicate it, multiplying by the number of road segments
  dates_exp <- do.call(bind_rows, replicate(nrow(state_network), dates, simplify = FALSE)) %>%
    select(!dates)
  
  # combine date/time columns from sample with osm_id and then crashes
  # expand road segment frame, multiplying by the number of date/time training samples
  temp_train <- do.call(bind_rows, replicate(nrow(dates), state_network %>% st_drop_geometry(), simplify = FALSE)) %>% 
    arrange(osm_id) %>%
    # merge with expanded version of date/time training frame
    cbind(dates_exp, row.names=NULL) %>%
    # bring in crash data
    left_join(total_crashes, by = c('osm_id', 'month', 'day', 'hour')) %>%
    mutate(crash = ifelse(is.na(crash), 0, crash))
  
  # save the temp object
  if(!dir.exists(file.path(intermediatedir,'Month_Frames'))) { dir.create(file.path(intermediatedir,'Month_Frames')) }
  
  save(list = c('temp_train'), file = file.path(intermediatedir,'Month_Frames',paste(state, year, "month_frame", m,".RData", sep = "_")))
  timediff = Sys.time() - starttime
  
  # clear memory
  #rm(num_days, month, day, hour, dates, dates_exp, temp, sample, temp_train, temp_test)
  
  cat("Created training data frame with date/time, road segments, and crashes for month ", m, ". ")
  cat(round(timediff,2), attr(timediff, "unit"), "elapsed", "\n")
  gc()
}

rm(total_crashes, dates, dates_exp, datalist, crash_files, file_path, i, l)
gc()
#load(file = file.path(intermediatedir,'Month_Frames',paste(state, year, "month_frame", m,".RData", sep = "_")))

## waze data -----------------------------------------------

# establish set of waze files that we'll be reading in
waze.files <- list.files(path = waze_dir, pattern = "\\.csv$")

waze.files.year <- file.path(inputdir, "Waze", state, waze.files[grep(as.character(year), waze.files)])

waze.jams.files <- list.files(path = waze_jams_dir, pattern = "\\.csv$")

waze.jams.files.year <- file.path(waze_jams_dir, waze.jams.files[grep(as.character(year), waze.jams.files)])

# uncomment for testing
m <- 1

gc()

# note that the waze_query.R script already converted to local time. If waze data were sourced in some other way, may need to
# uncomment the parts that convert to local time below, from UTC.

# If applicable, load CAD data for MN so as to join it in
if((year %in% c(2018,2019,2020)) & (state == "MN")){
  source('utility/MN_CAD_load.R')
}

for(m in 1:12){
  starttime = Sys.time()
  load(file.path(intermediatedir,'Month_Frames',paste(state, year, "month_frame", m,".RData", sep = "_")))
  # read in data frames for that month
  waze_temp = read.csv(waze.files.year[m]) 
  waze_temp = waze_temp %>%
    st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
    st_transform(projection) %>% 
    st_join(state_network %>% select(osm_id), join = st_nearest_feature)
  
  if(!one_zone){
    waze_temp <- waze_temp %>% st_join(timezone_adj, join = st_nearest_feature) %>% 
      mutate(time_local = as.POSIXct(time_local, tz = tz_name))
  } else {waze_temp <- waze_temp %>% mutate(time_local = as.POSIXct(time_local, tz = time_zone_name))}
  
  waze_temp = waze_temp %>%
    st_drop_geometry() %>%
    mutate(
           month = lubridate::month(time_local),
           day = as.numeric(lubridate::day(time_local)),
           hour = as.numeric(lubridate::hour(time_local))
    ) %>%
    select(osm_id, month, day, hour, alert_type) %>%
    mutate(n = 1) %>%
    group_by(osm_id, month, day, hour, alert_type) %>% 
    summarize(n = sum(n)) %>%
    pivot_wider(names_from = alert_type, values_from = n)
    
  temp_train = temp_train %>% 
    left_join(waze_temp, by = c('osm_id', 'month', 'day', 'hour')) %>%
    replace_na(list(ACCIDENT = 0, JAM = 0, ROAD_CLOSED = 0, WEATHERHAZARD = 0))
  
  # If CAD data are applicable, join them in
  if((year %in% c(2018,2019,2020)) & (state == "MN")){
    temp_train = left_join(temp_train, CAD, by = c(osm_id, month, day, hour))
  }
  
  # if jams data are available, read them in for that month
  if(length(waze.jams.files.year) > 0){
    waze_temp = read.csv(waze.jams.files.year[m]) %>%
      rename(month = month_local,
             day = day_local,
             hour = hour_local) %>%
      mutate(osm_id = as.character(osm_id))
    
    temp_train = temp_train %>% 
      left_join(waze_temp %>% select(!pub_utc_timestamp_mean), by = c('osm_id', 'month', 'day', 'hour')) %>%
      replace_na(list(level_mode = 0)) %>%
      rename(jam_level = level_mode)
  }
  # create the imputed averages - two different versions depending on whether there are data on jams level
  if(length(waze.jams.files.year) > 0){
    waze_averages <- temp_train %>%
      group_by(osm_id, month, weekday, hour) %>%
      summarize(average_jams = mean(JAM),
                average_weather = mean(WEATHERHAZARD),
                average_closure = mean(ROAD_CLOSED),
                average_accident = mean(ACCIDENT),
                average_jam_level = mean(jam_level))
    
  } else {
    waze_averages <- temp_train %>%
      group_by(osm_id, month, weekday, hour) %>%
      summarize(average_jams = mean(JAM),
                average_weather = mean(WEATHERHAZARD),
                average_closure = mean(ROAD_CLOSED),
                average_accident = mean(ACCIDENT))
  }
  
  # thin data, if needed, to avoid running out of memory when running Join_Road_Weather.R script
  if(!is.null(keep_prop)){
    temp_train = temp_train[sample(1:nrow(temp_train), size = nrow(temp_train)*keep_prop),]
  }
  
  # save the objects
  save(list = c('temp_train'), file = file.path(intermediatedir,'Month_Frames',paste(state, year, "month_frame_waze", m,".RData", sep = "_")))
  save(waze_averages, file = file.path(intermediatedir,'Month_Frames',paste(state, year, "month_frame_imputed_waze", m,".RData", sep = "_")))
  timediff = Sys.time() - starttime
  
  # clear memory
  rm(temp_train, waze_temp, waze_averages)
  gc()
  cat("Added waze data for month ", m, ". ")
  cat(round(timediff,2), attr(timediff, "unit"), "elapsed", "\n")
}
#load(file.path(intermediatedir,'Month_Frames',paste(state, year, "month_frame_waze", m,".RData", sep = "_")))
#load(file = file.path(intermediatedir,'Month_Frames',paste(state, year, "month_frame_imputed_waze", m,".RData", sep = "_")))
dif_time <- round(difftime(Sys.time(), top_time, units = "mins"), 2)
cat(paste0("Created all month frames with crashes and waze data. ", dif_time, " minutes elapsed thus far in total."))
cat("Now adding weather.")

gc()

# Run Weather Script ------------------------------------------------------
source(file.path("Utility", "Join_Road_weather.R"))

rm(timezone_adj, tz_adjustments, US_timezones)

gc()
#########
# create consolidated version of training and test data frames
# training_frame <- data.frame(osm_id = character(),
#                              Month = numeric(),
#                              Day = numeric(),
#                              Hour = numeric(),
#                              crash = numeric(),
#                              alert_type = factor(),
#                              sub_type = factor(),
#                              day_of_week = factor(),
#                              precipitation = numeric(),
#                              temperature = numeric(),
#                              SNOW = numeric())
# 
# for(m in 1:12){
#   starttime = Sys.time()
#   load(file.path('Intermediate','Month_Frames', paste(state, year, m, 'month_frame_full.Rdata', sep = "_")))
#   training_frame <- training_frame %>% bind_rows(temp_train)
#   rm(temp_train)
#   gc()
#   timediff = Sys.time() - starttime
#   cat("Appended to master train frames for month ", m, ". ")
#   cat(round(timediff,2), attr(timediff, "unit"), "elapsed", "\n")
# }
# 
# save(list = c('training_frame'), file = file.path(intermediatedir,paste(state, year, "train_test_frames.RData", sep = "_")))
# 
# timediff = Sys.time() - top_time
# cat("Completed osm_query.R script. ", round(timediff,2), attr(timediff, "unit"), " elapsed in total.", "\n")

########

# m <- 1
# load(file.path('Intermediate','Month_Frames',paste0("month_frame_waze",m,".RData")))
# training_frame_rc <- temp_train
# rm(temp_train)

# load(file.path('Intermediate','Month_Frames',paste0("month_frame_waze",m,".RData")))

### Below is scratch

# total_crashes <- total_crashes %>%
#   mutate(hour_end = as.POSIXct(paste0(year,"-",
#                                         formatC(month, width = 2, flag = "0"),"-",
#                                         formatC(day, width = 2, flag = "0")," ",
#                                         hour,":","00:00"), tz = time_zone_name),
#          hours_prior = hour_end - hours(4)
#          )

# st_as_sf(coords = c("lon", "lat"), crs=projection)
# mean(st_length(state_network))
# hist(st_length(state_network))
# summary(st_length(state_network))

# define a function to add in start hour, end hour, and 4 hours prior to start hour based on year, month, hour, day ------------
# add_stamps <- function(x # object (dataframe, tibble, sf)
# ) {
#   if(!one_zone){
#     y <- x %>% 
#       st_join(timezone_adj, join = st_nearest_feature) %>% 
#       mutate(hour_start = as.POSIXct(paste0(year,"-",
#                                             formatC(month, width = 2, flag = "0"),"-",
#                                             formatC(day, width = 2, flag = "0")," ",
#                                             formatC(hour, width = 2, flag = "0"),
#                                             ":","00:00"
#       ), 
#       tz = tz_name
#       )
#       )
#   } else {
#     y <- x %>% 
#       mutate(hour_start = as.POSIXct(paste0(year,"-",
#                                             formatC(month, width = 2, flag = "0"),"-",
#                                             formatC(day, width = 2, flag = "0")," ",
#                                             formatC(hour, width = 2, flag = "0"),
#                                             ":","00:00"
#       ), 
#       tz = time_zone_name
#       )
#       )
#   } # end of if clause
#   y$hours_prior <- y$hour_start + hours(-4)
#   y$hour_end <- y$hour_start + hours(1)
#   return(y)
# } # end of function

##### OLD VERSION

## Set up monthly training and test dataframes ----------------------

# # Switching to operate month-by-month to avoid running out of memory 
# # when trying to work with an entire year all at the same time.
# monthIDs <- formatC(1:12, width = 2, flag = "0")
# yearmonths <- c(paste(year, monthIDs, sep="-"))
# yearmonths.1 <- paste(yearmonths, "01", sep="-")
# lastdays <- days_in_month(as.POSIXct(yearmonths.1)) # from lubridate
# yearmonths.end <- paste(yearmonths, lastdays, sep="-")
# 
# # uncomment for testing
# #m <- 1
# 
# # set seed for drawing samples, so team members will have replicable results for comparison
# set.seed(46)
# 
# # buffer <- st_buffer(state_network, dist = 50)
# #ggplot() + geom_sf(data = buffer)
# 
# for (m in 1:12){
#   starttime = Sys.time()
#   num_days = as.numeric(lastdays[m])
#   month = as.numeric(rep(monthIDs[m],times = 24 * num_days))
#   day = seq(from=1, to=num_days, by=1)
#   #day = formatC(day, width = 2, flag = "0")
#   day = rep(day, each=24)
#   hour = rep(0:23,times=num_days)
#   #hour = formatC(hour, width = 2, flag = "0")
#   dates <- data.frame(month=month,day=day,hour=hour)
#   
#   # generate TRAINING sample for the month
#   train_sample <- sample(x=nrow(dates), size = nrow(dates)*training_proportion)
#   dates_train <- dates[train_sample,]
#   # replicate it, multiplying by the number of road segments
#   dates_train_exp <- do.call(bind_rows, replicate(nrow(state_network), dates_train, simplify = FALSE))
#   
#   # combine date/time columns from sample with osm_id and then crashes
#   # expand road segment frame, multiplying by the number of date/time training samples
#   temp_train <- do.call(bind_rows, replicate(nrow(dates_train), state_network %>% st_drop_geometry(), simplify = FALSE)) %>% 
#     arrange(osm_id) %>%
#     # merge with expanded version of date/time training frame
#     cbind(dates_train_exp, row.names=NULL) %>%
#     # bring in crash data
#     left_join(total_crashes, by = c('osm_id', 'month', 'day', 'hour')) %>%
#     mutate(crash = ifelse(is.na(crash), 0, crash))
#   
#   # generate TEST sample for the month
#   # only select from among the rows that were NOT in training sample
#   remaining <- dates[-train_sample,]
#   test_sample <- sample(x=nrow(remaining), size = nrow(remaining)*test_proportion)
#   dates_test <- remaining[test_sample,]
#   # replicate it, multiplying by the number of road segments
#   dates_test_exp <- do.call(bind_rows, replicate(nrow(state_network), dates_test, simplify = FALSE))
#   
#   # combine date/time columns from sample with osm_id and then crashes
#   # expand road segment frame, multiplying by the number of date/time training samples
#   temp_test <- do.call(bind_rows, replicate(nrow(dates_test), state_network %>% st_drop_geometry(), simplify = FALSE)) %>% 
#     arrange(osm_id) %>% 
#     # merge with expanded version of date/time test frame
#     cbind(dates_test_exp, row.names=NULL) %>%
#     # bring in crash data
#     left_join(total_crashes, by = c('osm_id', 'month', 'day', 'hour')) %>%
#     mutate(crash = ifelse(is.na(crash), 0, crash))
#   
#   # save the objects
#   save(list = c('dates','temp_train','temp_test'), file = file.path(intermediatedir,'Month_Frames',paste0("month_frame_",m,".RData")))
#   timediff = Sys.time() - starttime
#   
#   # clear memory
#   rm(num_days, month, day, hour, dates, dates_test, dates_test_exp, dates_train, dates_train_exp, remaining, temp_test, temp_train, train_sample, test_sample)
#   
#   cat("Created training and test data frames with date/time, road segments, and crashes for month ", m, ". ")
#   cat(round(timediff,2), attr(timediff, "unit"), "elapsed", "\n")
# }

#######
# start_date <- as.POSIXct(c(paste(year, 01, 01, sep="-")))
# end_date <- as.POSIXct(c(paste(year, 12, 31, sep="-")))
# 
# dates <- seq(from = start_date, to = end_date, by = "day")
# # replicate it, multiplying by the number of road segments
# dates_exp <- replicate(nrow(state_network), dates, simplify = FALSE)

# num_days = as.numeric(lastdays[m])
# month = as.numeric(rep(monthIDs[m],times = 24 * num_days))
# day = seq(from=1, to=num_days, by=1)
# #day = formatC(day, width = 2, flag = "0")
# day = rep(day, each=24)
# hour = rep(0:23,times=num_days)
# #hour = formatC(hour, width = 2, flag = "0")
# dates <- data.frame(month=month,day=day,hour=hour)
