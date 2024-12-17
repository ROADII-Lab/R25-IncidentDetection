
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

i <- "D:/Documents/Andrew/R25-IncidentDetection/Code/Incident_Prediction/Input/Crash/Washington_State/wa20crash.shp"

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
        mutate(time_local = as.POSIXct(ACC_DATE, format = ifelse(grepl("20", i),'%m/%d/%Y %H%M', '%Y-%m-%d %H%M'), tz = tz_name))
    } else {temp <- temp %>% mutate(time_local = as.POSIXct(ACC_DATE, format = ifelse(grepl("20", i),'%m/%d/%Y %H%M', '%Y-%m-%d %H%M'), tz = time_zone_name))}
    
    temp <- temp %>% select(time_local, crashID)
    
    datalist[[l]] <- temp
    
    timediff = Sys.time() - starttime
    cat("Loaded ",i,"... ")
    cat(round(timediff,2), attr(timediff, "unit"), "elapsed", "\n")
    rm(temp)
  }
}

is_weekday <- function(timestamps){lubridate::wday(timestamps, week_start = 1) < 6}

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
  filter(YEAR != year) %>%
  st_join(state_network %>% select(osm_id), join = st_nearest_feature) %>%
  # removing geometry to speed group by operation and limit use of memory. Will add geometry back in later, but it will be the geometry for
  # the road segment, not the geometry for the crash point, because there are sometimes multiple crashes per observation (row)
  st_drop_geometry() %>%
  select(osm_id, YEAR, month, day, hour, crash) %>%
  group_by(osm_id, YEAR, month, day, hour) %>%
  summarise(crash = sum(crash)) %>%
  ungroup() %>%
  mutate(day_of_week = lubridate::wday(dates, label = TRUE),
         weekday = is_weekday(dates))
  
  # convert day_of_week into unordered since we want to treat it as a nominal variable. 
  total_crashes$day_of_week <- factor(total_crashes$day_of_week, ordered = FALSE)

timediff = Sys.time() - starttime
cat("Created total_crashes for other years")
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