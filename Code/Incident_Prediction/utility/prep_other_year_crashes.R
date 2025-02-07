
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
# split into month by month frames
years <- unique(total_crashes$YEAR)
if(!dir.exists(file.path(intermediatedir,'Month_Frames'))) { dir.create(file.path(intermediatedir,'Month_Frames')) }

for(y in years){
  temp_train = total_crashes %>% filter(YEAR = y)
  # save the temp object
  save(list = c('temp_train'), file = file.path(intermediatedir,'Month_Frames',paste(state, y, "month_frame_crashes_only", m,".RData", sep = "_")))
}

# uncomment for testing
m <- 1
y <- 2020

rm(datalist, crash_files, file_path, i, l)
gc()
#load(file = file.path(intermediatedir,'Month_Frames',paste(state, year, "month_frame", m,".RData", sep = "_")))

## waze data -----------------------------------------------

# If applicable, load CAD data for MN so as to join it in
if((year %in% c(2018,2019,2020)) & (state == "MN")){
  source('utility/MN_CAD_load.R')
}

# establish set of waze files that we'll be reading in
waze.files <- list.files(path = waze_dir, pattern = "\\.csv$")

waze.jams.files <- list.files(path = waze_jams_dir, pattern = "\\.csv$")

for(y in years){

waze.files.year = file.path(inputdir, "Waze", state, waze.files[grep(as.character(y), waze.files)])

waze.jams.files.year = file.path(waze_jams_dir, waze.jams.files[grep(as.character(y), waze.jams.files)])

# uncomment for testing

# note that the waze_query.R script already converted to local time. If waze data were sourced in some other way, may need to
# uncomment the parts that convert to local time below, from UTC.

for(m in 1:12){
  starttime = Sys.time()
  load(file.path(intermediatedir,'Month_Frames',paste(state, y, "month_frame_crashes_only", m,".RData", sep = "_")))
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
  
  # # if jams data are available, read them in for that month
  # if(length(waze.jams.files.year) > 0){
  #   waze_temp = read.csv(waze.jams.files.year[m]) %>%
  #     rename(month = month_local,
  #            day = day_local,
  #            hour = hour_local) %>%
  #     mutate(osm_id = as.character(osm_id))
  #   
  #   temp_train = temp_train %>% 
  #     left_join(waze_temp %>% select(!pub_utc_timestamp_mean), by = c('osm_id', 'month', 'day', 'hour')) %>%
  #     replace_na(list(level_mode = 0)) %>%
  #     rename(jam_level = level_mode)
  # }
  # # create the imputed averages - two different versions depending on whether there are data on jams level
  # if(length(waze.jams.files.year) > 0){
  #   waze_averages <- temp_train %>%
  #     group_by(osm_id, month, weekday, hour) %>%
  #     summarize(average_jams = mean(JAM),
  #               average_weather = mean(WEATHERHAZARD),
  #               average_closure = mean(ROAD_CLOSED),
  #               average_accident = mean(ACCIDENT),
  #               average_jam_level = mean(jam_level))
  #   
  # } else {
    waze_averages <- temp_train %>%
      group_by(osm_id, month, weekday, hour) %>%
      summarize(average_jams = mean(JAM),
                average_weather = mean(WEATHERHAZARD),
                average_closure = mean(ROAD_CLOSED),
                average_accident = mean(ACCIDENT))
  #}
  
  # thin data, if needed, to avoid running out of memory when running Join_Road_Weather.R script
  if(!is.null(keep_prop)){
    temp_train = temp_train[sample(1:nrow(temp_train), size = nrow(temp_train)*keep_prop),]
  }
  
  # save the objects
  save(list = c('temp_train'), file = file.path(intermediatedir,'Month_Frames',paste(state, y, "month_frame_waze_crash_only", m,".RData", sep = "_")))
  save(waze_averages, file = file.path(intermediatedir,'Month_Frames',paste(state, y, "month_frame_imputed_waze_crash_only", m,".RData", sep = "_")))
  timediff = Sys.time() - starttime
  
  # clear memory
  rm(temp_train, waze_temp, waze_averages)
  gc()
  cat("Added waze data for other years (just crash) for month ", m, ", year ", y, ". ")
  cat(round(timediff,2), attr(timediff, "unit"), "elapsed", "\n")
}
dif_time <- round(difftime(Sys.time(), top_time, units = "mins"), 2)
cat(paste0("Created all month frames with crashes and waze data. ", dif_time, " minutes elapsed thus far in total."))
cat("Now adding weather.")

gc()

# Run Weather Script ------------------------------------------------------
source(file.path("Utility", "Join_Road_weather.R"))

#### Now apply the join_road_weather function for each month #####
timeA <- Sys.time()

for(m in 1:12){
  gc()
  innertime = Sys.time()
  
  # Do first half of the month first
  load(file.path(intermediatedir,'Month_Frames',paste(state, y, "month_frame_waze_crash_only", m,".RData", sep = "_")))
  
  temp_train = temp_train %>% filter(day <= 15)
  gc()
  
  temp_trainA = join_road_weather(temp_train)
  rm(temp_train)
  gc()
  
  save(temp_trainA, file = file.path(intermediatedir, 'Month_Frames', paste(state, y, m, 'month_frame_full_A_crash_only.Rdata', sep = "_")))
  
  rm(temp_trainA)
  
  gc()
  
  # then do second half of the month
  load(file.path(intermediatedir,'Month_Frames',paste(state, y, "month_frame_waze_crash_only", m,".RData", sep = "_")))
  
  temp_train = temp_train %>% filter(day > 15)
  gc()
  
  temp_train = join_road_weather(temp_train)
  
  gc()
  
  # load back up the first half of the month
  load(file.path(intermediatedir, 'Month_Frames', paste(state, y, m, 'month_frame_full_A_crash_only.Rdata', sep = "_")))
  
  temp_train = bind_rows(temp_trainA, temp_train)
  
  save(temp_train, file = file.path(intermediatedir, 'Month_Frames', paste(state, y, m, 'month_frame_full_crash_only.Rdata', sep = "_")))
  
  rm(temp_train, temp_trainA)
  
  gc()
  
  dif_time <- round(difftime(Sys.time(), innertime, units = "mins"), 2)
  cat(paste0("Added weather for month ", m, ", year ", y, ", in ", dif_time, " minutes."))
  
  dif_time <- round(difftime(Sys.time(), top_time, units = "mins"), 2)
  cat(paste0(dif_time, " minutes elapsed thus far in total in the creation of training/test frames.\n\n"))
}

timeB <- Sys.time()
dif_time <- round(difftime(timeB, timeA, units = "mins"), 2)
cat(paste0("Weather Script took ", dif_time, " minutes."))
rm(timeA, timeB, dif_time)

# Remove variables and gc() before moving on
rm(KNN, precip, road_points, snow, state_network_KNN, state_stations, stations, stations_KNN, temp, temp_df, temp_df_c, working_df, working_list, wx, xy, overall_time, precip_time, snow_time, starttime,
   temp_time, year2, timezone_adj, tz_adjustments, US_timezones)

gc()
}