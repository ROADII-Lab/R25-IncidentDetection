
top_time <- Sys.time()

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

#i <- "Input/Crash/Washington_State/wa21crash.shp" 

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

## create historical crashes across all other years (excluding the year in question)
starttime = Sys.time()
hist_crashes <- do.call(bind_rows, datalist) %>%
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
  select(osm_id, crash) %>%
  group_by(osm_id) %>%
  summarise(crash = sum(crash)) %>%
  ungroup() %>%
  mutate(crash = ifelse(is.na(crash), 0, crash)) %>%
  rename(hist_crashes = crash)

hist_crashes <- state_network %>% 
  select(osm_id) %>% 
  st_drop_geometry() %>%
  left_join(hist_crashes, by = "osm_id") %>%
  replace_na(list(hist_crashes = 0))

timediff = Sys.time() - starttime
cat("Created historical crash data. ")
cat(round(timediff,2), attr(timediff, "unit"), "elapsed", "\n")