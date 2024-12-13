# Info --------------------------------------------------------------------
# Purpose: Merges hourly weather data with road segments. 
# Author: Joey Reed (joseph.reed1@dot.gov)
# Last edited: 3/25/2024

# Notes: 
# Some crazy high numbers for precip and temp. 
# There is still a lot of NAs for snow when doing it weekly
# Need more weather data

# Prep --------------------------------------------------------------------

# Load packages 
library(data.table) # contains FREAD
library(RANN)
library(dplyr)
library(sf)
library(stringr)

### Prep for joining ###

# Loading Hourly Data ------------------------------------------------------------

## Load Station Line --------------------------------------------------------

# Read in list of GHCN hourly stations
stations <- read.csv(file=file.path(inputdir, "Weather","GHCN", "Hourly","ghcnh-station-list.csv"), 
                     header = FALSE, strip.white = TRUE)
stations <- stations[,1:6]
colnames(stations) <- c('Station_ID','Latitude','Longitude','Elevation','State','Station_name') 

# Filter down to the list of stations in the state of interest
state_stations <- stations[stations$State==state,]

## Hourly Nearest Neighbor --------------------------------------------------------

road_points <- state_network %>% st_cast("POINT") %>% group_by(osm_id) %>% slice_head(n=1) %>% arrange(osm_id)

state_stations_sf <- state_stations %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>%
  st_transform(crs = projection) %>%
  mutate(nrow = row_number())

sf_use_s2(F)

gc()

load_statehourly <- function(year_var){ 
  
  file_name <- file.path(intermediatedir, paste0(state, "_", year_var, "_hourlyweather.RData"))
  
  if(file.exists(file_name)){ # if data is already aggregated, save time and just load the data 
    state_hourly_hist_weather <- readRDS(file_name)
    return(state_hourly_hist_weather)
  }
  
  
  # Filter down to the list of stations for which we have hourly data in the year of interest
  available <- logical(nrow(state_stations))
  for (i in 1:nrow(state_stations)) {
    ID = state_stations[i,'Station_ID']
    filename = paste0('GHCNh_',ID,'_',year_var,'.psv')
    print(filename) # check what it is checking
    ifelse(file.exists(file.path(inputdir, "Weather", "GHCN", "Hourly", year_var, state, filename)
    ),
    available[i] <- TRUE,
    available[i] <- FALSE
    )
  }
  hr_state_stations <- state_stations[available,]
  
  # Initialize empty dataframe to store the hourly data from the files
  state_hourly_hist_weather <- data.frame(Station_ID=character(),
                                          Station_name=character(),
                                          Year=integer(),
                                          Month=integer(),
                                          Day=integer(),
                                          Hour=integer(),
                                          Latitude=numeric(),
                                          Longitude=numeric(),
                                          Elevation=numeric(),
                                          temperature=numeric(),
                                          precipitation=numeric(),
                                          snow_depth=numeric()
  )
  
  # Read in the data from each station file for the state and compile
  for (i in 1:nrow(hr_state_stations)){
    ID = hr_state_stations[i,'Station_ID']
    filename = paste0('GHCNh_',ID,'_',year_var,'.psv')
    df_i = read.table(file.path(inputdir, "Weather", "GHCN", "Hourly", year_var, state, filename),
                      header=TRUE,
                      sep = "|",
                      fill = TRUE
    ) %>% dplyr::select('Station_ID',
                        'Station_name', 
                        'Year', 
                        'Month', 
                        'Day', 
                        'Hour', 
                        'Latitude', 
                        'Longitude', 
                        'Elevation', 
                        'temperature', 
                        'precipitation', 
                        'snow_depth')
    state_hourly_hist_weather <- rbind(state_hourly_hist_weather,df_i)
  }
  
  saveRDS(state_hourly_hist_weather, file_name)
  
  return(state_hourly_hist_weather)
}

state_hourly_hist_weather1 <- load_statehourly(year_var = year)

# calculate stations timezone adjustments 
tz_adjustments <- stations %>% filter(State == state) %>% 
  distinct(Station_ID, .keep_all = T) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>% # these are in crs 4326, so need to load, then convert 
  st_transform(crs = projection) %>% 
  st_join(timezone_adj, join=st_intersects) %>% 
  st_drop_geometry() %>%
  select(Station_ID, adjustment)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

tz_adjust <- getmode(tz_adjustments$adjustment)

if(tz_adjust<0){ # if the timezone is < 0 
  
  year2 = year+1
  
}

if(tz_adjust>0){
  
  year2 = year-1
  
}

state_hourly_hist_weather2 <- load_statehourly(year_var = year2)

stations_KNN <- state_stations_sf %>% st_drop_geometry() %>%
  select("Station_ID", "nrow")

wx <- state_hourly_hist_weather1 %>% bind_rows(state_hourly_hist_weather2) %>% 
  left_join(tz_adjustments, by="Station_ID") %>%
  mutate(Date = ymd_h(paste0(Year, "-", Month, "-", Day, " ", Hour))+hours(adjustment),
         #y_day = yday(Date), new approach doesn't use y_day for hourly
         Hour = hour(Date),
         Day = day(Date),
         Month = month(Date),
         Year = year(Date)) %>% 
  filter(Year==year) %>% 
  left_join(stations_KNN, by="Station_ID") %>%
  group_by(Year, Month, Day, Hour, nrow) %>% # variables to keep 
  summarise(precipitation = mean(precipitation, na.rm = TRUE), # gets rid of duplicate hour reports for a station
            temperature = mean(temperature, na.rm = TRUE),
            snow_depth = max(snow_depth)) %>% # max makes sense for depth right? 
  mutate_all(~ifelse(is.nan(.), NA, .)) 

# Loading Daily Data ------------------------------------------------------

load_statedaily <- function(year_var){
  
  file_list <- list.files(path = file.path(inputdir, "Weather","GHCN", "Daily", year_var), pattern = paste0("^", state))
  df_list <- list()
  df_list = vector("list", length = length(file_list))
  
  for(i in 1:length(file_list)){
    print(i)
    df_list[[i]] <- fread(file.path(inputdir, "Weather","GHCN", "Daily", year_var, file_list[[i]]))
  }
  state_daily_hist_weather <- do.call(bind_rows, df_list)
  
  return(state_daily_hist_weather)
}

state_daily_hist_weather1 <- load_statedaily(year_var = year)
start.time = Sys.time()
if(year2 > year){
  
  state_daily_hist_weather1 <- state_daily_hist_weather1 %>% 
    mutate(Date = ymd(DATE),
           Day = yday(Date)) %>%
    select(Day, SNOW, LONGITUDE, LATITUDE) 
  
  state_daily_hist_weather2 <- load_statedaily(year_var = year2)  
  
  state_daily_hist_weather2 <- state_daily_hist_weather2 %>% 
    mutate(Date = ymd(DATE),
           Day = yday(Date)+365) %>% 
    filter(Day == 366) %>% # only care about day 1 of Year 2
    select(Day, SNOW, LONGITUDE, LATITUDE) 
  
  xy <- state_daily_hist_weather1 %>% 
    rbind(state_daily_hist_weather2) %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs=4326) %>%
    st_transform(crs = projection) 
  
} else{ # if in Guam - needs to be fixed, no time as of now 
  
  
}


# Remove variables and gc() before next run
rm(state_daily_hist_weather1, state_daily_hist_weather2, state_hourly_hist_weather1, state_hourly_hist_weather2)
gc()

####### Define function to join the weather for a given data frame (will be applied month-by-month) ######

join_road_weather <- function(training_frame_rc){
  
#training_frame_rc <- training_frame
#colnames(training_frame_rc)[2:4] <- str_to_title(colnames(training_frame_rc[,2:4]))

lookup <- c(Month = "month", Day = "day", Hour = "hour")
training_frame_rc <- training_frame_rc %>% rename(all_of(lookup))

# k_hourly <- nrow(state_stations_sf)
k_hourly <- 20
KNN <- as.data.frame(nn2(st_coordinates(state_stations_sf), st_coordinates(road_points),  k=k_hourly, treetype = "kd", searchtype = "standard")$nn.id)

state_network_KNN <- state_network %>% arrange(osm_id) %>% st_drop_geometry() %>%
  bind_cols(KNN) #%>% select(-highway, -ref)

training_frame_rc <- training_frame_rc %>% st_drop_geometry() %>% 
  left_join(state_network_KNN, join_by(osm_id))

rm(KNN, state_network_KNN)

## Followed up with Matthew Menne at NOAA to understand how the date assignment works.
## He said the observation times for daily summaries tend to be local midnight for airport sites, 
## and local morning for many Cooperative and CoCoRaHS observers.  
## Based on his reply, we could consider commenting out the below, which attempted to adjust assuming that the
## day assignment was based on the UTC day, and instead simply assign N_Day as the original day.

if(year2 > year){
# this approach is pretty slow but I'm not sure how else to do it.
training_frame_rc <- training_frame_rc %>% # create a column on the "real" Day adjusted for timezone
  mutate(Date = ymd_h(paste0(year, "-", Month, "-", Day, " ", Hour)),
         N_Day = case_when(as.numeric(Hour)+abs(tz_adjust)>23 ~ as.numeric(yday(Date))+1,
                           .default = as.numeric(yday(Date)))) %>%
  select(-Date)
} else { # if in Guam - needs to be fixed, no time as of now
  }
  
# training_frame_rc$N_Day <- lubridate::yday(training_frame_rc$Day)


# Merge with KNN ------------------------------------------------------------

temp_df <- training_frame_rc

starttime = Sys.time()
  
## Precipitation -----------------------------------------------------------
precip <- wx %>% data.frame() %>% 
  select(nrow, Day, Month, Hour, precipitation)

working_list <- list()
o <- 0
  
repeat{
  o = o + 1
  var_name <- paste0("V", o)
  if(o == k_hourly+1){ # if no data in any station
    temp_df <- temp_df %>% mutate(precipitation = 0) # assume precipitation is zero 
    working_list[[o]] <- temp_df
    break
  } 
  temp_df <- temp_df %>% left_join(precip, join_by(!!as.name(var_name) == nrow, Day, Month, Hour)) # join first closest station
  temp_df_c <- temp_df %>% filter(!is.na(precipitation)) # only keep matched data
  working_list[[o]] <- temp_df_c # save matched data
  temp_df <- temp_df %>% filter(is.na(precipitation)) %>% select(-precipitation) # create next dataframe to run through 
  if(nrow(temp_df)==0) break # break the loop if it's fully matched
}
temp_df <- do.call(bind_rows, working_list)

precip_time <- Sys.time()
dif_time <- round(difftime(precip_time, starttime, units = "mins"), 2)
cat(paste0("Precipitation took ", dif_time, " minutes."))

## Temperature -----------------------------------------------------------
temp <- wx %>% data.frame() %>% 
  select(nrow, Day, Month, Hour, temperature)

working_list <- list()
o <- 0

repeat{
  o = o + 1
  var_name <- paste0("V", o)
  if(o == k_hourly+1) { # if no data in any station
    temp_df <- temp_df %>% mutate(temperature = 9999) # assume temperature is 9999 as a missing value 
    working_list[[o]] <- temp_df
    break
  } 
  temp_df <- temp_df %>% left_join(temp, join_by(!!as.name(var_name) == nrow, Day, Month, Hour)) # join first closest station
  temp_df_c <- temp_df %>% filter(!is.na(temperature)) # only keep matched data
  working_list[[o]] <- temp_df_c # save matched data
  temp_df <- temp_df %>% filter(is.na(temperature)) %>% select(-temperature) # create next dataframe to run through 
  if(nrow(temp_df)==0) break # break the loop if it's fully matched
}
temp_df <- do.call(bind_rows, working_list)

temp_time <- Sys.time()
dif_time <- round(difftime(temp_time, precip_time, units = "mins"), 2)
cat(paste0("Temperature took ", dif_time, " minutes."))

if(max(temp_df$temperature)==9999){
  cat("One hour had no temperature data, fill with nearby data.")
  
  temp_df <- temp_df %>% 
    arrange(osm_id, Month, Day, Hour) %>% 
    mutate(temperature = ifelse(temperature==9999, # if temperature is NA
                                mean(c(lead(temperature), lag(temperature)), na.rm=T), # make it the average of the two nearest times
                                temperature)) # if not NA, keep as is
}


## Snow -----------------------------------------------------------
# ~ 79% of snow is NA
# there are also significantly more stations for some reason. 
# these two factors make the approach used for hourly not valid 
# as such, we do a NN calculation everyday based on non-NA stations. 
# It is assumed this would be significantly slower for hourly as the loop would be 24x larger and would have to be a nested loop 
# though the below was not tested for hourly 

temp_df <- temp_df %>% select(-starts_with("V")) # get rid of hourly KNN results 
  

#rm(state_network_KNN)

working_list <- list()
x<-1
for(x in unique(training_frame_rc$N_Day)){
  
  working_df <- temp_df %>% 
    filter(N_Day == x)
  
  if(nrow(working_df)==0){ # if no data for this day skip it
    } else{
      
      snow <- xy %>% 
        filter(Day == x & !is.na(SNOW)) %>% # weather day is in UTC already, day 1 is equal to N_Day 1 in 
        select(-Day)
  
  if(nrow(snow) == 0){ # if all weather stations are zero, assume day is zero
    
    working_df <- temp_df %>% 
      filter(N_Day == x) %>% # N_Day matches with weather data day
      mutate(SNOW = 0)
    
  } else{
    snow <- snow %>% 
      mutate(V1 = row_number())
    
    KNN <- as.data.frame(nn2(st_coordinates(snow), st_coordinates(road_points),  k=1, treetype = "kd", searchtype = "standard")$nn.id)
    
    state_network_KNN <- state_network %>% 
      st_drop_geometry() %>%
      bind_cols(KNN) 
    
    working_df <- working_df %>% 
      left_join(state_network_KNN, join_by(osm_id)) %>% 
      left_join(snow, join_by(V1)) %>% 
      select(-V1, -geometry)
    
  }
  
  working_list[[x]] <- working_df
  
    }
}

temp_df <- do.call(bind_rows, working_list)
training_frame_rc <- temp_df %>% select(-N_Day)

snow_time <- Sys.time()
dif_time <- round(difftime(snow_time, temp_time, units = "mins"), 2)
cat(paste0("Snow took ", dif_time, " minutes.\n"))

overall_time <- Sys.time()
dif_time <- round(difftime(overall_time, starttime, units = "mins"), 2)
cat(paste0("Precipitation, temperature, and snow took ", dif_time, " minutes overall for month ", m, ".\n\n"))

gc()
return(training_frame_rc)
}
