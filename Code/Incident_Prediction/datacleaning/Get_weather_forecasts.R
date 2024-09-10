# Get weather forecasts for state of interest

# Setup ----

source('utility/get_packages.R')

library(ggmap)
library(httr) # for GET
library(tidyverse)
library(xml2) # for xml parsing in tidy way
library(XML) # for xmlToList
library(jsonlite)
library(tigris) # for state shapefiles (or other census geographies)
library(sf)

# Get weather wx_data ----

# The below is a more generalized approach to set the points for which to query weather data 
# (based on state of interest). Method seems to work for all states except for Alaska. It does work
# for Hawaii, but only puts 2 points there.
# Eventual TO - DO item - figure out why it doesn't work for Alaska and fix it.

## Setting directories, state, year, projection, etc., here for now, but should move the setting of these parameters 
## to main parent script/location later
## (i.e. set them in analysis/PredictWeek.R script)

inputdir <- file.path(getwd(),"Input")
outputdir<- file.path(getwd(),"Output")

state <- "WA"

year <- 2021

# Indicate whether the state has a unified time zone
one_zone <-TRUE
# If one_zone is set to T or TRUE, meaning that the state has one time zone, specify the name of the time zone, selecting from 
# among the options provided after running the first line below (OlsonNames())

# OlsonNames()
# time_zone_name <- "US/Central"
time_zone_name <- "US/Pacific"

# projection used in training is epsg 5070, so using that here as well. 
projection <- 5070

# The coordinate reference system for the TomorrowIO API is WGS84 (epsg code 4326)
api_crs <- 4326

#Timezones --------------------------------------------------------------
onSDC <- T

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
#-----------------------------------------------------------------------

state_osm <- ifelse(state == "WA", 'Washington State',
                    ifelse(state == "MN", "Minnesota", NA))

state_osm <- gsub(" ", "_", state_osm) # Normalize state name

########
# Access state boundary using tigris package, which loads Census TIGER/Line Shapefiles
# default coordinate reference system for this is EPSG 4269 for all states, including Hawaii and Alaska, however - 
# in the below code converting this immediately to the coordinate reference system for the TomorrowIO API,
# since we will be using this to form the API queries. Ultimately, we then convert the outputs from this script 
# to the default projection that we've been using (epsg 5070, defined above as 'projection')

boundary_file <- paste0(state_osm,"_boundary")
file_path <- file.path(inputdir,'Roads_Boundary', state_osm, paste0(boundary_file, '.gpkg'), paste0(boundary_file,'.shp'))

if (file.exists(file.path(file_path))){
  print("State boundary file found")
  state_map <- read_sf(file_path) %>% st_transform(crs = api_crs)
} else{
  if (state_osm == 'Washington_State'){
    state_border <- 'Washington'
  }else{
    state_border <- state_osm
  }
  state_map <- states(cb = TRUE, year = 2021) %>%
    filter_state(state_border) %>%
    st_transform(crs = api_crs)
  }

# Overlay a grid of points within the state - current resolution is 1 degree by 1 degree - 
# This is imperfect because the distances between longitude degrees are greater as one 
# moves closer to the equator and smaller as one moves closer to one of the poles.
# Close enough for our purposes as it puts about the right number of query points in each 
# state.
grd <- state_map %>% 
  st_make_grid(cellsize = c(1,1), what = "centers") %>% 
  st_intersection(state_map) 

# Uncomment the below code to view the grid overlay
# ggplot() +
#   geom_sf(data = state_map, aes(), fill = NA, alpha = 1) +
#   geom_sf(data = grd, aes())

# Create dataframe with the longitude and latitude of the grid points.
queries <- st_coordinates(grd) %>% as.data.frame() %>% 
  mutate(ID = row.names(.))

# Using TomorrowIO now for the actual API call (can add NOAA GFS as an option later)
TomorrowIO = T
if(TomorrowIO) {
  # User needs to visit the tomorrow io website to obtain a free account and then put their api key into
  # a text file called "WeatherAPI_key.txt" in the "Weather" folder within the "Input" folder.
  # Next line obtains the user's api key.
  w_key = scan(file.path(inputdir,"Weather", 'WeatherAPI_key.txt'), what = 'character')
  # This is the format needed for an API call in TomorrowIO using "Forecast" endpoint, with four parameters that
  # need to be filled in (latitude, longitude, API key, units):
  # https://api.tomorrow.io/v4/weather/forecast?location={latitude},{longitude}&apikey={key}&units={imperial/metric}
  # there are additional parameters that the user can specify - view this page and click on one of the 'recipes':
  # https://docs.tomorrow.io/reference/weather-forecast
  # If additional flexibility/customizability is needed, one can use the "Timelines" endpoint instead - 
  # View this page and click on one of the 'recipes' for detail: https://docs.tomorrow.io/reference/post-timelines
  # There is also the "Realtime" endpoint: https://docs.tomorrow.io/reference/realtime-weather
  
  # Next line appends a column to the queries dataframe with the constructed url for each query.
  queries = queries %>% 
    mutate(url = paste0('https://api.tomorrow.io/v4/weather/forecast?location=',Y,',',X,'&apikey=', w_key,'&units=imperial'))
  
  # Daily data are available for 6 days (including the current day, so only 5 future days)
  # Hourly data are available for 120 hours in the future (i.e. 5 future days), so about the 
  # same timeframe. We can discuss whether we want to shift to use hourly data instead of daily.
  # By default, time zone is Coordinated Universal Time (UTC)... the offset will vary by state
  # based on the time zone(s). May also vary based on whether it is daylight savings or standard time?
   
  # TO-DO: figure out what to do for the larger states (limit for TomorrowIO is 25 requests per hour,
  # so grd object created above should not have more than 25 points).
  
  # create blank dataframes to populate with weather data
   
  
  weather_hourly <- data.frame(ID = as.character(),
                               lon = numeric(),
                               lat = numeric(),
                               utc_hour = POSIXct(), 
                               temperature = numeric(),
                               snowAccumulation = numeric(),
                               rainAccumulation = numeric())
  
  # uncomment this for testing/development 
  # i <- 1
  
  # Loop over the json queries for each forecast point and add to the data frames (daily and hourly)
  # Pause for 0.34 seconds in between each query because the limit is 3 requests per second
  # Note there are also limits of 25 requests per hour and 500 requests per day.
   
  for(i in 1:nrow(queries)){
    wx_dat_i = fromJSON(queries$url[i])
    
    # extract the weather attributes for the six days as a dataframe
    # wx_dat_daily_values = wx_dat_i$timelines$daily$values %>%
      # extract the dates and assign to a new column
     # mutate(date = as.Date(wx_dat_i$timelines$daily$time),
            # lon = queries$X[i],
            # lat = queries$Y[i]) %>%
      # subset to only retain the necessary columns
     # select(lon, lat, date, temperatureMin,temperatureMax,snowAccumulationSum, rainAccumulationSum, sleetAccumulationLweSum,iceAccumulationSum)
    
    # extract the weather attributes for the 120 hours as a dataframe
    wx_dat_hourly_values = wx_dat_i$timelines$hourly$values %>% 
      # extract the hours and assign to a new column
      mutate(utc_hour = ymd_hms(wx_dat_i$timelines$hourly$time),
             ID = queries$ID[i],
             lon = queries$X[i],
             lat = queries$Y[i]) %>%
      # subset to only retain the necessary columns - Joey removed sleet and ice
      select(ID, lon, lat, utc_hour,temperature,snowAccumulation, rainAccumulation)#, sleetAccumulationLwe,iceAccumulation)
    
    # add forecasts for this point to the data frames along with the rest of the points
    weather_hourly <- rbind(weather_hourly,wx_dat_hourly_values)
    
    Sys.sleep(0.34)
  }

  rm(wx_dat_hourly_values)

  
  weather_hourly.proj <- st_as_sf(weather_hourly,
                                  coords = c('lon', 'lat'),
                                  crs = api_crs) %>% st_transform(crs=projection)

  # Set local time based on time zone and the UTC hour
  if(!one_zone){
    weather_hourly.proj <- weather_hourly.proj %>% st_join(timezone_adj, join = st_nearest_feature) %>% 
      mutate(time_local = as.POSIXct(utc_hour, tz = tz_name))
  } else {weather_hourly.proj <- weather_hourly.proj %>% mutate(time_local = as.POSIXct(utc_hour, tz = time_zone_name))}
  
  # Save forecasts ----
  fn = paste0("Weather_Forecasts_", state, "_", Sys.Date(), ".RData")

  save(list=c('weather_hourly', 'weather_hourly.proj'),
       file = file.path(inputdir, "Weather", fn))
}