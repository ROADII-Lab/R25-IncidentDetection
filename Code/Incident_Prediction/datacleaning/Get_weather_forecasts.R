# Get weather forecasts for state of interest

# Setup ----
inputdir <- file.path(getwd(),"Input")
outputdir<- file.path(getwd(),"Output")

source('utility/get_packages.R')
ON_SDC = F
if(ON_SDC){
  teambucket <- "s3://prod-sdc-sdi-911061262852-us-east-1-bucket"
  user <- file.path( "/home", system("whoami", intern = TRUE)) # the user directory 
}

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
# (based on state of interest). For TN it comes up with 14 points, whereas the original version for TN had
# 11 points throughout the state. Method seems to work for all states except for Alaska. It does work
# for Hawaii, but only puts 2 points there.
# Eventual TO - DO item - figure out why it doesn't work for Alaska and fix it.

## Setting State here for now, but will likely move to main script/location later
state <- "Tennessee"

# Access state boundary using tigris package, which loads Census TIGER/Line Shapefiles
state_map <- states(cb = TRUE, year = 2021) %>%
  filter_state(state)
# default projection from the above is EPSG 4269 for all states, including Hawaii and Alaska

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
queries <- st_coordinates(grd) %>% as.data.frame()

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
   
  weather_daily <- data.frame(lon = numeric(),
                              lat = numeric(),
                              date = Date(), 
                              temperatureMin = numeric(),
                              temperatureMax = numeric(),
                              snowAccumulationSum = numeric(),
                              rainAccumulationSum = numeric(),
                              sleetAccumulationLweSum = numeric(),
                              iceAccumulationLweSum = numeric())
  
  weather_hourly <- data.frame(lon = numeric(),
                               lat = numeric(),
                               utc_hour = POSIXct(), 
                               temperature = numeric(),
                               snowAccumulation = numeric(),
                               rainAccumulation = numeric(),
                               sleetAccumulationLwe = numeric(),
                               iceAccumulation = numeric())
  
  # uncomment this for testing/development 
  # i <- 1
  
  # Loop over the json queries for each forecast point and add to the data frames (daily and hourly)
  # Pause for 0.34 seconds in between each query because the limit is 3 requests per second
  # Note there are also limits of 25 requests per hour and 500 requests per day.
   
  for(i in 1:nrow(queries)){
    wx_dat_i = fromJSON(queries$url[i])
    
    # extract the weather attributes for the six days as a dataframe
    wx_dat_daily_values = wx_dat_i$timelines$daily$values %>%
      # extract the dates and assign to a new column
      mutate(date = as.Date(wx_dat_i$timelines$daily$time),
             lon = queries$X[i],
             lat = queries$Y[i]) %>%
      # subset to only retain the necessary columns
      select(lon, lat, date, temperatureMin,temperatureMax,snowAccumulationSum, rainAccumulationSum, sleetAccumulationLweSum,iceAccumulationSum)
    
    # extract the weather attributes for the 120 hours as a dataframe
    wx_dat_hourly_values = wx_dat_i$timelines$hourly$values %>% 
      # extract the hours and assign to a new column
      mutate(utc_hour = ymd_hms(wx_dat_i$timelines$hourly$time),
             lon = queries$X[i],
             lat = queries$Y[i]) %>%
      # subset to only retain the necessary columns
      select(lon, lat, utc_hour,temperature,snowAccumulation, rainAccumulation, sleetAccumulationLwe,iceAccumulation)
    
    # add forecasts for this point to the data frames along with the rest of the points
    weather_daily <- rbind(weather_daily,wx_dat_daily_values)
    weather_hourly <- rbind(weather_hourly,wx_dat_hourly_values)
    
    Sys.sleep(0.34)
  }

  rm(list=c("wx_dat_daily_values","wx_dat_hourly_values"))
  
  ## TO-DO: need generic method for automatically getting/setting time zone. What to do if state
  ## crosses multiple time zones?
  # By default, TomorrowIO reported time zone is Coordinated Universal Time (UTC).

  weather_daily.proj <- st_as_sf(weather_daily,
                                 coords = c('lon', 'lat'),
                                 crs = 4269)
  
  weather_hourly.proj <- st_as_sf(weather_hourly,
                                  coords = c('lon', 'lat'),
                                  crs = 4269)

  # CRS("+proj=longlat +datum=WGS84")

  # Read shapefile with time zone IDs to indicate which areas belong to which time zones
  tz <- read_sf(file.path(inputdir,"Shapefiles", 'TimeZone'), layer = 'combined-shapefile')  
  tz <- st_transform(tz, crs = 4269)

  # Assign time zone ID ('tzid') to each row for daily and hourly based on intersection with the time zones shapefile, 'tz'
  weather_daily.proj <- weather_daily.proj %>% st_join(tz[,"tzid"])
  weather_hourly.proj <- weather_hourly.proj %>% st_join(tz[,"tzid"])

  # Set local time based on time zone ID and the UTC hour
  weather_hourly.proj$local_time <- with_tz(weather_hourly.proj$utc_hour, weather_hourly.proj$tzid)

  # TO-DO: incorporate daylight savings time.
  # If the state observes daylight savings time, and if the date/time of an observation is between
  # The second Sunday in March and the first Sunday in November, then adjust the local time to be one hour later.
  # The only two states that do not observe daylight savings time are Arizona and Hawaii, as of March 2024.

  # ggplot() +
  #   geom_sf(data=tz, aes(), fill = NA) +
  #   geom_sf(data = state_map, aes(), fill = NA, alpha = 1) +
  #   geom_sf(data = weather_daily.proj, aes(color=tzid))+
  #   coord_sf(xlim = c(-80, -91), ylim = c(34.9, 36.7), expand = FALSE)+
  #   theme_minimal()
  
  # Save forecasts ----
  fn = paste0("TN_Forecasts_", Sys.Date(), ".RData")

  save(list=c('weather_daily', 'weather_daily.proj','weather_hourly', 'weather_hourly.proj'),
       file = file.path(inputdir, "Weather", fn))
  if(ON_SDC){
    # Copy to S3
    system(paste("aws s3 cp",
                 file.path(inputdir, "Weather", fn),
                 file.path(teambucket, "TN", "Weather", fn)))
  }
}
  
# Next steps: interpolate to state level using kriging or other methods, see 
# http://rspatial.org/analsis/rst/4-interpolation.html

