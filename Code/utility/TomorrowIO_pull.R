# Title: TomorrowIO Pull
# Purpose: Pulls TomorrowIO data and processes it.
# Generated Variables: weather_points, weather_points.proj

# Check if Ran ------------------------------------------------------------

if(file.exists(file.path(inputdir, 'Weather', paste0("Weather_Forecasts_", state, "_", Sys.Date(), ".RData")))){ 
    
  load(file.path(inputdir, 'Weather', paste0("Weather_Forecasts_", state, "_", Sys.Date(), ".RData")))
    
} else {

# Check API Key -----------------------------------------------------------

if(file.exists("WeatherAPI_key.txt")){ # if the WeatherAPI_key.txt exists, then check if it has a API key.
  
  if(length(scan('WeatherAPI_key.txt', what = 'character')) == 0){
    
    stop("No TomorrowIO API Key found. Navigate to https://www.tomorrow.io/, create an account, 
         and create a TomorrowIO API Key which should be placed in ",
         getwd(), "/WeatherAPI_key.txt.")
    
  }else{
    
    print("TomorrowIO API Key Found! Proceeding with TomorrowIO pull.")
    
    w_key = scan('WeatherAPI_key.txt', what = 'character')
    
  }
  
} else{
  
  suppressMessages(file.create("WeatherAPI_key.txt"))
  
  stop("No TomorrowIO API Key found. Navigate to https://www.tomorrow.io/, create an account, 
         and create a TomorrowIO API Key which should be placed in ",
       getwd(), "/WeatherAPI_key.txt.")
  
}

# State Boundary File -----------------------------------------------------

if(file.exists(file.path(inputdir,'Roads_Boundary', state, paste0(state, '_boundary.gpkg')))){ # Look for the boundary file if it already exist

  print("State boundary file found.")
  
  state_map <- read_sf(file.path(inputdir,'Roads_Boundary', state, paste0(state, '_boundary.gpkg'))) %>% st_transform(crs = 4326) # if exists, load it
  
} else{ # else pull it from the web
  
  # Pull state boundaries
  state_map <- states(cb = TRUE, year = year) %>%
    filter(STUSPS == state) %>%
    st_transform(crs = projection)
  
  if(state == "AK") { # need to cut out Aleaution Islands as they break the point generation.
    
    state_map <- state_map %>%
      st_transform(4326) %>%
      st_crop(xmin = -180,
              xmax = 0,
              ymax = 90,
              ymin = -90) %>%
      st_transform(projection)
    
    print("Predictions will not be generated for Alaskan points beyond the international date line.")
    
  }
  
  # Create directory for the road network file and state boundary file, if it does not yet exist.
  if(!dir.exists(file.path(inputdir,'Roads_Boundary', state))){dir.create(file.path(inputdir,'Roads_Boundary', state), recursive = T)}
  
  write_sf(state_map, file.path(inputdir,'Roads_Boundary', state, paste0(state, '_boundary.gpkg')), driver = "ESRI Shapefile") # save it for later
  
}

# Create points for API pull -------------------------------------------------------

# Need to create points within a state
# The free version of Tomorrow IO can only pull 25 points so ideally we find 25 points within each state. 
# A 5x5 for a perfect square should produce 25 points, but for more complex shaped state (like TX, AK, CA) a lot of this grid get's cut off
# Therefore 5 is the min, but we should try larger grids until there is the most amount of points < 25.

x <- 5 

repeat{
  
  try_x <- state_map %>% 
    st_make_grid(n = c(x, x), what = "centers") %>% 
    st_intersection(state_map)
  
  if(length(try_x) > 25) break # as mentioned above if we find a grid size thats > 25 we want to use x-1 grid size. 
  
  grd <- try_x # if try_x has < 25 points assign it as the next possible grd
  
  x <- x + 1 # for the repeat to look at the next grid size
    
}

grd <- st_transform(grd, crs = 4326) # convert to API CRS

# Uncomment the below code to view the grid overlay
# ggplot() +
#   geom_sf(data = state_map, aes(), fill = NA, alpha = 1) +
#   geom_sf(data = grd, aes())

# Create dataframe with the longitude and latitude of the grid points for the query, and the 
queries <- st_coordinates(grd) %>% 
  as.data.frame() %>% 
  mutate(ID = row.names(.)) %>% 
  mutate(url = paste0('https://api.tomorrow.io/v4/weather/forecast?location=', Y, ',', X, '&apikey=', w_key, '&units=metric')) 

rm(x, try_x, grd)


# Query TomorrowIO ---------------------------------------------------------------

# create blank dataframes to populate with weather data
weather_daily <- data.frame(ID = as.character(),
                            date = Date(), 
                            snowAccumulationSum = numeric())
  
weather_hourly <- data.frame(ID = as.character(),
                             lon = numeric(),
                             lat = numeric(),
                             utc_hour = POSIXct(), 
                             temperature = numeric(),
                             snowAccumulation = numeric(),
                             rainAccumulation = numeric())
  
for(i in 1:nrow(queries)){ # Loop over the json queries for each forecast point and add to the data frames (daily and hourly)
    
  wx_dat_i = fromJSON(queries$url[i])
    
  wx_dat_daily_values = wx_dat_i$timelines$daily$values %>% # extract the weather attributes for the six days as a dataframe
    mutate(date = as.Date(wx_dat_i$timelines$daily$time), # extract the dates and assign to a new column
           ID = queries$ID[i]) %>%
    select(ID, date, snowAccumulationSum) # select the necessary columns

  wx_dat_hourly_values = wx_dat_i$timelines$hourly$values %>% # extract the weather attributes for the 120 hours as a dataframe
    mutate(utc_hour = ymd_hms(wx_dat_i$timelines$hourly$time), # extract the hours and assign to a new column
           ID = queries$ID[i],
           lon = queries$X[i],
           lat = queries$Y[i]) %>%
    select(ID, lon, lat, utc_hour,temperature, rainAccumulation) # select the necessary columns 
    
    # add forecasts for this point to the data frames along with the rest of the points
    weather_daily <- rbind(weather_daily,wx_dat_daily_values)
    weather_hourly <- rbind(weather_hourly,wx_dat_hourly_values)
    
    Sys.sleep(0.34) # Pause for 0.34 seconds in between each query because the limit is 3 requests per second
    
}

cat(paste0("TomorrowIO API pulled at ", Sys.time()))

# Data Post-Processing ----------------------------------------------------

# merge 
weather_points <- weather_hourly %>% 
  rename(date = utc_hour) %>%
  left_join(weather_daily, by=c("ID", "date"), relationship = "many-to-one") %>% # each row in weather_hourly should match at most one row in weather_daily, hence "many-to-one" relationship
  mutate(date = with_tz(date, tzone = time_zone_name))

# add spatial to a seperate DF, we need both
weather_points.proj <- weather_points %>%
  st_as_sf(coords = c('lon', 'lat'),
           crs = 4326) %>% 
  st_transform(crs=projection)

# save the weather pull
save(list=c('weather_points', 'weather_points.proj'),
      file = file.path(inputdir, "Weather", paste0("Weather_Forecasts_", state, "_", Sys.Date(), ".RData")))
  
rm(wx_dat_daily_values, wx_dat_hourly_values, wx_dat_i, queries, state_map, weather_daily, weather_hourly, w_key)

}

# Additional Details about TomorrowIO -------------------------------------

  # This is the format needed for an API call in TomorrowIO using "Forecast" endpoint, with four parameters that
  # need to be filled in (latitude, longitude, API key, units):
  # https://api.tomorrow.io/v4/weather/forecast?location={latitude},{longitude}&apikey={key}&units={imperial/metric}
  # there are additional parameters that the user can specify - view this page and click on one of the 'recipes':
  # https://docs.tomorrow.io/reference/weather-forecast
  # If additional flexibility/customizability is needed, one can use the "Timelines" endpoint instead - 
  # View this page and click on one of the 'recipes' for detail: https://docs.tomorrow.io/reference/post-timelines
  # There is also the "Realtime" endpoint: https://docs.tomorrow.io/reference/realtime-weather
  
  # Daily data are available for 6 days (including the current day, so only 5 future days)
  # Hourly data are available for 120 hours in the future (i.e. 5 future days), so about the 
  # same timeframe. We can discuss whether we want to shift to use hourly data instead of daily.
  # By default, time zone is Coordinated Universal Time (UTC)... the offset will vary by state
  # based on the time zone(s). May also vary based on whether it is daylight savings or standard time?
  