# Intro -----------------------------------
# Purpose: This script sources Get_weather_forecast.R then spatial joins those results with the osm network

library(RANN) # holds nn2 
library(sf)
library(osmdata)
library(dplyr)

projection <- 5070


# Load Roads --------------------------------------------------------------

source(file.path("Utility", "Prep_OSMNetwork.R"))

# Weather Merge -----------------------------------------------------------

prepname = paste0("OSM_Weather_", state, "_", Sys.Date(), ".RData")

if(!file.exists(file.path(inputdir, 'Weather', prepname))) {

  # Get weather forecast for the next week
  # First check to see if forecast has been run already from today (to not over-use API call)

  if(file.exists(file.path(inputdir, 'Weather', paste0("Weather_Forecasts_", state, "_", Sys.Date(), ".RData")))) {
    load(file.path(inputdir, 'Weather', paste0("Weather_Forecasts_", state, "_", Sys.Date(), ".RData")))
  } else {
    source('datacleaning/Get_weather_forecasts.R')
  }

# make points file for KNN 
road_points <- state_network %>% st_cast("POINT") %>% group_by(osm_id) %>% slice_head(n=1) %>% arrange(osm_id)

ID_geometry <- weather_points.proj %>% 
  distinct(ID, .keep_all = T) %>% 
  select(ID, geometry)
  
KNN <- as.data.frame(nn2(st_coordinates(ID_geometry), st_coordinates(road_points),  k=1, treetype = "kd", searchtype = "standard")$nn.id)

weather_forecast <- road_points %>% 
  st_drop_geometry() %>%
  bind_cols(KNN) %>% 
  mutate(ID = as.character(V1)) %>% 
  full_join(weather_points, by= "ID", relationship="many-to-many") %>% 
  rename(precipitation = rainAccumulation, # to match historical names 
         SNOW = snowAccumulationSum,
         date = utc_hour) %>% 
  select(osm_id, date, temperature, precipitation, SNOW) 

  if(time_bins){
    weather_forecast <- weather_forecast %>% 
      as_tbl_time(index = date) %>%
      collapse_by("6 hours", side = "start", clean = TRUE) %>%
      group_by(osm_id, date) %>%
      summarize(temperature = mean(temperature, na.rm = T), 
                precipitation = mean(precipitation, na.rm = T), 
                SNOW = mean(SNOW, na.rm = T))
  }
   
  save(weather_forecast, file = file.path(inputdir, "Weather", prepname))

   rm(KNN, ID_geometry, grd, queries, state_map, weather_points, weather_points.proj, wx_dat_i,
      api_crs, timezone_adj, US_timezones, road_points)
  
  } else {
  load(file.path(inputdir, "Weather", prepname))
}
