# Intro -----------------------------------
# Purpose: This script sources Get_weather_forecast.R then spatial joins those results with the osm network

library(RANN) # holds nn2 

# REMOVE THIS ONCE PULLING INTO PREDICTWEEK.R (WILL ALSO NEED TO CHANGE roads THROUGHOUT HERE)
roads <- st_read(file.path(inputdir, "Roads_Boundary", "Washington_State", "Washington_State_network.gpkg", "Washington_State_network.shp"))
#load("D:/Documents/Joey/R25-IncidentDetection/Code/Incident_Prediction/Input/Weather/Weather_Forecasts_WA_2024-09-10.RData")

prepname = paste0("OSM_Weather_", state, "_", Sys.Date(), ".RData")

if(!file.exists(file.path(inputdir, 'Weather', prepname))) {

  # Get weather forecast for the next week
  # First check to see if forecast has been run already from today (to not over-user API call)

  if(file.exists(file.path(inputdir, 'Weather', paste0("Weather_Forecasts_", state, "_", Sys.Date(), ".RData")))) {
    load(file.path(inputdir, 'Weather', paste0("Weather_Forecasts_", state, "_", Sys.Date(), ".RData")))
  } else {
    source('datacleaning/Get_weather_forecasts.R')
  }

# make points file for KNN 
road_points <- roads %>% st_cast("POINT") %>% group_by(osm_id) %>% slice_head(n=1) %>% arrange(osm_id)

ID_geometry <- weather_hourly.proj %>% 
  distinct(ID, .keep_all = T) %>% 
  select(ID, geometry)
  
KNN <- as.data.frame(nn2(st_coordinates(ID_geometry), st_coordinates(road_points),  k=1, treetype = "kd", searchtype = "standard")$nn.id)

weather_forecast <- road_points %>% 
  st_drop_geometry() %>%
  bind_cols(KNN) %>% 
  mutate(ID = as.character(V1)) %>% 
  full_join(weather_hourly, by= "ID", relationship="many-to-many") %>% 
  select(osm_id, utc_hour, temperature, snowAccumulation, rainAccumulation)
   
  save(weather_forecast, file = file.path(inputdir, "Weather", prepname))

  rm(KNN, ID_geometry, road_points, grd, queries, state_map, weather_hourly, weather_hourly.proj, wx_dat_i,
     api_crs)
  
  } else {
  load(file.path(inputdir, "Weather", prepname))
}

# left join this with the predict week df here or later? 