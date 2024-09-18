# Intro -----------------------------------
# Purpose: This script sources Get_weather_forecast.R then spatial joins those results with the osm network

library(RANN) # holds nn2 
library(sf)
library(osmdata)
library(dplyr)

projection <- 5070

# REMOVE THIS ONCE PULLING INTO PREDICTWEEK.R (WILL ALSO NEED TO CHANGE roads THROUGHOUT HERE)
#roads <- st_read(file.path(inputdir, "Roads_Boundary", "Washington_State", "Washington_State_network.gpkg", "Washington_State_network.shp"))
#load("D:/Documents/Joey/R25-IncidentDetection/Code/Incident_Prediction/Input/Weather/Weather_Forecasts_WA_2024-09-10.RData")

state_osm <- ifelse(state == "WA", 'Washington State',
                    ifelse(state == "MN", "Minnesota", NA))

state_osm <- gsub(" ", "_", state_osm) # Normalize state name

# Create directory for the road network file and state boundary file, if it does not yet exist.
if(!dir.exists(file.path(inputdir,'Roads_Boundary', state_osm))){dir.create(file.path(inputdir,'Roads_Boundary', state_osm), recursive = T)}

# Load Road Data ----------------------

##select server to query OSM api
#If the below doesn't work, try https://overpass.kumi.systems/api/interpreter

new_url <- "https://overpass-api.de/api/interpreter"
set_overpass_url(new_url)

##identify road types you'd like to query for; can pick from c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential')
road_types <- c('motorway', 'trunk', 'primary', 'secondary', 'tertiary')

network_file <- paste0(state_osm,"_network")
boundary_file <- paste0(state_osm,"_boundary")
file_path <- file.path(inputdir,'Roads_Boundary', state_osm, paste0(network_file, '.gpkg'), paste0(network_file,'.shp'))

if (file.exists(file.path(file_path))){
  roads <- read_sf(file_path) %>% select(osm_id)
  print("File Found")
} else{
  state_bbox <- getbb(state_osm) # Retrieves relevant coordinates; always a rectangle
  n = as.numeric(length(road_types)) 
  datalist <- list()
  datalist = vector("list", length = n)
  
  l = 0
  
  for (i in road_types){
    l = l + 1 
    print(paste("File Not Found. Pulling", state, i, "OSM Data from Server"))
    map_data <- opq(bbox = state_bbox) %>%
      add_osm_feature(key = 'highway', value = i) %>%
      osmdata_sf() 
    lines <- map_data$osm_lines
    
    datalist[[l]] <- lines
    
  }
  
  total_network <- do.call(bind_rows, datalist)
  
  # if (!(dir.exists(state_osm))){
  #   dir.create(state_osm)}
  
  total_network <- st_transform(total_network, crs = projection) 
  
  # Pull state boundaries
  if (state_osm == 'Washington_State'){
    state_border <- 'Washington'
  }else{
    state_border <- state_osm
  }
  state_maps <- states(cb = TRUE, year = 2021) %>%
    filter_state(state_border) %>%
    st_transform(crs = projection)
  
  #Filter out roadways outside the state
  
  roads <- st_join(total_network, state_maps, join = st_within) %>%
    filter(!is.na(NAME)) %>%
    select(osm_id, geometry)
  
  write_sf(roads, file.path(inputdir,'Roads_Boundary', state_osm, paste0(network_file, '.gpkg')), driver = "ESRI Shapefile")
  write_sf(state_maps, file.path(inputdir,'Roads_Boundary', state_osm, paste0(boundary_file, '.gpkg')), driver = "ESRI Shapefile")
  rm(state_border)
}

#ggplot() + geom_sf(data = state_network)

#Transform state_network crs to NAD83 before joining with crash_files; should probably change this in the files we have and bring this into the query loop

if(st_crs(roads) != projection){
  roads <- st_transform(roads, projection)
}

# Weather Merge -----------------------------------------------------------

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
   
  save(weather_forecast, file = file.path(inputdir, "Weather", prepname))

   rm(KNN, ID_geometry, grd, queries, state_map, weather_points, weather_points.proj, wx_dat_i,
      api_crs, timezone_adj, US_timezones, road_points)
  
  } else {
  load(file.path(inputdir, "Weather", prepname))
}
