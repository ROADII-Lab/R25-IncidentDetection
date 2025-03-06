# Intro -----------------------------------
# Purpose: This script sources TomorrowIO_pull.R then spatial joins those results with the osm network

projection <- 5070

# Load Road Network --------------------------------------------------------------

if (file.exists(file.path(inputdir, "Roads_Boundary", state, paste0(state, '_network.gpkg'), paste0(state, '_network.shp')))){

  print("State road network found.")
  
  state_network <- read_sf(file.path(inputdir, "Roads_Boundary", state, paste0(state, '_network.gpkg'), paste0(state, '_network.shp')))

} else{
  
  print("State road network not found. Pulling from OpenStreetMaps and performing post processing.")

  source(file.path("utility", "OpenStreetMap_pull.R"))

}

# Weather Merge -----------------------------------------------------------

prepname = paste0("OSM_Weather_", state, "_", Sys.Date(), ".RData")

if(!file.exists(file.path(inputdir, 'Weather', prepname))) { # check if this script has already been ran

  # Get weather forecast for the next week
  # First check to see if forecast has been run already from today (to not over-use API call)

  if(file.exists(file.path(inputdir, 'Weather', paste0("Weather_Forecasts_", state, "_", Sys.Date(), ".RData")))){
    
    load(file.path(inputdir, 'Weather', paste0("Weather_Forecasts_", state, "_", Sys.Date(), ".RData")))
    
  } else {
    
    source(file.path("utility", "TomorrowIO_pull.R"))
    
  }

# Perform KNN -------------------------------------------------------------
  
# KNN needs points not lines so we first generate the road points
road_points <- state_network %>%
  st_cast("POINT") %>% 
  group_by(osm_id) %>% 
  slice_head(n=1) %>% 
  arrange(osm_id)

# creates a dataframe with the ID and the geometry for joining later
ID_geometry <- weather_points.proj %>% 
  distinct(ID, .keep_all = T) %>% 
  select(ID, geometry)
  
# nn2 is much faster than an sf solution
KNN <- as.data.frame(nn2(st_coordinates(ID_geometry), 
                         st_coordinates(road_points), # the two geometries to do KNN on 
                         k=1, # We only care about the exact closest solution
                         treetype = "kd", 
                         searchtype = "standard")$nn.id) # only care about the IDs, the order is the same order as road points

# merge the KNN results with the road points
weather_forecast <- road_points %>% 
  st_drop_geometry() %>% # don't care about geometry as we just need the IDs
  bind_cols(KNN) %>% # bind with KNN which is in the same order
  mutate(ID = as.character(V1)) %>% 
  full_join(weather_points, by= "ID", relationship="many-to-many") %>% # bring weather in
  rename(precipitation = rainAccumulation, # to match names in NN
         SNOW = snowAccumulationSum,
         date = utc_hour) %>% 
  select(osm_id, date, temperature, precipitation, SNOW) # only keep variables of interest

if(time_bins){ # if time_bin = T, collapse into 6 hour segments
  
  weather_forecast <- weather_forecast %>% 
    as_tbl_time(index = date) %>%
    collapse_by("6 hours", side = "start", clean = TRUE) %>%
    group_by(osm_id, date) %>%
    summarize(temperature = mean(temperature, na.rm = T), 
              precipitation = mean(precipitation, na.rm = T), 
              SNOW = mean(SNOW, na.rm = T))
  
}
   
save(weather_forecast, file = file.path(inputdir, "Weather", prepname)) # save the weather forecast by ROAD script

   rm(KNN, ID_geometry, grd, queries, state_map, weather_points, weather_points.proj, wx_dat_i,
      api_crs, timezone_adj, road_points)
  
  } else {
    
  load(file.path(inputdir, "Weather", prepname))

    }
