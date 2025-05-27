# Title: Prep_ForecastWeather
# Purpose: This script sources TomorrowIO_pull.R then spatial joins those results with the osm network.
# Generated Variables: weather_forecast

if(file.exists(file.path(inputdir, 'Weather', paste0("OSM_Weather_", state, "_", today, ".RData")))) { # check if this script has already been ran
  
  load(file.path(inputdir, "Weather", paste0("OSM_Weather_", state, "_", today, ".RData")))
                 
} else{ # if not, go through a pull osm, tomorrowIO, and then merge.
  
# Load Road Network --------------------------------------------------------------

source(file.path("utility", "OpenStreetMap_pull.R"))

# Weather Pull ----------------------------------------------------------

source(file.path("utility", "TomorrowIO_pull.R"))
    
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
         SNOW = snowAccumulationSum) %>% 
  select(osm_id, date, temperature, precipitation, SNOW) # only keep variables of interest

if(time_bins){
  
  weather_forecast <- weather_forecast %>%
    as_tsibble(index = date, key = osm_id) %>%
    arrange(osm_id, date) %>%
    group_by(osm_id) %>%
    # time_interval was defined in RandomForest_Train.R script when the model was trained originally. 
    # if time bins are not being used then it just aggregates by hour.
    index_by(interval = floor_date(x = date, unit = ifelse(time_bins, time_interval, "hours"))) %>%
    summarise(temperature = mean(temperature, na.rm = T), 
              precipitation = mean(precipitation, na.rm = T), 
              SNOW = mean(SNOW, na.rm = T),
              .groups = "drop") %>%
    rename(date = interval) %>%
    as.data.frame()
  
}
   
save(weather_forecast, file = file.path(inputdir, "Weather", paste0("OSM_Weather_", state, "_", Sys.Date(), ".RData"))) # save the weather forecast by ROAD script

   rm(KNN, ID_geometry, grd, queries, state_map, weather_points, weather_points.proj, wx_dat_i,
      api_crs, timezone_adj, road_points)
  
} 
    


    
