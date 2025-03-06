# Title: OpenStreetMap (OSM) Pull
# Purpose: Pulls OpenStreetMap data and processes it.
# Generated Variables: state_network

# Check if Ran ------------------------------------------------------------

if (file.exists(file.path(inputdir, "Roads_Boundary", state, paste0(state, '_network.gpkg'), paste0(state, '_network.shp')))){
  
  print("State road network found.")
  
  state_network <- read_sf(file.path(inputdir, "Roads_Boundary", state, paste0(state, '_network.gpkg'), paste0(state, '_network.shp'))) 
  
} else{
  
  print("State road network not found. Pulling from OpenStreetMaps and performing post processing.")

# Link to OSM network -----------------------------------------------

# all possible urls for set_overpass_url as shown in help(set_overpass_url)
overpass_urls <- c("https://overpass-api.de/api/interpreter",
                   "https://overpass.kumi.systems/api/interpreter",
                   "https://overpass.osm.rambler.ru/cgi/interpreter",
                   "https://api.openstreetmap.fr/oapi/interpreter",
                   "https://overpass.osm.vi-di.fr/api/interpreter")

x <- 0 # tracking the repeat function / telling which URL to try

repeat{
  
  x <- x + 1 # next url to check
  
  break(x == 6) # there is no 6th link so break if it gets to x = 6
  
  valid_link <- T # by default, the link is valid

  tryCatch(set_overpass_url(overpass_urls[x]), error = function(a) valid_link <<- F) # try the link, if invalid, set invalid_link to TRUE
  
  break(valid_link) # stop looping if valid link is TRUE
    
}

if(x == 6){
  
  print("Error: No valid OSM link found. It's possible you're not connected to the internet or all URLs are outdated. Check OpenStreetMap_pull.R to troubleshoot.")
  
}

rm(x, overpass_urls, valid_link) # clean up 
  
# Define variables for OSM pull -------------------------------------------
  
# identify road types you'd like to query for; can pick from c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential')
road_types <- c('motorway', 'trunk', 'primary', 'secondary', 'tertiary')
  
# define state osm
state_osm <- state.name[which(state.abb == state)]
state_osm <- ifelse(state_osm == "Washington", 'Washington State', state_osm) # WA
state_osm <- ifelse(state_osm == "New York", "New York State", state_osm) # NY
state_osm <- gsub(" ", "_", state_osm) # Normalize state name
state_osm <- paste0(state_osm, "_US") # this is needed for Georgia, and maybe other states that share names with countries.

state_bbox <- getbb(state_osm) # Retrieves relevant coordinates; always a rectangle

# Perform OSM pull --------------------------------------------------------

datalist <- list() # define a list to add the road type df's to
  
for (i in road_types){ # for each road type
    
  print(paste("File Not Found. Pulling", state, i, "OSM Data from Server"))
  
  map_data <- opq(bbox = state_bbox) %>% # area to pull from
    add_osm_feature(key = 'highway', value = i) %>% # which road types
    osmdata_sf() # osm query
  
  lines <- map_data$osm_lines # define just the lines
     
  datalist[[i]] <- lines # place lines in the list created earlier 
    
}
  
total_network <- do.call(bind_rows, datalist) # bind all lines together
  
total_network <- st_transform(total_network, crs = projection) # convert projection

# Perform filter ----------------------------------------------------------

if(file.exists(file.path(inputdir,'Roads_Boundary', state, paste0(state, '_boundary.gpkg')))){ # Look for the boundary file if it already exist
  
  print("State boundary file found.")
  
  state_map <- read_sf(file.path(inputdir,'Roads_Boundary', state, paste0(state, '_boundary.gpkg'))) %>% st_transform(crs = api_crs) # if exists, load it
  
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
  
state_network <- st_join(total_network, state_map, join = st_within) %>%
  filter(!is.na(NAME)) %>%
  select(osm_id, geometry, highway, maxspeed) %>%
  mutate(maxspeed = gsub("[^0-9.-]", "", maxspeed), # convert speed limit to numeric, removing "mph" unit label
          maxspeed = substr(maxspeed, start = 1, stop = 2), # take first two characters (rarely the maxspeed is originally expressed as a range)
          maxspeed = as.numeric(maxspeed)) %>% # make maxspeed numeric
  st_transform(crs = projection) # ENSURE it's correct projetion
  
  replace_na(list(maxspeed = median(state_network$maxspeed, na.rm = T))) # replace NA's with numeric

# Save files to computer --------------------------------------------------
  
# Save the state road network for future use
write_sf(state_network, file.path(inputdir,'Roads_Boundary', state, paste0(network_file, '.gpkg')), driver = "ESRI Shapefile")

#ggplot() + geom_sf(data = state_network) # view road network to test if it's valid

rm(total_network, datalist, lines, map_data, road_types, state_bbox, state_osm, state_map) # clean environment

}