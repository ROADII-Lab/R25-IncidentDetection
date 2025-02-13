
# Create directory for the road network file and state boundary file, if it does not yet exist.
if(!dir.exists(file.path(inputdir,'Roads_Boundary', state))){dir.create(file.path(inputdir,'Roads_Boundary', state), recursive = T)}

# Load Road Data ----------------------

##select server to query OSM api
#If the below doesn't work, try https://overpass.kumi.systems/api/interpreter

new_url <- "https://overpass-api.de/api/interpreter"
set_overpass_url(new_url)

##identify road types you'd like to query for; can pick from c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential')
road_types <- c('motorway', 'trunk', 'primary', 'secondary', 'tertiary')

network_file <- paste0(state,"_network")
boundary_file <- paste0(state,"_boundary")
file_path <- file.path(inputdir,'Roads_Boundary', state, paste0(network_file, '.gpkg'), paste0(network_file,'.shp'))

if (file.exists(file.path(file_path))){

  state_network <- read_sf(file_path) %>% select(osm_id, highway, maxspeed)

  print("File Found")
  
} else{
  
  state_osm <- state.name[which(state.abb == state)]
  
  state_osm <- ifelse(state_osm == "Washington", 'Washington State', state_osm) # WA
  
  state_osm <- ifelse(state_osm == "New York", "New York State", state_osm) # NY
  
  state_osm <- gsub(" ", "_", state_osm) # Normalize state name
  
  state_osm <- paste0(state_osm, "_US") # this is needed for Georgia, and maybe other states that share names with countires.
  
  state_bbox <- getbb(state_osm) # Retrieves relevant coordinates; always a rectangle
  
  rm(state_osm) # no longer need state_osm
  
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
  
  total_network <- st_transform(total_network, crs = projection) 
  
  # Pull state boundaries

  state_maps <- states(cb = TRUE, year = year) %>%
    filter(STUSPS == state) %>%
    st_transform(crs = projection)
  
  #Filter out roadways outside the state
  
  state_network <- st_join(total_network, state_maps, join = st_within) %>%
    filter(!is.na(NAME)) %>%

    select(osm_id, geometry, highway, maxspeed) %>%
    # convert speed limit to numeric, removing "mph" unit label
    mutate(maxspeed = gsub("[^0-9.-]", "", maxspeed)) %>%
    # take first two characters (rarely the maxspeed is originally expressed as a range)
    mutate(maxspeed = substr(maxspeed, start = 1, stop = 2)) %>%
    mutate(maxspeed = as.numeric(maxspeed))
    replace_na(list(maxspeed = median(state_network$maxspeed, na.rm = T)))

  
  write_sf(state_network, file.path(inputdir,'Roads_Boundary', state, paste0(network_file, '.gpkg')), driver = "ESRI Shapefile")
  write_sf(state_maps, file.path(inputdir,'Roads_Boundary', state, paste0(boundary_file, '.gpkg')), driver = "ESRI Shapefile")

}

#ggplot() + geom_sf(data = state_network)

#Transform state_network crs to NAD83 before joining with crash_files; should probably change this in the files we have and bring this into the query loop

if(st_crs(state_network) != projection){
  state_network <- st_transform(state_network, projection)
}
