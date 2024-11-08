library(osmdata)
library(sf)
library(tigris)
# I created this script as we use copy and pasted this code frequently in different scripts so this will normalize it and make it easier to make changes. 
# This script is different than Save_Road_Networks.R as that is intended for looping through and saving OSM networks. This script is for loading one state. 

state_osm <- state.name[which(state.abb == state)]

state_osm <- ifelse(state_osm == "Washington", 'Washington State', state_osm)

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
  state_network <- read_sf(file_path) %>% select(osm_id)
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
  
  state_network <- st_join(total_network, state_maps, join = st_within) %>%
    filter(!is.na(NAME)) %>%
    select(osm_id, geometry, highway)
  
  write_sf(state_network, file.path(inputdir,'Roads_Boundary', state_osm, paste0(network_file, '.gpkg')), driver = "ESRI Shapefile")
  write_sf(state_maps, file.path(inputdir,'Roads_Boundary', state_osm, paste0(boundary_file, '.gpkg')), driver = "ESRI Shapefile")
  rm(state_border)
}

#ggplot() + geom_sf(data = state_network)

#Transform state_network crs to NAD83 before joining with crash_files; should probably change this in the files we have and bring this into the query loop

if(st_crs(state_network) != projection){
  state_network <- st_transform(state_network, projection)
}
