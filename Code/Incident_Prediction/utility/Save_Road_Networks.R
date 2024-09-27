# This script was created to produce many OSM networks at once for used in the SDC. It is not required for incident detection runs. 

# Background ----------------------------
library(dplyr)
library(tidyr)
library(osmdata)
library(sf)
library(ggplot2)
library(tigris)
library(doParallel)
library(lubridate)
library(stringr)

top_time <- Sys.time()

# state_osm <- ifelse(state == "WA", 'Washington State',
#                     ifelse(state == "MN", "Minnesota", NA))
# 
# state_osm <- gsub(" ", "_", state_osm) # Normalize state name

inputdir <- file.path(getwd(),"Input")
outputdir <-file.path(getwd(),"Output")
intermediatedir <- file.path(getwd(), "Intermediate")

# Make outputdir and intermediatedir if not already there
if(!dir.exists(intermediatedir)) { dir.create(intermediatedir) }
if(!dir.exists(outputdir)) { dir.create(outputdir) }

# Projection 
projection <- 5070 

#------------------------------------------------------------------------

get_osm <- function(state_osm){

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
    print(paste("File Not Found. Pulling", state_osm, i, "OSM Data from Server"))
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
    select(osm_id, geometry)
  
  write_sf(state_network, file.path(inputdir,'Roads_Boundary', state_osm, paste0(network_file, '.gpkg')), driver = "ESRI Shapefile")
  write_sf(state_maps, file.path(inputdir,'Roads_Boundary', state_osm, paste0(boundary_file, '.gpkg')), driver = "ESRI Shapefile")
  rm(state_border, state_maps, total_network, datalist)
}

}

states <- c('Maine','Illinois','Texas', 'California')

for(s in states){get_osm(s)}




