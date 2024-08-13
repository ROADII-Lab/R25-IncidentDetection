
library(dplyr)
library(osmdata)
library(sf)
library(ggplot2)
library(tigris)
library(doParallel)
library(lubridate)

outputdir <- file.path(getwd(),"Output") # to hold daily output files as they are generated
inputdir <- file.path(getwd(),"Input")

# Identify state for analysis; Need to specify 'Washington State' for Washington
state <- "WA" 

state_osm <- ifelse(state == "WA", 'Washington State',
                    ifelse(state == "MN", "Minnesota", NA))

state_bbox <- getbb(state_osm) # Retrieves relevant coordinates; always a rectangle

state_osm <- gsub(" ", "_", state_osm) # Normalize state name

# Projection 
projection <- 5070 

# Year
year <- 2021

# Load Road Data ----------------------

##select server to query OSM api
#If the below doesn't work, try https://overpass.kumi.systems/api/interpreter

new_url <- "https://overpass-api.de/api/interpreter"
set_overpass_url(new_url)


##identify road types you'd like to query for; can pick from c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential')
road_types <- c('motorway', 'trunk', 'primary', 'secondary', 'tertiary')

#initialize object
roadways <- list()

network_file <- paste0(state_osm,"_network")
boundary_file <- paste0(state_osm,"_boundary")
file_path <- file.path(inputdir,'Roads_Boundary', state_osm, paste0(network_file, '.gpkg'), paste0(network_file,'.shp'))

if (file.exists(file.path(file_path))){
  state_network <- read_sf(file_path)
  state_map <- read_sf(file.path(inputdir,'Roads_Boundary', state_osm, paste0(boundary_file, '.gpkg')))
  print("File Found")
} else{
  
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
  state_map <- states(cb = TRUE, year = 2021) %>%
    filter_state(state_border) %>%
    st_transform(crs = projection)
  
  #Filter out roadways outside the state
  
  state_network <- st_join(total_network, state_map, join = st_within) %>%
    filter(!is.na(NAME)) %>%
    select(osm_id, highway, ref, geometry)
  
  write_sf(state_network, file.path(inputdir,'Roads_Boundary', state_osm, paste0(network_file, '.gpkg')), driver = "ESRI Shapefile")
  write_sf(state_map, file.path(inputdir,'Roads_Boundary', state_osm, paste0(boundary_file, '.gpkg')), driver = "ESRI Shapefile")
  rm(state_border)
}

if(st_crs(state_network) != projection){
  state_network <- st_transform(state_network, projection)
}

######## Create other elements ##############

# specify how many days out the predictions will include
num_days <- 5

# generate a sequence of dates starting from day and extending out the number of days specified above
dates <- seq(from = today(), by = "day", length.out = num_days)

# replicate all 24 hours for each of the dates
hours <- rep(0:23, times = length(dates))

# replicate all dates 24 times (for each hour in the day)
dates <- rep(dates, each = 24)

# combine dates and hours into one dataframe
dates <- data.frame(date = dates, 
                    Year = year(dates), 
                    Month = month(dates), 
                    Day = day(dates), 
                    Hour = hours, 
                    day_of_week = wday(dates, label = TRUE)) %>%
  select(!date)

# replicate it, multiplying by the number of road segments
dates_exp <- do.call(bind_rows, replicate(nrow(state_network), dates, simplify = FALSE))

observations <- do.call(bind_rows, 
                      replicate(nrow(dates), 
                                state_network %>% st_drop_geometry(), 
                                simplify = FALSE)) %>% 
  arrange(osm_id) %>% 
  # merge with expanded version of date/time training frame
  cbind(dates_exp, row.names = NULL)
  
rm(dates, dates_exp, roadways, file_path, hours, network_file, new_url, num_days)

