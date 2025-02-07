
# Background ----------------------------

library(dplyr)
library(osmdata)
library(sf)
library(ggplot2)
library(tigris)
library(doParallel)
library(lubridate)
library(stringr)

rm(list=ls()) # clear enviroment

outputdir <- file.path(getwd(),"Output")
inputdir <- file.path(getwd(),"Input")
intermediatedir <- file.path("Intermediate")

# make directories if not already there
if(!dir.exists(outputdir)){dir.create(outputdir)}
if(!dir.exists(intermediatedir)){dir.create(intermediatedir)}
if(!dir.exists(file.path(intermediatedir,'CAD_Waze'))){dir.create(file.path(intermediatedir,'CAD_Waze'))}

# Identify state for analysis; Need to specify Washington State for Washington
state <- "MN" 

state_osm <- ifelse(state == "WA", "Washington State",
                    ifelse(state == "MN", "Minnesota", NA))

state_osm <- gsub(" ", "_", state_osm) # Normalize state name

# Projection 
projection <- 5070 

# Year
year <- 2020

# Load Road Data ----------------------

##select server to query OSM api
#If the below does not work, try https://overpass.kumi.systems/api/interpreter

new_url <- "https://overpass-api.de/api/interpreter"
set_overpass_url(new_url)

##identify road types you want to query for; can pick from c(motorway, trunk, primary, secondary, tertiary, unclassified, residential)
road_types <- c("motorway", "trunk", "primary", "secondary", "tertiary")

#initialize object
roadways <- list()

network_file <- paste0(state_osm,"_network")
boundary_file <- paste0(state_osm,"_boundary")
file_path <- file.path(inputdir,"Roads_Boundary", state_osm, paste0(network_file, ".gpkg"), paste0(network_file,".shp"))

if (file.exists(file.path(file_path))){
  state_network <- read_sf(file_path)
  print("File Found")
} else{
  
  n = as.numeric(length(road_types)) 
  datalist <- list()
  datalist = vector("list", length = n)
  state_bbox <- getbb(state_osm) # Retrieves relevant coordinates; always a rectangle
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
    select(osm_id, highway, ref, geometry)
  
  write_sf(state_network, file.path(inputdir,'Roads_Boundary', state_osm, paste0(network_file, '.gpkg')), driver = "ESRI Shapefile")
  write_sf(state_border, file.path(inputdir,'Roads_Boundary', state_osm, paste0(boundary_file, '.gpkg')), driver = "ESRI Shapefile")
  rm(state_border)
}

#ggplot() + geom_sf(data = state_network)

#Transform state_network crs to the projection
if(st_crs(state_network) != projection){
  state_network <- st_transform(state_network, projection)
}

####Read in CAD data for 2020
CAD20 <- read.csv(file.path(inputdir,"Crash","2020_mndot_cadevents.csv")) %>%
  mutate(lat=as.numeric(lat),
         lon=as.numeric(lon),
         centraltime=as.POSIXct(open,format="%m/%d/%Y %H:%M",tz="America/Chicago"),
         close = as.POSIXct(close,format="%m/%d/%Y %H:%M",tz="America/Chicago"),
         time_open = difftime(close, centraltime, units = "mins"),
         month = lubridate::month(centraltime)) %>%
  # some of the lat and lon values had the string "None," so above function resulted in NAs by coercion.
  # filter them out now
  filter(!is.na(lon)&!is.na(lat)) %>%
  # filter out also the rows that were duplicates, mistakes, or 'unable to locate' based on police or traffic reports
  filter(!p_disposition %in% c('DUPNCAN','UTL','CE','DUP','CANCELEV')) %>%
  filter(!t_disposition %in% c('DUPNCAN','UTL','CE','DUP','CANCELEV')) %>%
  # filter out also the rows where the time_open is only a few minutes. According to MnDOT,  
  # those are likely erroneous because they were closed so soon after being opened.
  filter(time_open > 5) %>%
  # select columns of interest
  dplyr::select(pkey, eid, class, lat, lon, centraltime, time_open, location, month) %>%
  # convert to sf object
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
  st_transform(projection) %>%
  # associated each record with the road segment it is nearest to
  st_join(state_network, join = st_nearest_feature)

###### establish set of waze files that we'll be reading in
waze.files <- dir(file.path(inputdir, "Waze", state))

waze.files.year <- file.path(inputdir, "Waze", state, waze.files[grep(as.character(year), waze.files)])

waze_jan <- read.csv(waze.files.year[1])
unique(waze_jan$sub_type)

source('MN_CAD_analysis/CAD_Waze_functions.R')


############################# COMPUTE MATCHES FOR DIFFERENT EVENT TYPES #######################
gc()
## FOR CRASH/ACCIDENT
crash_match_files <- get_matches_by_type(CADtypes = C("CRASH"), 
                                         Wazetypes = C("ACCIDENT"),
                                         sub_or_alert = alert_type)
crash_match_CAD <- crash_match_files$CADfull
crash_match_waze <- crash_match_files$wazefull
rm(crash_match_files)
save(list = c("crash_match_CAD", "crash_match_waze"), file = file.path(getwd(),'MN_CAD_Analysis','CAD_Waze_matches.Rdata'))


gc()
## FOR STALL/ CAR STOPPED ON ROAD OR SHOULDER
parked_veh_match_files <- get_matches_by_type(CADtypes = c("STALL"), 
                                              Wazetypes = c("HAZARD_ON_SHOULDER_CAR_STOPPED", "HAZARD_ON_ROAD_CAR_STOPPED"),
                                              sub_or_alert = sub_type)
parked_veh_match_CAD <- parked_veh_match_files$CADfull
parked_veh_match_waze <- parked_veh_match_files$wazefull
rm(parked_veh_match_files)

parked_road_match_waze <- parked_veh_match_waze %>% filter(sub_type == "HAZARD_ON_ROAD_CAR_STOPPED")
parked_shoulder_match_waze <- parked_veh_match_waze %>% filter(sub_type == "HAZARD_ON_SHOULDER_CAR_STOPPED")

save(list = c("parked_veh_match_CAD", "parked_veh_match_waze","parked_road_match_waze","parked_shoulder_match_waze"), file = file.path(getwd(),'MN_CAD_Analysis','parked_veh_matches.Rdata'))

gc()
## FOR ROADWORK/ CONSTRUCTION
roadwork_match_files <- get_matches_by_type(CADtypes = c("ROADWORK"), 
                                            Wazetypes = c("HAZARD_ON_ROAD_CONSTRUCTION", "ROAD_CLOSED_CONSTRUCTION"),
                                            sub_or_alert = sub_type)
roadwork_match_CAD <- roadwork_match_files$CADfull
roadwork_match_waze <- roadwork_match_files$wazefull
rm(roadwork_match_files)
save(list = c("roadwork_match_CAD", "roadwork_match_waze"), file = file.path(getwd(),'MN_CAD_Analysis','roadwork_matches.Rdata'))

gc()
## FOR HAZARD
hazard_match_files <- get_matches_by_type(CADtypes = c("HAZARD"), 
                                            Wazetypes = c("HAZARD_ON_ROAD_ICE", 
                                                          "HAZARD_ON_ROAD",
                                                          "HAZARD_ON_ROAD_POT_HOLE",
                                                          "HAZARD_WEATHER_HEAVY_SNOW",
                                                          "HAZARD_WEATHER_FOG",
                                                          "HAZARD_WEATHER_FLOOD",
                                                          "ROAD_CLOSED_HAZARD",
                                                          "HAZARD_ON_SHOULDER_MISSING_SIGN",
                                                          "HAZARD_ON_ROAD_OBJECT",
                                                          "HAZARD_ON_ROAD_TRAFFIC_LIGHT_FAULT",
                                                          "HAZARD_WEATHER_HAIL",
                                                          "HAZARD_ON_SHOULDER",
                                                          "HAZARD_ON_ROAD_ROAD_KILL",
                                                          "HAZARD_ON_SHOULDER_ANIMALS",
                                                          "HAZARD_WEATHER",
                                                          "HAZARD_ON_ROAD_LANE_CLOSED"),
                                            sub_or_alert = sub_type)
hazard_match_CAD <- hazard_match_files$CADfull
hazard_match_waze <- hazard_match_files$wazefull
rm(hazard_match_files)
save(list = c("hazard_match_CAD", "hazard_match_waze"), file = file.path(getwd(),'MN_CAD_Analysis','hazard_matches.Rdata'))


######################################################################################################

#######################LOAD MATCH FILES PREVIOUSLY CREATED AND SAVED#################################
load(file = file.path(getwd(),'MN_CAD_Analysis','CAD_Waze_matches.Rdata'))
load(file = file.path(getwd(),'MN_CAD_Analysis','parked_veh_matches.Rdata'))


# # add "matched" column if not yet there
# crash_match_CAD$matched = ifelse(crash_match_CAD$matches > 0, 1, 0)
# crash_match_waze$matched = ifelse(crash_match_waze$matches > 0, 1, 0)
# 
# parked_veh_match_CAD$matched = ifelse(parked_veh_match_CAD$matches > 0, 1, 0)
# parked_veh_match_waze$matched = ifelse(parked_veh_match_waze$matches > 0, 1, 0)

#####################################################################################################

unique(CAD20$class)
unique(crash_match_waze$alert_type)
unique(crash_match_waze$sub_type)


######################## SUMMARIZE MATCHES BY COUNTY ##########################

## Creating base objects for all maps
boundary_file <- paste0(state_osm, "_boundary")
boundary_file_path <- file.path(inputdir, 'Roads_Boundary', state_osm, paste0(boundary_file, '.gpkg'), paste0(state_osm, '_border.shp'))

if (file.exists(file.path(boundary_file_path))){
  state_boundary <- read_sf(boundary_file_path)
  print("File found")
} else {print("No state boundary file.")}

state_boundary <- state_boundary %>% st_transform(crs = projection)

counties <- read_sf(file.path(inputdir, "Roads_Boundary", state_osm, "shp_bdry_counties_in_minnesota","mn_county_boundaries.shp")) %>%
  st_transform(crs = projection)

## Creating specific summaries and maps for each type

crash_counties <- sum_by_county(CAD_sf = crash_match_CAD, 
                                Waze_sf = crash_match_waze,
                                county_file = counties)

parked_veh_counties <- sum_by_county(CAD_sf = parked_veh_match_CAD, 
                                     Waze_sf = parked_veh_match_waze,
                                     county_file = counties)

## Now just for onroad waze
parked_road_counties <- sum_by_county(CAD_sf = parked_veh_match_CAD, 
                                      Waze_sf = parked_road_match_waze,
                                      county_file = counties)

## Now just for shoulder waze
parked_shoulder_counties <- sum_by_county(CAD_sf = parked_veh_match_CAD, 
                                          Waze_sf = parked_shoulder_match_waze,
                                          county_file = counties)

parked_road_counties <- parked_road_counties %>% rename(CAD_match_percentage = CAD_match_percentage.x,
                                                        waze_match_percentage = waze_match_percentage.x)

parked_shoulder_counties <- parked_shoulder_counties %>% rename(CAD_match_percentage = CAD_match_percentage.x,
                                                                waze_match_percentage = waze_match_percentage.x)


make_county_maps(county_file_with_matches = crash_counties,
                 CAD_class = "Crash",
                 Waze_type = "Accident",
                 year = 2020)

make_county_maps(county_file_with_matches = parked_veh_counties,
                 CAD_class = "Stall",
                 Waze_type = "Parked Vehicle (Roadway or Shoulder)",
                 year = 2020)

make_county_maps(county_file_with_matches = parked_road_counties,
                 CAD_class = "Stall",
                 Waze_type = "Parked Vehicle (Roadway)",
                 year = 2020)

make_county_maps(county_file_with_matches = parked_shoulder_counties,
                 CAD_class = "Stall",
                 Waze_type = "Parked Vehicle (Shoulder)",
                 year = 2020)


####################### BY ROADTYPE ##############################

view_by_road_type(CAD_sf = crash_match_CAD,
                  Waze_sf = crash_match_waze,
                  CAD_class = "Crash",
                  Waze_type = "Accident",
                  year = 2020)

view_by_road_type(CAD_sf = parked_veh_match_CAD,
                  Waze_sf = parked_veh_match_waze,
                  CAD_class = "Stall",
                  Waze_type = "Parked Vehicle (Roadway or Shoulder)",
                  year = 2020)



###################### BY PEAK VERSUS OFF-PEAK#########################

# add column for "time_of_day" if not already there
crash_match_CAD$time_of_day = case_when(
  hour(crash_match_CAD$centraltime) >= 6 & hour(crash_match_CAD$centraltime) < 9 ~ "peak",
  hour(crash_match_CAD$centraltime) >= 16 & hour(crash_match_CAD$centraltime) < 19 ~ "peak",
  .default="off-peak"
  )

crash_match_waze$time_of_day = case_when(
  hour(crash_match_waze$time_local) >= 6 & hour(crash_match_waze$time_local) < 9 ~ "peak",
  hour(crash_match_waze$time_local) >= 16 & hour(crash_match_waze$time_local) < 19 ~ "peak",
  .default="off-peak"
)
  
parked_veh_match_CAD$time_of_day = case_when(
  hour(parked_veh_match_CAD$centraltime) >= 6 & hour(parked_veh_match_CAD$centraltime) < 9 ~ "peak",
  hour(parked_veh_match_CAD$centraltime) >= 16 & hour(parked_veh_match_CAD$centraltime) < 19 ~ "peak",
  .default="off-peak"
)
  
parked_veh_match_waze$time_of_day = case_when(
  hour(parked_veh_match_waze$time_local) >= 6 & hour(parked_veh_match_waze$time_local) < 9 ~ "peak",
  hour(parked_veh_match_waze$time_local) >= 16 & hour(parked_veh_match_waze$time_local) < 19 ~ "peak",
  .default="off-peak"
)


# create function to view by peak versus off-peak
view_by_time <- function(CAD_sf,
                         Waze_sf,
                         CAD_class,
                         Waze_type,
                         year){
  
  by_time_CAD <- CAD_sf %>% group_by(time_of_day) %>% summarize(match_percentage = mean(matched)*100) %>% st_drop_geometry()
  
  by_time_Waze <- Waze_sf %>% group_by(time_of_day) %>% summarize(match_percentage = mean(matched)*100) %>% st_drop_geometry()
  
  CADchart = ggplot(by_time_CAD) + 
    geom_col(aes(x = time_of_day, y = match_percentage)) +
    labs(title = "Percentage of CAD Crash Records with a \nWaze Match (for Peak/Off-Peak)")  +
    theme_minimal()
  ggsave(file.path(outputdir, paste0("CAD_by_time_", CAD_class, "_", year, ".png")), CADchart)
  
  Wazechart = ggplot(by_time_Waze) + 
    geom_col(aes(x = time_of_day, y = match_percentage)) +
    labs(title = "Percentage of Waze Crash Records with a \nCAD Match (for Peak/Off-Peak)")  +
    theme_minimal()
  ggsave(file.path(outputdir, paste0("Waze_by_time_", Waze_type, "_", year, ".png")), Wazechart)
}

view_by_time(CAD_sf = crash_match_CAD,
             Waze_sf = crash_match_waze,
             CAD_class = "Crash",
             Waze_type = "Accident",
             year = 2020)

view_by_time(CAD_sf = parked_veh_match_CAD,
             Waze_sf = parked_veh_match_waze,
             CAD_class = "Stall",
             Waze_type = "Parked Vehicle (Roadway or Shoulder)",
             year = 2020)


# Load HSIS crash data for 2020
crash_files <- list.files(file.path(inputdir, "Crash", state_osm), pattern = 'crash.shp$', full.names = TRUE)

timestamps <- read.csv(file.path(inputdir, 'Crash', state_osm, paste0(state_osm, '_timestamps.csv'))) %>%
  rename(INCIDEN = INCIDENT_ID) %>%
  mutate(centraltime = as.POSIXct(DATE_TIME_OF_INCIDENT, format = "%m/%d/%Y %H:%M", tz = "America/Chicago"),
         month = lubridate::month(centraltime))

HSIS20 <- read_sf(crash_files[5]) %>%
                    left_join(timestamps, by = 'INCIDEN') %>%
                    select(INCIDEN, centraltime, month) %>%
                    st_transform(projection) %>%
                    st_join(state_network, join = st_nearest_feature)


#### BELOW IS JUST NOTES #####

# for MN, the first 5 crash shapefiles were originally in WGS84 coordinate reference system (4326)
# and the 6th shapefile was in UTM zone 15N coordinate reference system (4269)...
# for CAD we'll have to assume that the coordinates are in 4326

