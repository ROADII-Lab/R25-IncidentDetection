# Waze events by month (based on local time) 
# Query Redshift database for one month at a time, export Waze records 
source("utility/get_packages.R")
Sys.setenv(TZ="UTC")

library(tidyverse) 
library(lubridate)
library(sf)
library(osmdata)

# Location of SDC SDI Waze team S3 bucket. Files will first be written to a temporary directory in this EC2 instance, then copied to the team bucket.
teambucket <- "s3://prod-sdc-sdi-004118380849"

# Project-specific temp out directory
user <- normalizePath('~/')
tempdir <- normalizePath(file.path(user, "/incident_tempout/")) # full path for readOGR
workingdir <- normalizePath(file.path(user, 'workingdata'))

inputdir <- file.path(getwd(),"Input")
outputdir<- file.path(getwd(),"Output")

# Projection 
projection <- 5070

#Timezones --------------------------------------------------------------
onSDC <- T

if(onSDC){
  US_timezones <- st_read(file.path(inputdir,"Shapefiles","Time_Zones","time_zones_ds_timezone_polygons.shp"))
}else{
  US_timezones <- st_read("https://geo.dot.gov/server/rest/services/Hosted/Time_Zones_DS/FeatureServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson")
}

# OlsonNames()
tz_adj_to_names <- data.frame(tz_adj = c(-5,-6,-7,-8,-9,-10,-11), tz_name = c("US/Eastern","US/Central","US/Mountain","US/Pacific", "US/Alaska", "US/Hawaii", "US/Samoa"))

timezone_adj <- US_timezones %>% st_transform(crs=projection) %>% 
  mutate(adjustment = as.numeric(paste0(str_sub(utc, 1, 1), str_sub(utc, 2, 3)))) %>% 
  select(adjustment) %>% 
  left_join(tz_adj_to_names,by = join_by(adjustment==tz_adj))

#------------------------------------------------------------------------

# Connect to Redshift 
source('utility/connect_redshift_pgsql.R')

### Query parameters ----

# Get year/month, year/month/day, and last day of month vectors to create the SQL queries
# Set up for monthly queries

#year <- 2021

# Note some states have multiple regions
#state <- "MN"

# Indicate whether the state has a unified time zone
#one_zone <-TRUE
# If one_zone is set to T or TRUE, meaning that the state has one time zone, specify the name of the time zone, selecting from 
# among the options provided after running the first line below (OlsonNames())

#OlsonNames()
#time_zone_name <- "US/Eastern"
#time_zone_name <- "US/Central"
#time_zone_name <- "US/Pacific"

#if(!dir.exists(tempdir)) {dir.create(tempdir)}

#-------------------------------------------------------

# Load Road Data ----------------------

state_osm <- ifelse(state == "WA", 'Washington State',
                    ifelse(state == "MN", "Minnesota", NA))

state_osm <- gsub(" ", "_", state_osm) # Normalize state name

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
  state_network <- read_sf(file_path) %>% select(osm_id, ref)
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
    select(osm_id, geometry)
  
  write_sf(state_network, file.path(inputdir,'Roads_Boundary', state_osm, paste0(network_file, '.gpkg')), driver = "ESRI Shapefile")
  write_sf(state_maps, file.path(inputdir,'Roads_Boundary', state_osm, paste0(boundary_file, '.gpkg')), driver = "ESRI Shapefile")
  rm(state_border)
}

#ggplot() + geom_sf(data = state_network)

if(st_crs(state_network) != projection){
  state_network <- st_transform(state_network, projection)
}

# Add timezone for each road segment (osm_id)
if(!one_zone){
  state_network <- state_network %>%
    st_join(timezone_adj, join = st_nearest_feature)
} else {
  state_network <- state_network %>% 
    mutate(tz_name = time_zone_name)
}

# The line commented out below created an object that was once used for filtering out jams that are too far from 
# the road network we are using (suggesting they actually belong to residential or unclassified roads, which we 
# are not including. In practice this is not necessary because so few of the jams occur on these road types anyway.
# Now skipping this step because the filtering takes too long, for little benefit.

# network_buffer <- st_buffer(state_network, dist = 20, endCapStyle = "FLAT") %>% select(!colnames(state_network)) %>% st_union()

#-------------------------------------------------------

wazedir <- file.path(inputdir,"Waze", "jams", state)

if(!dir.exists(wazedir)) {dir.create(wazedir, recursive = T)}

yearmonths = c(
  paste(as.character(year), formatC(1:12, width = 2, flag = "0"), sep="-")
)

yearmonths.1 <- paste(yearmonths, "01", sep = "-")
lastdays <- days_in_month(as.POSIXct(yearmonths.1)) # from lubridate
yearmonths.end <- paste(yearmonths, lastdays, sep="-")

# modify query to also get 11 hours of data from the prior month and 10 hours from the next month, 
# so that when switching to local time the month will still be complete.
yearmonths.1 <- format(as.POSIXct(yearmonths.1) + hours(-10))
yearmonths.end <- format(as.POSIXct(yearmonths.end) + hours(11))

# Query by month  ----
starttime = Sys.time()

# uncomment for testing
i <- 1
# i in 1:length(yearmonths)

for(i in 1:length(yearmonths)){
  
  file_save_name <- file.path(wazedir,
                              paste0('Waze_jams_', state,"_",yearmonths[i],'.csv'))
  
  # Check to see if already completed this one
  if(!file.exists(file_save_name)){
    cat(i, ' - ', yearmonths[i], '\t', format(Sys.time(), '%c'), '\n')

    # Using SELECT DISTINCT in the below query to eliminate redundant entries in the jam table, 
    # where everything is the same except the 'delay' is slightly different. 
    # Using 'level' to measure severity instead of 'delay,' because it has fewer unique values (1 - 5).
    
    # When joining the jam_point_sequence table to the subset of the jam table there are two conditions for the join: 
    # not only must the IDs match, but also the 'persist_timestamp' from the jam_point_sequence table
    # must be no more than one week later than the pub_utc_timestamp from the jam table. This eliminates the errant association of records 
    # from another disparate event, where the ID is repurposed.
    
    jams_query <- paste0("SELECT * 
                         FROM (SELECT DISTINCT jam_uuid, state, level, street, pub_utc_timestamp, pub_utc_timestamp + interval '7 day' AS weeklater FROM dw_waze.jam WHERE pub_utc_timestamp BETWEEN to_timestamp('", yearmonths.1[i],"','YYYY-MM-DD HH24:MI:SS') AND to_timestamp('", yearmonths.end[i], "','YYYY-MM-DD HH24:MI:SS') AND state = '",state,"') a
                         JOIN
                         (SELECT jam_id, location_x, location_y, sequence_order, persist_timestamp FROM dw_waze.jam_point_sequence WHERE persist_timestamp BETWEEN to_timestamp('", yearmonths.1[i],"','YYYY-MM-DD HH24:MI:SS') and (to_timestamp('", yearmonths.end[i], "','YYYY-MM-DD HH24:MI:SS')) + interval '7 day') b
                         ON a.jam_uuid = b.jam_id and b.persist_timestamp < a.weeklater")
    
    starttime = Sys.time()
    results <- DBI::dbGetQuery(conn, jams_query) 
    
    numconv <- c("location_y", "location_x")
      
    results[numconv] <- apply(results[numconv], 2, function(x) as.numeric(x))
    
    results <- results %>% 
      st_as_sf(coords = c("location_x", "location_y"),
               crs = st_crs('+proj=longlat datum=WGS84')) %>%
      st_transform(crs=projection) %>%
      st_join(state_network %>% select(osm_id, tz_name, ref), join = st_nearest_feature) %>%
      mutate(time_local = as.POSIXct(pub_utc_timestamp, tz = tz_name))
    
    # the original purpose of st_intersection below was to remove the records where the point is too far away a road segment
    # because these probably belong to "residential" or "unclassified" roads that are not part of the network we are using.
    # In practice this is not necessary because so few of the jams occur on these road types anyway.
    # Now skipping this step because the filtering takes too long, for little benefit. (commented out below)
    
    # results <- st_intersection(results, network_buffer)
  
    get_mode <- function(values){which.max(tabulate(values))}
    
    results <- results %>% 
      mutate(YEAR = as.integer(lubridate::year(time_local))) %>%
      filter(YEAR==year) %>%
      select(osm_id, level, pub_utc_timestamp, time_local) %>%
      mutate(month_local = as.integer(lubridate::month(time_local)),
             day_local = as.integer(lubridate::day(time_local)),
             hour_local = as.integer(lubridate::hour(time_local))) %>%
      st_drop_geometry() %>%
      filter(as.numeric(month_local) == i) %>%
      select(osm_id, month_local, day_local, hour_local, level, pub_utc_timestamp) %>%
      group_by(osm_id, month_local, day_local, hour_local) %>%
      summarise(level_mode = get_mode(level),
                pub_utc_timestamp_mean = mean(pub_utc_timestamp)) %>%
      ungroup()
    
    readr::write_csv(results, file = file_save_name)
    
    cat(format(nrow(results), big.mark = ","), "observations in", i, "-", yearmonths[i],"\n")
    timediff <- Sys.time() - starttime
    cat(round(timediff, 2), attr(timediff, "units"), "elapsed \n\n")
      
    rm(results); gc()
  } # end check if file exists
} # end for loop

dbDisconnect(conn) #  disconnect to prevent leakage
gc()

timediff <- Sys.time() - starttime
cat(round(timediff, 2), attr(timediff, "units"), "elapsed \n\n") 

# Save ----
# system(paste(
#   'aws s3 cp',
#   path.expand(file.path(wazedir, zipname)),
#   file.path(teambucket, 'export_requests', zipname)
# ))

