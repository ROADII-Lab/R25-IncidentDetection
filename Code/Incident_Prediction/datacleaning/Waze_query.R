# Waze events by month (based on local time) 
# Query Redshift database for one month at a time, export Waze records 
source("utility/get_packages.R")
Sys.setenv(TZ="UTC")

library(tidyverse) 
library(lubridate)
library(sf)

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

#year <- 2019

# Note some states have multiple regions
#state <- "NY"

# Indicate whether the state has a unified time zone
#one_zone <-TRUE
# If one_zone is set to T or TRUE, meaning that the state has one time zone, specify the name of the time zone, selecting from 
# among the options provided after running the first line below (OlsonNames())

#OlsonNames()
#time_zone_name <- "US/Eastern"
#time_zone_name <- "US/Central"
# time_zone_name <- "US/Pacific"

#if(!dir.exists(tempdir)) {dir.create(tempdir)}

wazedir <- file.path(inputdir,"Waze",state)

if(!dir.exists(wazedir)) {dir.create(wazedir)}

yearmonths = c(
#  paste(2019, formatC(1:12, width = 2, flag = "0"), sep="-"),
#  paste(2020, formatC(1:12, width = 2, flag = "0"), sep="-"),
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
#i <- 1
# i in 1:length(yearmonths)

for(i in 1:length(yearmonths)){
  
  file_save_name <- file.path(wazedir, 
                              paste0('Waze_', state,"_",yearmonths[i],'.csv'))
  
  # Check to see if already completed this one
  if(!file.exists(file_save_name)){
    cat(i, ' - ', yearmonths[i], '\t', format(Sys.time(), '%c'), '\n')

    alert_query <- paste0("SELECT alert_uuid, state, alert_type, sub_type, location_lat, location_lon, pub_utc_timestamp, road_type, reliability 
                          FROM dw_waze.alert 
                          WHERE state = '",state,"' AND 
                          pub_utc_timestamp BETWEEN to_timestamp('", yearmonths.1[i],"','YYYY-MM-DD HH24:MI:SS') AND to_timestamp('", yearmonths.end[i], "','YYYY-MM-DD HH24:MI:SS')")

    results <- DBI::dbGetQuery(conn, alert_query) 
      
    # De-duplicate based on alert_uuid. After de-duplicating, drop alert_uuid
    dups <- duplicated(results$alert_uuid)
    results <- results[!dups,-1] 
      
    numconv <- c("location_lon", "location_lat")
      
    results[numconv] <- apply(results[numconv], 2, function(x) as.numeric(x))
      
    # Omit Railroad events. These are special event types which show up as HAZARD_ON_ROAD_OBJECT, have repeating location and timestamp values, but distinct alert_uuids, which is why they show up a distinct events in this results_multi data frame. We could consider applying this process to every sub_type, but it is computationally expensive and we only have reason to believe this sub_type is a problem.
    # We only observed this problem in CT and NY, but apply it here to all states to be sure.
    rx = results
    rx = rx %>% filter(sub_type == 'HAZARD_ON_ROAD_OBJECT')
    rd <- duplicated(rx %>% dplyr::select(location_lat, location_lon, pub_utc_timestamp))
    rx <- rx[!rd,]
      
    # Replace that part of the results_multi data frame with the new de-duplicated rx data frame
    results = results %>% filter(sub_type != 'HAZARD_ON_ROAD_OBJECT')
    results = rbind(results, rx)
    
    results <- results %>% 
      mutate(lon = location_lon, lat = location_lat) 
    
    if(!one_zone){
      results <- results %>% 
        st_as_sf(coords = c("location_lon", "location_lat"),
                 crs = st_crs('+proj=longlat datum=WGS84')) %>%
        st_transform(crs=projection) %>%
        st_join(timezone_adj, join = st_nearest_feature) %>% 
        mutate(time_local = as.POSIXct(pub_utc_timestamp, tz = tz_name))
      } else {
        results <- results %>% mutate(time_local = as.character(as.POSIXct(pub_utc_timestamp, tz = time_zone_name)))
        }
    
    results <- results %>% 
      mutate(YEAR = as.integer(lubridate::year(time_local))) %>%
      filter(YEAR==year) %>%
      select(alert_type, sub_type, road_type, reliability, time_local, pub_utc_timestamp, lon, lat)
      
    if(!one_zone){
      results <- results %>% 
        st_transform(4326) %>% 
        st_drop_geometry()
      }
    
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

