# Get Weather forecasts for Tennessee, using Weatherunderground API


# Setup ----
inputdir <- file.path(getwd(),"Input")
outputdir<- file.path(getwd(),"Output")

source('utility/get_packages.R')
ON_SDC = F
if(ON_SDC){
  teambucket <- "s3://prod-sdc-sdi-911061262852-us-east-1-bucket"
  user <- file.path( "/home", system("whoami", intern = TRUE)) # the user directory 
}

library(ggmap)
library(httr) # for GET
library(tidyverse)
library(xml2) # for xml parsing in tidy way
library(XML) # for xmlToList
library(jsonlite)

#setwd(inputdir)

proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +wx_datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Get weather wx_data ----

# Provided by TN: WeatherUnderground_171228.txt. This is a small script to be run in a Windows enviroment, to get XML-format files for 11 locations in TN. Here we'll read in the file as is, just to extract the API addresses
# WUndground *no longer is providing free API keys*, so we will see if the API key in these addresses will work. Free version works through end of 2018.

wu <- scan(file = file.path(inputdir,"Weather/WeatherUnderground_171228.txt"),
           what = 'character')


xmlnames <- wu[grep(".xml$", wu)]

outnames <- xmlnames[nchar(xmlnames) < 15]
innames <- xmlnames[nchar(xmlnames) > 15]

# Using TomorrowIO now for the actual API call
TomorrowIO = T
if(TomorrowIO) {
  w_key = scan(file.path(inputdir,"Weather", 'WeatherAPI_key.txt'), what = 'character')
  ds_ll_query = ll = vector()

  # This is the format needed for an API call in TomorrowIO, with three parameters that
  # need to be filled in (latitude, longitude, and API key):
  # https://api.tomorrow.io/v4/weather/forecast?location={latitude},{longitude}&apikey={key}
  
    for(i in innames){
    # extract the lat and lon separately and plug them into the query for API call
    i_ll = strsplit(i, '/')
    i_ll = i_ll[[1]][[length(i_ll[[1]])]]
    i_ll = sub('.xml', '', i_ll)
    i_ll_sep = strsplit(i_ll, ',')
    i_lat = sapply(i_ll_sep,"[[",1)
    i_lon = sapply(i_ll_sep,"[[",2)
    ds_ll_query = c(ds_ll_query,
                    paste0('https://api.tomorrow.io/v4/weather/forecast?location=',i_lat,',',i_lon,'&apikey=', w_key))
    ll = c(ll, i_ll)
  }
  innames = ds_ll_query
  
  # Parse JSON
  
  wx_dat <- vector()
  
  # uncomment this for testing/development 
   i <- 1
  
  # Loop over the json queries from this forecast and put into a data frame
  
  # THIS WAS AN OLD COMMENT BASED ON DARK SKY:
  # Use daily data for the next week, hourly only available for next 48 hours. Could combine these for more precise estimates within 48 hour window.
  
  # NEW RELATED COMMENT BASED ON TOMORROWIO:
  # Daily data are available for 6 days (including the current day, so only 5 future days)
  # Hourly data are available for 120 hours in the future (i.e. 5 future days), so about the 
  # same timeframe. We can discuss whether we want to shift to use hourly data instead of daily.
  for(i in 1:length(innames)){
    
    ll_i = strsplit(ll[i], ",")[[1]]
    #ll_i = ll[i]
    
    wx_dat_i = fromJSON(innames[i])
    
    #wx_dat_i = data.frame(lat = ll_i[1], lon = ll_i[2], wx_dat_i)
    
    # extract the weather attributes for the six days
    wx_dat_daily_values = wx_dat_i$timelines$daily$values
    # extract the dates for the six days
    wx_dat_daily_time = wx_dat_i$timelines$daily$time
    # assign the dates as a new column in wx_dat_daily_values
    wx_dat_daily_values$day <- wx_dat_daily_time
    # subset to only retain the necessary columns
    dailykeep <- c('day', 'temperatureMin','temperatureMax','snowAccumulationSum', 'rainAccumulationSum', 'sleetAccumulationLweSum','iceAccumulationSum')
    wx_dat_daily_values <- wx_dat_daily_values[,dailykeep]
    
    # extract the weather attributes for the hours
    wx_dat_hourly_values = wx_dat_i$timelines$hourly$values
    # extract the hours
    wx_dat_hourly_time = wx_dat_i$timelines$hourly$time
    # assign the hours as a new column in wx_dat_hourly_values
    wx_dat_hourly_values$hour <- wx_dat_hourly_time
    # subset to only retain the necessary columns
    hourlykeep <- c('temperature','snowAccumulation', 'rainAccumulation', 'sleetAccumulationLwe','iceAccumulation')
    
    wx_dat_i = data.frame(lat = ll_i[1], lon = ll_i[2], wx_dat_i)
    wx_dat = rbind(wx_dat, wx_dat_i)
    
  }

  # Get correct time zone from shapefile
  wx_dat$time <- as.POSIXct(wx_dat$time, origin = '1970-01-01', tz = 'America/Chicago')
  wx_dat$lat = as.numeric(as.character(wx_dat$lat))
  wx_dat$lon = as.numeric(as.character(wx_dat$lon))

# Key reference for cross-walking sp to sf commands:
# https://github.com/r-spatial/sf/wiki/migrating
# 
# from: https://stackoverflow.com/questions/73900335/alternatives-to-sptransfrom-function-due-retirement-of-rgdal
# You have to switch to using sf spatial data frames instead of sp spatial data frames 
# in order to use st_transform with sf.
# You construct these with functions like st_as_sf to convert from a data frame or st_read 
# to read from standard data formats like Shapefiles (although use GeoPackage if you can). 
# Tutorials are online.
# 
# The big problem is if you want to call functions in other packages that need sp data frames, 
# and have not been converted to use sf class data frames yet. In this case you should try and 
# keep your workflow using sf for as much as possible, and then convert from one format to the 
# other when necessary for interoperability:
#     
# sfdata = st_as_sf(spdata)
# spdata = as(sfdata, "Spatial")
# 
# See also: https://r-spatial.github.io/sf/reference/st_as_sf.html
# See also: https://r-spatial.github.io/sf/reference/st_transform.html

  # Project weather to Albers equal area, ESRI 102008
  proj <- showP4(showWKT("+init=epsg:102008"))
 
#  wx_dat.proj <- SpatialPointsDataFrame(coords = wx_dat[c('lon', 'lat')],
#                                    data = wx_dat,
#                                    proj4string = CRS("+proj=longlat +datum=WGS84"))
  wx_dat.proj <- st_as_sf(wx_dat,
                          coords = wx_dat[c('lon', 'lat')],
                          crs = CRS("+proj=longlat +datum=WGS84"))
  
#  wx_dat.proj <- spTransform(wx_dat.proj, CRS(proj.USGS))
  wx_dat.proj <- st_transform(wx_dat.proj, CRS(proj.USGS))  
  
  # Overlay timezone ----
  # Read tz file
  
#  tz <- readOGR(file.path(inputdir,"Shapefiles", 'TimeZone'), layer = 'combined-shapefile')
  tz <- read_sf(file.path(inputdir,"Shapefiles", 'TimeZone'), layer = 'combined-shapefile')  
  
#  tz <- spTransform(tz, CRS(proj.USGS))
  tz <- st_transform(tz, CRS(proj.USGS))

# replace this next one with one of the two commands from:  https://github.com/r-spatial/sf/wiki/migrating
# Figure out which one is appropriate
#  wx_tz <- over(wx_dat.proj, tz[,"tzid"]) # Match a tzid name to each row in wx_dat.proj weather wx_data 
  wx_tz <- st_intersects(wx_dat.proj, tz[,"tzid"]) # Match a tzid name to each row in wx_dat.proj weather wx_data   
  
  wx_dat.proj@data <- data.frame(wx_dat.proj@data, tzid = as.character(wx_tz$tzid))
  
  # Loop over every row and apply the correct time zone to the weather forecast times
  local_time = vector()
  for(k in 1:nrow(wx_dat.proj)) {
    local_time = c(local_time, format(as.POSIXct(wx_dat.proj$time[k], origin = '1970-01-01', format = '%F', 
                                     tz = as.character(wx_dat.proj$tzid[k])),
                          "%Y-%m-%d %H:%M:%S", usetz = T))
    
  } # end loop over rows for local_time
  wx_dat <- data.frame(wx_dat, local_time)
  wx_dat.proj@data <- data.frame(wx_dat.proj@data, local_time)
  
  fn = paste0("TN_Forecasts_", Sys.Date(), ".RData")
  
  save(list=c('wx_dat', 'wx_dat.proj'),
       file = file.path(inputdir, "Weather", fn))
  if(ON_SDC){
    # Copy to S3
    system(paste("aws s3 cp",
                 file.path(inputdir, "Weather", fn),
                 file.path(teambucket, "TN", "Weather", fn)))
  }
}

if(!DARKSKY){
  # WUNDERGROUND ONLY 
  # Parse XML ----
  
  # Parse to data frame, apply to spatial grid, overlay on crash wx_data for use in models
  # Useful to look at elements of the XML file
  # wx_dat <- xmlParse(get(xmls[1]))
  # wx_dat
  # xmlToList(wx_dat) 
  
  # Within simpleforecast, pick out forecastdays, then each forecastday
  # wx_date: yday, hour
  # high: fahrenheit
  # low: fahrenheit
  # conditions
  # qpf_allday: in
  # snow_allday: in
  # maxwind: mph
  # avewind: mph
  
  wx_dat <- vector()
  
  # Loop over the xml files from this forecast and put into a wx_data frame
  for(i in 1:length(xmls)){
    check <- xmlToList(xmlParse(get(xmls[i])))[1]
    check <- grep("backend failure", check)
    if(length(check) > 0) { cat("XML query", xmls[i], "failed, skipping \n")
      
      } else { 
      
        d1 <- read_xml(get(xmls[i]))
      
        ll <- unlist(strsplit(innames[i], "/")); ll <- ll[length(ll)]
        ll <- unlist(strsplit(ll, ","))
        
        lat <- as.numeric(ll[1])
        lon <- as.numeric(sub("\\.xml", "", ll[2])) 
        yr <- d1 %>% xml_find_all("//wx_date/year") %>% xml_text()
        ydays <- d1 %>% xml_find_all("//wx_date/yday") %>% xml_text()
        th <- d1 %>% xml_find_all("//high/fahrenheit") %>% xml_double()
        tl <- d1 %>% xml_find_all("//low/fahrenheit") %>% xml_double()
        cond <- d1 %>% xml_find_all("//conditions") %>% xml_text()
        qpf <- d1 %>% xml_find_all("//qpf_allday/in") %>% xml_double() # accumulated precip
        snow <- d1 %>% xml_find_all("//snow_allday/in") %>% xml_double()
        maxwind <- d1 %>% xml_find_all("//maxwind/mph") %>% xml_double()
        avewind <- d1 %>% xml_find_all("//avewind/mph") %>% xml_double()
        
        wx_dat <- rbind(wx_dat, wx_data.frame(lat, lon, yr, ydays, th, tl, cond, qpf, snow, maxwind, avewind))
      }
  }

  
  
  
  # Save forecasts ----
  
  save(list='wx_dat',
       file = file.path(inputdir, "Weather", paste0("TN_Forecasts_", Sys.wx_date(), ".Rwx_data")))
  
  forecasts <- dir(file.path(inputdir, "Weather"))[grep("^TN_Forecasts_", dir(file.path(inputdir, "Weather")))]
  
  if(ON_SDC){
    # Copy to S3
    for(f in forecasts){
      system(paste("aws s3 cp",
                   file.path(inputdir, "Weather", f),
                   file.path(teambucket, "TN", "Weather", f))) 
  }
  }
  
  
# Next steps: interpolate to state level using kriging or other methods, see 
# http://rspatial.org/analsis/rst/4-interpolation.html
} # end Weather Underground

