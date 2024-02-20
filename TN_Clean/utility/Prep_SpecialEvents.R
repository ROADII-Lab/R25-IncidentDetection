#Not needed for TN Case Study, as processed special events data has been provided. Code provided for completeness.

# Prepare historical crash data for random forest work
# Need to apply points to grids
# This now expands the special events from a point to a buffered polygon. Currently using a fixed 3 mile radius for each special event, but this can be changed; consider different radius by event type.
# Also, now using America/Chicago for all analysis steps.

# Run from RandomForest_WazeGrid_TN.R
# already in memory are inputdir and g, which repreresents the grid type to use
# Run after TN_Data_Format.R for initial formatting of crash and special event data

# append.hex2(hexname = w, data.to.add = "TN_SpecialEvent", state = state, na.action = na.action)
library(rgeos)
library(rgdal)
library(doParallel)
library(foreach)


# # If not running from RandomForest_Wazegrid_TN.R, set it up manually by running following code.
# grids = c("TN_01dd_fishnet",
#           "TN_1sqmile_hexagons")
# g = grids[2]
# 
# user <- if(length(grep("@securedatacommons.com", getwd())) > 0) {
#   paste0( "/home/", system("whoami", intern = TRUE), "@securedatacommons.com")
# } else {
#   paste0( "/home/", system("whoami", intern = TRUE))
# } # find the user directory to use
# 
# inputdir <- "~/TN/Input"


# Check to see if these processing steps have been done yet; load from prepared file if so
prepname = paste("Prepared", "TN_SpecialEvent", g, sep="_")

# Apply holidays statewide later.

if(!file.exists(file.path(inputdir, "SpecialEvents", prepname))) { # if doen't exist in TN/SpecialEvents, make it
  
  cat("Preparing", "TN_SpecialEvent", g, "\n")

  proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  # Combine 2017, 2018, 2019 data
  load(file.path(inputdir, "SpecialEvents", "TN_SpecialEvent_2017.RData")) # 9 columns
  spev2017 <- spev # 670 rows
  load(file.path(inputdir, "SpecialEvents", "TN_SpecialEvent_2018.RData")) # 13 columns
  spev2018 <- spev[,names(spev2017)] # 813 rows
  load(file.path(inputdir, "SpecialEvents", "TN_SpecialEvent_2019.RData")) # 13 columns
  spev2019 <- spev[,names(spev2017)] # 634 rows
  
  spev <- rbind(spev2017, spev2018, spev2019) # 2117 rows
  table(spev$TimeZone) # All have TimeZone information, except statewide holidays. Only have for 2017. 
    
  dd <- spev %>% filter( ( !is.na(Lon) | !is.na(Lat) ) & TimeZone != 'error') 
  
  # Make numeric, make sure no white spaces are there
  dd$Lat <- as.numeric(gsub("[[:space:]]", "", dd$Lat))
  dd$Lon <- as.numeric(gsub("[[:space:]]", "", dd$Lon))
  
  # Apply to grid
  grid_shp <- rgdal::readOGR(file.path(inputdir, "Shapefiles"), layer = g)
  grid_shp <- spTransform(grid_shp, CRS(proj.USGS))
  
  # Project special events
  dd <- SpatialPointsDataFrame(dd[c("Lon", "Lat")], dd, 
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))  
  
  dd <- spTransform(dd, CRS(proj.USGS))
  
  buffdist.mi = 3 # Change this to have different buffers by event type
  
  buffdist <- buffdist.mi * 1609.344 # units are meters in shapefiles
  grid_dd_pip <- vector()
  
  for(b in 1:nrow(dd)){
    xx <- gBuffer(dd[b,], width =  buffdist) # produces class SpatialPolygons
  
    xx <- SpatialPolygonsDataFrame(xx, data = data.frame(dd@data[b,], row.names = 'buffer'))
  
    g.i <- gIntersects(xx, grid_shp[,"GRID_ID"], byid = T) # over() to match one grid ID. gIntersects(byid = T) for multiple
    
    grid_ids <- grid_shp$GRID_ID[g.i]
    
    # Omit any which are outside this grid
    if( length(grid_ids) > 0 ) {
      grid_dd_pip <- rbind(grid_dd_pip, data.frame(dd[b,], GRID_ID = grid_ids))
    }
  }
  
  dd <- grid_dd_pip 
  # Add year, day of year
  dd$Year = format(dd$Event_Date, "%Y")
  dd$day = format(dd$Event_Date, "%j")    
  
  # Expand grid by hour
  # If there is no start / end time, apply from 0 to 24
  
  # Make framework to join into. Max in 2019 now
  dd <- dd %>% filter(Event_Date <= '2019-12-31')
  
  all.hour <- seq(from = as.POSIXct(paste(min(dd$Event_Date), "0:00"), tz = "America/Chicago"), 
                    to = as.POSIXct(paste(max(dd$Event_Date), "0:00"), tz = "America/Chicago"),
                    by = "hour")
  
  GridIDTime <- expand.grid(all.hour, unique(dd$GRID_ID))
  names(GridIDTime) <- c("GridDayHour", "GRID_ID")
  
  # Convert all to Central for consistency. We will use Central time (America/Chicago) for all analysis, and convert results back to appropriate local time as needed.
  # Cannot pass tz arguments as a vector of mixed tz in strptime. Need to loop.
  to_tz = "America/Chicago"
  start.hr <- end.hr <- vector()
  
  for(t in 1:nrow(dd)){
    start.date.hr <- as.POSIXct(paste(dd$Event_Date[t], 
                                      dd$StartTime[t]), tz = dd$TimeZone[t], format = "%Y-%m-%d %H:%M:%S")
   
    s.hr <- as.numeric(format(start.date.hr, format = "%H", tz = to_tz))
    s.hr[is.na(s.hr)] = 0 # start at midnight for all day events

    end.date.hr <- as.POSIXct(paste(dd$Event_Date[t], 
                                      dd$EndTime[t]), tz = dd$TimeZone[t], format = "%Y-%m-%d %H:%M:%S")
    
    e.hr <- as.numeric(format(end.date.hr, format = "%H", tz = to_tz))
    e.hr[is.na(e.hr)] = 23 # end at 11pm hr for all day events
    
    start.hr <- c(start.hr, s.hr)
    end.hr <- c(end.hr, e.hr)
  }
  
  dd <- data.frame(dd, 
                   start.hr = as.POSIXct(paste(dd$Event_Date, 
                                               start.hr), tz = to_tz, format = "%Y-%m-%d %H"),
                   end.hr = as.POSIXct(paste(dd$Event_Date, 
                                             end.hr), tz = to_tz, format = "%Y-%m-%d %H"))
  
  # Apply special events to GridIDTime ----
  # Now as parallel, to speed it up
  
  cl <- makeCluster(parallel::detectCores())
  registerDoParallel(cl)
  
  StartTime <- Sys.time()
  t.min = min(GridIDTime$GridDayHour)
  t.max = max(GridIDTime$GridDayHour)
  
  hr.seq <- seq(t.min, t.max, by = 3600)
  
  spev.grid.time.all = foreach(hr = hr.seq, 
                              .combine = rbind,
                              .packages = c('dplyr')) %dopar% { # hr = hr.seq[1]
    
    ti.GridIDTime = filter(GridIDTime, GridDayHour == hr)
    
    ti.spev = filter(dd, start.hr >= hr & start.hr <= hr+3600 | end.hr >= hr & end.hr <= hr+3600)
    
    ti.spev.hex <- inner_join(ti.GridIDTime, ti.spev, by = "GRID_ID") # Use left_join to get zeros if no match  

    ti.spev.hex
  } # end parallel loop
  
  EndTime <- Sys.time() - StartTime
  cat(round(EndTime, 2), attr(EndTime, "units"), "\n")
  stopCluster(cl)
  
  # Deduplicate
  
  spev.grid.time.all <- spev.grid.time.all[!duplicated(spev.grid.time.all),]   
  
  # Add holidays ----
  
  dd <- spev %>% filter( TimeZone == 'error') 
  
  # for these whole days, state-wide, apply to all grid cells and hours. Simply make a new data frame by repeating grid ID by hour
  grid_id = unique(GridIDTime$GRID_ID)
  hours = formatC(seq(0, 23, by = 1), width = 2, flag = 0)
  gdh.dd = vector()
  
  for(i in 1:nrow(dd)){
    
    dh = strptime(paste(dd$Event_Date[i], paste0(hours, ":00:00")),
             tz = to_tz,
             format = "%Y-%m-%d %H:%M:%S")
    gdh =   expand.grid(GridDayHour = dh, GRID_ID = grid_id)

    gdh.dd = rbind(gdh.dd, data.frame(gdh, dd[i,]) )
    
  }

  
  spev.grid.time <- filter(spev.grid.time.all, !is.na(GRID_ID))   
  
  spev.grid.time.holiday <- rbind(spev.grid.time[names(gdh.dd)],
                                  gdh.dd)
  spev.grid.time.holiday = spev.grid.time.holiday[order(spev.grid.time.holiday$GridDayHour, spev.grid.time.holiday$GRID_ID),]
  
  save(list = c("spev.grid.time", "spev.grid.time.holiday"), 
       file = file.path(inputdir, "SpecialEvents", paste0(prepname, ".RData")))
  
  rm(dd, grid_dd_pip, grid_shp, spev2017, spev2018, spev2019, cl,
     dh, g.i, gdh, gdh.dd, GridIDTime, spev, xx,
     all.hour, b, buffdist, buffdist.mi, e.hr, end.date.hr,
     end.hr, grid_id, grid_ids, hours, hr.seq, i, s.hr, start.date.hr, start.hr,
     t.max, t.min, t)
  
} else {
  load(file.path(inputdir, "SpecialEvents", paste0(prepname, ".RData")))
}


