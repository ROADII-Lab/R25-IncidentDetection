# Prepare historical weather for random forest work

# Clear 
rm(list=ls())

# Run from RandomForest_WazeGrid_TN.R
# already in memory are  g, which repreresents the grid type to use
# Next step after this script is to run append.hex function to add 
censusdir <- file.path(getwd(),"Input","census")
outputdir <- file.path(getwd(),"Output") # to hold daily output files as they are generated
inputdir <- file.path(getwd(),"Input")

# Update to specify state of interest here - TN by default
state <- 'TN'
# Update to specify year of interest here - 2019 by default
year <- 2019

proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#### TO DELETE LATER #####
g <- "TN_01dd_fishnet"
##########################
# Check to see if these processing steps have been done yet; load from prepared file if so
prepname = paste("Prepared", "Weather", state, year, sep="_")

#if(length(grep(prepname, dir(file.path(inputdir, "Weather")))) == 0) { 
  library(gstat) # For kriging
  #library(raster) # masks several functions from dplyr, use caution
  library(terra) # replaces the raster package (deprecated)
  library(tidyterra)
  library(doParallel)
  library(foreach)
  library(tidyverse)
  library(sf)
  library(sp)
  library(osmdata)
  library(stars)
  library(starsExtra)
  library(cubelyr)

  # Change out for road network? 
  # Read in grid
  #grid_shp <- rgdal::readOGR(file.path(inputdir, "Shapefiles"), layer = g)
  grid_shp <- read_sf(dsn = file.path(inputdir, "Shapefiles"), layer = g)
  
  #grid_shp <- spTransform(grid_shp, CRS(proj.USGS))
  grid_shp <- st_transform(grid_shp, crs = st_crs(proj.USGS))
  
  # Read in buffered state shapefile
  #tn_buff <- readOGR(censusdir, layer = "TN_buffered")
  tn_buff <- read_sf(dsn = censusdir, layer = "TN_buffered")
  
  #tn_buff <- spTransform(tn_buff, CRS(proj.USGS))
  tn_buff <- st_transform(tn_buff, crs = st_crs(proj.USGS))
  
  grd1 <- tn_buff %>% 
    st_make_grid(cellsize = c(7540.43,2630.079), what = "polygons", square = T) %>% 
    st_as_stars()
    
  data(meuse.grid)
  
  ggplot() +
    geom_stars(data = grd1) + 
    geom_sf(data = tn_buff)
  
  # Clip grid to county shapefile
  #grid_intersects <- gIntersects(tn_buff, grid_shp, byid = T)
  grid_intersects <- st_intersects(tn_buff, grid_shp, sparse = FALSE)
  
  grid_shp <- grid_shp[as.vector(grid_intersects),]
  
  
  grd2 <- tn_buff %>% 
    st_make_grid(cellsize = c(7540.43,2630.079), what = "polygons", square = T) %>% 
    st_intersection(tn_buff) 
  
  plot(grd2)
  
  grd2.SP <- as_Spatial(grd2)


  cat("Preparing", "Weather", state, year, "\n")
  

  
  
  # Read in list of GHCN hourly stations
  stations <- read.csv(file=file.path(inputdir, "Weather","GHCN","Hourly","ghcnh-station-list.csv"), 
                       header = FALSE, strip.white = TRUE)
  stations <- stations[,1:6]
  colnames(stations) <- c('Station_ID','Latitude','Longitude','Elevation','State','Station_name') 
  # Filter down to the list of stations in the state of interest
  state_stations <- stations[stations$State==state,]
  # Filter down to the list of stations for which we have data in the year of interest
  available <- logical(nrow(state_stations))
  for (i in 1:nrow(state_stations)) {
    ID = state_stations[i,'Station_ID']
    filename = paste0('GHCNh_',ID,'_',year,'.psv')
    print(filename) # check what it is checking
    ifelse(file.exists(file.path(inputdir, "Weather", "GHCN", "Hourly", year, state, filename)
                      ),
           available[i] <- TRUE,
           available[i] <- FALSE
           )
    }
  state_stations <- state_stations[available,]
  
  # Initialize empty dataframe to store the hourly data from the files
  state_hourly_hist_weather <- data.frame(Station_ID=character(),
                                   Station_name=character(),
                                   Year=integer(),
                                   Month=integer(),
                                   Day=integer(),
                                   Hour=integer(),
                                   Latitude=numeric(),
                                   Longitude=numeric(),
                                   Elevation=numeric(),
                                   temperature=numeric(),
                                   precipitation=numeric(),
                                   snow_depth=numeric()
  )
  # Read in the data from each station file for the state and compile
  for (i in 1:nrow(state_stations)){
    ID = state_stations[i,'Station_ID']
    filename = paste0('GHCNh_',ID,'_',year,'.psv')
    df_i = read.table(file.path(inputdir, "Weather", "GHCN", "Hourly", year, state, filename),
                      header=TRUE,
                      sep = "|",
                      fill = TRUE
    ) %>% dplyr::select('Station_ID',
                 'Station_name', 
                 'Year', 
                 'Month', 
                 'Day', 
                 'Hour', 
                 'Latitude', 
                 'Longitude', 
                 'Elevation', 
                 'temperature', 
                 'precipitation', 
                 'snow_depth')
    state_hourly_hist_weather <- rbind(state_hourly_hist_weather,df_i)
  }
  
  # data processing 
  
  wx <- state_hourly_hist_weather %>% 
    dplyr::filter(Month == 1 & Day == 1) %>% 
    mutate(Date = ymd_h(paste0(Year, "-", Month, "-", Day, " ", Hour))) %>% 
    group_by(Date, Station_ID, Longitude, Latitude, Elevation) %>% # variables to keep 
    summarise(precipitation = mean(precipitation, na.rm = TRUE)) %>% # gets rid of duplicate hour reports for a station
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>% 
    st_transform(crs = proj.USGS)
  
  all_wx_hours <- unique(wx$Date)
  
  x = all_wx_hours[1]
  
  wx.hour <- wx %>% dplyr::filter(Date == x)
  
  f.p <- as.formula(precipitation ~ 1)
  
  vg_prcp <- gstat::variogram(precipitation ~ 1, locations = wx.hour %>% filter(!is.na(precipitation)))
  dat.fit <- fit.variogram(vg_prcp, fit.ranges = F, fit.sills = F,
                           vgm(model = "Sph"))
  #plot(vg_prcp, dat.fit) # Plot the semi variogram. 
  dat.krg.precipitation <- krige(f.p, wx.hour %>% filter(!is.na(precipitation)), grd1, dat.fit)
  
  dat.krg.precipitation_df <- dat.krg.precipitation %>% as.data.frame 
  
  
  
  wx.SP <- as_Spatial(wx)

    # Kriging. Intercept only = ordinary kriging.
    cl <- makeCluster(parallel::detectCores())
  registerDoParallel(cl)
  
  # 
  wx_test <- wx %>% dplyr::filter(Date == x)
  
  ggplot() +
    geom_sf(data = wx_test)
  
  ggplot() +
    geom_sf(data = grd2) + 
    geom_sf(data = wx_test)
   
  ggplot() +
    geom_sf(data = grid_shp) + 
    geom_sf(data = wx_test)
    
  for(x in all_wx_hours){
    
    x = all_wx_hours[1]
    wx.hour = wx.SP[wx.SP$Date == x,]
  
  f.p <- as.formula(precipitation ~ 1)
  
  vg_prcp <- gstat::variogram(precipitation ~ 1, locations = wx.hour[!is.na(wx.hour$precipitation),])
  dat.fit <- fit.variogram(vg_prcp, fit.ranges = F, fit.sills = F,
                           vgm(model = "Sph"))
  #plot(vg_prcp, dat.fit) # Plot the semi variogram. 
  dat.krg.precipitation <- krige(f.p, wx.hour[!is.na(wx.hour$precipitation),], grd2.SP, dat.fit)
  
  dat.krg.precipitation_df <- dat.krg.precipitation %>% as.data.frame 
  
  # Rasterize
  precipitation_r <- terra::rast(dat.krg.precipitation)
  precipitation_r <- mask(precipitation_r, grid_shp)

  plot(precipitation_r, values)
  
  # Now do tmin, tmax, and snow
  f.tmin <- as.formula(TMIN ~ 1)
  vg_tmin <- variogram(TMIN ~ 1, wx.day[!is.na(wx.day$TMIN),])
  dat.fit <- fit.variogram(vg_tmin, fit.ranges = F, fit.sills = F,
                           vgm(model = "Sph"))
  dat.krg.tmin <- krige(f.tmin, wx.day[!is.na(wx.day$TMIN),], grd, dat.fit)
  tmin_r <- raster::raster(dat.krg.tmin)
  tmin_r <- mask(tmin_r, grid_shp)
  
  f.tmax <- as.formula(TMAX ~ 1)
  vg_tmax <- variogram(TMAX ~ 1, wx.day[!is.na(wx.day$TMAX),])
  dat.fit <- fit.variogram(vg_tmax, fit.ranges = F, fit.sills = F,
                           vgm(model = "Sph"))
  dat.krg.tmax <- krige(f.tmax, wx.day[!is.na(wx.day$TMAX),], grd, dat.fit)
  tmax_r <- raster::raster(dat.krg.tmax)
  tmax_r <- mask(tmax_r, grid_shp)
  
  f.snow <- as.formula(SNOW ~ 1)
  vg_snow <- variogram(SNOW ~ 1, wx.day[!is.na(wx.day$SNOW),])
  dat.fit <- fit.variogram(vg_snow, fit.ranges = F, fit.sills = F,
                           vgm(model = "Sph"))
  dat.krg.snow <- krige(f.snow, wx.day[!is.na(wx.day$SNOW),], grd, dat.fit)
  snow_r <- raster::raster(dat.krg.snow)
  snow_r <- mask(snow_r, grid_shp)
  
  # Apply to grid cells in year-day ----
  
  # Need to extract values from the raster layers to the polygons
  prcp_extr <- raster::extract(x = prcp_r,   # Raster object
                               y = grid_shp, # SpatialPolygons
                               fun = mean,
                               df = TRUE)
  
  names(prcp_extr)[2] = "PRCP"
  prcp_extr$ID = grid_shp$GRID_ID
  
  daily_result <- data.frame(day, prcp_extr)
  
  tmin_extr <- raster::extract(x = tmin_r,   # Raster object
                               y = grid_shp, # SpatialPolygons
                               fun = mean,
                               df = TRUE)
  names(tmin_extr)[2] = "TMIN"
  tmin_extr$ID = grid_shp$GRID_ID
  
  daily_result <- full_join(daily_result, tmin_extr)
  
  tmax_extr <- raster::extract(x = tmax_r,   # Raster object
                               y = grid_shp, # SpatialPolygons
                               fun = mean,
                               df = TRUE)
  names(tmax_extr)[2] = "TMAX"
  tmax_extr$ID = grid_shp$GRID_ID
  
  daily_result <- full_join(daily_result, tmax_extr)
  
  snow_extr <- raster::extract(x = snow_r,   # Raster object
                               y = grid_shp, # SpatialPolygons
                               fun = mean,
                               df = TRUE)
  names(snow_extr)[2] = "SNOW"
  snow_extr$ID = grid_shp$GRID_ID
  
  daily_result <- full_join(daily_result, snow_extr)
  
  # Save to S3 as temporary location in case the process is interrupted
  fn = paste("Prep_Weather_Daily_", day,"_", g, ".RData", sep="")
  
  save(list="daily_result", file = file.path(outputdir, fn))
  
  EndTime <- Sys.time()-StartTime
  cat(as.character(day), 'completed', round(EndTime, 2), attr(EndTime, 'units'), '\n',
      file = paste0("Prep_Weather_", g, "_log.txt"), append = T) 
  
  }
    
  #unique(state_hourly_hist_weather$Station_ID)
  #testsnow <- state_hourly_hist_weather[!is.na(state_hourly_hist_weather$snow_depth),]
  #testrain <- state_hourly_hist_weather[!is.na(state_hourly_hist_weather$precipitation),]
  #unique(testrain$Station_ID)
  
  # inport station data 
  wx.daily.inventory.files <- dir(file.path(inputdir, "Weather", "GHCN", "Daily"))
  
  daily_station_file <- file.path(inputdir, "Weather", "GHCN", "Daily" , wx.daily.inventory.files[grep('stations', wx.daily.inventory.files)])
  
  daily_stations <- read_fwf(daily_station_file,
                             fwf_positions(
                               start = c(1,14,23,32,42,73,81), 
                               end = c(11,21,30,37,70,75,85)
                             )
  )
  names(daily_stations) = c("STATION", "lat", "lon", "masl", "NAME", "x1", "x2")
  
  daily_stations$NAME <- NULL 
  
  
  
  # Read in daily data from GHCN
  
  wx.files <- dir(file.path(inputdir, "Weather", "GHCN", "Daily", year))
  wx <- vector()
  
  # Most stations don't have most of the wx variables. Simplify to core variables, per GHCN documentation:
  core_vars = c("STATION", "NAME", "DATE", 
                "PRCP", "SNOW", "SNWD", "TMAX", "TMIN")
  # longer set of possible variables
  # vars = c("STATION", "NAME", "DATE", "AWND", "DAPR", "MDPR", "PGTM", "PRCP", "SNOW", "SNWD", "TAVG", "TMAX", "TMIN", "TOBS", "WSFG", "WT01", "WT02", "WT03", "WT04", "WT05", "WT06", "WT08", "WT10", "WT11")

  # import daily data 
  j = 0 
  repeat{ 
    j = j + 1 
    filename <- paste0(state, "_daily_", year, "_", j, ".csv")
    print(filename)
    if(!file.exists(file.path(inputdir, "Weather", "GHCN", "Daily", year, filename))){break}
    wxx <- read.csv(file.path(inputdir, "Weather", "GHCN", "Daily", year, filename))
    wx <- rbind(wx, wxx[core_vars])
    rm(wxx)
    }

  wx$DATE <- ymd(as.character(wx$DATE)) # converted to lubridate 
  
  wx <- wx[order(wx$DATE, wx$STATION),]

  wx$TMIN[wx$TMIN == -99] = NA
  wx$TMAX[wx$TMAX == -99] = NA

  # To match with forecast, want the following by day
  # date: yday, hour
  # high: fahrenheit
  # low: fahrenheit
  # precip
  # snow_allday: in

  wx <- left_join(wx %>% dplyr::select(-NAME), daily_stations[1:5], by = "STATION") # merge hourly data with station info 
  
  ## TO DO: need to convert the below over to non-deprecated spatial packages
  # E.g., replace sp functions with sf functions, 
  # and replace raster functions with terra functions
  # This resource provides a 1-to-1 crosswalk of sp and sf functions:
  # https://github.com/r-spatial/sf/wiki/migrating
  
  # Interpolate over state ----
  # http://rspatial.org/analsis/rst/4-interpolation.html
  # First, make this a spatial points data frame
  # Project crashes
  
  # wx.proj <- SpatialPointsDataFrame(wx[c("lon", "lat")], 
  #                                      wx,
  #                                      proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  wx.proj <- wx %>% st_as_sf(coords = c("lon", "lat"), crs = "proj.USGS") 
  
  ggplot() +
    geom_sf(data=wx.proj, size=2)
  
  # wx.proj <- spTransform(wx.proj, CRS(proj.USGS))
  
 # wx.proj <- st_transform(wx.proj, crs = st_crs(proj.USGS))
  
  ### MAY NO LONGER NEED THE BELOW? ####


  
  ######################################
  
  wx <- wx %>%
    mutate(mo = format(DATE, "%m"))
  
  ### TO DO: the below line assumes we are using grids (fishnet or hex). 
  ### Is there an update that makes sense for how we are currently doing it?
  source(file.path(getwd(), 'datacleaning', 'Plot_weather_points.R'))
  
  # Options: nearest neighbor interpolation, inverse distance weighted, ordinary kriging...
  # Will make one raster for each variable of interest, per day, and then apply to grid/hour.
  # Here use kriging from gstat. Models are all based on spatial variance of the target variable
  
  ### TO DO - Consider above questions, then continue from here.

  # Create an empty raster grid for the state; we will interpolate over this grid, then assign values from the raster to each grid cell. Increase n for smaller raster cells (more time-intensive)
  # grd <- as.data.frame(spsample(grid_shp, 'regular', n = 10000))
  # names(grd) <- c("X", "Y")
  # coordinates(grd) <- c("X", "Y")
  # gridded(grd) <- TRUE # for SpatialPixel
  # fullgrid(grd) <- TRUE # for SpatialGrid
  # proj4string(grd) <- proj4string(grid_shp)
  
  ## TO-DO: consider on what basis to set cell size.
  
  # On what basis do we want to set cell size?
  
  # grd <- tn_buff %>% 
  #   st_make_grid(cellsize = c(7540.43,2630.079), what = "centers") %>% 
  #   st_intersection(tn_buff)                                           
  

  #plot(grd)
  
  #ext(tn_buff)
  
  # grd <- do.call(rbind, st_geometry(grd)) %>% 
  #   as_tibble() %>% setNames(c("X","Y"))
  
  StartTime <- Sys.time()
  
  writeLines(c(""), paste0("Prep_Weather_", g, "_log.txt"))  
  
  ### REMOVE THE BELOW AFTER DEVELOPMENT AND TESTING ARE COMPLETE ####
  do.months = paste("2019", c("01","02","03","04","05","06","07","08","09", "10", "11", "12"), sep="-")
  ####### ULTIMATELY THE ABOVE DO.MONTHS IS SPECIFIED IN RANDOMFOREST_WAZEGRIDS SCRIPT.
  
  
  # Start parallel loop ----
  # limit to dates in the set of months we are using, see do.months specified in the RandomForest_WazeGrids file.
  all_wx_days = unique(wx$DATE)
  all_wx_ym = format(all_wx_days, "%Y-%m")
  use_wx_days = all_wx_days[all_wx_ym %in% do.months]
  
  ### REMOVE AFTER DEVELOPMENT AND TESTING ####
  day <- "2019-01-01"
  #############################################
  
  wx.grd.day <- foreach(day = use_wx_days, 
                      .packages = c('terra','gstat','dplyr','sf'), 
                      .combine = rbind) %dopar% {
                        
    cat(paste(Sys.time()), as.character(day), "\n", 
        file = paste0("Prep_Weather_", g, "_log.txt"), append = T) 
        
    # Scan team bucket for completed daily weather prep ----
    fn = paste("Prep_Weather_Daily_", day,"_", g, ".RData", sep="")
    
    # See if exists. Load if so. If not, carry out kriging steps.
    
    if(file.path(outputdir, fn)){
      load(file.path(outputdir, fn))
    } else {
    
    wx.day = wx.proj[wx.proj$DATE == day,]
                        
    f.p <- as.formula(PRCP ~ 1)
    
    vg_prcp <- gstat::variogram(PRCP ~ 1, locations = wx.day[!is.na(wx.day$PRCP),])
    dat.fit <- fit.variogram(vg_prcp, fit.ranges = F, fit.sills = F,
                             vgm(model = "Sph"))
    # plot(vg_prcp, dat.fit) # Plot the semi variogram. 
    dat.krg.prcp <- krige(f.p, wx.day[!is.na(wx.day$PRCP),], grd2, dat.fit)
    
    ####### THE ABOVE IS WHERE ANDREW LEFT OFF MARCH 1, 2024 #####
    
    # Rasterize
    prcp_r <- raster::raster(dat.krg.prcp)
    prcp_r <- mask(prcp_r, grid_shp)

    # Now do tmin, tmax, and snow
    f.tmin <- as.formula(TMIN ~ 1)
    vg_tmin <- variogram(TMIN ~ 1, wx.day[!is.na(wx.day$TMIN),])
    dat.fit <- fit.variogram(vg_tmin, fit.ranges = F, fit.sills = F,
                             vgm(model = "Sph"))
    dat.krg.tmin <- krige(f.tmin, wx.day[!is.na(wx.day$TMIN),], grd, dat.fit)
    tmin_r <- raster::raster(dat.krg.tmin)
    tmin_r <- mask(tmin_r, grid_shp)
    
    f.tmax <- as.formula(TMAX ~ 1)
    vg_tmax <- variogram(TMAX ~ 1, wx.day[!is.na(wx.day$TMAX),])
    dat.fit <- fit.variogram(vg_tmax, fit.ranges = F, fit.sills = F,
                             vgm(model = "Sph"))
    dat.krg.tmax <- krige(f.tmax, wx.day[!is.na(wx.day$TMAX),], grd, dat.fit)
    tmax_r <- raster::raster(dat.krg.tmax)
    tmax_r <- mask(tmax_r, grid_shp)
    
    f.snow <- as.formula(SNOW ~ 1)
    vg_snow <- variogram(SNOW ~ 1, wx.day[!is.na(wx.day$SNOW),])
    dat.fit <- fit.variogram(vg_snow, fit.ranges = F, fit.sills = F,
                             vgm(model = "Sph"))
    dat.krg.snow <- krige(f.snow, wx.day[!is.na(wx.day$SNOW),], grd, dat.fit)
    snow_r <- raster::raster(dat.krg.snow)
    snow_r <- mask(snow_r, grid_shp)
    
    # Apply to grid cells in year-day ----

    # Need to extract values from the raster layers to the polygons
    prcp_extr <- raster::extract(x = prcp_r,   # Raster object
                                 y = grid_shp, # SpatialPolygons
                                 fun = mean,
                                 df = TRUE)

    names(prcp_extr)[2] = "PRCP"
    prcp_extr$ID = grid_shp$GRID_ID
    
    daily_result <- data.frame(day, prcp_extr)
    
    tmin_extr <- raster::extract(x = tmin_r,   # Raster object
                                 y = grid_shp, # SpatialPolygons
                                 fun = mean,
                                 df = TRUE)
    names(tmin_extr)[2] = "TMIN"
    tmin_extr$ID = grid_shp$GRID_ID
    
    daily_result <- full_join(daily_result, tmin_extr)
    
    tmax_extr <- raster::extract(x = tmax_r,   # Raster object
                                 y = grid_shp, # SpatialPolygons
                                 fun = mean,
                                 df = TRUE)
    names(tmax_extr)[2] = "TMAX"
    tmax_extr$ID = grid_shp$GRID_ID
    
    daily_result <- full_join(daily_result, tmax_extr)
    
    snow_extr <- raster::extract(x = snow_r,   # Raster object
                                 y = grid_shp, # SpatialPolygons
                                 fun = mean,
                                 df = TRUE)
    names(snow_extr)[2] = "SNOW"
    snow_extr$ID = grid_shp$GRID_ID
    
    daily_result <- full_join(daily_result, snow_extr)
    
    # Save to S3 as temporary location in case the process is interrupted
    fn = paste("Prep_Weather_Daily_", day,"_", g, ".RData", sep="")
    
    save(list="daily_result", file = file.path(outputdir, fn))
    
    EndTime <- Sys.time()-StartTime
    cat(as.character(day), 'completed', round(EndTime, 2), attr(EndTime, 'units'), '\n',
        file = paste0("Prep_Weather_", g, "_log.txt"), append = T) 
    
    } # end if exists_fn
    daily_result
  } # end parallel loop
   
  # Plot gridded versions of same point maps to check
  source(file.path(codeloc, 'datacleaning', 'Plot_weather_gridded.R'))
  
  EndTime <- Sys.time() - StartTime
  cat(round(EndTime, 2), attr(EndTime, "units"), "\n")
   
  save(list = c("wx.grd.day"), 
       file = file.path(inputdir, "Weather", paste0(prepname, ".RData")))

#   } else {
#   load(file.path(inputdir, "Weather", paste0(prepname, ".RData")))
# }

