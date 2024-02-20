# Prepare forecasted weather for random forest work.
# Need to create an interpolated grid based on forecasted weather.

# Run from PredictWeek_TN.R, following Get_weather_forecasts.R, which provides dat 
# already in memory: inputdir, teambucket, codeloc, state, and g, which repreresents the grid type to use

# Also already in memory is `next_week`, a data frame of the grid ID and date/time for the next week

censusdir <- file.path(getwd(),"Input","census")
outputdir <- file.path(getwd(),"Output") # to hold daily output files as they are generated
inputdir <- file.path(getwd(),"Input")

proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

library(gstat) # For kriging
library(rgeos) # for gIntersects
library(sf)
library(raster) # masks several functions from dplyr, use caution
library(doParallel)
library(foreach)

prepname =  paste0("TN_Forecasts_Gridded_", g, Sys.Date(), ".RData")

# ADD FUNCTION HERE TO GENERATE PLACEHOLDER FOR WEATHER DATA. SOMETHING CALLED paste0("TN_Forecasts_", Sys.Date(), ".RData")

if(!file.exists(file.path(inputdir, 'Weather', prepname))) {

  cat("Preparing Forecasts to", as.character(max(next_week$date)), "for", g, "\n")
  
  # Get weather forecast for the next week
  # First check to see if forecast has been run already from today (to not over-user API call)
  
  if(file.exists(file.path(inputdir, 'Weather', paste0("TN_Forecasts_", Sys.Date(), ".RData")))) {
    load(file.path(inputdir, 'Weather', paste0("TN_Forecasts_", Sys.Date(), ".RData")))
  } else {
    source('datacleaning/Get_weather_forecasts.R')
  }
  
  
  # Interpolate over state ----
  # http://rspatial.org/analsis/rst/4-interpolation.html
  # First, make this a spatial points data frame
  # Project crashes

  wx.proj <- spTransform(wx_dat.proj, CRS(proj.USGS))
  
  # Read in grid
  # grid_shp <- rgdal::readOGR(file.path(inputdir, "Shapefiles"), layer = g)
  grid_shp <- read_sf(file.path(inputdir, "Shapefiles"), layer = g)
  grid_shp <- spTransform(grid_shp, CRS(proj.USGS))
  
  # Read in buffered state shapefile
  tn_buff <- read_sf(censusdir, layer = "TN_buffered")
  tn_buff <- spTransform(tn_buff, CRS(proj.USGS))
  
  # Clip grid to county shapefile
  grid_intersects <- gIntersects(tn_buff, grid_shp, byid = T)
  
  grid_shp <- grid_shp[as.vector(grid_intersects),]
  
  wx <- wx_dat %>%
    mutate(local_time = as.POSIXct(local_time),
           mo = format(local_time, "%m"),
           date = format(local_time,'%Y-%m-%d'))
  
  wx.proj@data <- wx.proj@data %>%
    mutate(local_time = as.POSIXct(local_time),
           mo = format(local_time, "%m"),
           date = format(local_time, '%Y-%m-%d'))
  
  # Kriging. Intercept only = ordinary kriging.
  # Parallize by day -- only need up to 7 cores
  use.cores = ifelse(parallel::detectCores() > 7, 7, parallel::detectCores()) 
  cl <- makeCluster(use.cores)
  registerDoParallel(cl)
  
  # Create an empty raster grid for the state; we will interpolate over this grid, then assign values from the raster to each grid cell. Increase n for smaller raster cells (more time-intensive)
  grd <- as.data.frame(spsample(grid_shp, 'regular', n = 10000))
  names(grd) <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd) <- TRUE # for SpatialPixel
  fullgrid(grd) <- TRUE # for SpatialGrid
  proj4string(grd) <- proj4string(grid_shp)
  
  StartTime <- Sys.time()
  
  writeLines(c(""), paste("Prep_Weather_to", max(next_week$date), g, "log.txt", sep="_"))    
  
  # Start parallel loop ----
  # limit to dates in the set of months we are using, see do.months specified in the RandomForest_WazeGrids file.
  all_wx_days = unique(wx$date)
 
  wx.grd.day <- foreach(day = all_wx_days, 
                      .packages = c('raster','gstat','dplyr','rgdal'), 
                      .combine = rbind) %dopar% {
    cat(paste(Sys.time()), as.character(day), "\n", 
        file = paste("Prep_Weather_to", max(next_week$date), g, "log.txt", sep="_"), append = T) 
        
    # Scan team bucket for completed daily weather prep ----
    fn = paste("Prep_Weather_Daily_", day,"_", g, ".RData", sep="")
    
    # See if exists in outputdir. Load if so. If not, carry out kriging steps.
    if(file.exists(file.path(file.path(inputdir,"Weather", fn)))){
      load(file.path(inputdir,"Weather", fn))
    } else {
    
    wx.day = wx.proj[wx.proj$date == day,]
                        
    f.p <- as.formula(precipIntensityMax  ~ 1)
    
    vg_prcp <- gstat::variogram(precipIntensityMax  ~ 1, locations = wx.day[!is.na(wx.day$precipIntensityMax ),])
    dat.fit <- fit.variogram(vg_prcp, fit.ranges = F, fit.sills = F,
                             vgm(model = "Sph"))
    dat.krg.prcp <- krige(f.p, wx.day[!is.na(wx.day$precipIntensityMax),], grd, dat.fit)
    
    # Rasterize
    prcp_r <- raster::raster(dat.krg.prcp)
    prcp_r <- mask(prcp_r, grid_shp)
    
    # Now do tmin, tmax, and snow
    f.tmin <- as.formula(temperatureMin ~ 1)
    vg_tmin <- variogram(temperatureMin ~ 1, wx.day[!is.na(wx.day$temperatureMin),])
    dat.fit <- fit.variogram(vg_tmin, fit.ranges = F, fit.sills = F,
                             vgm(model = "Sph"))
    dat.krg.tmin <- krige(f.tmin, wx.day[!is.na(wx.day$temperatureMin),], grd, dat.fit)
    tmin_r <- raster::raster(dat.krg.tmin)
    tmin_r <- mask(tmin_r, grid_shp)
    
    f.tmax <- as.formula(temperatureMax ~ 1)
    vg_tmax <- variogram(temperatureMax ~ 1, wx.day[!is.na(wx.day$temperatureMax),])
    dat.fit <- fit.variogram(vg_tmax, fit.ranges = F, fit.sills = F,
                             vgm(model = "Sph"))
    dat.krg.tmax <- krige(f.tmax, wx.day[!is.na(wx.day$temperatureMax),], grd, dat.fit)
    tmax_r <- raster::raster(dat.krg.tmax)
    tmax_r <- mask(tmax_r, grid_shp)
    
    # SNOW: model fitted with historical data had a variable called SNOW. Forecast does not, but does have precipType, so create SNOW from this.
    wx.day@data <- wx.day@data %>%
      mutate(SNOW = ifelse(precipType == 'snow', precipIntensity, 0))
    
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
    
    # Save in temporary location in case the process is interrupted
    fn = paste("Prep_Weather_Daily_", day,"_", g, ".RData", sep="")
    
    save(list="daily_result", file = file.path(inputdir,"Weather", fn))
    
    EndTime <- Sys.time()-StartTime
    cat(as.character(day), 'completed', round(EndTime, 2), attr(EndTime, 'units'), '\n',
        file = paste0("Prep_Weather_", g, "_log.txt"), append = T) 
    
    } # end if exists_fn
    daily_result
  } # end parallel loop
   
  EndTime <- Sys.time() - StartTime
  cat(round(EndTime, 2), attr(EndTime, "units"), "\n")
   
  save(list = c("wx.grd.day"), 
       file = file.path(inputdir, "Weather", prepname))

  } else {
  load(file.path(inputdir, "Weather", prepname))
}

