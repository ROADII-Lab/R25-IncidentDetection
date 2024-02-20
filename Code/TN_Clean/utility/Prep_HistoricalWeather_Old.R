# Prepare historical weather for random forest work

# Run from RandomForest_WazeGrid_TN.R
# already in memory are  g, which repreresents the grid type to use
# Next step after this script is to run append.hex function to add 
censusdir <- "~/TN/Input/census"
outputdir <- "~/TN/Output" # to hold daily output files as they are generated
inputdir <- "~/TN/Input"

proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Check to see if these processing steps have been done yet; load from prepared file if so
prepname = paste("Prepared", "Weather", g, sep="_")

if(length(grep(prepname, dir(file.path(inputdir, "Weather")))) == 0) { 
  library(gstat) # For kriging
  library(raster) # masks several functions from dplyr, use caution
  library(doParallel)
  library(foreach)
  
  cat("Preparing", "Weather", g, "\n")
  
  # Read in GHCN data
  wx.files <- dir(file.path(inputdir, "Weather", "GHCN"))
  wx <- vector()
  
  # Most stations don't have most of the wx variables. Simplify to core variables, per GHCN documentation:
  core_vars = c("STATION", "NAME", "DATE", 
                "PRCP", "SNOW", "SNWD", "TMAX", "TMIN")
  # longer set of possible variables
  # vars = c("STATION", "NAME", "DATE", "AWND", "DAPR", "MDPR", "PGTM", "PRCP", "SNOW", "SNWD", "TAVG", "TMAX", "TMIN", "TOBS", "WSFG", "WT01", "WT02", "WT03", "WT04", "WT05", "WT06", "WT08", "WT10", "WT11")

  for(k in wx.files){
    if(length(grep('stations', k))==0){
      wxx <- read.csv(file.path(inputdir, "Weather", "GHCN", k))
      wx <- rbind(wx, wxx[core_vars])
      rm(wxx)
    }
  }

  station_file <- file.path(inputdir, "Weather", "GHCN", wx.files[grep('stations', wx.files)])
  stations <- read_fwf(station_file,
                       col_positions = fwf_empty(station_file))
  names(stations) = c("STATION", "lat", "lon", "masl", "NAME", "x1", "x2", "x3")
  
  wx$DATE <- as.Date(as.character(wx$DATE))
  
  wx <- wx[order(wx$DATE, wx$STATION),]

   
  wx$TMIN[wx$TMIN == -99] = NA
  wx$TMAX[wx$TMAX == -99] = NA

  # To match with forecast, want the following by day
  # date: yday, hour
  # high: fahrenheit
  # low: fahrenheit
  # precip
  # snow_allday: in

  wx <- left_join(wx, stations[1:5], by = "STATION")
  
  # Interpolate over state ----
  # http://rspatial.org/analsis/rst/4-interpolation.html
  # First, make this a spatial points data frame
  # Project crashes
  wx.proj <- SpatialPointsDataFrame(wx[c("lon", "lat")], 
                                       wx,
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  wx.proj <- spTransform(wx.proj, CRS(proj.USGS))
  
  # Read in grid
  grid_shp <- rgdal::readOGR(file.path(inputdir, "Shapefiles"), layer = g)
  grid_shp <- spTransform(grid_shp, CRS(proj.USGS))
  
  # Read in buffered state shapefile
  tn_buff <- readOGR(censusdir, layer = "TN_buffered")
  tn_buff <- spTransform(tn_buff, CRS(proj.USGS))
  
  # Clip grid to county shapefile
  grid_intersects <- gIntersects(tn_buff, grid_shp, byid = T)
  
  grid_shp <- grid_shp[as.vector(grid_intersects),]
  
  wx <- wx %>%
    mutate(mo = format(DATE, "%m"))
  
  source(file.path(codeloc, 'datacleaning', 'Plot_weather_points.R'))
  
  # Options: nearest neighbor interpolation, inverse distance weighted, ordinary kriging...
  # Will make one raster for each variable of interest, per day, and then apply to grid/hour.
  # Here use kriging from gstat. Models are all based on spatial variance of the target variabel
  
  # Kriging. Intercept only = ordinary kriging.
  cl <- makeCluster(parallel::detectCores())
  registerDoParallel(cl)
  
  # Create an empty raster grid for the state; we will interpolate over this grid, then assign values from the raster to each grid cell. Increase n for smaller raster cells (more time-intensive)
  grd <- as.data.frame(spsample(grid_shp, 'regular', n = 10000))
  names(grd) <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd) <- TRUE # for SpatialPixel
  fullgrid(grd) <- TRUE # for SpatialGrid
  proj4string(grd) <- proj4string(grid_shp)
  
  StartTime <- Sys.time()
  
  writeLines(c(""), paste0("Prep_Weather_", g, "_log.txt"))    
  
  # Start parallel loop ----
  # limit to dates in the set of months we are using, see do.months specified in the RandomForest_WazeGrids file.
  all_wx_days = unique(wx$DATE)
  all_wx_ym = format(all_wx_days, "%Y-%m")
  use_wx_days = all_wx_days[all_wx_ym %in% do.months]
  
  wx.grd.day <- foreach(day = use_wx_days, 
                      .packages = c('raster','gstat','dplyr','rgdal'), 
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
    dat.krg.prcp <- krige(f.p, wx.day[!is.na(wx.day$PRCP),], grd, dat.fit)
    
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

  } else {
  load(file.path(inputdir, "Weather", paste0(prepname, ".RData")))
}

