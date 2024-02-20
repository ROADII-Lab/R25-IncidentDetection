# Prepare historical crash data for random forest work
# Need to apply points to grids

# Run from RandomForest_WazeGrid_TN.R
# already in memory are inputdir and g, which repreresents the grid type to use

# append.hex2(hexname = w, data.to.add = "TN_Crash", state = state, na.action = na.action)
library(rgeos)
library(rgdal)
library(doParallel)
library(foreach)

proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Check to see if these processing steps have been done yet; load from prepared file if so
prepname = paste("Prepared", "TN_Crash", g, sep="_")

if(length(grep(prepname, dir(file.path(inputdir, "Crash")))) == 0) { # if doen't exist in TN/Crash, make it
  
  cat("Preparing", "TN_Crash", g, "\n")
  
  load(file.path(inputdir, 'Crash', 'TN_Crash_Simple_2008-2018.RData'))
    
   
  # Apply to grid
  grid_shp <- rgdal::readOGR(file.path(inputdir, "Shapefiles"), layer = g)
  grid_shp <- spTransform(grid_shp, CRS(proj.USGS))

  # Project crashes
  crash.proj <- SpatialPointsDataFrame(crash[c("LongDecimalNmb", "LatDecimalNmb")], 
                                       crash,
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))

  crash.proj <- spTransform(crash.proj, CRS(proj.USGS))

  crash_grid <- over(crash.proj, grid_shp[,"GRID_ID"])

  crash <- data.frame(crash, GRID_ID = crash_grid$GRID_ID) 

  crash$day = format(crash$date, "%j")    
  
  save(list = c("crash"),
       file = file.path(inputdir, "Crash", paste0(prepname, ".RData")))


} else {
  load(file.path(inputdir, "Crash", paste0(prepname, ".RData")))
}


