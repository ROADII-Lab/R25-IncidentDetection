# Get all necessary packages across data prep and analysis scripts for SDI Waze project. Edit as necessary when adding more pac kages

loadpacks <- c(
            "Amelia",
            "aws.s3", # AWS convenience functions s3save and s3load
            "circular", # circular transformation of magvar
            "corrplot",
            "doParallel",
            "DT",
            "foreach", # for parallel implementation
            "geonames",
            "getPass",
            "GGally",
            "ggmap",
            "gstat",
            "kableExtra",
            "lubridate",
            "maps",
            "maptools",
            "mapproj",  # for coord_map, 
            "maptree", # for better graphing of decision trees
            "mlbench",
            "pander",
            "party",
            "partykit",
            "pROC",
            "randomForest",
            "raster", 
            "rgdal",  # for readOGR, , needed for reading in ArcM shapefiles
            "sf",  # for gIntersection, to clip two shapefiles
            "sp",
            "tidyverse",
            "utils",
			"dplyr")


for(i in loadpacks){if(length(grep(i, (.packages(all.available=T))))==0) install.packages(i, dependencies =TRUE)}
