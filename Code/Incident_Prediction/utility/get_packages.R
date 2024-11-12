# Get all necessary packages across data prep and analysis scripts for SDI Waze project. Edit as necessary when adding more pac kages

loadpacks <- c(
            "Amelia",
            "aws.s3", # AWS convenience functions s3save and s3load
            "brulee",
            "circular", # circular transformation of magvar
            "corrplot",
            "data.table",
            "doParallel",
            "DT",
            "foreach", # for parallel implementation
            "geonames",
            "getPass",
            "GGally",
            "ggmap",
            "gstat",
            "kableExtra",
            #"keras",
            "lubridate",
            "maps",
            "mapproj",  # for coord_map, 
            "maptree", # for better graphing of decision trees
            "mlbench",
            "osmdata",
            "pander",
            "party",
            "partykit",
            "pROC",
            "performanceEstimation",
            "randomForest",
            "RANN",
            "raster", 
            "recipes",
            "ROSE",
            "RPostgres",
            "sf",  # for gIntersection, to clip two shapefiles
            "sp",
            "stringr",
            "terra",
            #"tensorflow",
            "tidyverse",
            "tigris",
            "torch",
            "utils",
			      "dplyr",
			      "rgeos",
			      "yardstick")


for(i in loadpacks){if(length(grep(i, (.packages(all.available=T))))==0) install.packages(i, dependencies =TRUE)}
