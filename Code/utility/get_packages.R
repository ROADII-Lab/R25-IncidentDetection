# Title: Install and load packages
# Purpose: Identifies the requires packages, check if they are installed, if not installs them, then loads all packages.
# Generated Variables: 

requiredpackages <- c("randomForest", # random forest
                      "foreach", # for parallel implementation
                      "doParallel", # includes iterators and parallel
                      "parallelly", 
                      "tidyverse", 
                      "ROSE",
                      "performanceEstimation",
                      "caret",
                      "sf", # for working with spatial data
                      "tsibble", # for working with time bins
                      "RANN", # holds nn2 
                      "osmdata", # pull data from osm servers
                      "ggmap", 
                      "httr", # for GET
                      "xml2", # for xml parsing in tidy way
                      "XML", # for xmlToList
                      "jsonlite",
                      "tigris", # for state shapefiles (or other census geographies)
                      "pROC")

install_load <- function(packages){
  for (p in packages) {
    if (p %in% rownames(installed.packages())) {
      library(p, character.only=TRUE)
    } else {
      install.packages(p)
      library(p,character.only = TRUE)
    }
  }
}

install_load(packages = requiredpackages)

rm(requiredpackages, install_load)
