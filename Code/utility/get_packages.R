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
                      "plotly", # for interactive precision-recall graphic
                      "htmlwidgets", # for interactive precision-recall graphic
                      "ggmap", 
                      "httr", # for GET
                      "xml2", # for xml parsing in tidy way
                      "XML", # for xmlToList
                      "jsonlite",
                      "tigris", # for state shapefiles (or other census geographies)
                      "pROC", # for receiver operating characteristic (ROC) curve and AUC
                      "PRROC", # for precision-recall curve and AUC
                      "openxlsx", # used in createdash
                      "zip") # used in createdash

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
