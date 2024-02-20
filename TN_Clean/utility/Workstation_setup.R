# Setting up workstation on local machine. This example script can be modified for the computing infrastructure used by THP
# Not used in TN Case Study, but provided as a potential way to set up directories 

# Make directories to work in:

# Pull key data files from S3 bucket, including:
# - Shapefiles (census counties, hex tesselations)
# - EDT data
# - Prepared Waze data that is the output of the datacleaning steps

# Main idea: pull files from our permanent storage in S3 to the temporary working data directory on the user's analysis EC2 instance home directory.
# Syntax is aws s3 cp <teambucket/file/path> <local/file/path>
# Pass this to the terminal via R using the system() command

# Simple case: system('aws s3 cp s3://<bucketname>/<filename>.csv ~/workingdata/<filename>.csv')

# More flexible: create copy commands by pasting together the team bucket name, directory, and file name as the source, and ~/workingdata, sub-directory, and file name as destination.

GETOUTPUT = F # Set to T to get Random Forest Output, leave as F to save space

# Create directory structure ----

toplevel = c('workingdata', 'agg_out', 'tempout')

for (i in toplevel){
  if(length(dir(file.path("~", i)))==0) system(paste('mkdir -p', file.path("~", i)))
}

# Create directories within 'workingdata' 

workinglevel = c('census', 'EDT', 'Figures', 'Hex', 'Link', 'Overlay', 'Random_Forest_Output',
                 'AADT', 'FARS', 'LODES_LEHD', 'SpecialEvents', 'Weather')

for (i in workinglevel){
  system(paste('mkdir -p', file.path("~", "workingdata", i)))
}

# Populate with S3 contents ----

teambucket <- "s3://<bucketname>"

states = "TN"


for (i in states){
  system(paste('mkdir -p', file.path("~", "workingdata", i)))
}

# Waze data ----
# Grab any <state>_<yyyy-mm>.RData files from S3 and place in appropriate state directory on local


for(state in states){
  Waze.ls <- system(paste("aws s3 ls", 
                             file.path(teambucket, paste0(state, "/"))
  ),
  intern = T)
  
  # parse to file names
  Waze.ls <- unlist(lapply(strsplit(Waze.ls, " "), function(x) x[[length(x)]]))
  
  Waze.ls <- Waze.ls[grep(paste0("^", state, "_"), Waze.ls)] # get just Waze files: starts with <state>_
  Waze.ls <- Waze.ls[grep("RData$", Waze.ls)] # ends with RData
  Waze.ls <- Waze.ls[nchar(Waze.ls)==16] # is 16 characters long 
  
  for(i in Waze.ls){
    if(length(grep(i, dir(file.path('~', 'workingdata', state, 'Waze'))))==0){
      
      system(paste("aws s3 cp",
                   file.path(teambucket, state, i),
                   file.path('~', 'workingdata', state, 'Waze', i)))
    }
  }
}



# census ----
# Get all contents 

census.ls <- system(paste("aws s3 ls", 
             file.path(teambucket, "census/")
             ),
       intern = T)

# parse to file names
census.ls <- unlist(lapply(strsplit(census.ls, " "), function(x) x[[length(x)]]))

for(i in census.ls){
  if(length(grep(i, dir(file.path('~', 'workingdata', 'census'))))==0){
    system(paste("aws s3 cp",
               file.path(teambucket, 'census', i),
               file.path('~', 'workingdata', 'census', i)))
  }
}

# Tennessee data ----

tn.ls = c('TN.zip', 'Weather/TN_Weather_GHCN.zip', 'OpenWeatherAPI.txt',
          'Shapefiles/TN_Roadway_Shapefiles.zip', 'SpecialEvents/TN_SpecialEvent_2018.RData', 
          'Shapefiles/timezones.shapefile.zip', "SpecialEvents/TN_SpecialEvent_2017.RData",
          'SpecialEvents/2019_Special_Events.xlsx',
          'Crash/TITAN_Crash_181108.zip', 'Crash/TN_Crash_Simple_2008-2018.RData')

for(i in tn.ls){
  subdir = as.character(lapply(strsplit(i, '/'), function(x) x[1]))
  fileinsubdir = as.character(lapply(strsplit(i, '/'), function(x) x[2]))
  
  if(!is.na(fileinsubdir)){
    
    if(length(grep(fileinsubdir, dir(file.path('~', 'workingdata', 'TN', subdir))))==0){
      
      system(paste("aws s3 cp",
                   file.path(teambucket, 'TN', i),
                   file.path('~', 'workingdata', 'TN', i)))
      if(length(grep('zip$', i))!=0) {
        system(paste('unzip -o', file.path('~', 'workingdata', 'TN', i), '-d',
                     file.path('~', 'workingdata', 'TN', paste0(subdir, '/'))))
      }
    }
    # If no sub directory:
  } else {
    if(length(grep(i, dir(file.path('~', 'workingdata', 'TN'))))==0){
      
      system(paste("aws s3 cp",
                   file.path(teambucket, 'TN', i),
                   file.path('~', 'workingdata', 'TN', i)))
      if(length(grep('zip$', i))!=0) {
        system(paste('unzip -o', file.path('~', 'workingdata', 'TN', i), '-d',
                     file.path('~', 'workingdata', 'TN/')))
      }    
      
    }
  } # end if else for subdirectory
}
