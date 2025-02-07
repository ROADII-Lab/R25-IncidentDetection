
library(fs)

inputdir <- file.path(getwd(),"Input")

# Update to specify state of interest here - TN by default
state <- 'NC'
# Update to specify year of interest here - 2019 by default
year <- 2019

# Read in list of GHCN hourly stations
stations <- read.csv(file=file.path(inputdir, "Weather","GHCN","Hourly","ghcnh-station-list.csv"), 
                     header = FALSE, strip.white = TRUE)
stations <- stations[,1:6]
colnames(stations) <- c('Station_ID','Latitude','Longitude','Elevation','State','Station_name') 
# Filter down to the list of stations in the state of interest
state_stations <- stations[stations$State==state,]

# Create a place to put the copied files for the state to enable upload/transfer, if it does not yet exist.
state_dir <- file.path(inputdir, "Weather", "GHCN", "Hourly", year, state)
if (!dir.exists(state_dir)){dir.create(state_dir)}

# Copy the files for the identified state into the dedicated folder for that state.
for (i in 1:nrow(state_stations)) {
  ID = state_stations[i,'Station_ID']
  filename = paste0('GHCNh_',ID,'_',year,'.psv')
  filepath = file.path(inputdir, "Weather", "GHCN", "Hourly", year, filename)
  if(file.exists(filepath)){
    file_copy(path = filepath,
              new_path = file.path(state_dir,filename),
              overwrite = FALSE)
  }
}