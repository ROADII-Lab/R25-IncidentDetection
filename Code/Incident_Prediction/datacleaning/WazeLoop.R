
years <- c(2019,2020,2021,2022,2023)

state <- "DE"
#states <- c("CA1","CA2","CA3")

# Indicate whether the state has a unified time zone
one_zone <- TRUE

#OlsonNames()
time_zone_name <- "US/Eastern"
#time_zone_name <- "US/Central"
# time_zone_name <- "US/Pacific"
#time_zone_name <- "US/Pacific"

# version for simple states
for (y in years){
  year <- y
  source('datacleaning/Waze_query.R')
}

# alternate version for some years for CA, FL, TX, NY
# for (y in years){
#   year <- y
#   for (state in states){
#     source('datacleaning/Waze_query.R')
#   }
# }

# CA has four regions in the historical data, CA, CA1, CA2, CA3. Also manually add DC
# In the current data, no 'CA' buff exists, only the three CA1, CA2, CA3, so remove from the list of states. NY/FL/TX now have 1-3 regions as well, as of 2020-08-01.
# states <- c(state.abb, paste0(rep(c("CA", "FL", "NY", "TX"), each = 3), 1:3), 'DC')
# 
# #states <- states[!grepl("^CA$", states)] 
# states <- states[!grepl("^FL$", states)] 
# states <- states[!grepl("^NY$", states)] 
# states <- states[!grepl("^TX$", states)] 