library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Load the dataset
####################################################
#########UPDATE WITH ACTUAL ROADII PATH#############
####################################################
datatall <- read_csv("~/GitHub/MACVSM/Dashboard/MN.csv")

# Define the necessary columns
datatall <- datatall %>% select(osm_id, date, Year, Month, Hour, DayOfWeek, weekday, Hourly_CrashRisk)

# Determine the minimum day of the week
min_day <- min(datatall$DayOfWeek)
min_hour <- min(datatall$Hour[datatall$DayOfWeek == min_day])
datatall <- mutate(datatall, AdjustedHourOfWeek = ((DayOfWeek - min_day) %% 7) * 24 + Hour - min_hour)

##################### NOTE FROM ANDREW ON THE ABOVE###################################################################
# the above does not appear to be working in all scenarios.
# if we continue to use the above approach, which is trying to create an AdjustedHourofWeek that is set relative to
# the first hour of the predictions, we could consider using an alternate approach for this, which would be as follows:

#earliest <- min(datatall$date)

# there is a plus 1 below so that the first hour will start at 1 - otherwise the first hour would start at zero.

#datatall <- datatall %>% mutate(AdjustedHourOfWeek = as.numeric(difftime(date, earliest), units="hours") + 1)

# # However, a simpler overall approach (assuming we can get the Tableau dashboard to dynamically represent the actual
# # day and hour) would just be to do the group_by below based on the date field.
# # I have an idea for how to get Tableau to do that, which I will send via Teams.

#####################################################################################################################

# Filter out incomplete first day
#first_day_data <- filter(datatall, DayOfWeek == min_day)
#first_day_hours <- first_day_data$Hour
#if (min(first_day_hours) > 0) {
#  datatall <- filter(datatall, DayOfWeek > min_day)
#  datatall <- mutate(datatall, DayOfWeek = DayOfWeek - 1)
#}

# Pivot the data
pivoted_data <- datatall %>%
  select(osm_id, AdjustedHourOfWeek, Hourly_CrashRisk) %>%
  group_by(osm_id, AdjustedHourOfWeek) %>%
  summarise(Hourly_CrashRisk = mean(Hourly_CrashRisk, na.rm = TRUE)) %>%
  pivot_wider(names_from = AdjustedHourOfWeek, values_from = Hourly_CrashRisk)

# Rename columns
colnames(pivoted_data)[-1] <- paste0("D", (as.numeric(colnames(pivoted_data)[-1]) %/% 24) + 1, "H", (as.numeric(colnames(pivoted_data)[-1]) %% 24) + 1)

##################### NOTE FROM ANDREW ON THE ABOVE###################################################################
# # Note that the pivot_wider() function can also accept multiple values in the names_from argument, so for example,
# # If we define a field "Day," based on the date field using lubridate::day(date), we could specify something like this:
#names_from = c(Day, Hour)
# # and then the new columns would be based on a concatenation of the Day and Hour, with whatever separator is specified
# # in the optional argument. this may be an alternative method to the above, if we continue using the above approach.
# # - - for consideration.
# # However, as mentioned above, a simpler overall approach (assuming we can get the Tableau dashboard to dynamically 
# # represent the actual day and hour) would just be to do the group_by above based on the date field.
# # I have an idea for how to get Tableau to do that, which I will send via Teams.

#####################################################################################################################

library(sf)
library(dplyr)
#######################################################
##########PULL IN STATE NETWORK FILE SCRIPT############
#######################################################
state_network <- #state network filepath
state_network$osm_id <- as.integer(gdf$osm_id)
pivoted_data$osm_id <- as.integer(na.omit(as.numeric(df$osm_id)))
state_geo <- merge(x = state_network, 
                   y = pivoted_data, 
                   by.x = "osm_id", 
                   by.y = "osm_id", all.x = TRUE)

output_shapefile_path <- "TableauShapefile.shp"
st_write(state_geo, output_shapefile_path)
