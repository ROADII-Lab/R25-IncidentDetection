library(dplyr)
library(tidyr)
library(readr)

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
