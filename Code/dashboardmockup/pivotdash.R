library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Load the dataset
####################################################
######### UPDATE WITH ACTUAL ROADII PATH ##########
####################################################
datatall <- read_csv("~/GitHub/MACVSM/Dashboard/MN.csv")

datatall <- datatall %>% select(osm_id, date,Hourly_CrashRisk)

DateInfo <- datatall %>%
  select(date) %>%
  distinct() %>%
  arrange(date) %>%
  rename(Date = date)

CrashPrediction <- datatall %>%
  select(osm_id, date, Hourly_CrashRisk) %>%
  group_by(osm_id, date) %>%
  summarise(Hourly_CrashRisk = mean(Hourly_CrashRisk, na.rm = TRUE)) %>%
  pivot_wider(names_from = date, values_from = Hourly_CrashRisk)
names(CrashPrediction)[-1] <- seq(0, ncol(CrashPrediction) - 2)


