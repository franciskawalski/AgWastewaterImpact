library(tidyverse)
library(summarytools)
library(data.table)
library(sf)
library(dplyr)

##Set wd to the raw data folder to access data
setwd("../Data/Raw")

stateRegs = read_csv("WastewaterRegulationByState.csv")

waterDataList = list(first = read.csv("resultphyschem00-04.csv"),
                     second = read.csv("resultphyschem04-08.csv"),
                     third = read.csv("resultphyschem08-12.csv"),
                     forth = read.csv("resultphyschem12-16.csv"),
                     fifth = read.csv("resultphyschem16-20.csv"),
                     sixth = read.csv("resultphyschem20-24.csv"))

waterData = rbindlist(waterDataList, use.names = T)

##Remove all obs that do not have values and only keep obs in mg/l (most)
waterDataCleaned = waterData |> 
  filter(ResultMeasureValue != "",
         str_detect(tolower(ResultMeasure.MeasureUnitCode), "mg/l")) |> 
  mutate(ActivityStartDate = as.Date(ActivityStartDate),
         month = month(ActivityStartDate),
         year = year(ActivityStartDate),
         ResultMeasureValue = as.numeric(ResultMeasureValue)) |> 
  filter(!is.na(ResultMeasureValue))

##Write to csv so that we can clear up the memory in R

setwd("../Data/Clean")
write.csv(waterDataCleaned,"waterDataCleaned.csv")

waterDataCleaned = read.csv("waterDataCleaned.csv")

waterDataCleaned = waterDataCleaned |> 
  filter(!is.na(ActivityLocation.LatitudeMeasure),
         !is.na(ActivityLocation.LongitudeMeasure))

coords = data.frame(lat = waterDataCleaned$ActivityLocation.LatitudeMeasure,
                    lon = waterDataCleaned$ActivityLocation.LongitudeMeasure)

coords = st_as_sf(coords, coords = c("lon", "lat"),
                 crs = 4326)
  
states = read_sf("cb_2018_us_state_500k.shp", crs = 4326)
coords_with_states = st_join(coords, states, join = st_within) 
 
waterDataCleaned$state = coords_with_states$NAME

waterDataMthAvg = waterDataCleaned |> 
  group_by(year, month, state, CharacteristicName) |> 
  summarise(avgValue = mean(ResultMeasureValue))

write.csv(waterDataCleaned,"waterDataCleaned.csv")
write.csv(waterDataMthAvg, "AverageMonthlyWaterData.csv")

