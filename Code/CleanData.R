library(tidyverse)
library(summarytools)
library(data.table)
library(sf)
library(dplyr)

##Current WD is assumed to be in the code folder
##Set wd to the raw data folder to access data
setwd("..")
setwd(str_c(getwd(),"/Data/Raw"))

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
         str_detect(tolower(ResultMeasure.MeasureUnitCode), "mg/l"),
         !is.na(ResultMeasureValue),
         ResultMeasureValue >=0) |> 
  mutate(ActivityStartDate = as.Date(ActivityStartDate),
         month = month(ActivityStartDate),
         year = year(ActivityStartDate),
         ResultMeasureValue = as.numeric(ResultMeasureValue)) |> 
  filter()

##Write to csv so that we can clear up the memory in R
setwd("..")
setwd(str_c(getwd(),"/Clean"))
write.csv(waterDataCleaned,"waterDataCleaned.csv")
rm(list=ls())
waterDataCleaned = read.csv("waterDataCleaned.csv")

##For obs without coordinates get the state from the organization name
stateWithoutCoords = waterDataCleaned |> 
  filter(is.na(ActivityLocation.LatitudeMeasure),
         is.na(ActivityLocation.LongitudeMeasure)) |> 
  separate(OrganizationIdentifier,  into = c("org", "stateFromOrg"), sep = "-") |> 
  mutate(stateFromOrg = str_trim(stateFromOrg),
         state = ifelse(stateFromOrg %in% state.abb, stateFromOrg, NA)) |> 
  filter(!is.na(state)) |> 
  select(-stateFromOrg, -org)
  

waterDataWithCoords = waterDataCleaned |> 
  filter(!is.na(ActivityLocation.LatitudeMeasure),
         !is.na(ActivityLocation.LongitudeMeasure))

##Get the state that coordinates are in from shape files of US states
coords = data.frame(lat = waterDataWithCoords$ActivityLocation.LatitudeMeasure,
                    lon = waterDataWithCoords$ActivityLocation.LongitudeMeasure)

coords = st_as_sf(coords, coords = c("lon", "lat"),
                 crs = 4326)
  
states = read_sf("cb_2018_us_state_500k.shp", crs = 4326)
coords_with_states = st_join(coords, states, join = st_within) 
 
waterDataWithCoords$state = coords_with_states$NAME
waterDataWithCoords = waterDataWithCoords |> select(-OrganizationIdentifier)

##Add all the state data found earlier 
dataWithState = rbind(waterDataWithCoords, stateWithoutCoords)

##Convert data into monthly averages
waterDataMthAvg = dataWithState |> 
  group_by(year, month, state, CharacteristicName) |> 
  summarise(avgValue = mean(ResultMeasureValue))

write.csv(dataWithState,"waterDataCleaned.csv")
write.csv(waterDataMthAvg, "AverageMonthlyWaterData.csv")

