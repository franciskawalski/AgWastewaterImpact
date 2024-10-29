library(tidyverse)
library(summarytools)
library(data.table)
library(sf)
library(dplyr)

##WD is assumed to be the code folder
##Set wd to the raw data folder to access data
setwd("../Data/Raw")

##Get data from different files into one dataframe to work with
waterDataList = list(first = read.csv("resultphyschem00-04.csv"),
                     second = read.csv("resultphyschem04-08.csv"),
                     third = read.csv("resultphyschem08-12.csv"),
                     forth = read.csv("resultphyschem12-16.csv"),
                     fifth = read.csv("resultphyschem16-20.csv"),
                     sixth = read.csv("resultphyschem20-24.csv"))

waterData = rbindlist(waterDataList, use.names = T)

##Remove all obs that do not have values and only keep obs in mg/l (>90% reported this way)
waterDataCleaned = waterData |> 
  filter(ResultMeasureValue != "",
         str_detect(tolower(ResultMeasure.MeasureUnitCode), "mg/l"),
         !is.na(ResultMeasureValue),
         ResultMeasureValue >=0) |> 
  mutate(ActivityStartDate = as.Date(ActivityStartDate),
         month = month(ActivityStartDate),
         year = year(ActivityStartDate),
         ResultMeasureValue = as.numeric(ResultMeasureValue)) 


##Write to csv so that we can clear up the memory in R
setwd("../Clean")
write.csv(waterDataCleaned,"waterDataCleaned.csv")

##Free up memory
rm(list=ls())
gc()

waterDataCleaned = read.csv("waterDataCleaned.csv")

##For obs without coordinates get the state from the organization name when possible
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
waterDataWithCoords = waterDataWithCoords |> 
  select(-OrganizationIdentifier) |> 
  filter(!is.na(state))

##Add all the state data found earlier 
dataWithState = rbind(waterDataWithCoords, stateWithoutCoords) |> 
  mutate(state = state.abb[match(state,state.name)])

##Convert data into monthly averages
waterDataMthAvg = dataWithState |> 
  group_by(year, month, state, CharacteristicName) |> 
  summarise(avgValue = mean(ResultMeasureValue, na.rm = T)) |> 
  filter(!is.na(state))

write.csv(dataWithState,"waterDataCleaned.csv")
write.csv(waterDataMthAvg, "AverageMonthlyWaterData.csv")

##Free up memory
rm(list=ls())
gc()

setwd("../Raw")
stateRegs = read_csv("WastewaterRegulationByState.csv")

##Get regs just for nitrogen and nitrate
stateNitrRegs = stateRegs |> 
  mutate(`Water Quality Parameter` = str_replace_all(`Water Quality Parameter`, "[^[:alnum:][:space:]]", ""), ##Get rid of special char
         `Water Quality Parameter` = tolower(`Water Quality Parameter`)) |> 
  filter(str_detect(`Water Quality Parameter`, 'nitr' )) |> ##Remove parameters we don't care about
  mutate(`Water Quality Parameter` = str_replace_all(`Water Quality Parameter`, ##Fix typos
                                                     c("nitrogenj" = "nitrogen",
                                                       "nitrogend" = "nitrogen", 
                                                       "nitrogena"= "nitrogen",
                                                       "total nitrogen"= "nitrogen"))) |> 
  mutate(`Water Quality Parameter` = ifelse(str_detect(`Water Quality Parameter`,"nitrogen"), "nitrogen", `Water Quality Parameter`),
         `Water Quality Parameter` = ifelse(str_detect(`Water Quality Parameter`,"nitrate"), "nitrate", `Water Quality Parameter`))

##Get values for regs into simple categories
stateRegsByCat = stateNitrRegs |> 
  mutate(Specification = str_replace_all(Specification, "[^[:alnum:][:space:]/]", ""),
         Specification = tolower(Specification),
         Specification = ifelse(str_detect(Specification, "agronomic"), "agronomic", Specification),
         Specification = ifelse(str_detect(Specification, "provided the site"), "agronomic", Specification), ##Fancy way of saying agronomic
         Specification = ifelse(str_detect(Specification, "mg/l"), ##Remove everything after mg/l, and only keep nums
                                str_c(str_replace_all(str_split_i(Specification, "mg/l",1), "[^0-9]", ""), " mg/L"), 
                                Specification),
         Specification = str_replace_all(Specification, "specific value not provided", "not specified")) #Combine like terms

##Make clean when year policies were implemented and when current policy was passed
stateRegYears = read_csv("State_Water_Reuse_Regs.csv")
stateRegYears = stateRegYears |> 
  mutate(firstRegYear = Year1,
         currentRegYear = EPA_Year) 

setwd("../Clean")
write.csv(stateRegsByCat, "stateRegs.csv")
write.csv(stateRegYears, "stateRegYear.csv")  
  
  
  
  
  
