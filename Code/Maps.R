library(tidyverse)
library(sf)
library(tmap)

states = st_read("../Data/Clean/States_shape/cb_2018_us_state_500k.shp") |> 
  st_make_valid()

stateRegs = read_csv("../Data/Clean/stateRegYear.csv")

wqData = read_csv("../Data/Clean/cleanMergedData_StationLevel.csv")

stateAvgNitrogen = wqData |> 
  filter(year == 2010, CharacteristicName == "Nitrogen") |> 
  group_by(state, ResultMeasureValue, MonitoringLocationIdentifier) |> ##Remove duplicates from merge
  group_by(state, MonitoringLocationIdentifier) |> 
  summarise(avgStationNitrogen = mean(ResultMeasureValue, na.rm = T)) |> 
  group_by(state) |> 
  summarize(avgStateNitrogen = mean(avgStationNitrogen))

states$hasReg = ifelse(states$NAME %in% stateRegs$State, "1", "0")
states = states |> 
  filter(!NAME %in% c("Alaska", "Hawaii", "Guam", "Puerto Rico", "United States Virgin Islands", 
  "American Samoa", "Commonwealth of the Northern Mariana Islands")) 


tm_shape(states) + 
  tm_fill("hasReg", palette = c("1" = "royalblue", "0" = "grey"),
          title = "Has Wastewater \nRegulation") +
  tm_borders(col = "black") + 
  tm_layout(title = "States with Agrictultural Wasterwater Reuse Regulations",
            title.position = c("center", "top"),
            inner.margins = c(0.01, 0.01, .125, 0.01))

