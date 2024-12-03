library(tidyverse)
library(sf)
library(tmap)
library(RColorBrewer)

states = st_read("../Data/Clean/States_shape/cb_2018_us_state_500k.shp") |> 
  st_make_valid()

stateRegs = read_csv("../Data/Clean/stateRegYear.csv")

wqData = read_csv("../Data/Clean/cleanMergedData_StationLevel.csv")

stateAvgNitrogen = wqData |> 
  filter(CharacteristicName == "Nitrogen", year %in% c(2008:2012)) |> 
  group_by(state, ResultMeasureValue, MonitoringLocationIdentifier) |> ##Remove duplicates from merge
  group_by(state, MonitoringLocationIdentifier) |> 
  summarise(avgStationNitrogen = mean(ResultMeasureValue, na.rm = T)) |> 
  group_by(state) |> 
  summarize(avgStateNitrogen = mean(avgStationNitrogen, na.rm = T))

states$hasReg = ifelse(states$NAME %in% stateRegs$State, "1", "0")
states = states |> 
  filter(!NAME %in% c("Alaska", "Hawaii", "Guam", "Puerto Rico", "United States Virgin Islands", 
  "American Samoa", "Commonwealth of the Northern Mariana Islands")) 

statesWithNitrogen = states |> 
  left_join(stateAvgNitrogen, by = c("STUSPS" = "state")) |> 
  filter(!is.na(avgStateNitrogen))

tm_shape(states) + 
  tm_fill("hasReg", palette = c("1" = "royalblue", "0" = "grey"),
          title = "Has Wastewater \nRegulation") +
  tm_borders(col = "black") + 
  tm_layout(title = "States with Agrictultural Wasterwater Reuse Regulations",
            title.position = c("center", "top"),
            inner.margins = c(0.01, 0.01, .125, 0.01))

stateNitrogenWithReg = statesWithNitrogen |> 
  filter(hasReg == "1")

stateNitrogenWOReg = statesWithNitrogen |> 
  filter(hasReg == "0")

breaks = c(0,0.5, 1, 2, 4, 8)
tm_shape(states)  + tm_borders(col = "black") +
  tm_shape(stateNitrogenWithReg) + tm_fill(col = "avgStateNitrogen", breaks = breaks, palette = c("#bef7ff", "#a0dcff", "#82c2ff", "#63a7ff", "#458cff"), alpha = .8) +
  tm_shape(stateNitrogenWOReg)+ tm_fill(col = "avgStateNitrogen", breaks = breaks, palette = "Greys", alpha = .8) 
  
tm_shape(states) + tm_borders(col = "black") + 
  tm_shape(statesWithNitrogen) + tm_fill(col = "avgStateNitrogen", breaks = breaks, palette = "Blues", alpha = .8)

