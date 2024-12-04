library(tidyverse)
library(sf)
library(tmap)

##Load in the US spacial data
states = st_read("../Data/Clean/States_shape/cb_2018_us_state_500k.shp") |> 
  st_make_valid()

stateRegs = read_csv("../Data/Clean/stateRegYear.csv")

wqData = read_csv("../Data/Clean/cleanMergedData_StationLevel.csv")

stateAvgNitrogen = wqData |> 
  filter(CharacteristicName == "Nitrogen", year %in% c(2008:2012)) |> 
  group_by(state, ResultMeasureValue, MonitoringLocationIdentifier) |> ##Remove duplicates from merge
  group_by(state, MonitoringLocationIdentifier) |> 
  summarise(avgStationNitrogen = mean(ResultMeasureValue, na.rm = T)) |>  ##Get average by station
  group_by(state) |> 
  summarize(avgStateNitrogen = mean(avgStationNitrogen, na.rm = T)) ##Get state average from station average

##Clean and combine the data
states$hasReg = ifelse(states$NAME %in% stateRegs$State, "1", "0")
states = states |> 
  filter(!NAME %in% c("Alaska", "Hawaii", "Guam", "Puerto Rico", "United States Virgin Islands", 
  "American Samoa", "Commonwealth of the Northern Mariana Islands")) 

statesWithNitrogen = states |> 
  left_join(stateAvgNitrogen, by = c("STUSPS" = "state")) |> 
  filter(!is.na(avgStateNitrogen))

##Map of state with regulations
stateRegMap = tm_shape(states) + 
  tm_fill("hasReg", palette = c("1" = "dodgerblue4", "0" = "azure"),
          title = "Has Wastewater \nRegulation", labels = c("No", "Yes")) +
  tm_borders(col = "black") + 
  tm_layout(title = "States with Agrictultural Wasterwater Reuse Regulations",
            title.position = c("center", "top"),
            inner.margins = c(0.01, 0.01, .125, 0.01))

##Map of national nitrogen concentrations
breaks = c(0, .5, 1, 2, 4, 6)
stateNitrMap = tm_shape(states) + tm_borders(col = "black") + 
  tm_shape(statesWithNitrogen) + tm_fill(col = "avgStateNitrogen",  palette = "Blues", breaks = breaks, alpha = .8,
                                         title = "Avg. mg/L of\nNitrogen") +
  tm_layout(title = "Avgerage Nitrogen Sample Concentrations 2008-2012",
            title.position = c("center", "top"),
            legend.position = c("right", "bottom"),
            inner.margins = c(0.01, 0.01, .125, 0.01))

