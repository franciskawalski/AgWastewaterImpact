library(tidyverse)
library(sf)
library(tmap)

states = st_read("../Data/Clean/States_shape/cb_2018_us_state_500k.shp") |> 
  st_make_valid()

stateRegs = read_csv("../Data/Clean/stateRegYear.csv")

states$hasReg = ifelse(states$NAME %in% stateRegs$State, 1, 0)
states = states |> 
  filter(!NAME %in% c("Alaska", "Hawaii", "Guam", "Puerto Rico", "United States Virgin Islands", 
  "American Samoa", "Commonwealth of the Northern Mariana Islands")) |> 
  st_transform(crs = 4326)



tm_shape(states) + tm_polygons(col = "hasReg")

