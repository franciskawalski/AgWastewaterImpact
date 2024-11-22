library(tidyverse)

setwd("../Data/Clean")

##Import all the datasets, remove some columns and rename so the merge is easy
stateRegs = read_csv("stateRegs.csv") |> 
  select(-c(`...1`, `...2`)) |> 
  rename(state = State) |> ##Match case for rest of data
  mutate(state = state.abb[match(state,state.name)]) ##Make state names abbr

stateRegYear = read_csv("stateRegYear.csv") |> 
  select(c(State, Type, firstRegYear, currentRegYear)) |> 
  mutate(State = state.abb[match(State,state.name)]) |>  ##Make state names abbr
  rename(state = State)

avgMthWaterData = read_csv("AverageMonthlyWaterData.csv") |> 
  select(-`...1`)

totalWaterData = read_csv("waterDataCleaned.csv") |> 
  select(-`...1`)

regions = read_csv("USDA Regions.csv") |> 
  mutate(State = state.abb[match(State,state.name)]) |> 
  rename(state = State)

##Join water quality data with state data
mergedStateAvg = avgMthWaterData |> 
  left_join(stateRegs, by = "state", relationship = "many-to-many") |> 
  mutate(hasStateReg = !is.na(Specification)) |> 
  filter(tolower(CharacteristicName) == `Water Quality Parameter` | !hasStateReg) |> 
  left_join(stateRegYear, by = "state", relationship = "many-to-one") |> 
  rename(regType = Type) |> 
  left_join(regions, by = "state", relationship = "many-to-one")


write.csv(mergedStateAvg, "cleanMergedData_StateAvg.csv")

mergeredTotal = totalWaterData |> 
  left_join(stateRegs, by = "state", relationship = "many-to-many") |> 
  mutate(hasStateReg = !is.na(Specification)) |> 
  filter(tolower(CharacteristicName) == `Water Quality Parameter` | !hasStateReg) |> 
  left_join(stateRegYear, by = "state", relationship = "many-to-one") |> 
  rename(regType = Type) |> 
  left_join(regions, by = "state", relationship = "many-to-one")

write.csv(mergeredTotal, "cleanMergedData_StationLevel.csv")


