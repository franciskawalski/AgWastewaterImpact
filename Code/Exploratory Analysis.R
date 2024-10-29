library(tidyverse)
library(zoo)

setwd("../Data/Clean")

data = read_csv("cleanMergedData.csv") |> 
  select(-`...1`) |> 
  mutate(monthYear = as.yearmon(str_c(year, month), "%Y %m")) |> 
  rename(USDARegion = `USDA Farm Production Region`)

##There are repeated rows from different categories. This combines them so they aren't weighted heavier
dataWithoutCat = data |> 
  group_by(year, month, monthYear, state, CharacteristicName, hasStateReg, firstRegYear, currentRegYear, USDARegion) |> 
  summarise(avgValue = mean(avgValue))

##See the differences between states that do and do not have regs
ggplot(dataWithoutCat, aes(x = avgValue, fill = hasStateReg)) +
  geom_histogram(position = "identity", alpha = .5, bins  = 50) +
  scale_x_log10()

##See if there are any stochastic differences between states that do and do not have regs
dataWithoutCat |> 
  filter(CharacteristicName == "Nitrogen") |> 
  group_by(year, hasStateReg) |> 
  summarise(avgVal = mean(avgValue, na.rm = T)) |> 
  ggplot(aes(x = year, y = avgVal, color = hasStateReg)) +
  geom_line()

##See what's driving the extreme data in 2015
dataWithoutCat |> 
  filter(CharacteristicName == "Nitrogen",
         !hasStateReg,
         year == 2015,
         avgValue > 10)

##Remove extreme NH data
dataWithoutCat |> 
  filter(CharacteristicName == "Nitrogen",
         state != "NH") |> #Remove extreme data to see trends better
  group_by(year, hasStateReg) |> 
  summarise(avgVal = mean(avgValue, na.rm = T)) |> 
  ggplot(aes(x = year, y = avgVal, color = hasStateReg)) +
  geom_line()


##See if there are systematic stochastic differences between specification types
data |> 
  filter(CharacteristicName == "Nitrogen",
         hasStateReg) |>
  group_by(year, Specification, state, month) |> 
  summarize(avgVal = mean(avgValue)) |> 
  mutate(Specification = ifelse(str_detect(Specification, "[0-9]"), "Specified Amount", Specification)) |> ##Group all nums together
  group_by(Specification, year) |> 
  summarise(avgVal = mean(avgVal)) |> 
  ggplot(aes(x = year, y = avgVal, color = Specification)) + 
  geom_line() 

##See if there is any 
dataWithoutCat |> 
  filter(hasStateReg,
         avgValue < 20) |> ##Remove outliers for plot to get better view of data
  mutate(event = year - currentRegYear,
         event = ifelse(event > 9, 10, event),
         event = ifelse(event < -9, -10, event))  |> 
  ggplot(aes(x = event, y = avgValue)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_smooth()
  
##See if there are any systematic stochastic differences by region
dataWithoutCat |> 
  group_by(year, USDARegion)
  
  









