library(tidyverse)
library(zoo)
library(RColorBrewer)

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
  scale_x_log10() +
  theme_bw() +
  theme_minimal() +
  xlab("Nitrogen Concentration in mg/L (logged)") +
  ylab("Number of Observations") +
  labs(title = "Nitrogen Concentration by Regulation Status", fill = "State Has\nRegulations")

dataWithoutCat = dataWithoutCat |> 
  mutate(lnAvgVal = log(avgValue))

##See if there are any stochastic differences between states that do and do not have regs
dataWithoutCat |> 
  filter(CharacteristicName == "Nitrogen") |> 
  group_by(year, hasStateReg) |> 
  summarise(avgVal = mean(lnAvgVal, na.rm = T)) |> 
  ggplot(aes(x = year, y = avgVal, color = hasStateReg)) +
  geom_line() +
  theme_bw() +
  theme_minimal() +
  xlab("Year") +
  ylab("Nitrogen in mg/L (logged)") +
  labs(title = "Nitrogen Concentration Over time", color = "State Has\nRegulations")


##See if there are systematic stochastic differences between specification types
data |> 
  filter(CharacteristicName == "Nitrogen",
         hasStateReg) |>
  group_by(year, Specification, state, month) |> 
  summarize(avgVal = mean(log(avgValue))) |> 
  mutate(Specification = ifelse(str_detect(Specification, "[0-9]"), "Specified Amount", Specification)) |> ##Group all nums together
  group_by(Specification, year) |> 
  summarise(avgVal = mean(avgVal)) |> 
  ggplot(aes(x = year, y = avgVal, color = Specification)) + 
  geom_line() +
  theme_bw() +
  theme_minimal() +
  xlab("Year") +
  ylab("Nitrogen Concentration, mg/L") +
  labs(title = "Nitrogen Concentration by State Regulatory Specification", color = "Regulatory\nSpecification")

##See if there are any systematic differences across the treatment horizon
dataWithoutCat |> 
  filter(hasStateReg) |> 
  mutate(event = year - currentRegYear,
         event = ifelse(event > 9, 10, event),
         event = ifelse(event < -9, -10, event))  |> 
  ggplot(aes(x = event, y = lnAvgVal)) +
  geom_point(alpha = .3, size = 2) +
  geom_vline(xintercept = 0) +
  geom_smooth() +
  theme_bw()+ 
  xlab("Years Relative to Treatment") +
  ylab("Nitrogen Concentration (mg/L)") +
  labs(title = "Nitrogen Concentration Across Regulation Periods")
  
##See if there are any systematic stochastic differences by region
dataWithoutCat |> 
  ggplot(aes(x = avgValue, fill = USDARegion)) +
  geom_histogram(position = "Identity", alpha = .4, bins = 50)  +
  scale_x_log10() +
  theme_bw() +
  xlab("Nitrogen Concentration (mg/L)") +
  ylab("Number of Observations") +
  labs(title = "Nitrogen Concentrations by Region", 
       fill = "USDA Region")

  
  









