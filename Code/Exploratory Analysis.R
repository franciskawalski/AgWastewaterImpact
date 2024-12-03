library(tidyverse)
library(zoo)
library(RColorBrewer)

setwd("../Data/Clean")

#For state level analysis
# data = read_csv("cleanMergedData_StateAvg.csv") |> 
#   select(-`...1`) |> 
#   mutate(monthYear = as.yearmon(str_c(year, month), "%Y %m")) |> 
#   rename(USDARegion = `USDA Farm Production Region`)

#For station level analysis
data = read_csv("cleanMergedData_StationLevel.csv") |> 
  select(-`...1`) |> 
  mutate(monthYear = as.yearmon(str_c(year, month), "%Y %m")) |> 
  rename(USDARegion = `USDA Farm Production Region`) |> 
  filter(ResultMeasureValue <= 100) ##These outliers do not represent the true DGP

##There are repeated rows from different categories. This combines them so they aren't weighted heavier
dataWithoutCat = data |> 
  group_by(year, month, monthYear, state, CharacteristicName, hasStateReg, firstRegYear, currentRegYear, USDARegion, MonitoringLocationIdentifier) |> 
  summarise(avgValue = mean(ResultMeasureValue))

write.csv(dataWithoutCat, "stateAvgWithoutCat.csv")

##See the differences between states that do and do not have regs
RegVsNot = ggplot(dataWithoutCat, aes(x = avgValue, fill = hasStateReg)) +
  geom_histogram(position = "identity", alpha = .5, bins  = 50) +
  scale_x_log10() +
  theme_bw() +
  theme_minimal() +
  xlab("Nitrogen Concentration in mg/L") +
  ylab("Number of Observations") +
  labs(title = "Nitrogen Concentration by Regulation Status", fill = "State Has\nRegulations")+
  theme(plot.title = element_text(hjust = 0.5)) ##Center title

##See if there are any stochastic differences between states that do and do not have regs
ConcentrationByReg = dataWithoutCat |> 
  filter(CharacteristicName == "Nitrogen") |> 
  group_by(year, hasStateReg) |> 
  summarise(avgVal = mean(avgValue, na.rm = T)) |> 
  ggplot(aes(x = year, y = avgVal, color = hasStateReg)) +
  geom_line() +
  scale_y_log10() + 
  theme_bw() +
  theme_minimal() +
  xlab("Year") +
  ylab("Nitrogen in mg/L (logged)") +
  labs(title = "Nitrogen Concentration Over time", color = "State Has\nRegulations") +
  theme(plot.title = element_text(hjust = 0.5)) ##Center title


##See if there are systematic stochastic differences between specification types
ConcentrationBySpec = data |> 
  filter(CharacteristicName == "Nitrogen",
         hasStateReg) |>
  group_by(year, Specification, state, month) |> ##Combine state level data into only relevant categories
  summarize(avgVal = mean(ResultMeasureValue, na.rm = T)) |> 
  mutate(Specification = ifelse(str_detect(Specification, "[0-9]"), "Specified Amount", Specification)) |> ##Group all nums together
  group_by(Specification, year) |> 
  summarise(avgVal = mean(avgVal)) |> 
  ggplot(aes(x = year, y = avgVal, color = Specification)) + 
  geom_line() +
  scale_y_log10() +
  theme_bw() +
  theme_minimal() +
  xlab("Year") +
  ylab("Nitrogen Concentration in mg/L (logged)") +
  labs(title = "Nitrogen Concentration by State Regulatory Specification", color = "Regulatory\nSpecification") +
  theme(plot.title = element_text(hjust = 0.5)) ##Center title


##See if there are any systematic differences across the treatment horizon
ConcentrationAcrossReg = dataWithoutCat |> 
  filter(hasStateReg) |> 
  mutate(event = year - currentRegYear,
         event = ifelse(event > 9, 10, event),
         event = ifelse(event < -9, -10, event))  |> 
  ggplot(aes(x = event, y = avgValue)) +
  geom_point(alpha = .3, size = 2) +
  geom_vline(xintercept = 0) +
  geom_smooth() +
  theme_bw()+ 
  xlab("Years Relative to Treatment") +
  ylab("Nitrogen Concentration in mg/L (logged)") +
  labs(title = "Nitrogen Concentration Across Regulation Periods") +
  scale_y_log10(labels = scales::comma) +
  theme(plot.title = element_text(hjust = 0.5)) ##Center title
  
##See if there are any systematic stochastic differences by region
ConcentrationByRegion = dataWithoutCat |> 
  ggplot(aes(x = avgValue, fill = USDARegion)) +
  geom_histogram(position = "Identity", alpha = .4, bins = 50)  +
  scale_x_log10(labels = scales::comma) +
  theme_bw() +
  xlab("Nitrogen Concentration in mg/L (logged)") +
  ylab("Number of Observations") +
  labs(title = "Nitrogen Concentrations by Region", 
       fill = "USDA Region")+
  theme(plot.title = element_text(hjust = 0.5)) ##Center title

ggsave("plots/NitrogenConcentrationByRegion.png", ConcentrationByRegion, height = 4, width = 6)
  
ggsave("plots/NitrogenConcentrationByRegulation.png", ConcentrationByReg, height = 4, width = 6)

ggsave("plots/NitrogenConcentrationBySpecification.png", ConcentrationBySpec, height = 4, width = 6)

ggsave("plots/NitrogenConcentrationAcrossRegulationTime.png", ConcentrationAcrossReg, height = 4, width = 6)

ggsave("plots/NitrogenConcentrationWithWithoutRegulations.png", RegVsNot, height = 4, width = 6)







