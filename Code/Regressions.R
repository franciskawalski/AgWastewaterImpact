library(tidyverse)
library(fixest)
library(did2s)
library(DescTools)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

##Load data
#mnthAvgStateWQ = read_csv("../Data/Clean/cleanMergedData_StateAvg.csv") 
stationStateWQ = read.csv("../Data/Clean/cleanMergedData_StationLevel.csv", header = T) |> 
  filter(ResultMeasureValue <= 100)

stationStateWQNoCat = stationStateWQ |> 
  group_by(month, year, USDA.Farm.Production.Region, firstRegYear, currentRegYear, state,
           MonitoringLocationIdentifier, ResultMeasureValue, CharacteristicName, MethodSpeciationName ) |> 
  summarise(Specification = mode(Specification))


##Add indicators for treatment
# mnthAvgStateWQ = mnthAvgStateWQ |> 
#   mutate(firstRegTreated = ifelse(year >= firstRegYear, 1, 0),
#          firstRegTreated = ifelse(is.na(firstRegTreated), 0, firstRegTreated),
#          currentRegTreated = ifelse(year >= currentRegYear, 1, 0),
#          currentRegTreated = ifelse(is.na(currentRegTreated), 0, currentRegTreated))

stationStateWQ = stationStateWQNoCat |> 
  mutate(firstRegTreated = ifelse(year >= firstRegYear, 1, 0),
         firstRegTreated = ifelse(is.na(firstRegTreated), 0, firstRegTreated),
         currentRegTreated = ifelse(year >= currentRegYear, 1, 0),
         currentRegTreated = ifelse(is.na(currentRegTreated), 0, currentRegTreated),
         lnMeasureValue = log(ResultMeasureValue + .01), ##Many results are 0
         relYearCurrent = ifelse(is.na(currentRegYear), Inf, year - currentRegYear),
         regAmountSpecified = ifelse((currentRegTreated == 1 & Specification != "not specified"), 1, 0),
         regAmountSpecified = ifelse(is.na(regAmountSpecified), 0, regAmountSpecified),
         regAmountNotSpecified = ifelse(currentRegTreated == 1 & Specification == "not specified", 1, 0),
         regAmountNotSpecified = ifelse(is.na(regAmountNotSpecified), 0, regAmountNotSpecified))

##Seperate data by chem tested 
stationNitrogenWQ = stationStateWQ |> 
  filter(CharacteristicName == "Nitrogen")

stationNitriteWQ = stationStateWQ |> 
  filter(CharacteristicName == "Nitrite"| MethodSpeciationName == "as NO2")

stationNitrateWQ = stationStateWQ |> 
  filter(CharacteristicName == "Nitrate" | MethodSpeciationName == "as NO3")

##Run initial regressions
regImpactNitrate = feols(lnMeasureValue ~ currentRegTreated | 
                           state + year + MonitoringLocationIdentifier + month + USDA.Farm.Production.Region^year, 
                              data = stationNitrateWQ, cluster = "MonitoringLocationIdentifier")
summary(regImpactNitrate)

regImpactNitrite = feols(lnMeasureValue ~  currentRegTreated | 
                           state + year + MonitoringLocationIdentifier + month + USDA.Farm.Production.Region^year, 
                               data = stationNitriteWQ, cluster = "MonitoringLocationIdentifier")
summary(regImpactNitrite)

regImpactNitrogen = feols(lnMeasureValue ~  currentRegTreated | 
                            state + year + MonitoringLocationIdentifier + month + USDA.Farm.Production.Region^year, 
                          data = stationNitrogenWQ, cluster = "MonitoringLocationIdentifier")
summary(regImpactNitrogen)

##Regressions by regulation type
regTypeImpactNitrate = feols(lnMeasureValue ~  regAmountSpecified  | 
                               state + year + MonitoringLocationIdentifier + month + USDA.Farm.Production.Region^year, 
                             data = stationNitrateWQ, cluster = "MonitoringLocationIdentifier")
summary(regTypeImpactNitrate)

regTypeImpactNitrite = feols(lnMeasureValue ~  regAmountSpecified + regAmountNotSpecified | 
                               state + year + MonitoringLocationIdentifier + month + USDA.Farm.Production.Region^year, 
                             data = stationNitriteWQ, cluster = "MonitoringLocationIdentifier")
summary(regTypeImpactNitrite)

regTypeImpactNitrogen = feols(lnMeasureValue ~  regAmountSpecified + regAmountNotSpecified | 
                                state + year + MonitoringLocationIdentifier + month + USDA.Farm.Production.Region^year, 
                              data = stationNitrogenWQ, cluster = "MonitoringLocationIdentifier")
summary(regTypeImpactNitrogen)


##Gardner 2-Stage Regressions
nitrogenRelYearGard = did2s(stationNitrogenWQ, yname = "lnMeasureValue", 
                  first_stage = ~  0 | state + year + MonitoringLocationIdentifier 
                  + month + USDA.Farm.Production.Region^year,
                  second_stage = ~i(relYearCurrent, ref= c(-1, Inf)), treatment = "currentRegTreated",
                  cluster_var = "MonitoringLocationIdentifier")
iplot(nitrogenRelYearGard)



