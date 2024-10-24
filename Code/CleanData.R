library(tidyverse)
library(summarytools)

##Set wd to the raw data folder to access data
setwd("../Data/Raw")

stateRegs = read_csv("WastewaterRegulationByState.csv")

param = stateRegs |> 
  group_by(`Water Quality Parameter`) |> 
  summarise(count = n()) |> 
  arrange(desc(count))
        