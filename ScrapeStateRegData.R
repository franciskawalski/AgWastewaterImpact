library(tidyverse)
library(rvest)

url = "https://www.epa.gov/waterreuse/reusing-water-agricultural-activities-resources"
page = read_html(url)

#Navigate to the relevent table and extract all the elements
states = page |> 
  html_element("#main > div > div.l-constrain > div.l-sidebar.l-sidebar--reversed > div.l-sidebar__main > article > div.l-grid.l-grid--3-col.l-column-layout") |> 
  html_elements("a")

##Get the state names so we can add that data in later
stateNames = states |> 
  html_text()

##Set up an empty dataframe to fill in loop
allStateRegs = data.frame(matrix(nrow=0, ncol = 6))
colnames(allStateRegs) = c("Recycled Water Class/Category", 
                           "Source Water Type", 
                           "Water Quality Parameter", 
                           "Specification", 
                           "Sampling/Monitoring Requirements (Frequency of monitoring; site/ location of sample; quantification methods)*",
                           "State")

baseURL = "https://www.epa.gov"

for (i in 1:length(states)) {
  endPoint = states[[i]] |>  html_attr("href") ##Get the link to the state page
  page = read_html(str_c(baseURL, endPoint))
  
  ##Get the important table and add information to the larger dataframe
  table = page |> 
    html_nodes(".usa-table") 
  table = table[[1]] |> 
    html_table()
  table$State = stateNames[[i]]
  colnames(table) = colnames(allStateRegs)
  allStateRegs = rbind(allStateRegs, table)
}

write.csv(allStateRegs, "WastewaterRegulationByState.csv")
