# This data was handed to me as a pdf. I first had to transform it to xlsx 
# via a webpage. The rest of the data manipulation was carried out below.

library(rio)
library(tidyverse)
library(zoo)
flights <- import("flights_2006.xlsx")

# Clean the data, rename columns
flights <- flights %>% as_tibble %>% filter(row_number()>=7)
names(flights) <- flights %>% filter(row.names(.) == 1)  %>% as.character(.)
flights <-  flights %>% 
  filter(row_number()>1) %>% 
  mutate(`Jan-Mar` = as.numeric(str_replace(`Jan-Mar`, " ", "")),
         `Apr-Jun` = as.numeric(str_replace(`Apr-Jun`, " ", "")),
         `Jul-Sep` = as.numeric(str_replace(`Jul-Sep`, " ", "")),
         `Okt-Dec` = as.numeric(str_replace(`Okt-Dec`, " ", "")),
         Total = as.numeric(str_replace(Total, " ", "")))
                                    
# delete a few non informative rows
flights <- flights %>% filter(!(airport %in% c("Rapporterande", "flygplats", 
                                   "Reporting","airport")), 
                              !grepl("Total", airport))
# add an airport for each observation.
flights <- flights %>% 
  mutate(airport = ifelse(airport == "AIRPORT", NA, airport),
         airport = ifelse(airport == "LANDVETTER", "GÖTEBORG-", airport),
         airport = ifelse(airport == "ARLANDA", "STOCKHOLM-", airport)) 

flights <- flights %>% 
  fill(everything()) %>% fill(everything(), .direction = "up")

# Select relevant starting points and destinations
start <- c("KARLSTAD", "JÖNKÖPING", "NORRKÖPING *",
           "SUNDSVALL-", "HÄRNÖSAND") # ALmost everything: "STOCKHOLM-", "BROMMA",
destination <- c("KÖPENHAMN", "ROSKILDE", "MALMÖ-STURUP")

flights <- flights %>% filter(airport %in% start & Airport %in% destination)

# The "Total" column looks off and can be redefined:
flights <- flights %>% 
  mutate(Total = `Jan-Mar` + `Apr-Jun` + `Jul-Sep` + `Okt-Dec`)

flights %>% ggplot(aes(x = airport, y = Total)) +
  geom_col()