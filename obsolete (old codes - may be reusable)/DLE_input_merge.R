# library(XML)
# library(gtools)
# library(birk)
# library(spatstat)
# library(foreign)
# library(gdata)
# library(readxl)
# library(Hmisc)
# library(stringr)
# library(gbm)
# library(XLConnect)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
# library(dplyrExtras)



### Initialization
# To convert inconsistet column names
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

view <- function(data, autofilter=TRUE) {
  # data: data frame
  # autofilter: whether to apply a filter to make sorting and filtering easier
  open_command <- switch(Sys.info()[['sysname']],
                         Windows= 'open',
                         Linux  = 'xdg-open',
                         Darwin = 'open')
  require(XLConnect)
  temp_file <- paste0(tempfile(), '.xlsx')
  wb <- XLConnect::loadWorkbook(temp_file, create = TRUE)
  XLConnect::createSheet(wb, name = "temp")
  XLConnect::writeWorksheet(wb, data, sheet = "temp", startRow = 1, startCol = 1)
  if (autofilter) setAutoFilter(wb, 'temp', aref('A1', dim(data)))
  XLConnect::saveWorkbook(wb, )
  system(paste(open_command, temp_file))
}

path.integ <- "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Analysis/Final results/"



### 1) Final Energy ###

### Data import
stock.raw <- read.csv(paste0(path.integ, "stock_timeseries_JM.csv")) %>% select(-X) 
build.raw <- read.csv(paste0(path.integ, "Build_timeseries.csv")) %>% select(-X)
names(build.raw) <- sapply(names(build.raw), firstup)
build.raw <- build.raw %>% 
  mutate(Country=gsub("Brazil", "BRA", Country)) %>%
  mutate(Country=gsub("India", "IND", Country)) %>%
  mutate(Country=gsub("S. Africa", "ZAF", Country)) 

uncertainty.case <- ""
if (uncertainty.case == "low") {
  intensity.raw <- read.csv(paste0(path.integ, "DLE intensity by comp - uncertainty low.csv")) %>% select(-X)
} else if (uncertainty.case == "high") {
  intensity.raw <- read.csv(paste0(path.integ, "DLE intensity by comp - uncertainty high.csv")) %>% select(-X)
} else {
  intensity.raw <- read.csv(paste0(path.integ, "DLE intensity by comp.csv")) %>% select(-X)
}

ssp.raw <- read.csv(paste0(path.integ, "ssp_timeseries.csv")) %>% select(-X)
names(ssp.raw) <- sapply(names(ssp.raw), firstup)
ssp.raw <- ssp.raw %>% 
  mutate(Country=gsub("Brazil", "BRA", Country)) %>%
  mutate(Country=gsub("India", "IND", Country)) %>%
  mutate(Country=gsub("S. Africa", "ZAF", Country)) 

intensity.NR.raw <- read.csv(paste0(path.integ, "NR_intensities_timeseries.csv")) %>% select(-X)
names(intensity.NR.raw) <- sapply(names(intensity.NR.raw), firstup)
intensity.NR.raw <- intensity.NR.raw %>% 
  mutate(Country=gsub("Brazil", "BRA", Country)) %>%
  mutate(Country=gsub("India", "IND", Country)) %>%
  mutate(Country=gsub("S. Africa", "ZAF", Country)) 



### Arrange tables for consistency
name.harmonized <- gsub("Education", "Educ", names(intensity.raw))
names(intensity.raw) <- gsub("Refrigerator", "Fridge", name.harmonized)



### Merge intensity tables
intensity <- intensity.raw %>% left_join(intensity.NR.raw, by=c("Country", "Scenario", "Year", "Type", "Carrier")) %>%
  mutate(Fridge=ifelse(is.na(Fridge.y), Fridge.x, Fridge.y), TV=ifelse(is.na(TV.y), TV.x, TV.y)) %>%
  select(-ends_with(".x"), -ends_with(".y"))
intensity <- intensity %>% select(Scenario, Country, Type, Year, everything()) %>% 
  arrange(Scenario, Country, Type, Carrier, Year)

stock <- stock.raw %>% mutate(Type="OP") %>% mutate(Lighting.R=Housing.R, Lighting.U=Housing.U, AC.R=NA, AC.U=NA, Roads=NA) %>% 
  select(setdiff(names(intensity), "Carrier"))
build <- build.raw %>% mutate(Type="CON") %>% select(intersect(names(intensity), names(build.raw)))  # Should be in the same order

# Combining stock and build
unit.tot <- stock
unit.tot[,] <- NA
unit.tot[, which(names(stock) %in% names(build))] <- build
unit.tot <- unit.tot %>% rbind(stock) %>% arrange(Scenario, Country, Type, Year)

# Convert JM's components from unit/cap to total unit (million kcal or million kg per year)
unit.tot <- unit.tot %>% left_join(ssp.raw) %>% 
  mutate_at(vars(Clothing, Food), funs(.*Pop)) %>% # Pop  in millions - Now footwear/clothing combined
  # mutate_at(vars(Clothing, Food, Footwear), funs(.*Pop)) %>% # Pop  in millions
  select(-(GDP:Urban)) 

# Lengthen unit.tot 
unit.tot <- intensity %>% select(Scenario:Carrier) %>% left_join(unit.tot) %>% select(names(intensity))

# Derive total energy
energy.calc <- (unit.tot %>% select(-(Scenario:Carrier))) * (intensity %>% select(-(Scenario:Carrier))) %>%
  mutate(Mobility=Mobility*1e3, Roads=Roads/1e6) / 1e12 * 1e6 # To EJ (& all other units were in millions except Mobility (billion p-km) & Roads (km))
energy.tot <- data.frame(unit.tot %>% select(Scenario:Carrier), energy.calc) %>% 
  mutate(yrs=Year-lag(Year,1)) %>% 
  mutate(yrs=ifelse(yrs<0 | is.na(yrs), 1, yrs)) 
  # mutate(yrs=ifelse(yrs<0, NA, yrs)) 
energy.tot <- energy.tot %>% filter(Type=="OP") %>%
  rbind(energy.tot %>% filter(Type=="CON") %>% mutate_at(vars(Housing.R:TV), funs(./yrs))) %>% select(-yrs)  %>% 
  arrange(Scenario, Country, Type, Carrier, Year)
energy.tot[is.na(energy.tot)] <- 0
energy.tot.sum <- energy.tot %>% group_by(Scenario, Country, Type, Year) %>% summarise_at(vars(Housing.R:TV), sum, na.rm=TRUE) %>% ungroup() %>%
  mutate(Tot.EJ = rowSums(.[5:23], na.rm=TRUE))

energy.pcap <- energy.tot %>% left_join(ssp.raw %>% select(Country, Year, Pop)) %>%
  mutate_at(vars(Housing.R:TV), funs(./Pop*1e9/1e6)) %>% select(-Pop)
energy.pcap.sum <- energy.pcap %>% group_by(Scenario, Country, Type, Year) %>% summarise_at(vars(Housing.R:TV), sum, na.rm=TRUE) %>% ungroup() %>%
  mutate(Tot.GJ.pcap = rowSums(.[6:23], na.rm=TRUE))

# Save outputs as XLSX
xlsx::write.xlsx(as.data.frame(energy.tot), paste0(path.integ, "Result - Total final energy", uncertainty.case, ".xlsx"), sheetName="EJ by comp")
xlsx::write.xlsx(as.data.frame(energy.tot.sum), paste0(path.integ, "Result - Total final energy", uncertainty.case, ".xlsx"), sheetName="Aggregated ", append=TRUE)
xlsx::write.xlsx(as.data.frame(energy.pcap), paste0(path.integ, "Result - Total final energy per cap", uncertainty.case, ".xlsx"), sheetName="GJ.pcap by comp")
xlsx::write.xlsx(as.data.frame(energy.pcap.sum), paste0(path.integ, "Result - Total final energy per cap", uncertainty.case, ".xlsx"), sheetName="Aggregate", append=TRUE)

xlsx::write.xlsx(as.data.frame(intensity), paste0(path.integ, "Input - Intensity & Units", uncertainty.case, ".xlsx"), sheetName="FE Intensity (TFEI)")
xlsx::write.xlsx(as.data.frame(unit.tot), paste0(path.integ, "Input - Intensity & Units", uncertainty.case, ".xlsx"), sheetName="Unit", append=TRUE)



### 2) Emissions ###

# 1. Energy-related emissions
EF.ENE.country <- read_xlsx(paste0(path.integ, "DLE Pathway construction.xlsx"), sheet="Emissions Calcs", range="R6:S8", col_names = FALSE) %>% 
  mutate(Country=c("BRA", "IND", "ZAF")) 
names(EF.ENE.country)[1:2] <- c("elec", "non.elec")  # kg/MJ
EF.ENE.country <- EF.ENE.country %>% gather(Carrier, EF, -Country)

emissions.ENE.tot <- energy.tot %>% group_by(Scenario, Country, Type, Year, Carrier) %>% summarise_at(vars(Housing.R:TV), sum, na.rm=TRUE) %>% ungroup() %>%
  # mutate(Tot.EJ = rowSums(.[6:24], na.rm=TRUE)) %>% 
  left_join(EF.ENE.country) %>% 
  mutate_at(vars(Housing.R:TV), funs(.*EF*1e12/1e9)) %>% select(-EF) # M.ton CO2e
  # mutate(Tot.CO2.Mton = EF*Tot.EJ*1e12/1e9)
emissions.ENE.tot.sum <- emissions.ENE.tot %>% group_by(Scenario, Country, Year) %>% 
  select(-Type, -Carrier) %>%
  summarise_all(sum, na.rm=TRUE)

# 2. Non-energy emissions
EF.NENE <- read.csv(paste0(path.integ, "Emissions by comp w.food.csv")) %>% select(-X) %>% #tCO2/m2 & tCO2/kcal
  rename(Housing.R=Housing.cement.R, Housing.U=Housing.cement.U) %>% arrange(Scenario, Country, Type, Year)
# Seeking same dimension
# Non-energy EF has no Carrier dimension, so half the number of rows
EF.NENE.all <- unit.tot %>% filter(Carrier=="elec") %>% select(-Carrier) %>% arrange(Scenario, Country, Type, Year)
EF.NENE.all[,] <- NA
EF.NENE.all[, which(names(EF.NENE.all) %in% names(EF.NENE))] <- EF.NENE

# Total in M.ton CO2e
emissions.NENE.calc <- (unit.tot %>% filter(Carrier=="elec") %>% select(-(Scenario:Carrier))) * (EF.NENE.all %>% select(-(Scenario:Year))) 
emissions.NENE.tot <- data.frame(EF.NENE.all %>% select(Scenario:Year) %>% mutate(Carrier="non.energy"), emissions.NENE.calc) # M.ton CO2

# Total in M.ton CO2e
emissions.tot <- emissions.ENE.tot %>% rbind(emissions.NENE.tot) %>% arrange(Scenario, Country, Type, Carrier, Year) %>%
  mutate(Tot.CO2e.Mton = rowSums(.[6:24], na.rm=TRUE)) 
emissions.tot[is.na(emissions.tot)] <- 0
emissions.tot.sum <- emissions.tot %>% group_by(Scenario, Country, Year) %>% 
  select(-Type, -Carrier) %>%
  summarise_all(sum, na.rm=TRUE)

# Total.cap in ton CO2e / cap
emissions.pcap <- emissions.tot %>% left_join(ssp.raw %>% select(Country, Year, Pop)) %>% # Pop in millions
  mutate_at(vars(Housing.R:Tot.CO2e.Mton), funs(./Pop*1e6/1e6)) %>% select(-Pop) %>% rename(CO2e.ton.pcap=Tot.CO2e.Mton) # ton/cap
emissions.pcap.sum <- emissions.tot.sum %>% left_join(ssp.raw %>% select(Country, Year, Pop)) %>%
  mutate_at(vars(Housing.R:Tot.CO2e.Mton), funs(./Pop*1e6/1e6)) %>% select(-Pop) %>% rename(CO2e.ton.pcap=Tot.CO2e.Mton) # ton/cap
# emissions.cum <- emissions.pcap %>% group_by(Scenario, Country, Year) %>% summarise_at(vars(Housing.R:TV), sum, na.rm=TRUE) %>% ungroup() %>%
#   mutate(Tot.CO2e.ton.pcap = rowSums(.[6:22], na.rm=TRUE))

# Save outputs as XLSX
xlsx::write.xlsx(as.data.frame(emissions.tot), paste0(path.integ, "Result - Total emissions.xlsx"), sheetName="Emissions by comp")
xlsx::write.xlsx(as.data.frame(emissions.tot.sum), paste0(path.integ, "Result - Total emissions.xlsx"), sheetName="Aggregate", append=TRUE)

xlsx::write.xlsx(as.data.frame(emissions.pcap), paste0(path.integ, "Result - Total emissions per cap.xlsx"), sheetName="Emissions by comp")
xlsx::write.xlsx(as.data.frame(emissions.pcap.sum), paste0(path.integ, "Result - Total emissions per cap.xlsx"), sheetName="Aggregate", append=TRUE)
