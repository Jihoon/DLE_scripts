#############################
### DLE Integration works ###
#############################

# Assume Init.R is already run.

### Define additinoal functions for this analysis
# source("DLE_integration_Functions.R")

# This is based on EXIO3 Final Energy extension.


### General prep ###
library(WDI)

PPP <- WDI(country = c("IN", "BR", "ZA"), indicator = c("PA.NUS.PPP"), start = 2011, end = 2011, extra = FALSE, cache = NULL)
PPP_IND <- as.numeric(PPP %>% filter(year==2011 & iso2c=='IN') %>% select(PA.NUS.PPP))
PPP_BRA <- as.numeric(PPP %>% filter(year==2011 & iso2c=='BR') %>% select(PA.NUS.PPP))
PPP_ZAF <- as.numeric(PPP %>% filter(year==2011 & iso2c=='ZA') %>% select(PA.NUS.PPP))
CPI_IND <- as.numeric(CPI %>% filter(year==2011 & iso2c=='IN') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==2007 & iso2c=='IN') %>% select(FP.CPI.TOTL))
CPI_BRA <- as.numeric(CPI %>% filter(year==2011 & iso2c=='BR') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==2007 & iso2c=='BR') %>% select(FP.CPI.TOTL))
CPI_ZAF <- as.numeric(CPI %>% filter(year==2011 & iso2c=='ZA') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==2007 & iso2c=='ZA') %>% select(FP.CPI.TOTL))


DLE.sectors <- c("Food", "Clothing", "Health.Edu", "Housing.BL", "Housing.OP", "Water.Sani", "Road", "Appliance", "Transport")
DLE.countries <- c('IND', 'BRA', 'ZAF')

# Returns time-series (now, 2030, 2050)
Year.end <- 2050
Year.base <- 2015
Year.obs <- c(Year.base, seq(2020, Year.end, 10))

# Then we need to overwrite L_inverse, tot_demand, final_demand, and the country list with EXIO3 data.
exio_ctys <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
               "FR", "GR", 
               "HR", # Added new at EXIO3
               "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
               "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", 
               "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW", 
               "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")

num.cty <- length(exio_ctys)
exio.len <- length(exio_ctys)*200
exio.fd.len <- length(exio_ctys)*7

# Get IND final demand from EXIO [M.EUR to M.USD]
IND_place <- which(exio_ctys=="IN")
IND_idx_fd <- seq(7*(IND_place-1)+1, 7*IND_place)   # 7 final demand columns per country
IND_idx_ex <- seq(200*(IND_place-1)+1, 200*IND_place)   # 200 EXIO comodities

# Get BRA final demand from EXIO [M.EUR to M.USD]
BRA_place <- which(exio_ctys=="BR")
BRA_idx_fd <- seq(7*(BRA_place-1)+1, 7*BRA_place)   # 7 final demand columns per country
BRA_idx_ex <- seq(200*(BRA_place-1)+1, 200*BRA_place)   # 7 final demand columns per country

# Get ZAF final demand from EXIO [M.EUR to M.USD]
ZAF_place <- which(exio_ctys=="ZA")
ZAF_idx_fd <- seq(7*(ZAF_place-1)+1, 7*ZAF_place)   # 7 final demand columns per country
ZAF_idx_ex <- seq(200*(ZAF_place-1)+1, 200*ZAF_place)   # 200 EXIO comodities



# Overwrite these with values from EXIO3
L_inverse <- read.csv(paste0(EXIO3_path, "L_2007.csv"), header = FALSE)
final_demand <- read.csv(paste0(EXIO3_path, "Y_2007.csv"), header = FALSE)
tot_output <- read.csv(paste0(EXIO3_path, "x_2007.csv"), header = FALSE)
tot_demand <- rowSums(tot_output) 

save(L_inverse, file="./Saved tables/L_inverse_EXIO3_2007.Rda")
# save(final_demand, file="./Saved tables/L_inverse_EXIO3_2007.Rda"))
# save(L_inverse, file="./Saved tables/L_inverse_EXIO3_2007.Rda"))
load(file="./Saved tables/L_inverse_EXIO3_2007.Rda") # L_inverse

### Derive intensities ###

# Read TFEI and DFEI from EXIO3 (tfei.exio / dfei.exio)
# source("import_EXIO_FE_extension.R")
# Already run at Init.R

# Derive ICP TFEI (or TPEI if necessary)
attach(BRA_val)
val_BR_BR <- construct_val_mtx(as.matrix(SupBP), as.matrix(TrdMrg), as.matrix(TrpMrg), as.matrix(TotTaxSub))
detach(BRA_val)

# Re-set val_mtx for further analysis
val_mtx <- list(val_FR, val_BR_BR, val_US, val_IN, val_ZA)
names(val_mtx) <- c('FR', 'BR', 'US', 'IN', 'ZA')

BRA_fd_exio_pp_BR <- get_purch_price(BRA_fd_exio, "BR")
scaler_BRA <- sum(BRA_FD_ICP_usd2007[,1]) / sum(BRA_fd_exio_pp_BR)
init_FD_BRA <- BRA_FD_ICP_usd2007[,1] / scaler_BRA

n_draw <- 100

list[BRA.tpei.use.icp, BRA.alloc.use, NC_BRA_val_BRA.use, BRA_FD_adj_val_BRA.use] <- 
  DeriveIntensities('BRA', 'primary', pri.intensity.mat=tpei.USE)
list[IND_intensity.use, IND_alloc.use, NC_IND.use, IND_FD_adj.use] <- 
  DeriveIntensities('IND', 'primary', pri.intensity.mat=tpei.USE)
list[ZAF_intensity.use, ZAF_alloc.use, NC_ZAF.use, ZAF_FD_adj.use] <- 
  DeriveIntensities('ZAF', 'primary', pri.intensity.mat=tpei.USE) 

list[BRA.tfei.icp, BRA.alloc, NC_BRA_val_BRA, BRA_FD_adj_val_BRA] <- DeriveIntensities('BRA', 'final', final.intensity.mat=tfei.exio)
list[IND.tfei.icp, IND_alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND', 'final', final.intensity.mat=tfei.exio)
list[ZAF.tfei.icp, ZAF_alloc, NC_ZAF, ZAF_FD_adj] <- DeriveIntensities('ZAF', 'final', final.intensity.mat=tfei.exio)

list[BRA.tfei.icp.elec, BRA.alloc, NC_BRA_val_BRA, BRA_FD_adj_val_BRA] <- DeriveIntensities('BRA', 'final', final.intensity.mat=tfei.elec)
list[IND.tfei.icp.elec, IND_alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND', 'final', final.intensity.mat=tfei.elec)
list[ZAF.tfei.icp.elec, ZAF_alloc, NC_ZAF, ZAF_FD_adj] <- DeriveIntensities('ZAF', 'final', final.intensity.mat=tfei.elec)

list[BRA.tfei.icp.non.elec, BRA.alloc, NC_BRA_val_BRA, BRA_FD_adj_val_BRA] <- DeriveIntensities('BRA', 'final', final.intensity.mat=tfei.non.elec)
list[IND.tfei.icp.non.elec, IND_alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND', 'final', final.intensity.mat=tfei.non.elec)
list[ZAF.tfei.icp.non.elec, ZAF_alloc, NC_ZAF, ZAF_FD_adj] <- DeriveIntensities('ZAF', 'final', final.intensity.mat=tfei.non.elec)

save(BRA.tfei.icp, file="./Saved tables/BRA.tfei.icp.Rda")
save(IND.tfei.icp, file="./Saved tables/IND.tfei.icp.Rda")
save(ZAF.tfei.icp, file="./Saved tables/ZAF.tfei.icp.Rda")

save(BRA.tfei.icp.elec, file="./Saved tables/BRA.tfei.icp.elec.Rda")
save(IND.tfei.icp.elec, file="./Saved tables/IND.tfei.icp.elec.Rda")
save(ZAF.tfei.icp.elec, file="./Saved tables/ZAF.tfei.icp.elec.Rda")

save(BRA.tfei.icp.non.elec, file="./Saved tables/BRA.tfei.icp.non.elec.Rda")
save(IND.tfei.icp.non.elec, file="./Saved tables/IND.tfei.icp.non.elec.Rda")
save(ZAF.tfei.icp.non.elec, file="./Saved tables/ZAF.tfei.icp.non.elec.Rda")






### Additional analysis for TFEI & Units for DLE IO components
source("DLE_integration_analysis_clothing.R")
source("DLE_integration_analysis_food.R")

idx.education.exio <- 174
idx.health.exio <- 175


### Set up the class structures for DLE integration data - Not used for GRC
# source("DLE_integration_data_structure.R")


TFEI.EXIO.ts <- sapply(DLE.countries, TFEI.ApplyKeyTechImprovement.EXIO, dfei=dfei.exio, simplify=FALSE, USE.NAMES = TRUE) # Returns a list
TFEI.EXIO.elec.ts <- sapply(DLE.countries, TFEI.ApplyKeyTechImprovement.EXIO, dfei=dfei.elec, simplify=FALSE, USE.NAMES = TRUE) # Returns a list


# Health/Education TFEI (long format)
# idx.HE <- 174:175
# a <- do.call("rbind", lapply(seq_along(TFEI.EXIO.ts), function(x, name, i) {
#   temp <- data.frame(t(x[[i]][idx.HE,-1])) 
#   names(temp) <- c("Education", "Health")
#   temp <- temp %>% mutate(Country=name[i], Year=names(x[[i]])[-1])
#   return(temp)
# }, name=names(TFEI.EXIO.ts), x=TFEI.EXIO.ts))


### Generate input tables
Scenarios <- c("DLE.ACCEL", "DLE.ACCEL.LCT", "DLE.ACCEL.LCT.BHV", "DLE.BAU")
result_path <- "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Analysis/Final results/"

# Health and Education
DLE.intensity.input.tot <- FormatDLEInputTable.EXIO(c("Education", "Health"), "Intensity", carr="total", 
                                                TFEI.EXIO.ts, c(idx.education.exio, idx.health.exio))
DLE.intensity.input.elec <- FormatDLEInputTable.EXIO(c("Education", "Health"), "Intensity", carr="elec", 
                                                     TFEI.EXIO.elec.ts, c(idx.education.exio, idx.health.exio))

DLE.intensity.input.non.elec <- data.frame(DLE.intensity.input.tot %>% select(Country:Carrier), 
                                           DLE.intensity.input.tot %>% select(-(Country:Carrier)) - DLE.intensity.input.elec %>% select(-(Country:Carrier))) %>%
  mutate(Carrier="non.elec")
DLE.intensity.input <- rbind(DLE.intensity.input.tot, DLE.intensity.input.elec, DLE.intensity.input.non.elec) 

# Clothing and Footwear
for (i in Scenarios) {
  # DLE.intensity.input <- AddComponentToDLETable("Clothing", tabl="Intensity", scenario=i, val.list=tfei.clothing, carr="total", table=DLE.intensity.input)
  # DLE.intensity.input <- AddComponentToDLETable("Footwear", tabl="Intensity", scenario=i, val.list=tfei.footwear, carr="total", table=DLE.intensity.input)
  # DLE.intensity.input <- AddComponentToDLETable("Clothing", tabl="Intensity", scenario=i, val.list=tfei.elec.clothing, carr="elec", table=DLE.intensity.input)
  # DLE.intensity.input <- AddComponentToDLETable("Footwear", tabl="Intensity", scenario=i, val.list=tfei.elec.footwear, carr="elec", table=DLE.intensity.input)
  # DLE.intensity.input <- AddComponentToDLETable("Clothing", tabl="Intensity", scenario=i, val.list=tfei.non.elec.clothing, carr="non.elec", table=DLE.intensity.input)
  # DLE.intensity.input <- AddComponentToDLETable("Footwear", tabl="Intensity", scenario=i, val.list=tfei.non.elec.footwear, carr="non.elec", table=DLE.intensity.input)
  # Combined clothing/footwear
  DLE.intensity.input <- AddComponentToDLETable("Clothing", tabl="Intensity", scenario=i, val.list=tfei.elec.clothing.all, carr="elec", table=DLE.intensity.input)
  DLE.intensity.input <- AddComponentToDLETable("Clothing", tabl="Intensity", scenario=i, val.list=tfei.non.elec.clothing.all, carr="non.elec", table=DLE.intensity.input)
}

### Modify health intensity based on GTAP (EXIO values too large.. Unknown reason)
library(tibble)
tpei.health.GTAP <- data.frame(IND=3.82, BRA=2.1, ZAF=5.1)  # MJ/USD
tpei.health.exio <- colSums(tpei.USE[,c(IND_idx_ex[idx.health.exio], BRA_idx_ex[idx.health.exio], ZAF_idx_ex[idx.health.exio])])
names(tpei.health.exio) <- names(tpei.health.GTAP)
ratio.GTAP <- tpei.health.GTAP / tpei.health.exio
ratio.GTAP <- data.frame(t(ratio.GTAP)) %>% rownames_to_column() 
names(ratio.GTAP) <- c("Country", "Ratio")

DLE.intensity.input <- DLE.intensity.input %>% left_join(ratio.GTAP) %>% mutate(Health=Health*Ratio) %>% select(-Ratio) 

# Food - total
DLE.intensity.input <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.base, scenario="DLE.BAU", carr="total", table=DLE.intensity.input)
DLE.intensity.input <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.accel, scenario="DLE.ACCEL", carr="total", table=DLE.intensity.input)
DLE.intensity.input <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.lctbhv, scenario="DLE.ACCEL.LCT.BHV", carr="total", table=DLE.intensity.input)
# Food - elec
DLE.intensity.input <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.elec.base, scenario="DLE.BAU", carr="elec", table=DLE.intensity.input)
DLE.intensity.input <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.elec.accel, scenario="DLE.ACCEL", carr="elec", table=DLE.intensity.input)
DLE.intensity.input <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.elec.lctbhv, scenario="DLE.ACCEL.LCT.BHV", carr="elec", table=DLE.intensity.input)
# Food - non.elec
DLE.intensity.input <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.non.elec.base, scenario="DLE.BAU", carr="non.elec", table=DLE.intensity.input)
DLE.intensity.input <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.non.elec.accel, scenario="DLE.ACCEL", carr="non.elec", table=DLE.intensity.input)
DLE.intensity.input <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.non.elec.lctbhv, scenario="DLE.ACCEL.LCT.BHV", carr="non.elec", table=DLE.intensity.input)

DLE.intensity.input <- FillEmptyCellsinDLEinputTable(DLE.intensity.input) %>% arrange(Scenario, Country, Year, Carrier) %>% filter(Carrier!="total")

# Merge to AM's intensity inputs
Intensity.table.AM <- read.csv(paste0(result_path, "All intensities by comp - carrier.csv")) %>% select(-X)
DLE.intensity.input <- Intensity.table.AM %>% left_join(DLE.intensity.input)

write.csv(DLE.intensity.input, paste0(result_path, "DLE intensity by comp.csv"))



# Update unit table with food, clothing, footwear 
# These values are still per capita per year.
DLE.use.input <- read.csv(paste0(result_path, "stock_timeseries.csv")) %>% select(-X)
names(DLE.use.input) <- sapply(names(DLE.use.input), firstup)
DLE.use.input <- DLE.use.input %>% 
  mutate(Country=gsub("Brazil", "BRA", Country)) %>%
  mutate(Country=gsub("India", "IND", Country)) %>%
  mutate(Country=gsub("S. Africa", "ZAF", Country)) 
  
# DLE.use.input <- AddComponentToDLETable("Clothing", tabl="Units", val.list=unit.clothing, scenario="DLE.BAU", type="Stock", table=DLE.use.input)
# DLE.use.input <- AddComponentToDLETable("Footwear", tabl="Units", val.list=unit.footwear, scenario="DLE.BAU", type="Stock", table=DLE.use.input)
DLE.use.input <- AddComponentToDLETable("Clothing", tabl="Units", val.list=unit.clothing.all, scenario="DLE.BAU", type="Stock", table=DLE.use.input)
DLE.use.input <- AddComponentToDLETable("Food", tabl="Units", val.list=unit.food.base, scenario="DLE.BAU", type="Stock", table=DLE.use.input)
DLE.use.input <- FillEmptyCellsinDLEinputTable(DLE.use.input, type="Units") %>% select(-Carrier)
write.csv(DLE.use.input %>% select(Clothing, Food), paste0(result_path, "stock_IO_sectors.csv"))
write.csv(DLE.use.input, paste0(result_path, "stock_timeseries_JM.csv"))


### Run_rIPFP just once (to save time) - Adopted from DeriveIntensities()
# This is for deriving the ICP intensities for DLE.ACCEL.LCT from TFEI.ApplyKeyTechImprovement
n_draw <- 10

final_alloc_list <- sapply(DLE.countries, Run_rIPFP, qual_map_init=bridge_ICP_EXIO_q[,-1], simplify=FALSE)
names(final_alloc_list) <- unlist(DLE.countries)
# TFEI.ICP.IND.TMAR <- TFEI.ApplyKeyTechImprovement("IND", DFEI.TMAR) #x7
# TFEI.ICP.BRA.TMAR <- TFEI.ApplyKeyTechImprovement("BRA", DFEI.TMAR) #x7
# TFEI.ICP.ZAF.TMAR <- TFEI.ApplyKeyTechImprovement("ZAF", DFEI.TMAR) #x7
TFEI.ICP.LCT <- lapply(DLE.countries, function(x) {TFEI.ApplyKeyTechImprovement(x, dfei.exio)})
TFEI.ICP.IND <- TFEI.ApplyKeyTechImprovement("IND", dfei.exio) %>% mutate(pct=(Y2015-Y2050)/Y2015*100)





### Food emissions incl. non-energy emissions
library("countrycode")
ssp.raw <- read.csv(paste0(result_path, "ssp_timeseries.csv")) %>% select(-X)
ssp.raw <- ssp.raw %>% rename(Region=country, Year=year) %>%
  mutate(Region=gsub("Brazil", "BRA", Region)) %>%
  mutate(Region=gsub("India", "IND", Region)) %>%
  mutate(Region=gsub("S. Africa", "ZAF", Region)) 

FAO.emissions <- read.csv(paste0(result_path, "FAOSTAT_data_5-8-2018.csv"), header = TRUE) %>% 
  filter(Item=="Agriculture total" & Element=="Emissions (CO2eq)") %>%
  group_by(Area, Year, Element) %>% rename(CO2eq.kton=Value) %>% 
  rename(Region=Area) %>% ungroup() %>% mutate(Region=countrycode(Region, 'country.name', 'iso3c')) 

FAO.emissions <- FAO.emissions %>% left_join(kcal.ssp2) %>% select(-SSP) %>%
  left_join(Popul %>% mutate(Region=countrycode(iso2c, 'iso2c', 'iso3c')) %>% rename(Year=year, Pop=SP.POP.TOTL) %>%
              select(Region, Year, Pop)) %>%
  filter(Year==2010) %>% select(Region, Year, CO2eq.kton, kcal.pday=Val, Pop) %>%
  mutate(emi.int=CO2eq.kton/Pop/kcal.pday/365*1e9) # g/kcal 

# Bring IND.food.tfei from "DLE_integration_analysis_food.R" 
# load(file="./Saved tables/IND.food.tfei.Rda")
Food.ef <- FAO.emissions %>% mutate(Scenario="DLE.BAU") %>%  # Assume identical intensity over time
  filter(Region!="IND") %>% select(Region, Scenario, emi.int) %>%
  left_join(ssp.raw %>% select(Region, Year)) %>%
  select(Scenario, Country=Region, Year, Food=emi.int)
Food.ef <- Food.ef %>%  # Add IND.seq.lctbhv.em & IND.seq.devmin.em
  rbind(IND.intensity.em) %>%
  mutate(Type="OP") %>% 
  select(Scenario, Country, Type, Year, Food) %>% # Disregard year (constant over period)
  mutate(Food=Food/1e6) #ton/kcal (since AM has ton/m2 for housing)
# Read in AM's EF
EF.AM <- read.csv(paste0(result_path, "Emissions by comp.csv")) %>% select(-X) %>%
  left_join(Food.ef)
EF.AM <- FillEmptyCellsinDLEinputTable(EF.AM, type="Units") 

# This csv is used in 'DLE_input_merge.R' later.
write.csv(EF.AM, paste0(result_path, "Emissions by comp w.food.csv"))



### Uncertainty band plot
tfei.range <- read_xlsx(paste0(result_path, "LCA_Analysis/uncertainty analysis.xlsx"), range="B1:O13") %>% select(-Country, -Year, -Clothing.BRA, -Clothing.ZAF)
comp.name <- c(
  Housing.U = "Housing.U [MJ/m2]",
  Housing.R = "Housing.R [MJ/m2]",
  HousingAC.U = "SpaceCond.U [MJ/m2]",
  HousingAC.R = "SpaceCond.R [MJ/m2]",
  Water.Access = "Water.Access [MJ/l]",
  Clothing = "Clothing [MJ/kg]",
  Food = "Food [MJ/kcal]"#,
  # Road = "Road [MJ/km]",
  # Mobility = "Mobility [MJ/p-km]"
)
tfei.range <- tfei.range %>% gather(key=Component, value=TFEI, -c(Type:Value)) %>% 
  mutate(TFEI=as.numeric(TFEI)) %>% spread(Value, TFEI) %>%
  mutate(Component=factor(Component, 
                          levels=c("Housing.R","Housing.U","HousingAC.R","HousingAC.U","Water.Access",
                                   # "Road","Mobility",
                                   "Clothing","Food")))
ggplot(tfei.range, aes(x=Type, y=Avg, ymin=High, ymax=Low, fill=Carrier)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(colour="black", width=.1, position = position_dodge(width=0.9)) +
  facet_wrap(~Component, ncol=4, scales = "free_y", labeller = labeller(Component=comp.name)) + 
  ylab("Final energy intensity")
