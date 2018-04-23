source(Load_init_data.R)

### Deriving DLE threshold for Clothing (IND)
datapath <- "C:/Users/min/IIASA/DLE - Documents/WS3 - Documents/Trajectories/Clothing/"
weight.clothing <- read_xlsx(paste0(datapath, "Weights-IND.xlsx"), range="A1:J31") %>% select(CODE, `g/each - MIN`, `g/each - MAX`) %>% 
  rename(weight.min=`g/each - MIN`, weight.max=`g/each - MAX`, code=CODE)

# Load IND_OTH_Alldata
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_OTH_ALLdata.Rda")
IND_clothing <- IND_OTH_Alldata %>% filter(code >=350 & code <=376)
IND_footwear <- IND_OTH_Alldata %>% filter(code >=390 & code <=395)

# Load ZAF_OTH.raw
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/ZAF_OTH_All.Rda")
ZAF_clothing <- ZAF_OTH.raw %>% mutate(code=as.numeric(code)) %>% filter(code >=3111100 & code <=3141200)
ZAF_footwear <- ZAF_OTH.raw %>% mutate(code=as.numeric(code)) %>% filter(code >=3211001 & code <=3213500)
 
# 
BRA_OTH_ALLdata <- selectDBdata(tables='BRA0_OTHCON') %>% left_join(data.frame(code=1:164, item=ICP_catnames))
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_HH.Rda")
BRA_clothing <- BRA_OTH_ALLdata %>% filter(code >=48 & code <=52) 
BRA_footwear <- BRA_OTH_ALLdata %>% filter(code >=53 & code <=55) 
# %>% left_join(BRA_HH, by = c("id" = "hhid"))

### India
# No quantity for some items (code=372, 356, 357, 368, 375) -> Assume same avg price as 'shirts, T-shirts' (for decile1) (code=363)
price.shirt <- as.numeric(IND_clothing %>% filter(code==363) %>% mutate(price=val_tot/qty_tot) %>% 
                            left_join(IND_HH,  by = c("id" = "hhid")) %>% filter(decile=="decile1") %>%
                            summarise(price.shirt=weighted.mean(price, weight, na.rm=T)))

# Get total weight by hh in gram (and val_tot too)
clothing.all <- IND_clothing %>% inner_join(weight.clothing) %>% 
  mutate_cond(code %in% c(372, 356, 357, 368, 375), qty_tot=val_tot/price.shirt) %>% # fill in qty for no-price items
  mutate(weight.tot=qty_tot*weight.min, weight.tot.max=qty_tot*weight.max) %>% 
  mutate_cond(code==374, weight.tot=qty_tot) %>% # knitting wool
  group_by(id) %>% summarise(val_tot=sum(val_tot, na.rm=TRUE), weight.tot=sum(weight.tot, na.rm=TRUE), weight.tot.max=sum(weight.tot.max, na.rm=TRUE)) %>% 
  left_join(IND_HH, by = c("id" = "hhid")) 

# Get total weight by hh in gram (and val_tot too)
footwear.all <- IND_footwear %>% inner_join(weight.clothing) %>% 
  mutate(weight.tot=qty_tot*weight.min, weight.tot.max=qty_tot*weight.max) %>% 
  group_by(id) %>% summarise(val_tot=sum(val_tot, na.rm=TRUE), weight.tot=sum(weight.tot, na.rm=TRUE), weight.tot.max=sum(weight.tot.max, na.rm=TRUE)) %>% 
  left_join(IND_HH, by = c("id" = "hhid")) 


# DLE threshold [kg/cap] (g to kg) - only Flow
dle.clothing.pcap.IND <- clothing.all %>% summarise(median = weighted.median(weight.tot/hh_size, weight, na.rm=TRUE)/1000) %>% as.numeric() # 1.3 kg/cap
dle.footwear.pcap.IND <- footwear.all %>% summarise(median = weighted.median(weight.tot/hh_size, weight, na.rm=TRUE)/1000) %>% as.numeric()  # 0.9 kg/cap
avg.clothing.pcap.IND <- clothing.all %>% summarise(mean = weighted.mean(weight.tot/hh_size, weight, na.rm=TRUE)/1000) %>% as.numeric() # 1.3 kg/cap
avg.footwear.pcap.IND <- footwear.all %>% summarise(mean = weighted.mean(weight.tot/hh_size, weight, na.rm=TRUE)/1000) %>% as.numeric()  # 0.9 kg/cap
# Mean expenditure $ - IND
exp.clothing.pcap.IND <- clothing.all %>% summarise(median = weighted.mean(val_tot/hh_size, weight, na.rm=TRUE)) %>% as.numeric() # 1.3 kg/cap
exp.footwear.pcap.IND <- footwear.all %>% summarise(median = weighted.mean(val_tot/hh_size, weight, na.rm=TRUE)) %>% as.numeric()  # 0.9 kg/cap

kg.pcap.dec <- clothing.all %>% group_by(decile) %>% summarise(clothing.per.cap = sum(weight.tot*weight, na.rm=TRUE) / sum(hh_size*weight, na.rm=TRUE) / 1000) 
kg.pcap.dec.max <- clothing.all %>% group_by(decile) %>% summarise(clothing.per.cap = sum(weight.tot.max*weight, na.rm=TRUE) / sum(hh_size*weight, na.rm=TRUE) / 1000) 

# Average price [$/kg] 2010USD ppp nationwide - Assume this ppp is same in all three countries
avg.price.clothing <- as.numeric(clothing.all %>% 
                                   summarise(avg.price = sum(val_tot*weight, na.rm=TRUE) / sum(weight.tot*weight, na.rm=TRUE) * 1000))
avg.price.footwear <- as.numeric(footwear.all %>% 
                                   summarise(avg.price = sum(val_tot*weight, na.rm=TRUE) / sum(weight.tot*weight, na.rm=TRUE) * 1000))



### ZAF
ZAF_clothing.all <- ZAF_clothing %>% left_join(ZAF_HH, by = c("id" = "hhid"))
ZAF_footwear.all <- ZAF_footwear %>% left_join(ZAF_HH, by = c("id" = "hhid"))

dle.clothing.pcap.ZAF <- ZAF_clothing.all %>%
  summarise(val_median=weighted.median(val_tot/hh_size, weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_median/avg.price.clothing) %>% 
  select(kg.pcap) %>% as.numeric()
dle.footwear.pcap.ZAF <- ZAF_footwear.all %>%
  summarise(val_median=weighted.median(val_tot/hh_size, weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_median/avg.price.clothing) %>% 
  select(kg.pcap) %>% as.numeric()

avg.clothing.pcap.ZAF <- ZAF_clothing.all %>%
  summarise(val_tot=sum(val_tot*weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_tot/ZAF_pop_2007/avg.price.clothing) %>%
  select(kg.pcap) %>% as.numeric()
avg.footwear.pcap.ZAF <- ZAF_footwear.all %>%
  summarise(val_tot=sum(val_tot*weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_tot/ZAF_pop_2007/avg.price.clothing) %>%
  select(kg.pcap) %>% as.numeric()

# Mean expenditure $ - ZAF
exp.clothing.pcap.ZAF <- ZAF_clothing.all %>%
  summarise(val_mean=weighted.mean(val_tot/hh_size, weight, na.rm=TRUE))
exp.footwear.pcap.ZAF <- ZAF_footwear.all %>%
  summarise(val_mean=weighted.mean(val_tot/hh_size, weight, na.rm=TRUE))

### BRA
BRA_clothing.all <- BRA_clothing %>% left_join(BRA_HH, by = c("id" = "hhid"))
BRA_footwear.all <- BRA_footwear %>% left_join(BRA_HH, by = c("id" = "hhid"))

dle.clothing.pcap.BRA <- BRA_clothing.all %>%
  summarise(val_median=weighted.median(val_tot/hh_size, weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_median/avg.price.clothing) %>% 
  select(kg.pcap) %>% as.numeric()
dle.footwear.pcap.BRA <- BRA_footwear.all %>%
  summarise(val_median=weighted.median(val_tot/hh_size, weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_median/avg.price.clothing) %>% 
  select(kg.pcap) %>% as.numeric()

avg.clothing.pcap.BRA <- BRA_clothing %>% left_join(BRA_HH, by = c("id" = "hhid")) %>%
  summarise(val_tot=sum(val_tot*weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_tot/BRA_pop_2007/avg.price.clothing) %>%
  select(kg.pcap) %>% as.numeric()
avg.footwear.pcap.BRA <- BRA_footwear %>% left_join(BRA_HH, by = c("id" = "hhid")) %>%
  summarise(val_tot=sum(val_tot*weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_tot/BRA_pop_2007/avg.price.clothing) %>%
  select(kg.pcap) %>% as.numeric()

# Mean expenditure $ - BRA
exp.clothing.pcap.BRA <- BRA_clothing.all %>%
  summarise(val_mean=weighted.mean(val_tot/hh_size, weight, na.rm=TRUE))
exp.footwear.pcap.BRA <- BRA_footwear.all %>%
  summarise(val_mean=weighted.mean(val_tot/hh_size, weight, na.rm=TRUE))


clothing.lifetime <- 3 # https://eastonstewartsvilledrycleaner36.wordpress.com/2010/11/30/the-average-life-expectancy-of-clothing/
footwear.lifetime <- 3






### General prep
DLE.sectors <- c("Food", "Clothing", "Health.Edu", "Housing.BL", "Housing.OP", "Water.Sani", "Road", "Appliance", "Transport")

# Returns time-series (now, 2030, 2050)
Year.end <- 2050
Year.base <- 2015
Year.obs <- c(Year.base, seq(2020, Year.end, 10))

df.dummy <- data.frame(year=Year.obs, setNames(data.frame(matrix(ncol = length(DLE.sectors), nrow = length(Year.obs))), DLE.sectors))
df.unit <- list(TFEI=df.dummy, use.pcap=df.dummy, E.tot=df.dummy)
df.cty <- list(sc.GDP=df.unit, sc.DLE.base=df.unit, sc.DLE.LCT=df.unit, sc.DLE.LCTBHV=df.unit, sc.BAU=df.unit)
DLE.scenario <- list(IND=df.cty, BRA=df.cty, ZAF=df.cty)
DLE.param <- list(IND=list(pop=Year.obs, gdp=Year.obs), 
                  BRA=list(pop=Year.obs, gdp=Year.obs), 
                  ZAF=list(pop=Year.obs, gdp=Year.obs)) 

DLE.component <- setRefClass("DLE.component", 
            fields=list(TFEI.OP="data.frame", # Can be long or wide. may have elec/non-elec breakdown
                        TFEI.CON="data.frame",
                        # TFEI.OP="data.frame",
                        # TFEI.OP="data.frame",
                        # TFEI.OP="data.frame",
                        units="data.frame", 
                        FE.tot="numeric"),
            methods = list(
              getFE.tot = function() {
                FE.tot <<- (TFEI.OP+TFEI.CON) * units
              },
              populate = function(rollout, intensity) {
                units <<- rollout
                TFEI.OP <<- intensity %>% filter(OpCon=="OP") %>% select(-OpCon)
                TFEI.CON <<- intensity %>% filter(OpCon=="CON") %>% select(-OpCon)
              }
            ))
DLE.scenario <- setRefClass("DLE.scenario", 
            fields=list(Housing="DLE.component", # Can be long or wide 
                        Clothing="DLE.component",
                        Food="DLE.component",
                        Health="DLE.component",
                        Education="DLE.component",
                        Water="DLE.component",
                        Sanitation="DLE.component",
                        Cooking="DLE.component",
                        TV="DLE.component",
                        Fridge="DLE.component",
                        AC="DLE.component",
                        Cellphone="DLE.component",
                        Mobility="DLE.component", # Will be car/bus/train/etc later
                        # Aggregate sectors
                        Shelter="numeric",
                        Moility.all="numeric",
                        Appliance="numeric",
                        Food="numeric"
                        # More possibly
                        ),
            methods = list(
              aggregateComps = function() {
                Shelter <<- Housing$FE.tot + Clothing$FE.tot
                Appliance <<- Cooking$FE.tot + TV$FE.tot + Fridge$FE.tot + AC$FE.tot
              },
              populateScenario = function(rollout, intensity) {
                Housing$populate(rollout %>% select(Year, Housing), intensity %>% select(Year, Housing, OpCon, Carrier))
                Water$populate(rollout %>% select(Year, Water), intensity %>% select(Year, Water, OpCon, Carrier))
                Sanitation$populate(rollout %>% select(Year, Sanitation), intensity %>% select(Year, Sanitation, OpCon, Carrier))
                Cooking$populate(rollout %>% select(Year, Cooking), intensity %>% select(Year, Cooking, OpCon, Carrier))
                TV$populate(rollout %>% select(Year, TV), intensity %>% select(Year, TV, OpCon, Carrier))
                Fridge$populate(rollout %>% select(Year, Fridge), intensity %>% select(Year, Fridge, OpCon, Carrier))
                AC$populate(rollout %>% select(Year, AC), intensity %>% select(Year, AC, OpCon, Carrier))
                Cellphone$populate(rollout %>% select(Year, Cellphone), intensity %>% select(Year, Cellphone, OpCon, Carrier))
                Mobility$populate(rollout %>% select(Year, Mobility), intensity %>% select(Year, Mobility, OpCon, Carrier))
              }
            ))
DLE.country <- setRefClass("DLE.country",
            fields=list(Accel="DLE.scenario",
                        Accel.LCT="DLE.scenario",
                        Accel.LCT.BHV="DLE.scenario",
                        BAU="DLE.scenario",
                        GDP="numeric",
                        rollout="data.frame",
                        intensity="data.frame"),
            methods = list(
              populateData = function() {
                Accel$populateScenario(extractScenario("Accel", rollout), extractScenario("Accel", intensity))
                Accel.LCT$populateScenario(extractScenario("Accel.LCT", rollout), extractScenario("Accel.LCT", intensity))
                Accel.LCT.BHV$populateScenario(extractScenario("Accel.LCT.BHV", rollout), extractScenario("Accel.LCT.BHV", intensity))
                BAU$populateScenario(extractScenario("BAU", rollout), extractScenario("BAU", intensity))
              }
              
            ))
# Assume two table inputs: Rollout.mat, TFEI.mat
DLE.sc <- list(IND=DLE.country(rollout=Rollout.mat %>% filter(country=="IND"), intensity=TFEI.mat %>% filter(country=="IND")), 
               BRA=DLE.country(rollout=Rollout.mat %>% filter(country=="BRA"), intensity=TFEI.mat %>% filter(country=="BRA")),  
               # BRA=DLE.country(rollout=Rollout.mat %>% filter(country=="BRA"), intensity=TFEI.mat %>% filter(country=="BRA")),
               ZAF=DLE.country(rollout=Rollout.mat %>% filter(country=="ZAF"), intensity=TFEI.mat %>% filter(country=="ZAF")))

extractScenario = function(df, field.name, match) {
  return(df[df[[field.name]] == match,])
}


# Clothing
# list.select(DLE.scenario$IND, use.pcap$Clothing)
# list.update(DLE.scenario$IND, use.pcap$Clothing=1:5)
# Function trial
updateDLEscenario.use <- function(scenarios="All", sect=DLE.sectors, val) {
  lapply(DLE.scenario, function(cty) {
    if(scenarios=="All") lapply(cty, function(sc) {sc$use.pcap[, match(sect, names(sc$use.pcap))] <- val})
    else {
      match(scenarios, names(cty))
    }
  })
}





### Manually populating DLE.scenario
# Clothing
# Only the flow matters for trajectories
DLE.scenario$IND$sc.BAU$E.tot$Clothing <- mean(tot.clothing.IND) * dle.clothing.pcap.IND / avg.clothing.pcap.IND +
  mean(tot.clothing.IND) * dle.clothing.pcap.IND / avg.clothing.pcap.IND
DLE.scenario$BRA$sc.BAU$E.tot$Clothing <- mean(tot.clothing.BRA) * dle.clothing.pcap.BRA / avg.clothing.pcap.BRA +
  mean(tot.footwear.BRA) * dle.footwear.pcap.BRA / avg.footwear.pcap.BRA
DLE.scenario$ZAF$sc.BAU$E.tot$Clothing <- mean(tot.clothing.ZAF) * dle.clothing.pcap.ZAF / avg.clothing.pcap.ZAF +
  mean(tot.footwear.ZAF) * dle.footwear.pcap.ZAF / avg.footwear.pcap.ZAF

# Food (TFEI in EJ/cap)
DLE.scenario$IND$sc.BAU$TFEI$Food <- tot.ENE.food.IND / IND_pop_2007
DLE.scenario$BRA$sc.BAU$TFEI$Food <- tot.ENE.food.BRA / BRA_pop_2007
DLE.scenario$ZAF$sc.BAU$TFEI$Food <- tot.ENE.food.ZAF / ZAF_pop_2007

DLE.scenario$IND$sc.DLE.LCTBHV$TFEI$Food <-  / IND_pop_2007
DLE.scenario$BRA$sc.DLE.LCTBHV$TFEI$Food <-  / BRA_pop_2007
DLE.scenario$ZAF$sc.DLE.LCTBHV$TFEI$Food <-  / ZAF_pop_2007




geoSeq <- function(base, r, n) {
  return(base * (1+r)^(1:n))
}

