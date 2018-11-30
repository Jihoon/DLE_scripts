source("P:/ene.general/DecentLivingEnergy/Surveys/Scripts/00 Load required packages.R")
source("P:/ene.general/DecentLivingEnergy/Surveys/Scripts/01 Load generic helper functions.R")
source("P:/ene.general/DecentLivingEnergy/Surveys/Generic function to access database.R")
source("P:/ene.general/DecentLivingEnergy/Surveys/Scripts/Functions for building Oracle DB tables.R")

# Tables to add to the DLE_DB

# 1. Mapping between IND1 survey CODE between ICP_SEQ code 
a1 <- join(IND_WB, icp_ntnu, by="ICP_SEQ") %>% select( -COICOP1, -COICOP2, -NTNU_109)  # into the DB

# 2. ICP 164 category names (including fuel)
a2 <- data.frame(ICP_CODE=1:164, ICP_NAME = ICP_catnames) # into the DB

# 3. COICOp Intensity (MJ/USD) table 
load( file="./Saved tables/BRA_intensities_val_BRA.Rda")
load( file="./Saved tables/IND_intensities.Rda")
load( file="./Saved tables/ZAF_intensities.Rda")

library(pastecs)
colnames(IND_intensity) <- ICP_catnames
colnames(BRA_intensity) <- ICP_catnames

# In MJ/USD 2007
IND_int_summary <- stat.desc(IND_intensity) 
IND_int_summary <- t(IND_int_summary[c(9,13),]) %>% round(digits=2)  # Just mean and sd
a3 <- data.frame(COUNTRY="IND", YR=2007, ICP_CODE=1:164, IND_int_summary)

BRA_int_summary <- stat.desc(BRA_intensity) 
BRA_int_summary <- t(BRA_int_summary[c(9,13),]) %>% round(digits=2)  # Just mean and sd
a3 <- rbind(a3, data.frame(COUNTRY="BRA", YR=2007, ICP_CODE=1:164, BRA_int_summary))   # into the DB

ZAF_int_summary <- stat.desc(ZAF_intensity) 
ZAF_int_summary <- t(ZAF_int_summary[c(9,13),]) %>% round(digits=2)  # Just mean and sd
a3 <- rbind(a3, data.frame(COUNTRY="ZAF", YR=2007, ICP_CODE=1:164, ZAF_int_summary))   # into the DB

# Quick visual comparison of a3
View(a3 %>% select(-std.dev) %>% spread(key=COUNTRY, value=mean))
ggplot(a3 %>% filter(ICP_CODE >=152 & ICP_CODE <=164), aes(ICP_CODE, mean)) +   
  geom_bar(aes(fill = COUNTRY), position = "dodge", stat="identity")
ggplot(a3 %>% filter(ICP_CODE >=101 & ICP_CODE <=150), aes(ICP_CODE, mean)) +   
  geom_bar(aes(fill = COUNTRY), position = "dodge", stat="identity")

# 4. Fuel category harmonization to 13 types
a <- which(DLE_fuel_sector_Q !=0, arr.ind = T)
a4 <- data.frame(FUEL=DLE_fuel_types[a[,1],], ICP_CODE=151 + a[,2], ICP_NAME=ICP_catnames[151 + a[,2]])

# Write to DB!
writeDF2Oracle(a1, "IND1_MAP", primary.keys='CODE')
writeDF2Oracle(a2, "ICP_CAT", primary.keys='ICP_CODE')
writeDF2Oracle(a3, "PRI_E_INT", primary.keys=c('COUNTRY', 'YR', 'ICP_CODE'))
writeDF2Oracle(a4, "FUEL_CAT", primary.keys='FUEL')

# Set foreign keys
sql = "ALTER TABLE SCODE_FUEL
  ADD CONSTRAINT SCODE_FUEL_FK
  FOREIGN KEY (FUEL) REFERENCES FUEL_CAT (FUEL)"
for (svy in c("IND1", "BRA1", "ZAF1", "IDN1")) {
  tryCatch({dbSendUpdate(conn, gsub("SCODE", svy, sql))}, error=function(e){cat("SKIPPED ERROR :",conditionMessage(e), "\n")})
}

scode <- "IND1"  # "BRA0"
# Reference _FOOD and _OTHCON tables to the CAT_MAP table
sql = "ALTER TABLE SCODE_XX
  ADD CONSTRAINT SCODE_XX_CODE_FK
  FOREIGN KEY (CODE) REFERENCES SCODE_MAP (CODE)"
for (type in c("FOOD","OTHCON")) {
  tryCatch({dbSendUpdate(conn, gsub("SCODE", scode, gsub("XX", type, sql)))}, error=function(e){cat("SKIPPED ERROR :",conditionMessage(e), "\n")})
}

# Modify column name
sql = "ALTER TABLE IND1_MAP RENAME COLUMN ICP_SEQ TO ICP_CODE"
tryCatch({dbSendUpdate(conn, sql)}, error=function(e){cat("SKIPPED ERROR :",conditionMessage(e), "\n")})

sql = "INSERT INTO IND_FOOD_AVG_WT (ITEM,AVG_WT) VALUES ('Orange, mausami', 0.1)"
sql = "INSERT INTO IND_FOOD_AVG_WT (ITEM,AVG_WT) VALUES ('Lemon', 0.7)"
sql = "UPDATE IND_FOOD_AVG_WT SET AVG_WT=0.07 WHERE ITEM='Lemon'"
tryCatch({dbSendUpdate(conn, sql)}, error=function(e){cat("SKIPPED ERROR :",conditionMessage(e), "\n")})

# Going back to the original val_tot names
sql = "ALTER TABLE IND1_FOOD RENAME COLUMN VAL_TOT TO VAL_TOT_ADJ"
tryCatch({dbSendUpdate(conn, sql)}, error=function(e){cat("SKIPPED ERROR :",conditionMessage(e), "\n")})
sql = "ALTER TABLE IND1_FOOD RENAME COLUMN VAL_TOT_ORG TO VAL_TOT"
tryCatch({dbSendUpdate(conn, sql)}, error=function(e){cat("SKIPPED ERROR :",conditionMessage(e), "\n")})



### Writing another version of total primary energy intensity based on use block
load( file="./Saved tables/BRA_intensities_val_BRA.use.Rda")
load( file="./Saved tables/IND_intensities.use.Rda")
load( file="./Saved tables/ZAF_intensities.use.Rda")

library(pastecs)
colnames(IND_intensity.use) <- ICP_catnames
colnames(BRA_intensity.use) <- ICP_catnames
colnames(ZAF_intensity.use) <- ICP_catnames

# In MJ/USD 2007
IND_int_summary <- stat.desc(IND_intensity.use) 
IND_int_summary <- t(IND_int_summary[c(9,13),]) %>% round(digits=2)  # Just mean and sd
a3 <- data.frame(COUNTRY="IND", YR=2007, ICP_CODE=1:164, IND_int_summary)

BRA_int_summary <- stat.desc(BRA_intensity.use) 
BRA_int_summary <- t(BRA_int_summary[c(9,13),]) %>% round(digits=2)  # Just mean and sd
a3 <- rbind(a3, data.frame(COUNTRY="BRA", YR=2007, ICP_CODE=1:164, BRA_int_summary))   # into the DB

ZAF_int_summary <- stat.desc(ZAF_intensity.use) 
ZAF_int_summary <- t(ZAF_int_summary[c(9,13),]) %>% round(digits=2)  # Just mean and sd
a3 <- rbind(a3, data.frame(COUNTRY="ZAF", YR=2007, ICP_CODE=1:164, ZAF_int_summary))   # into the DB

# Quick visual comparison of a3
View(a3 %>% select(-std.dev) %>% spread(key=COUNTRY, value=mean))
ggplot(a3 %>% filter(ICP_CODE >=152 & ICP_CODE <=164), aes(ICP_CODE, mean)) +   
  geom_bar(aes(fill = COUNTRY), position = "dodge", stat="identity")
ggplot(a3 %>% filter(ICP_CODE >=41 & ICP_CODE <=80), aes(ICP_CODE, mean)) +   
  geom_bar(aes(fill = COUNTRY), position = "dodge", stat="identity")

writeDF2Oracle(a3, "PRI_E_INT_USE", primary.keys=c('COUNTRY', 'YR', 'ICP_CODE'))




#####################################
### FE intensities from EXIOBASE3 ###
#####################################

# These values are generated from DLE_integration_Init.R.
load(file="./Saved tables/BRA.tfei.icp.Rda")
load(file="./Saved tables/IND.tfei.icp.Rda")
load(file="./Saved tables/ZAF.tfei.icp.Rda")

load(file="./Saved tables/BRA.tnei.icp.Rda")
load(file="./Saved tables/IND.tnei.icp.Rda")
load(file="./Saved tables/ZAF.tnei.icp.Rda")

load(file="./Saved tables/BRA.tfei.icp.elec.Rda")
load(file="./Saved tables/IND.tfei.icp.elec.Rda")
load(file="./Saved tables/ZAF.tfei.icp.elec.Rda")

load(file="./Saved tables/BRA.tfei.icp.non.elec.Rda")
load(file="./Saved tables/IND.tfei.icp.non.elec.Rda")
load(file="./Saved tables/ZAF.tfei.icp.non.elec.Rda")

types <- c('.tfei.icp', '.tnei.icp', '.tfei.icp.elec', '.tfei.icp.non.elec')
types.long <- c('Embodied final total', 'Embodied primary total', 'Embodied final electric', 'Embodied final non-electric')
ctys <- c('IND', 'BRA', 'ZAF')

int.db.mat <- list()
for (i in types) {
  cty.int <- paste0(ctys, i)
    # int.mat[[j]] <- eval(parse(text=cty.int))
  # }
  int.mat = list(eval(parse(text=cty.int[1])), eval(parse(text=cty.int[2])), eval(parse(text=cty.int[3])))
  names(int.mat) <- ctys
  
  int.sum <- list()
  for (j in 1:length(ctys)) {
    colnames(int.mat[[j]]) <- ICP_catnames
    int_summary <- stat.desc(int.mat[[j]]) 
    int_summary <- t(int_summary[c(9,13),]) %>% round(digits=2)  # Just mean and sd
    int.sum[[j]] <- data.frame(COUNTRY=ctys[j], YR=IO.year, ICP_CODE=1:164, TYPE=types.long[which(types==i)], int_summary)
  }
  int.db.mat[[which(types==i)]] <- do.call("rbind", int.sum)
}

int.db <- do.call("rbind", int.db.mat)
writeDF2Oracle(int.db, "ENE_INT_EXIO3", primary.keys=c('COUNTRY', 'YR', 'ICP_CODE', 'TYPE'))
