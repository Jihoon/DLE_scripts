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
