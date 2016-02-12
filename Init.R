library(RJDBC)
library(data.table)
library(tidyr)
library(XLConnect)
library(Surrogate)
library(ggplot2)
library(stringr)
library(dplyr)
library(pastecs)
library(countrycode)
library(scatterplot3d)
library(rgl)
library(car)
library(shape)
library(openxlsx)
library(graphics)
library(fields)

setwd("H:/MyDocuments/IO work/DLE_scripts")

#################
### Constants ###
#################

### EXIO country order ###

exio_ctys <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
               "FR", "GR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
               "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", 
               "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW", 
               "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")



############################################################
### Read final demand vector from each country's CES DB  ###
############################################################


source("P:/ene.general/DecentLivingEnergy/Surveys/Scripts/01 Load generic helper functions.R")
source("Read_final_demand_from_DB.R")
# Create Oracle DB connection via RJDBC (Java)
drv = JDBC("oracle.jdbc.driver.OracleDriver","P:/ene.model/Rcodes/Common_files/ojdbc6.jar", identifier.quote="\"") 
conn = dbConnect(drv, "jdbc:oracle:thin:@gp3.iiasa.ac.at:1521:gp3", "hh_data", "hh_data")

# Read total FD for all population
IND_FD <- readFinalDemandfromDB('IND')
IDN_FD <- readFinalDemandfromDB('IDN')
BRA_FD <- readFinalDemandfromDB('BRA')
ZAF_FD <- readFinalDemandfromDB('ZAF')

# Read in by decile group
for (i in 1:10) {
  a <- readFinalDemandfromDBbyDecile('IND', i)
  names(a)[2] <- paste("Decile", i, sep="") 
  IND_FD <- merge(IND_FD, a, by="ITEM", all.x=TRUE)
  a <- readFinalDemandfromDBbyDecile('IDN', i)
  names(a)[2] <- paste("Decile", i, sep="") 
  IDN_FD <- merge(IDN_FD, a, by="ITEM", all.x=TRUE)
}
dbDisconnect(conn)



##############################################
### Read in final demand vector for France ### (Auxiliary, from Lucas)
##############################################

Mapping <- system.file("2011_FRHHBS_per_decile.xlsx", package = "XLConnect")
wb <- loadWorkbook("H:/MyDocuments/IO work/Uncertainty/2011_FRHHBS_per_decile.xlsx")

n_sector_coicop <- 109  # Num of COICOP sectors
n_col <- 11  # Num of columns (10 deciles + avg)
fd_decile <- readWorksheet(wb, sheet="ValueDecile", header=T, forceConversion=T)
cpi_2007_fr <- 95.7	 # http://data.worldbank.org/indicator/FP.CPI.TOTL
cpi_2011_fr <- 102.1	
fd_decile[,2:12] <- fd_decile[,2:12]*cpi_2007_fr/cpi_2011_fr
names(fd_decile)

soc_transfer_ratio <- fd_decile[,14:24]
names(soc_transfer_ratio) <- names(fd_decile)[2:12]

fd_with_soctr <- fd_decile[,2:12] * (1+soc_transfer_ratio)  # Final demand including social transfer allocation
# fd_with_soctr_flat <- diag((1+soc_transfer_ratio[,1])) %*% as.matrix(fd_decile[,2:12])    # Final demand including social transfer allocation
fd_with_soctr_flat <- fd_decile[,2:12] + soc_transfer_ratio[,1]*fd_decile[,2]   # Flat means identical $/person, I guess.



#####################################################
### Read in (CES-Pseudo COICOP) mappings from WB  ###
#####################################################

### Read in ICP heading number following NTNU 109 mapping (not 100%, some ICP headings are aggregated) ###
Mapping <- system.file("ICP_SEQ.xlsx", package = "XLConnect")
wb <- loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/Worldbank/ICP_SEQ.xls")
# I added 'Sheet2' and fixed some mis-categorizations for my needs.
icp_seq <- readWorksheet(wb, sheet="Sheet2", header=TRUE, startRow=2, startCol=1, endCol=1, forceConversion=T)
icp_cat <- readWorksheet(wb, sheet="Sheet2", header=FALSE, startRow=3, startCol=4, endCol=4, forceConversion=T)
NTNU <- readWorksheet(wb, sheet="Sheet2", header=TRUE, startRow=2, startCol=7, endCol=8, forceConversion=T)
icp_ntnu <-cbind(icp_seq, icp_cat, NTNU)
names(icp_ntnu)[2] <- "Subcategory"
names(icp_ntnu)[4] <- "ICP_Heading"

source("Process_WB.R")  # Read in the function 'processWBscript' and resulting mtxs for 4 countries

# Issue: I still need to match with our CES DB and final NTNU 109 classification
#        How to combine fuel consumption and other (food etc)



##################################
### Read in CES-COICOP mapping ### (Specifically for IND for now)
##################################

# Using India CES for French example for now 
# Update: Received FRA consumption in NTNT 109 classification, so FRA doesn't use this
# Issue: bridge_CES_COICOP may be replaced with the IND mtx from WB (after comparison)

Mapping <- system.file("IND_CES-COICOP_mapping.xlsx", package = "XLConnect")
wb <- loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/IND_CES-COICOP_mapping.xlsx")

n_sector_coicop <- 109  # Num of COICOP sectors
n_row <- 347  # Num of CES items
bridge_CES_COICOP <- readWorksheet(wb, "Sheet2", header=FALSE, startRow=2, endRow=1+n_row, 
                        startCol=3, endCol=2+n_sector_coicop, forceConversion=T)

CES_catnames <- readWorksheet(wb, "Sheet2", header=FALSE, startRow=2, endRow=1+n_row, startCol=1, endCol=1)
# COICOP_catnames1 <- readWorksheet(wb, "Sheet2", header=FALSE, startRow=2, endRow=1, startCol=3, endCol=2+n_sector_coicop)

# Order rows of bridge mtx in the alphabatic order of CES item names
bridge_CES_COICOP <- bridge_CES_COICOP[order(CES_catnames),]

source("Bridging_uncertainty.R")  # Read in the function 'get_bridge_COICOP_EXIO'



#########################################
### Read in COICOP-EXIO bridge (Qual) ###
#########################################

Mapping <- system.file("COICOP3_EXIO_bridge.xlsx", package = "XLConnect")
wb <- loadWorkbook("H:/MyDocuments/IO work/Uncertainty/COICOP3_EXIO_bridge.xlsx")

# Qualitative mapping (0 or 1)
bridge_COICOP_EXIO_q <- readWorksheet(wb, sheet="Qual_DK+FR", header=FALSE, 
                                      startRow=3, endRow=2+n_sector_coicop, startCol=3, forceConversion=T)

# Final COICOP classification with 109 headings
n_sector_coicop <- 109
COICOP_catnames2 <- readWorksheet(wb, sheet="Qual_DK+FR", header=FALSE, startRow=3, endRow=2+n_sector_coicop, startCol=2, endCol=2)
EX_catnames <- readWorksheet(wb, sheet="Qual_DK+FR", header=FALSE, startRow=2, endRow=2, startCol=3)

# Issue: This qual mapping may change depending on countries, which we need to tackle then.



##########################################
### Read in function 'get_basic_price' ###
##########################################

source("Valuation.R")



