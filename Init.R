library(RJDBC)
library(data.table)
library(tidyr)
library(XLConnect)
library(Surrogate)
library(ggplot2)
library(stringr)
library(dplyr)
library(pastecs)
library(pastecs)

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

# Create Oracle DB connection via RJDBC (Java)
drv = JDBC("oracle.jdbc.driver.OracleDriver","P:/ene.model/Rcodes/Common_files/ojdbc6.jar", identifier.quote="\"") 
conn = dbConnect(drv, "jdbc:oracle:thin:@gp3.iiasa.ac.at:1521:gp3", "hh_data", "hh_data")

# Query to get total expenditure by CES category of a country (India)
query <- paste("SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
               (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM IND1_FOOD f
               JOIN IND1_HH hh ON f.ID = hh.ID)
               GROUP BY ITEM
               UNION ALL
               SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
               (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM IND1_OTHCON f
               JOIN IND1_HH hh ON f.ID = hh.ID)
               GROUP BY ITEM
               UNION ALL
               SELECT FUEL, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
               (SELECT f.FUEL, f.VAL_TOT, f.ID, hh.WEIGHT FROM IND1_FUEL f
               JOIN IND1_HH hh ON f.ID = hh.ID)
               GROUP BY FUEL")
res <- dbSendQuery(conn, query)

# Adjust to 2007 Euro as in EXIO
cpi_2010 <- 218.056 # http://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
cpi_2007 <- 207.3
exr_2007 <- 1.3415  # http://www.wikinvest.com/stock/Infineon_Technologies_(IFX)/Annual_Average_Exchange_Rates_Dollar_Per_Euro

fd_tot <- fetch(res, -1)
fd_tot <- fd_tot[order(fd_tot$ITEM),] # Order CES alphabetically

# Final demand of a country by CES category
fd_tot$FD_TOT <- fd_tot$FD_TOT*cpi_2007/cpi_2010/exr_2007  # Units are in USD-ppp 2010

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

icp_seq <- readWorksheet(wb, sheet="Sheet1", header=T, startRow=2, startCol=1, endCol=1, forceConversion=T)
NTNU <- readWorksheet(wb, sheet="Sheet1", header=T, startRow=2, startCol=7, endCol=8, forceConversion=T)
icp_ntnu <-cbind(icp_seq, NTNU)
names(icp_ntnu)[3] <- "ICP_Heading"

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



