library(RJDBC)
library(data.table)
library(dplyr)
library(tidyr)
library(XLConnect)
library(Surrogate)
library(ggplot2)


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




### France HH final consumption from LC ###
Mapping <- system.file("2011_FRHHBS_per_decile.xlsx", package = "XLConnect")
wb <- loadWorkbook("H:/MyDocuments/IO work/Uncertainty/2011_FRHHBS_per_decile.xlsx")

n_sector_coicop <- 109  # Num of COICOP sectors
n_col <- 11  # Num of COICOP sectors
fd_decile <- readWorksheet(wb, sheet="ValueDecile", header=T, forceConversion=T)
cpi_2007_fr <- 95.7	 # http://data.worldbank.org/indicator/FP.CPI.TOTL
cpi_2011_fr <- 102.1	
fd_decile[,2:12] <- fd_decile[,2:12]*cpi_2007_fr/cpi_2011_fr
names(fd_decile)

soc_transfer_ratio <- fd_decile[,14:24]
names(soc_transfer_ratio) <- names(fd_decile)[2:12]

fd_with_soctr <- fd_decile[,2:12] * (1+soc_transfer_ratio)  # Final demand including social transfer allocation
fd_with_soctr_flat <- diag((1+soc_transfer_ratio[,1])) %*% as.matrix(fd_decile[,2:12])    # Final demand including social transfer allocation

### EXIO country order ###

exio_ctys <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
               "FR", "GR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
               "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", 
               "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW", 
               "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")


### Read in CES-COICOP bridge ###

# Using India CES for French example for now
Mapping <- system.file("IND_CES-COICOP_mapping.xlsx", package = "XLConnect")
wb <- loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/IND_CES-COICOP_mapping.xlsx")

n_sector_coicop <- 109  # Num of COICOP sectors
n_row <- 347  # Num of CES items
bridge_CES_COICOP <- readWorksheet(wb, "Sheet2", header=F, startRow=2, endRow=1+n_row, 
                        startCol=3, endCol=2+n_sector_coicop, forceConversion=T)

CES_catnames <- readWorksheet(wb, "Sheet2", header=F, startRow=2, endRow=1+n_row, startCol=1, endCol=1)
COICOP_catnames1 <- readWorksheet(wb, "Sheet2", header=F, startRow=1, endRow=1, startCol=3, endCol=2+n_sector_coicop)

# Order rows of bridge mtx in the alphabatic order of CES item names
bridge_CES_COICOP <- bridge_CES_COICOP[order(CES_catnames),]


### Read in COICOP-EXIO bridge ###

Mapping <- system.file("COICOP3_EXIO_bridge.xlsx", package = "XLConnect")
wb <- loadWorkbook("H:/MyDocuments/IO work/Uncertainty/COICOP3_EXIO_bridge.xlsx")

bridge_COICOP_EXIO_q <- readWorksheet(wb, sheet="Qual_FR", header=F, startRow=3, endRow=2+n_sector_coicop, startCol=3, forceConversion=T)
n_sector_coicop <- 109

COICOP_catnames2 <- readWorksheet(wb, sheet="Qual_FR", header=F, startRow=3, endRow=2+n_sector_coicop, startCol=2, endCol=2)
EX_catnames <- readWorksheet(wb, sheet="Qual_FR", header=F, startRow=2, endRow=2, startCol=3)







