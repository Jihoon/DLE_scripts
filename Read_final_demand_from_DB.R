readFinalDemandfromDB = function(iso3='IND') {
  
# Query to get total expenditure by CES category of a country
query <- paste("SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
               (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "1_FOOD f
               JOIN ", iso3, "1_HH hh ON f.ID = hh.ID)
               GROUP BY ITEM
               UNION ALL
               SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
               (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "1_OTHCON f
               JOIN ", iso3, "1_HH hh ON f.ID = hh.ID)
               GROUP BY ITEM
               UNION ALL
               SELECT FUEL, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
               (SELECT f.FUEL, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "1_FUEL f
               JOIN ", iso3, "1_HH hh ON f.ID = hh.ID)
               GROUP BY FUEL", sep="")
res <- dbSendQuery(conn, query)

# Adjust to 2007 Euro as in EXIO
cpi_2010 <- 218.056 # http://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
cpi_2007 <- 207.3
exr_2007 <- 1.3415  # http://www.wikinvest.com/stock/Infineon_Technologies_(IFX)/Annual_Average_Exchange_Rates_Dollar_Per_Euro

fd_tot <- fetch(res, -1)
fd_tot <- fd_tot[order(fd_tot$ITEM),] # Order CES alphabetically

# Final demand of a country by CES category
fd_tot$FD_TOT <- fd_tot$FD_TOT*cpi_2007/cpi_2010/exr_2007  # Units are in USD-ppp 2010

return(fd_tot)
}