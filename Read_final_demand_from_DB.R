readFinalDemandfromDB = function(iso3='IND') {
  
  # Query to get total expenditure by CES category of a country
  query <- paste("SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
                 (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_FOOD f
                 JOIN ", iso3, "_HH hh ON f.ID = hh.ID)
                 GROUP BY ITEM
                 UNION ALL
                 SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
                 (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_OTHCON f
                 JOIN ", iso3, "_HH hh ON f.ID = hh.ID)
                 GROUP BY ITEM
                 UNION ALL
                 SELECT FUEL, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
                 (SELECT f.FUEL, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_FUEL f
                 JOIN ", iso3, "_HH hh ON f.ID = hh.ID)
                 GROUP BY FUEL", sep="")
  res <- dbSendQuery(conn, query)
  
#   # Adjust to 2007 Euro as in EXIO
#   cpi_2010 <- 218.056 # http://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
#   cpi_2007 <- 207.3
#   # exr_2007 <- 1.3415  # http://www.wikinvest.com/stock/Infineon_Technologies_(IFX)/Annual_Average_Exchange_Rates_Dollar_Per_Euro
  
  fd_tot <- fetch(res, -1)
  fd_tot <- fd_tot[order(fd_tot$ITEM),] # Order CES alphabetically
  
  # Final demand of a country by CES category
  # fd_tot$FD_TOT <- fd_tot$FD_TOT*cpi_2007/cpi_2010/exr_2007  # Units are in USD-ppp 2010
  
  return(fd_tot)
}

readFinalDemandfromDBbyDecile = function(iso3='IND1', decile=10) {
  
  # Query to get total expenditure by CES category of a country
  query <- paste("SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
                  (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_FOOD f
                  JOIN (SELECT * FROM
                    (SELECT ID, CONSUMPTION, WEIGHT, NTILE(10) OVER (ORDER BY CONSUMPTION ASC) as DECILE FROM ", iso3, "_HH)
                    WHERE DECILE=", decile, ") hh 
                    ON f.ID = hh.ID)
                  GROUP BY ITEM
                  UNION ALL
                  SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
                  (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_OTHCON f
                  JOIN (SELECT * FROM
                    (SELECT ID, CONSUMPTION, WEIGHT, NTILE(10) OVER (ORDER BY CONSUMPTION ASC) as DECILE FROM ", iso3, "_HH)
                    WHERE DECILE=", decile, ") hh 
                  ON f.ID = hh.ID)
                  GROUP BY ITEM
                  UNION ALL
                  SELECT FUEL, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
                  (SELECT f.FUEL, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_FUEL f
                  JOIN (SELECT * FROM
                    (SELECT ID, CONSUMPTION, WEIGHT, NTILE(10) OVER (ORDER BY CONSUMPTION ASC) as DECILE FROM ", iso3, "_HH)
                    WHERE DECILE=", decile, ") hh 
                    ON f.ID = hh.ID)
                  GROUP BY FUEL", sep="")
  res <- dbSendQuery(conn, query)
  
  # Adjust to 2007 Euro as in EXIO
#   cpi_2010 <- 218.056 # http://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
#   cpi_2007 <- 207.3
#   exr_2007 <- 1.3415  # http://www.wikinvest.com/stock/Infineon_Technologies_(IFX)/Annual_Average_Exchange_Rates_Dollar_Per_Euro
  
  fd_tot <- fetch(res, -1)
  fd_tot <- fd_tot[order(fd_tot$ITEM),] # Order CES alphabetically
  
  # Final demand of a country by CES category
  # fd_tot$FD_TOT <- fd_tot$FD_TOT*cpi_2007/cpi_2010/exr_2007  # Units are in USD-ppp 2010
  
  return(fd_tot)
}