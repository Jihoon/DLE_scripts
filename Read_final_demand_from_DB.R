# readFinalDemandfromDB = function(iso3='IND') {
#   
#   # Query to get total expenditure by CES category of a country
#   query <- paste("SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
#                  (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_FOOD f
#                  JOIN ", iso3, "_HH hh ON f.ID = hh.ID)
#                  GROUP BY ITEM
#                  UNION ALL
#                  SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
#                  (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_OTHCON f
#                  JOIN ", iso3, "_HH hh ON f.ID = hh.ID)
#                  GROUP BY ITEM
#                  UNION ALL
#                  SELECT FUEL, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
#                  (SELECT f.FUEL, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_FUEL f
#                  JOIN ", iso3, "_HH hh ON f.ID = hh.ID)
#                  GROUP BY FUEL", sep="")
#   res <- dbSendQuery(conn, query)
#   
# #   # Adjust to 2007 Euro as in EXIO
# #   cpi_2010 <- 218.056 # http://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
# #   cpi_2007 <- 207.3
# #   # exr_2007 <- 1.3415  # http://www.wikinvest.com/stock/Infineon_Technologies_(IFX)/Annual_Average_Exchange_Rates_Dollar_Per_Euro
#   
#   fd_tot <- fetch(res, -1)
#   fd_tot <- fd_tot[order(fd_tot$ITEM),] # Order CES alphabetically
#   
#   # Final demand of a country by CES category
#   # fd_tot$FD_TOT <- fd_tot$FD_TOT*cpi_2007/cpi_2010/exr_2007  # Units are in USD-ppp 2010
#   
#   return(fd_tot)
# }
# 
# readFinalDemandfromDBbyDecile = function(iso3='IND1', decile=10) {
#   
#   # Query to get total expenditure by CES category of a country
#   query <- paste("SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
#                   (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_FOOD f
#                   JOIN (SELECT * FROM
#                     (SELECT ID, CONSUMPTION, WEIGHT, NTILE(10) OVER (ORDER BY CONSUMPTION ASC) as DECILE FROM ", iso3, "_HH)
#                     WHERE DECILE=", decile, ") hh ON f.ID = hh.ID)
#                   GROUP BY ITEM
#                   UNION ALL
#                   SELECT ITEM, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
#                   (SELECT f.ITEM, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_OTHCON f
#                   JOIN (SELECT * FROM
#                     (SELECT ID, CONSUMPTION, WEIGHT, NTILE(10) OVER (ORDER BY CONSUMPTION ASC) as DECILE FROM ", iso3, "_HH)
#                     WHERE DECILE=", decile, ") hh ON f.ID = hh.ID)
#                   GROUP BY ITEM
#                   UNION ALL
#                   SELECT FUEL, SUM(VAL_TOT*WEIGHT) FD_TOT FROM 
#                   (SELECT f.FUEL, f.VAL_TOT, f.ID, hh.WEIGHT FROM ", iso3, "_FUEL f
#                   JOIN (SELECT * FROM
#                     (SELECT ID, CONSUMPTION, WEIGHT, NTILE(10) OVER (ORDER BY CONSUMPTION ASC) as DECILE FROM ", iso3, "_HH)
#                     WHERE DECILE=", decile, ") hh ON f.ID = hh.ID)
#                   GROUP BY FUEL", sep="")
#   res <- dbSendQuery(conn, query)
#   
#   # Adjust to 2007 Euro as in EXIO
# #   cpi_2010 <- 218.056 # http://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
# #   cpi_2007 <- 207.3
# #   exr_2007 <- 1.3415  # http://www.wikinvest.com/stock/Infineon_Technologies_(IFX)/Annual_Average_Exchange_Rates_Dollar_Per_Euro
#   
#   fd_tot <- fetch(res, -1)
#   fd_tot <- fd_tot[order(fd_tot$ITEM),] # Order CES alphabetically
#   
#   # Final demand of a country by CES category
#   # fd_tot$FD_TOT <- fd_tot$FD_TOT*cpi_2007/cpi_2010/exr_2007  # Units are in USD-ppp 2010
#   
#   return(fd_tot)
# }



# The functions below overwrite those above.
# They are simplified by using Kevin's DB call.

readFinalDemandfromDB = function(svy='IND1') {
  xlcFreeMemory()
  Food <- selectDBdata(ID, ITEM, VAL_TOT, tables=c(paste0(svy, '_FOOD')))
  xlcFreeMemory()
  OthCon <- selectDBdata(ID, ITEM, VAL_TOT, tables=c(paste0(svy, '_OTHCON')))
  xlcFreeMemory()
  Fuel <- selectDBdata(ID, FUEL, VAL_TOT, QTY_TOT, UNIT, tables=c(paste0(svy, '_FUEL')))
  xlcFreeMemory()
  HHold <- selectDBdata(ID, WEIGHT, CONSUMPTION, tables=c(paste0(svy, '_HH')))
  xlcFreeMemory()
  
  Fuel <- Fuel %>% rename(item = fuel)
  a <- rbind.fill(Food, OthCon, Fuel) %>% left_join(HHold) %>% # filter(!is.na(val_tot)) %>% 
    mutate(fd_tot =val_tot*weight) %>% group_by(item) %>%
    summarise(fd_tot=sum(fd_tot, na.rm = TRUE))
  
  return(a)
}

readFinalDemandfromDBbyDecile = function(svy='IND1') {
  xlcFreeMemory()
  Food <- selectDBdata(ID, ITEM, VAL_TOT, tables=c(paste0(svy, '_FOOD')))
  xlcFreeMemory()
  OthCon <- selectDBdata(ID, ITEM, VAL_TOT, tables=c(paste0(svy, '_OTHCON')))
  xlcFreeMemory()
  Fuel <- selectDBdata(ID, FUEL, VAL_TOT, QTY_TOT, UNIT, tables=c(paste0(svy, '_FUEL')))
  xlcFreeMemory()
  HHold <- selectDBdata(ID, WEIGHT, CONSUMPTION, EXPENDITURE, tables=c(paste0(svy, '_HH')))
  xlcFreeMemory()
  
  Fuel <- Fuel %>% rename(item = fuel)
  print(sum(is.na(HHold$consumption)))
  HHold <- HHold %>% 
    mutate(consumption = replace(consumption, is.na(consumption), expenditure)) %>%
    mutate(decile = cut(consumption,
                       breaks = quantile(consumption, probs = seq(0, 1, 0.1), na.rm=TRUE),
                       labels=paste0("decile", 1:10), include.lowest = TRUE, ordered=TRUE)) 
    
  print(sum(is.na(HHold$consumption)))
  
  
  # HHold <- HHold %>% mutate(decile = cut(expenditure, 
  #                                        breaks = quantile(expenditure, probs = seq(0, 1, 0.1), na.rm=TRUE), 
  #                                        labels=paste0("decile", 1:10), include.lowest = TRUE, ordered=TRUE))
  a_dec <- rbind.fill(Food, OthCon, Fuel) %>% left_join(HHold) %>% # filter(!is.na(val_tot)) %>% 
    mutate(fd_tot = val_tot*weight) %>% group_by(decile, item) %>%
    summarise(fd_tot = sum(fd_tot, na.rm = TRUE)) %>% 
    spread(decile, fd_tot)
  
  a_tot <- rbind.fill(Food, OthCon, Fuel) %>% left_join(HHold) %>% # filter(!is.na(val_tot)) %>% 
    mutate(fd_tot =val_tot*weight) %>% group_by(item) %>%
    summarise(fd_tot=sum(fd_tot, na.rm = TRUE)) %>% 
    rename(total = fd_tot)
  
  return(left_join(a_tot, a_dec))
}

