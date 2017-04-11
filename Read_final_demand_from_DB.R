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
  
  Fuel_summary <- Fuel %>% left_join(HHold) %>% # filter(!is.na(val_tot)) %>% 
    mutate(fd_tot =val_tot*weight) %>% group_by(fuel) %>%
    summarise(fd_tot=sum(fd_tot, na.rm = TRUE)) %>% arrange(fuel) %>% 
    right_join(DLE_fuel_types) %>% rename(item = fuel)
  Fuel_summary[is.na(Fuel_summary)] <- 0
  
  Fuel_summary <- data.frame(item=rownames(bridge_fuel_EXIO_q), 
                             fd_tot=t(DLE_fuel_sector_Q) %*% as.matrix(Fuel_summary[,2]))
  
  a <- rbind.fill(Food, OthCon) %>% left_join(HHold) %>% # filter(!is.na(val_tot)) %>% 
    mutate(fd_tot =val_tot*weight) %>% group_by(item) %>%
    summarise(fd_tot=sum(fd_tot, na.rm = TRUE)) %>% rbind(Fuel_summary)
  
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
  
  if (grepl("BRA", svy)) {
    HHold <- selectDBdata(ID, WEIGHT, INCOME, CONSUMPTION, HH_SIZE, EXPENDITURE, tables=c(paste0(svy, '_HH')))
    # income_proxy <- income
  }
  else {
    HHold <- selectDBdata(ID, WEIGHT, CONSUMPTION, HH_SIZE, EXPENDITURE, tables=c(paste0(svy, '_HH')))
    HHold$income <- HHold$consumption
  }
  xlcFreeMemory()
  
  print(sum(is.na(HHold$income)))
  HHold <- HHold %>% 
    arrange(income/hh_size) %>%
    mutate(cumpop = cumsum(hh_size*weight)/sum(HHold$weight*HHold$hh_size)) %>%
    mutate(decile = cut(cumpop, breaks = seq(0, 1, 0.1),
                        labels=paste0("decile", 1:10), include.lowest = TRUE, ordered=TRUE))  %>%
    filter(!is.na(income))
  
  Fuel_summary <- Fuel %>% left_join(HHold) %>% # filter(!is.na(val_tot)) %>% 
    mutate(fd_tot =val_tot*weight) %>% group_by(decile, fuel) %>%
    summarise(fd_tot=sum(fd_tot, na.rm = TRUE)) %>% arrange(fuel) %>% 
    right_join(DLE_fuel_types) %>% rename(item = fuel) %>%
    spread(decile, fd_tot) %>% mutate(total = rowSums(.[2:11], na.rm = TRUE)) %>%
    select(item, total, decile1:decile10)
  Fuel_summary[is.na(Fuel_summary)] <- 0
  
  print(sum(is.na(HHold$income)))
  
  Fuel_summary <- data.frame(item=rownames(bridge_fuel_EXIO_q), 
                             t(DLE_fuel_sector_Q) %*% as.matrix(Fuel_summary[,-1]))
  
  a <- rbind.fill(Food, OthCon) %>% left_join(HHold) %>% # filter(!is.na(val_tot)) %>% 
    mutate(fd_tot = val_tot*weight) %>% group_by(decile, item) %>%
    summarise(fd_tot = sum(fd_tot, na.rm = TRUE)) %>% 
    spread(decile, fd_tot) %>% mutate(total = rowSums(.[2:11], na.rm = TRUE)) %>%
    select(item, total, decile1:decile10) %>% rbind(Fuel_summary)
  a[is.na(a)] <- 0
  # 
  # a_tot <- rbind.fill(Food, OthCon) %>% left_join(HHold) %>% # filter(!is.na(val_tot)) %>%
  #   mutate(fd_tot =val_tot*weight) %>% group_by(item) %>%
  #   summarise(fd_tot=sum(fd_tot, na.rm = TRUE)) %>%
  #   rename(total = fd_tot)
  
  return(a)
}

readFinalDemandfromDBAllHH = function(svy='IND1') {
  xlcFreeMemory()
  Food <- selectDBdata(ID, ITEM, VAL_TOT, tables=c(paste0(svy, '_FOOD')))
  xlcFreeMemory()
  OthCon <- selectDBdata(ID, ITEM, VAL_TOT, tables=c(paste0(svy, '_OTHCON')))
  xlcFreeMemory()
  Fuel <- selectDBdata(ID, FUEL, VAL_TOT, QTY_TOT, UNIT, tables=c(paste0(svy, '_FUEL')))
  xlcFreeMemory()
  
  if (grepl("BRA", svy)) {
    HHold <- selectDBdata(ID, WEIGHT, INCOME, CONSUMPTION, HH_SIZE, EXPENDITURE, tables=c(paste0(svy, '_HH')))
    # income_proxy <- income
  }
  else {
    HHold <- selectDBdata(ID, WEIGHT, CONSUMPTION, HH_SIZE, EXPENDITURE, tables=c(paste0(svy, '_HH')))
    HHold$income <- HHold$consumption
  }
  xlcFreeMemory()
  
  # Fuel <- Fuel %>% rename(item = fuel)
  print(sum(is.na(HHold$income)))
  HHold <- HHold %>% rename(hhid = id) %>% 
    arrange(income/hh_size) %>%
    mutate(cumpop = cumsum(hh_size*weight)/sum(HHold$weight*HHold$hh_size)) %>%
    mutate(decile = cut(cumpop, breaks = seq(0, 1, 0.1),
                        labels=paste0("decile", 1:10), include.lowest = TRUE, ordered=TRUE))  %>%
    filter(!is.na(income))
  
  print(sum(is.na(HHold$income)))
  
  Fuel_summary <- Fuel %>% rename(hhid = id) %>% 
    right_join(HHold) %>%    # Using right_join to keep all the IDs and matching columns with a_dec
    # mutate(fd_tot =val_tot*weight) %>% 
    group_by(hhid, fuel) %>%
    summarise(fd_tot=sum(val_tot, na.rm = TRUE)) %>% arrange(fuel) 
  
  Fuel_summary <- Fuel_summary %>% 
    rbind.fill(data.frame(fuel = DLE_fuel_types, fd_tot=0)) %>% 
    rename(item = fuel) %>% 
    spread(hhid, fd_tot) %>% 
    filter(!is.na(item)) %>% select(-which(names(.)=="<NA>")) 
  # Fuel_summary[is.na(Fuel_summary)] <- 0
  Fuel_summary[,-1] <- NAer(Fuel_summary[,-1])
  
  Fuel_std <- data.frame(item=rownames(bridge_fuel_EXIO_q), 
                             t(DLE_fuel_sector_Q) %*% as.matrix(Fuel_summary[,-1]))
  names(Fuel_std) <- names(Fuel_summary)
  
  a_dec <- rbind.fill(Food, OthCon) %>% rename(hhid = id) %>% 
    right_join(HHold) %>% 
    # filter(!is.na(val_tot)) %>% 
    # mutate(fd_tot = val_tot*weight) %>% 
    group_by(hhid, item) %>%
    summarise(fd_tot = sum(val_tot, na.rm = TRUE)) %>% # Non-weighted FD (to get energy/cap)
    spread(hhid, fd_tot)
  a_dec[,-1] <- NAer(a_dec[,-1])
  
  a_dec <- a_dec %>% rbind(Fuel_std) 
  
  return(list(a_dec, HHold))
}

ConstructyFuelTypeSet = function() {
  xlcFreeMemory()
  DLE_fuel <- data.frame()
  for (svy in c("IND1", "BRA1", "ZAF1", "IDN1")) {
    Fuel <- selectDBdata(ID, FUEL, tables=c(paste0(svy, '_FUEL')))
    Fuel <- Fuel %>% distinct(fuel)
    DLE_fuel <- unique(rbind(DLE_fuel, Fuel))
  }
  
  return(DLE_fuel)
}
  


### Decile divide test

# a <- IND_HH %>% 
#   filter(!is.na(expenditure)) 
# aa <- a %>%
#   arrange(expenditure) %>%
#   mutate(cumpop = cumsum(hh_size*weight)/sum(a$weight*a$hh_size)) %>%
#   mutate(decile = cut(cumpop, breaks = seq(0, 1, 0.1),
#                       labels=paste0("decile", 1:10), include.lowest = TRUE, ordered=TRUE))  
# aa %>% group_by(decile) %>% summarise(sum(hh_size*weight))
# 
# 
# ab <- a %>%
#   arrange(expenditure/hh_size) %>%
#   mutate(cumpop = cumsum(hh_size*weight)/sum(a$weight*a$hh_size)) %>%
#   mutate(decile = cut(cumpop, breaks = seq(0, 1, 0.1),
#                       labels=paste0("decile", 1:10), include.lowest = TRUE, ordered=TRUE))  
# ab %>% group_by(decile) %>% summarise(sum(hh_size*weight))
