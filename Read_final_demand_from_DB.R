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
  Fuel <- selectDBdata(ID, FUEL, VAL_TOT, 
                       # QTY_TOT, UNIT, 
                       tables=c(paste0(svy, '_FUEL')))
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
  Fuel <- selectDBdata(ID, FUEL, VAL_TOT, 
                       # QTY_TOT, UNIT, 
                       tables=c(paste0(svy, '_FUEL')))
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
  for (svy in c("IND1", "BRA1", "ZAF1", "IDN1")) {   # Need to be updated if additional countries are to be integrated.
    Fuel <- selectDBdata(ID, FUEL, tables=c(paste0(svy, '_FUEL')))
    Fuel <- Fuel %>% distinct(fuel)
    DLE_fuel <- unique(rbind(DLE_fuel, Fuel))
  }
  
  return(DLE_fuel)
}
  

readFuelQuantDemandfromDBbyDecile = function(svy='IND1') {
  xlcFreeMemory()
  Fuel <- selectDBdata(ID, FUEL, VAL_TOT, QTY_TOT, UNIT, tables=c(paste0(svy, '_FUEL')))
  xlcFreeMemory()
  
  if (grepl("BRA", svy)) {
    HHold <- selectDBdata(ID, WEIGHT, INCOME, CONSUMPTION, HH_SIZE, EXPENDITURE, tables=c(paste0(svy, '_HH')))
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
    mutate(fd_tot =val_tot*weight) %>% 
    mutate(qt_tot =qty_tot*weight) %>% 
    group_by(decile, fuel) %>%
    summarise(fd_tot=sum(fd_tot, na.rm = TRUE), qt_tot=sum(qt_tot, na.rm = TRUE)) %>% arrange(fuel) %>% 
    right_join(DLE_fuel_types) %>% rename(item = fuel) %>%
    gather(temp, tot, ends_with("_tot")) %>% unite(temp1, temp, decile, sep = ".") %>% spread(temp1, tot) %>% 
    mutate(fd.total = rowSums(.[2:11], na.rm = TRUE), qt.total = rowSums(.[12:21], na.rm = TRUE)) %>% select(-ends_with('NA')) %>%
    select(item, fd_tot.decile1, fd_tot.decile2, fd_tot.decile3, fd_tot.decile4, fd_tot.decile5, 
           fd_tot.decile6, fd_tot.decile7, fd_tot.decile8, fd_tot.decile9, fd_tot.decile10, 
           qt_tot.decile1, qt_tot.decile2, qt_tot.decile3, qt_tot.decile4, qt_tot.decile5, 
           qt_tot.decile6, qt_tot.decile7, qt_tot.decile8, qt_tot.decile9, qt_tot.decile10, everything())
  Fuel_summary[is.na(Fuel_summary)] <- 0
  
  print(sum(is.na(HHold$income)))
  
  Fuel_summary <- data.frame(item=rownames(bridge_fuel_EXIO_q), 
                             t(DLE_fuel_sector_Q) %*% as.matrix(Fuel_summary[,-1]))
  
  return(Fuel_summary)
}


readFinalEnergyfromDBAllHH = function(svy='IND1') {
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
  
  Fuel_summary <- Fuel %>% left_join(IND_MJ_hh %>% select(id, fuel, MJ)) %>% rename(hhid = id) %>% 
    right_join(HHold) %>%    # Using right_join to keep all the IDs and matching columns with a_dec
    # mutate(fd_tot =val_tot*weight) %>% 
    group_by(hhid, fuel) %>%
    summarise(MJ_tot=sum(MJ, na.rm = TRUE), weight=first(weight)) %>% arrange(fuel) 
  
  Fuel_summary <- Fuel_summary %>% 
    rbind.fill(data.frame(fuel = DLE_fuel_types, MJ_tot=0)) %>% 
    rename(item = fuel) %>% 
    spread(item, MJ_tot) %>% 
    filter(!is.na(hhid)) %>% 
    select(-which(names(.)=="<NA>")) %>% arrange(hhid)
  # Fuel_summary[is.na(Fuel_summary)] <- 0
  Fuel_summary[,-1] <- NAer(Fuel_summary[,-1])
  
  # Fuel_std <- data.frame(item=rownames(bridge_fuel_EXIO_q), 
  #                        t(DLE_fuel_sector_Q) %*% as.matrix(Fuel_summary[,-1]))
  Fuel_std <- data.frame(hhid=Fuel_summary$hhid, weight=Fuel_summary$weight, 
                         as.matrix(Fuel_summary[,-c(1,2)]) %*% as.matrix(DLE_fuel_sector_Q))
  # names(Fuel_std) <- names(Fuel_summary)
  
  return(Fuel_std)
}


### This was for temporary use to derive expenditure share of individual food items (to tackle reviewrs' comment).
### "Other fresh fruits" didn't have quantity data, and we show it is really a small part.

readFinalFoodfromDBbyDecile = function(svy='IND1') {
  xlcFreeMemory()
  Food <- selectDBdata(ID, ITEM, VAL_TOT, tables=c(paste0(svy, '_FOOD')))
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
  
  a <- Food %>% left_join(HHold) %>% # filter(!is.na(val_tot)) %>% 
    mutate(fd_tot = val_tot*weight) %>% group_by(decile, item) %>%
    summarise(fd_tot = sum(fd_tot, na.rm = TRUE)) %>% 
    spread(decile, fd_tot) %>% mutate(total = rowSums(.[2:11], na.rm = TRUE)) %>%
    select(item, total, decile1:decile10) 
  a[is.na(a)] <- 0
  
  return(a)
}