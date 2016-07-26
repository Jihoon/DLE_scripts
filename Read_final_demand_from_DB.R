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
    arrange(consumption) %>%
    mutate(cumpop = cumsum(weight)/sum(HHold$weight)) %>%
    mutate(decile = cut(cumpop, breaks = seq(0, 1, 0.1),
                        labels=paste0("decile", 1:10), include.lowest = TRUE, ordered=TRUE))  %>%
    filter(!is.na(consumption))
  
  print(sum(is.na(HHold$consumption)))
  
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

readFinalDemandfromDBAllHH = function(svy='IND1') {
  xlcFreeMemory()
  Food <- selectDBdata(ID, ITEM, VAL_TOT, tables=c(paste0(svy, '_FOOD')))
  xlcFreeMemory()
  OthCon <- selectDBdata(ID, ITEM, VAL_TOT, tables=c(paste0(svy, '_OTHCON')))
  xlcFreeMemory()
  Fuel <- selectDBdata(ID, FUEL, VAL_TOT, QTY_TOT, UNIT, tables=c(paste0(svy, '_FUEL')))
  xlcFreeMemory()
  HHold <- selectDBdata(ID, WEIGHT, CONSUMPTION, EXPENDITURE, HH_SIZE, tables=c(paste0(svy, '_HH')))
  xlcFreeMemory()
  
  Fuel <- Fuel %>% rename(item = fuel)
  print(sum(is.na(HHold$consumption)))
  HHold <- HHold %>% 
    arrange(consumption) %>%
    mutate(cumpop = cumsum(weight)/sum(HHold$weight)) %>%
    mutate(decile = cut(cumpop, breaks = seq(0, 1, 0.1),
                        labels=paste0("decile", 1:10), include.lowest = TRUE, ordered=TRUE))  %>%
    filter(!is.na(consumption))
  
  print(sum(is.na(HHold$consumption)))
  
  a_dec <- rbind.fill(Food, OthCon, Fuel) %>% inner_join(HHold) %>% 
    # filter(!is.na(val_tot)) %>% 
    # mutate(fd_tot = val_tot*weight) %>% 
    group_by(id, item) %>%
    summarise(fd_tot = sum(val_tot, na.rm = TRUE)) %>% # Non-weighted FD (to get energy/cap)
    spread(id, fd_tot)
  
  return(list(a_dec, HHold))
}
