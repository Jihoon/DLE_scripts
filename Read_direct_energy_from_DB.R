# Energy density 
# Sources
# 1. https://en.wikipedia.org/wiki/Energy_content_of_biofuel
# 2. https://en.wikipedia.org/wiki/Energy_density
# 3. http://w.astro.berkeley.edu/~wright/fuel_energy.html
# 4. http://physics.info/energy-chemical/

fuel <- c('Charcoal', 'Coal', 'Coke', 'Diesel, non-transport', 'Diesel, transport', 'Electricity', 'Firewood', 
          'Gasoline, non-transport', 'Gasoline, transport', 'Kerosene', 'LPG', 'Dung', 'Biogas')
# e_val <- c(30, 29, 27, 39.6, 38.6, 3.6, 16.2, 33, 34.2, 36.8, 46.4, 12, NA)
e_val <- c(26, 18, 19, 35, 35, 3.6, 15, 32, 32, 33, 46.4, 10, NA)  # From Shonali
f_unit <- c('MJ/kg', 'MJ/kg', 'MJ/kg', 'MJ/l', 'MJ/l', 'MJ/kwh', 'MJ/kg', 'MJ/l', 'MJ/l', 'MJ/l', 'MJ/kg', 'MJ/kg', NA)
e_categ <- c(65, 65, 65, 65, 103, 63, 65, 65, 103, 65, 64, 65, 64)  # ICP sector number
E_density <- data.frame(fuel, e_val, f_unit, e_categ)

# Fuel price in 2007 IND
# from http://in.reuters.com/article/india-fuel-prices-idINSGE6130E720100204
p_petrol2007 <- 43 / PPP_IND * CPI_ratio_IND  # 2007 rupees per litre -> 2010 ppp$/litre
p_diesel2007 <- 30.4 / PPP_IND * CPI_ratio_IND

# fuel_p Returns NA
fuel_p = WDI(country = "IN", indicator = c("EP.PMP.SGAS.CD", "EP.PMP.DESL.CD"), start = 2007, end = 2010, extra = FALSE, cache = NULL)


readDirectEnergyfromDBbyDecile = function(svy='IND1') {
  
  xlcFreeMemory()
  DE = selectDBdata(ID, FUEL, VAL_TOT, QTY_TOT, UNIT, tables=c(paste0(svy, '_FUEL')))
  xlcFreeMemory()
  HH = selectDBdata(ID, WEIGHT, CONSUMPTION, EXPENDITURE, tables=c(paste0(svy, '_HH')))
  
  HH <- HH %>% 
    mutate(consumption = replace(consumption, is.na(consumption), expenditure)) %>%
    mutate(decile = cut(consumption,
                        breaks = quantile(consumption, probs = seq(0, 1, 0.1), na.rm=TRUE),
                        labels=paste0("decile", 1:10), include.lowest = TRUE, ordered=TRUE)) 
  
  # Combining same time together
  # 1. Direct energy [GJ]
  DE <- DE %>% mutate(fuel = replace(fuel, grepl("Kerosene", fuel), "Kerosene")) %>% 
    left_join(HH) %>% filter(!is.na(val_tot)) %>% 
    mutate(de_tot = qty_tot*weight, fd_tot =val_tot*weight, price = val_tot / qty_tot) %>%
    mutate(de_tot = replace(de_tot, fuel=="Diesel, transport", (fd_tot/p_diesel2007)[fuel=="Diesel, transport"])) %>%
    mutate(de_tot = replace(de_tot, fuel=="Gasoline, transport", (fd_tot/p_petrol2007)[fuel=="Gasoline, transport"])) 
    
  DE_dec <- DE %>% group_by(decile, fuel) %>%
    # summarise(de_tot=sum(de_tot, na.rm = TRUE), unique(unit), fd_tot = sum(fd_tot, na.rm = TRUE)) %>% 
    summarise(de_tot=sum(de_tot, na.rm = TRUE)) %>% 
    spread(decile, de_tot)
  
  DE_tot <- DE %>% group_by(fuel) %>%
    summarise(de_tot=sum(de_tot, na.rm = TRUE))
  
  # Summarize by fuel type
  # ene_tot in MJ & de_tot in respective physical unit
  DE_all <- left_join(DE_tot, DE_dec) %>% 
    left_join(E_density) %>% 
    # qty_tot for transport fuels were 0 in IND1_FUEL.
    # mutate(ene_tot = e_val * de_tot) %>%
    mutate_each(funs(whatever = .*e_val), de_tot:decile10) %>%
    group_by(e_categ)
  
  # 2. Expenditure on Direct energy [$]
  FD_dec <- DE %>% group_by(decile, fuel) %>%
    # summarise(de_tot=sum(de_tot, na.rm = TRUE), unique(unit), fd_tot = sum(fd_tot, na.rm = TRUE)) %>% 
    summarise(fd_tot=sum(fd_tot, na.rm = TRUE)) %>% 
    spread(decile, fd_tot)
  
  FD_tot <- DE %>% group_by(fuel) %>%
    summarise(fd_tot=sum(fd_tot, na.rm = TRUE))
  
  FD_all <- left_join(FD_tot, FD_dec) %>% 
    left_join(E_density) %>% group_by(e_categ)
  
  # Summarize by ICP energy sector
  # ene_tot in MJ
  # Note: Dung, biogas, other are excluded because of no price or unit info.
  # tot_DE <- summarise(b, ene_tot=sum(ene_tot, na.rm = TRUE), fd_tot=sum(fd_tot,na.rm = TRUE)) %>% 
  #   filter(!is.na(e_categ)) %>% mutate(e_inten = ene_tot / fd_tot) 
  
  tot_DE <- summarise_each(DE_all, funs(ss = sum(., na.rm=TRUE)), de_tot:decile10) %>% filter(!is.na(e_categ))
  tot_FD_DE <- summarise_each(FD_all, funs(ss = sum(., na.rm=TRUE)), fd_tot:decile10) %>% filter(!is.na(e_categ))
  
  return(list(tot_DE, tot_FD_DE))
}

