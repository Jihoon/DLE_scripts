library(zoo)  # for rollmean

getGINI <- function(x, y) {
  area <- sum(diff(x)*rollmean(y,2)) 
  gini <- (100*100/2 - area) / (100*100/2) # Assuming x, y are coded as pct values
  return(gini)
}

# All including cooking (fuelwood, LPG)
load(file="./Saved tables/BRA_intensities_val_BRA.Rda")
load(file="./Saved tables/BRA_ENEperCap_valBR_adjFD.Rda")  # get eHH_BRA
svy <- "BRA0"
BRA_HH_region <- selectDBdata(SURVEY, ID, REGION, URBAN, tables=c(paste0(svy, '_HH'))) %>% data.table(key="id")
names(BRA_HH_region)[2] <- "hhid"

svy <- "IND2"
IND2_HH_region <- selectDBdata(SURVEY, ID, REGION, URBAN, tables=c(paste0(svy, '_HH'))) %>% data.table(key="id")
names(IND2_HH_region)[2] <- "hhid"
# n_col <- dim(eHH_BRA)[2]

eHH_BRA <- eHH_BRA %>% mutate(expense2007MER = colSums(BRA_FD_ICP_HH_adj_BR) / scaler_BRA) %>%
  mutate(pri_e_avg = rowMeans(select(eHH_BRA,V1:V500), na.rm = TRUE) * hh_size) %>%
  mutate(intensity = pri_e_avg / expense2007MER * 1000)  # In MJ/USD (2007 MER)

BRA_HH_sum <- eHH_BRA %>% select(hhid, weight:intensity)  %>%  mutate(coalexpense = (BRA_FD_ICP_HH_adj_BR / scaler_BRA)[153,]) %>%
  left_join(BRA_HH_region %>% select(hhid, region, urban), by="hhid")
a <- BRA_HH_sum %>% group_by(region, decile) %>% summarise_each(funs(mean), coalexpense) %>% arrange(region, decile)

qplot(expense2007MER, coalexpense, data=BRA_HH_sum %>% filter(urban==0), geom="auto", xlim=c(0,15000),
      xlab="Annual expenditure [USD 2007MER] (Rural Brazil)", ylab="Coal USD", size=I(0.15))
a <- BRA_HH_sum %>% group_by(region, urban, decile) %>% summarise(coalexp.per.hh=sum(coalexpense*weight)/sum(weight))
View(a)

# [152] "Biogas"                                                                   
# [153] "Charcoal/coal/briquette/coke"                                             
# [154] "Diesel"                                                                   
# [155] "Electricity"                                                              
# [156] "Ethanol"                                                                  
# [157] "Firewood and other fuels"                                                 
# [158] "Other biomass"                                                            
# [159] "Fuel oil, generator"                                                      
# [160] "Gasoline"                                                                 
# [161] "Kerosene"                                                                 
# [162] "LPG"                                                                      
# [163] "Natural gas"                                                              
# [164] "Other household fuel"   

# Try excluding cooking fuel (fuelwood, LPG)
idx_cookingfuel <-  c(153, 157, 162)  #c(157,162) #c(153, 157,161,162)   # 
idx_fuelwood <- 157  #c(157,162) #c(153, 157,161,162)   # 
list[eHH_BRA_nocook, eHH_sd_nocook] <-  GetHHSectoralEnergyPerCap(setdiff(ICP_all_idx,idx_cookingfuel), 'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
list[eHH_BRA_nofw, eHH_sd_nofw] <-  GetHHSectoralEnergyPerCap(setdiff(ICP_all_idx,idx_fuelwood), 'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)

# eHH_BRA_nocook <- eHH_BRA_nocook %>% mutate(totexpense2007MER = colSums(BRA_FD_ICP_HH_adj_BR / scaler_BRA)) %>%
#   mutate(expense2007MER = colSums(BRA_FD_ICP_HH_adj_BR[setdiff(ICP_all_idx,idx_cookingfuel),] / scaler_BRA)) %>%
#   mutate(pri_e_avg = rowMeans(select(eHH_BRA_nocook,V1:V500), na.rm = TRUE) * hh_size) %>%
#   mutate(intensity = pri_e_avg / expense2007MER * 1000)  # In MJ/USD (2007 MER)
# 
# BRA_HH_sum_nocook <- eHH_BRA_nocook %>% select(hhid, weight:intensity) %>%
#   left_join(BRA_HH_region %>% select(hhid, region, urban), by="hhid")

BRA_HH_sum_nocook <- GetSummaryForPlot(eHH_BRA_nocook, BRA_FD_ICP_HH_adj_BR, "BRA", idx_cookingfuel)

# eHH_BRA_nofw <- eHH_BRA_nofw %>% mutate(totexpense2007MER = colSums(BRA_FD_ICP_HH_adj_BR / scaler_BRA)) %>%
#   mutate(expense2007MER = colSums(BRA_FD_ICP_HH_adj_BR[setdiff(ICP_all_idx,idx_fuelwood),] / scaler_BRA)) %>%
#   mutate(pri_e_avg = rowMeans(select(eHH_BRA_nofw,V1:V500), na.rm = TRUE) * hh_size) %>%
#   mutate(intensity = pri_e_avg / expense2007MER * 1000)  # In MJ/USD (2007 MER)
# 
# BRA_HH_sum_nofw <- eHH_BRA_nofw %>% select(hhid, weight:intensity) %>%
#   left_join(BRA_HH_region %>% select(hhid, region, urban), by="hhid")
BRA_HH_sum_food <- GetSummaryForPlot(eHH_BRA_food, BRA_FD_ICP_HH_adj_BR, "BRA", idx_fuelwood)


# Food intensity
max.int <- 25
max.exp <- 30000

idx_food <- 1:40  #c(157,162) #c(153, 157,161,162)   # 
list[eHH_BRA_food, eHH_sd_food] <-  GetHHSectoralEnergyPerCap(idx_food, 'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)

# eHH_BRA_food <- eHH_BRA_food %>% mutate(totexpense2007MER = colSums(BRA_FD_ICP_HH_adj_BR / scaler_BRA)) %>%
#   mutate(foodexpense2007MER = colSums(BRA_FD_ICP_HH_adj_BR[idx_food,] / scaler_BRA)) %>%
#   mutate(pri_e_avg = rowMeans(select(eHH_BRA_food,V1:V500), na.rm = TRUE) * hh_size) %>%
#   mutate(intensity = pri_e_avg / foodexpense2007MER * 1000)  # In MJ/USD (2007 MER)
# 
# BRA_HH_sum_food <- eHH_BRA_food %>% select(hhid, weight:intensity) %>%
#   left_join(BRA_HH_region %>% select(hhid, region, urban), by="hhid")

BRA_HH_sum_food <- GetSummaryForPlot(eHH_BRA_food, BRA_FD_ICP_HH_adj_BR, "BRA")

qplot(totexpense2007MER, intensity, data=BRA_HH_sum_food %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Rural Brazil)", ylab="MJ/USD (Food)", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=BRA_HH_sum_food %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Urban Brazil)", ylab="MJ/USD (Food)", size=I(0.15)) +
  stat_smooth()


# Plotting intensities vs hh expenditure
# PlotIntensityHist(eHH_BRA, "intensity", xmax=150, bin_size=0.1, drawline = F)

# Urban/Rural intensities

max.int <- 150
max.exp <- 30000

qplot(expense2007MER, intensity, data=BRA_HH_sum, geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (All Brazil)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=BRA_HH_sum_nocook, geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (All Brazil)", ylab="MJ/USD w/o fuelwood and coal", size=I(0.15)) +
  stat_smooth()
qplot(expense2007MER, intensity, data=BRA_HH_sum %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Rural Brazil)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=BRA_HH_sum_nocook %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Rural Brazil)", ylab="MJ/USD w/o fuelwood and coal", size=I(0.15)) +
  stat_smooth()
qplot(expense2007MER, intensity, data=BRA_HH_sum %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Urban Brazil)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=BRA_HH_sum_nocook %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Urban Brazil)", ylab="MJ/USD w/o fuelwood and coal", size=I(0.15)) +
  stat_smooth()

# Impact of Coal by region(?)
qplot(totexpense2007MER, intensity, data=BRA_HH_sum_nofw %>% filter(region=="Jammu and Kashmir"), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Urban Brazil)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=BRA_HH_sum_nofw %>% filter(region=="Tamil Nadu"), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Urban Brazil)", ylab="MJ/USD w/o fuelwood and coal", size=I(0.15)) +
  stat_smooth()



# See how much energy is used becasue of fuel consumption
a <- rbind(colSums(BRA_FD_ICP_HH_adj_BR[1:151,]), colSums(BRA_FD_ICP_HH_adj_BR[152:164,])) / scaler_BRA
list[eHH_BRA_fuel, eHH_sd_fuel] <-  GetHHSectoralEnergyPerCap(152:164, 'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
list[eHH_BRA_nonfuel, eHH_sd_nonfuel] <- GetHHSectoralEnergyPerCap(1:151, 'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)

BRA_HH_sum_fuel <- eHH_BRA_fuel %>% 
  mutate(pri_e_fuel = rowMeans(select(eHH_BRA_fuel,V1:V500), na.rm = TRUE) * hh_size) %>%
  select(hhid, weight:pri_e_fuel) 
BRA_HH_sum_compare <- eHH_BRA_nonfuel %>% 
  mutate(pri_e_nonfuel = rowMeans(select(eHH_BRA_nonfuel,V1:V500), na.rm = TRUE) * hh_size) %>%
  select(hhid, weight:pri_e_nonfuel) %>% 
  left_join(BRA_HH_sum_fuel) %>%
  left_join(BRA_HH_region %>% select(hhid, region, urban), by="hhid") %>%
  mutate(fuel_ene_ratio = pri_e_fuel / (pri_e_nonfuel+pri_e_fuel)) %>% 
  mutate(expense2007MER = colSums(BRA_FD_ICP_HH_adj_BR / scaler_BRA))

# % of fuel
qplot(expense2007MER, fuel_ene_ratio, data=BRA_HH_sum_compare %>% filter(urban==1), geom="auto", xlim=c(0,15000),
      xlab="Annual expenditure [USD 2007MER] (Urban Brazil)", ylab="Ratio of fuel energy", size=I(0.15)) +
  stat_smooth()
qplot(expense2007MER, fuel_ene_ratio, data=BRA_HH_sum_compare %>% filter(urban==0), geom="auto", xlim=c(0,15000), 
      xlab="Annual expenditure [USD 2007MER] (Rural Brazil)", ylab="Ratio of fuel energy", size=I(0.15)) +
  stat_smooth()






## GINI calculation
tot_pop <- sum(BRA_HH_sum$hh_size*BRA_HH_sum$weight)
tot_exp <- sum(BRA_HH_sum$expense2007MER*BRA_HH_sum$weight)
tot_ene <- sum(BRA_HH_sum$pri_e_avg*BRA_HH_sum$weight)


# Main table for plotting GINI and expenditure vs. energy by household
# Sort based on expenditure
BRA_summary <- BRA_HH_sum %>% arrange(expense2007MER) %>% select(hhid, expense2007MER, pri_e_avg, hh_size, weight) %>%
  mutate(cumene = cumsum(pri_e_avg*weight)/tot_ene*100, 
         cumpop = cumsum(hh_size*weight) / tot_pop*100, 
         cumexp = cumsum(expense2007MER*weight),
         cumexp_pct = cumsum(expense2007MER*weight) / tot_exp*100) %>%
  mutate(pop_decile = floor(cumpop/10))
pop_cut <- match(1:9, BRA_summary$pop_decile)  # find decile boundaries

# Cumulative energy vs expenditure plot
qplot(expense2007MER, cumene, data=BRA_summary, geom="auto", xlab="Annual expenditure [USD 2007MER] (All Brazil)", ylab="TJ") +
  geom_hline(yintercept=BRA_summary$cumene[pop_cut], alpha=0.4, linetype=2)

# Expenditure GINI plot
qplot(cumpop, cumexp_pct, data=BRA_summary, geom="line", xlab="Cum population (All Brazil)", ylab="Cum pct expenditure") 
qplot(cumexp_pct, cumene, data=BRA_summary, geom="line", xlab="Cum expenditure (All Brazil)", ylab="Cum pct energy") 

attach(BRA_summary)
GINI_exp <- getGINI(cumpop, cumexp_pct)
detach(BRA_summary)

# Sort again based on energy
BRA_summary <- BRA_summary %>% arrange(pri_e_avg) %>% 
  mutate(cumene = cumsum(pri_e_avg*weight)/tot_ene *100, cumpop = cumsum(hh_size*weight) / tot_pop * 100) 

# Energy GINI plot
qplot(cumpop, cumene, data=BRA_summary, geom="line", xlab="Cum population (All Brazil)", ylab="Cum pct energy") 

attach(BRA_summary)
GINI_ene <- getGINI(cumpop, cumene)
detach(BRA_summary)




# Incremental expenditure between IND1 and IND2

# Re-allocate 2011 FD vector following chng_pct_BRA
BRA_FD_HH_adj_2011 <- BRA_FD_AllHH_2011 * (chng_pct_BRA + 1)
idx_inf <- which(is.infinite(chng_pct_BRA))  # Identify rows with Inf adjustments
r_HH <- colSums(BRA_FD_AllHH_2011)/sum(BRA_FD_AllHH_2011)  # ratio of hh total to (unweighted) total
BRA_FD_HH_adj_2011[idx_inf,] <- t(sapply(BRA_FD_adj[idx_inf] * 1e6, # M.USD to USD
                                         function(x) x * r_HH / sum(r_HH * BRA_HH$weight))) * scaler_BRA * BRA_con_grwth  
rm(r_HH)
gc()

# Get total primary energy for all HHs in two years
list[eHH_BRA_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(1:164, 'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)

BRA_HH_sum_2011 <- GetSummaryForPlot(eHH_BRA_2011, BRA_FD_ICP_HH_adj_BR, "BRA", 0)

# Get total primary energy for all HHs, but excluding cooking fuel (fuelwood, LPG) and coal
idx_cookingfuel <-  c(153, 157, 162)   
list[eHH_BRA_2011_nosolid, eHH_sd_2011_nosolid] <-  GetHHSectoralEnergyPerCap(setdiff(ICP_all_idx,idx_cookingfuel), 'BRA', 
                                                                              BRA_FD_ICP_HH_adj_BR, BRA_intensity)

BRA_HH_sum_2011_nosolid <- GetSummaryForPlot(eHH_BRA_2011_nosolid, BRA_FD_ICP_HH_adj_BR, "BRA", idx_cookingfuel)

# Set range
max.int <- 150
max.exp <- 40000

# 2011
BRA_p1 <- qplot(totexpense2007MER, intensity, data=BRA_HH_sum_2011 %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2011 [USD 2007MER] (Rural Brazil)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
BRA_p2 <- qplot(totexpense2007MER, intensity, data=BRA_HH_sum_2011_nosolid %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2011 [USD 2007MER] (Rural Brazil)", ylab="MJ/USD w/o solid fuel & LPG", size=I(0.15)) +
  stat_smooth()
max.int <- 150
max.exp <- 60000
BRA_p3 <- qplot(totexpense2007MER, intensity, data=BRA_HH_sum_2011 %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2011 [USD 2007MER] (Urban Brazil)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
BRA_p4 <- qplot(totexpense2007MER, intensity, data=BRA_HH_sum_2011_nosolid %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2011 [USD 2007MER] (Urban Brazil)", ylab="MJ/USD w/o solid fuel & LPG", size=I(0.15)) +
  stat_smooth()
grid.arrange(BRA_p1, BRA_p2, BRA_p3, BRA_p4, nrow=2, ncol=2)

# Return average expenditure, primary energy, and sectoral intensity from eHH and HH data
# Sectors to be removed are specified by idx_remove (0 = include all ICP sectors)
GetSummaryForPlot <- function(eHH, fd_HH, cty, idx_remove=0) {
  scaler <- eval(parse(text=paste0("scaler_",cty)))
  HH_region <- eval(parse(text=paste0(cty, "_HH_region")))
  
  eHH <- eHH %>% 
    mutate(expense2007MER = colSums(fd_HH[setdiff(ICP_all_idx,idx_remove),]) / scaler,
           pri_e_avg = rowMeans(select(eHH, V1:V500), na.rm = TRUE) * hh_size,    # Total HH primary energy
           intensity = pri_e_avg / expense2007MER * 1000)  # In MJ/USD (2007 MER)
  eHH <- eHH %>% 
    mutate(totexpense2007MER = colSums(fd_HH) / scaler)
  
  HH_sum <- eHH %>% select(hhid, weight:totexpense2007MER) %>%
    left_join(HH_region %>% select(hhid, region, urban), by="hhid")
  
  return(HH_sum)
}



# Brazil: Sectoral comparison
ICP_food_idx <- 1:45
ICP_hhold_idx <- c(56:84, 138:151)  # Household goods/services
ICP_svc_idx <- 85:137   # Health, Transport, Communication, Recreation
ICP_fuel_idx <- 152:164
ICP_nosolid <- setdiff(152:164, c(153, 157, 162))
ICP_oth_idx <- 46:55

#2011
list[eHH_BRA_food_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_food_idx, 'BRA', BRA_FD_HH_adj_2011, BRA_intensity)
list[eHH_BRA_hhold_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_hhold_idx, 'BRA', BRA_FD_HH_adj_2011, BRA_intensity)
list[eHH_BRA_svc_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_svc_idx, 'BRA', BRA_FD_HH_adj_2011, BRA_intensity)
list[eHH_BRA_fuel_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_fuel_idx, 'BRA', BRA_FD_HH_adj_2011, BRA_intensity)
list[eHH_BRA_oth_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_oth_idx, 'BRA', BRA_FD_HH_adj_2011, BRA_intensity)

BRA_HH_food_2011 <- GetSummaryForPlot(eHH_BRA_food_2011, BRA_FD_HH_adj_2011, "BRA", setdiff(ICP_all_idx,ICP_food_idx)) %>%
  rename(En_food=pri_e_avg, I_food=intensity, Ex_food=expense2007MER)
BRA_HH_hhold_2011 <- GetSummaryForPlot(eHH_BRA_hhold_2011, BRA_FD_HH_adj_2011, "BRA", setdiff(ICP_all_idx,ICP_hhold_idx)) %>%
  select(hhid, En_hhold=pri_e_avg, I_hhold=intensity, Ex_hhold=expense2007MER)
BRA_HH_svc_2011 <- GetSummaryForPlot(eHH_BRA_svc_2011, BRA_FD_HH_adj_2011, "BRA", setdiff(ICP_all_idx,ICP_svc_idx)) %>%
  select(hhid, En_svc=pri_e_avg, I_svc=intensity, Ex_svc=expense2007MER)
BRA_HH_fuel_2011 <- GetSummaryForPlot(eHH_BRA_fuel_2011, BRA_FD_HH_adj_2011, "BRA", setdiff(ICP_all_idx,ICP_fuel_idx)) %>%
  select(hhid, En_fuel=pri_e_avg, I_fuel=intensity, Ex_fuel=expense2007MER)
BRA_HH_oth_2011 <- GetSummaryForPlot(eHH_BRA_oth_2011, BRA_FD_HH_adj_2011, "BRA", setdiff(ICP_all_idx,ICP_oth_idx)) %>%
  select(hhid, En_oth=pri_e_avg, I_oth=intensity, Ex_oth=expense2007MER)
BRA_HH_sum_sector_2011_nosolid <- BRA_HH_food_2011 %>% left_join(BRA_HH_hhold_2011) %>% 
  left_join(BRA_HH_svc_2011) %>% left_join(BRA_HH_nosolid_2011) %>% left_join(BRA_HH_oth_2011) %>%
  mutate(En_pri.tot = En_food+En_hhold+En_svc+En_nosol+En_oth)
# BRA_HH_sum_sector_2011 <- BRA_HH_food_2011 %>% left_join(BRA_HH_hhold_2011) %>% 
# left_join(BRA_HH_svc_2011) %>% left_join(BRA_HH_fuel_2011) %>% left_join(BRA_HH_oth_2011) %>%
# mutate(En_pri.tot = En_food+En_hhold+En_svc+En_fuel+En_oth)

list[eHH_BRA_nosolid_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_nosolid, 'BRA', BRA_FD_HH_adj_2011, BRA_intensity)
BRA_HH_nosolid_2011 <- GetSummaryForPlot(eHH_BRA_nosolid_2011, BRA_FD_HH_adj_2011, "BRA", setdiff(ICP_all_idx,ICP_nosolid)) %>%
  select(hhid, En_nosol=pri_e_avg, I_nosol=intensity, Ex_nosol=expense2007MER)

BRA_ene_2011 <- BRA_HH_sum_sector_2011 %>% group_by(decile) %>%
  summarise_at(vars(starts_with("En_")), sum) %>%
  mutate_at(2:6, funs("pct" = ./En_pri.tot))
BRA_ene_2011_nosolid <- BRA_HH_sum_sector_2011_nosolid %>% group_by(decile) %>%
  summarise_at(vars(starts_with("En_")), sum) %>%
  mutate_at(2:6, funs("pct" = ./En_pri.tot))


write.table(BRA_ene_2011_nosolid, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
write.table(BRA_ene_2011, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
write.table(BRA_ene_2004, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

rm(eHH_BRA_food_2011, eHH_BRA_hhold_2011, eHH_BRA_svc_2011, eHH_BRA_fuel_2011, 
   eHH_BRA_food_2004, eHH_BRA_hhold_2004, eHH_BRA_svc_2004, eHH_BRA_fuel_2004); gc()





# Check consumption vs expenditure
BRA_tempfood <- selectDBdata(ID, ITEM, VAL_TOT, VAL_OWN, VAL_FREE, VAL_BAR, tables="IND1_FOOD") %>% data.table(key="id")
BRA_tempfood %>% filter(val_own>16000)
BRA_tempfood %>% filter(val_own>10000)
# For milk and rice, val_own is larger than val_tot

