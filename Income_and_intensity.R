library(zoo)  # for rollmean

getGINI <- function(x, y) {
  area <- sum(diff(x)*rollmean(y,2)) 
  gini <- (100*100/2 - area) / (100*100/2) # Assuming x, y are coded as pct values
  return(gini)
}

# All including cooking (fuelwood, LPG)
load("./Saved tables/IND_intensities.Rda")
load(file="./Saved tables/IND_ENEperCap_adjFD.Rda")  # get eHH_IND
svy <- "IND1"
IND_HH_region <- selectDBdata(SURVEY, ID, REGION, URBAN, tables=c(paste0(svy, '_HH'))) %>% data.table(key="id")
names(IND_HH_region)[2] <- "hhid"

# svy <- "IND2"
# IND2_HH_region <- selectDBdata(SURVEY, ID, REGION, URBAN, tables=c(paste0(svy, '_HH'))) %>% data.table(key="id")
# names(IND2_HH_region)[2] <- "hhid"
# n_col <- dim(eHH_IND)[2]

eHH_IND <- eHH_IND %>% mutate(expense2007MER = colSums(IND_FD_ICP_HH_adj) / scaler_IND) %>%
  mutate(pri_e_avg = rowMeans(select(eHH_IND,V1:V500), na.rm = TRUE) * hh_size) %>%
  mutate(intensity = pri_e_avg / expense2007MER * 1000)  # In MJ/USD (2007 MER)

IND_HH_sum <- eHH_IND %>% select(hhid, weight:intensity)  %>%  mutate(coalexpense = (IND_FD_ICP_HH_adj / scaler_IND)[153,]) %>%
  left_join(IND_HH_region %>% select(hhid, region, urban), by="hhid")
a <- IND_HH_sum %>% group_by(region, decile) %>% summarise_each(funs(mean), coalexpense) %>% arrange(region, decile)

qplot(expense2007MER, coalexpense, data=IND_HH_sum %>% filter(urban==0), geom="auto", xlim=c(0,15000),
      xlab="Annual expenditure [USD 2007MER] (Rural India)", ylab="Coal USD", size=I(0.15))
a <- IND_HH_sum %>% group_by(region, urban, decile) %>% summarise(coalexp.per.hh=sum(coalexpense*weight)/sum(weight))
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
list[eHH_IND_nocook, eHH_sd_nocook] <-  GetHHSectoralEnergyPerCap(setdiff(ICP_all_idx,idx_cookingfuel), 'IND', IND_FD_ICP_HH_adj, IND_intensity)
list[eHH_IND_nofw, eHH_sd_nofw] <-  GetHHSectoralEnergyPerCap(setdiff(ICP_all_idx,idx_fuelwood), 'IND', IND_FD_ICP_HH_adj, IND_intensity)

# eHH_IND_nocook <- eHH_IND_nocook %>% mutate(totexpense2007MER = colSums(IND_FD_ICP_HH_adj / scaler_IND)) %>%
#   mutate(expense2007MER = colSums(IND_FD_ICP_HH_adj[setdiff(ICP_all_idx,idx_cookingfuel),] / scaler_IND)) %>%
#   mutate(pri_e_avg = rowMeans(select(eHH_IND_nocook,V1:V500), na.rm = TRUE) * hh_size) %>%
#   mutate(intensity = pri_e_avg / expense2007MER * 1000)  # In MJ/USD (2007 MER)
# 
# IND_HH_sum_nocook <- eHH_IND_nocook %>% select(hhid, weight:intensity) %>%
#   left_join(IND_HH_region %>% select(hhid, region, urban), by="hhid")

IND_HH_sum_nocook <- GetSummaryForPlot(eHH_IND_nocook, IND_FD_ICP_HH_adj, "IND", idx_cookingfuel)

# eHH_IND_nofw <- eHH_IND_nofw %>% mutate(totexpense2007MER = colSums(IND_FD_ICP_HH_adj / scaler_IND)) %>%
#   mutate(expense2007MER = colSums(IND_FD_ICP_HH_adj[setdiff(ICP_all_idx,idx_fuelwood),] / scaler_IND)) %>%
#   mutate(pri_e_avg = rowMeans(select(eHH_IND_nofw,V1:V500), na.rm = TRUE) * hh_size) %>%
#   mutate(intensity = pri_e_avg / expense2007MER * 1000)  # In MJ/USD (2007 MER)
# 
# IND_HH_sum_nofw <- eHH_IND_nofw %>% select(hhid, weight:intensity) %>%
#   left_join(IND_HH_region %>% select(hhid, region, urban), by="hhid")
# IND_HH_sum_food <- GetSummaryForPlot(eHH_IND_food, IND_FD_ICP_HH_adj, "IND", idx_fuelwood)

# Food intensity
max.int <- 25
max.exp <- 30000

idx_food <- 1:45  #c(157,162) #c(153, 157,161,162)   # 
list[eHH_IND_food, eHH_sd_food] <-  GetHHSectoralEnergyPerCap(idx_food, 'IND', IND_FD_ICP_HH_adj, IND_intensity)

# eHH_IND_food <- eHH_IND_food %>% mutate(totexpense2007MER = colSums(IND_FD_ICP_HH_adj / scaler_IND)) %>%
#   mutate(foodexpense2007MER = colSums(IND_FD_ICP_HH_adj[idx_food,] / scaler_IND)) %>%
#   mutate(pri_e_avg = rowMeans(select(eHH_IND_food,V1:V500), na.rm = TRUE) * hh_size) %>%
#   mutate(intensity = pri_e_avg / foodexpense2007MER * 1000)  # In MJ/USD (2007 MER)
# 
# IND_HH_sum_food <- eHH_IND_food %>% select(hhid, weight:intensity) %>%
#   left_join(IND_HH_region %>% select(hhid, region, urban), by="hhid")

IND_HH_sum_food <- GetSummaryForPlot(eHH_IND_food, IND_FD_ICP_HH_adj, "IND")

qplot(totexpense2007MER, intensity, data=IND_HH_sum_food %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Rural India)", ylab="MJ/USD (Food)", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=IND_HH_sum_food %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Urban India)", ylab="MJ/USD (Food)", size=I(0.15)) +
  stat_smooth()

a <- eHH_IND_food %>% cbind(sd_GJ_per_cap = apply(eHH_IND_food %>% select(V1:V500),1,sd))
library(Hmisc)
wtd.mean(IND_HH_sum_food$pri_e_avg/IND_HH_sum_food$hh_size, w=IND_HH_sum_food$weight)
sqrt(wtd.var(IND_HH_sum_food$pri_e_avg/IND_HH_sum_food$hh_size, w=IND_HH_sum_food$weight))
wtd.mean(a$sd_GJ_per_cap, w=a$weight)

# Plotting intensities vs hh expenditure
# PlotIntensityHist.decile(eHH_IND, "intensity", xmax=150, bin_size=0.1, drawline = F)

# Urban/Rural intensities

max.int <- 150
max.exp <- 30000

qplot(expense2007MER, intensity, data=IND_HH_sum, geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (All India)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=IND_HH_sum_nocook, geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (All India)", ylab="MJ/USD w/o solid fuel & LPG", size=I(0.15)) +
  stat_smooth()
IND_p1 <-qplot(expense2007MER, intensity, data=IND_HH_sum %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Rural India)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
IND_p2 <-qplot(totexpense2007MER, intensity, data=IND_HH_sum_nocook %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Rural India)", ylab="MJ/USD w/o solid fuel & LPG", size=I(0.15)) +
  stat_smooth()
IND_p3 <-qplot(expense2007MER, intensity, data=IND_HH_sum %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Urban India)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
IND_p4 <-qplot(totexpense2007MER, intensity, data=IND_HH_sum_nocook %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure [USD 2007MER] (Urban India)", ylab="MJ/USD w/o solid fuel & LPG", size=I(0.15)) +
  stat_smooth()
grid.arrange(IND_p1, IND_p2, IND_p3, IND_p4, nrow=2, ncol=2)

# Impact of Coal by region(?)
# qplot(totexpense2007MER, intensity, data=IND_HH_sum_nofw %>% filter(region=="Jammu and Kashmir"), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
#       xlab="Annual expenditure [USD 2007MER] (Urban India)", ylab="MJ/USD", size=I(0.15)) +
#   stat_smooth()
# qplot(totexpense2007MER, intensity, data=IND_HH_sum_nofw %>% filter(region=="Tamil Nadu"), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
#       xlab="Annual expenditure [USD 2007MER] (Urban India)", ylab="MJ/USD w/o fuelwood and coal", size=I(0.15)) +
#   stat_smooth()



# See how much energy is used becasue of fuel consumption
a <- rbind(colSums(IND_FD_ICP_HH_adj[1:151,]), colSums(IND_FD_ICP_HH_adj[152:164,])) / scaler_IND
list[eHH_IND_fuel, eHH_sd_fuel] <-  GetHHSectoralEnergyPerCap(152:164, 'IND', IND_FD_ICP_HH_adj, IND_intensity)
list[eHH_IND_nonfuel, eHH_sd_nonfuel] <- GetHHSectoralEnergyPerCap(1:151, 'IND', IND_FD_ICP_HH_adj, IND_intensity)

IND_HH_sum_fuel <- eHH_IND_fuel %>% 
  mutate(pri_e_fuel = rowMeans(select(eHH_IND_fuel,V1:V500), na.rm = TRUE) * hh_size) %>%
  select(hhid, weight:pri_e_fuel) 
IND_HH_sum_compare <- eHH_IND_nonfuel %>% 
  mutate(pri_e_nonfuel = rowMeans(select(eHH_IND_nonfuel,V1:V500), na.rm = TRUE) * hh_size) %>%
  select(hhid, weight:pri_e_nonfuel) %>% 
  left_join(IND_HH_sum_fuel) %>%
  left_join(IND_HH_region %>% select(hhid, region, urban), by="hhid") %>%
  mutate(fuel_ene_ratio = pri_e_fuel / (pri_e_nonfuel+pri_e_fuel)) %>% 
  mutate(expense2007MER = colSums(IND_FD_ICP_HH_adj / scaler_IND))

# % of fuel
qplot(expense2007MER, fuel_ene_ratio, data=IND_HH_sum_compare %>% filter(urban==1), geom="auto", xlim=c(0,15000),
      xlab="Annual expenditure [USD 2007MER] (Urban India)", ylab="Ratio of fuel energy", size=I(0.15)) +
  stat_smooth()
qplot(expense2007MER, fuel_ene_ratio, data=IND_HH_sum_compare %>% filter(urban==0), geom="auto", xlim=c(0,15000), 
      xlab="Annual expenditure [USD 2007MER] (Rural India)", ylab="Ratio of fuel energy", size=I(0.15)) +
  stat_smooth()






## GINI calculation
tot_pop <- sum(IND_HH_sum$hh_size*IND_HH_sum$weight)
tot_exp <- sum(IND_HH_sum$expense2007MER*IND_HH_sum$weight)
tot_ene <- sum(IND_HH_sum$pri_e_avg*IND_HH_sum$weight)


# Main table for plotting GINI and expenditure vs. energy by household
# Sort based on expenditure
IND_summary <- IND_HH_sum %>% arrange(expense2007MER) %>% select(hhid, expense2007MER, pri_e_avg, hh_size, weight) %>%
  mutate(cumene = cumsum(pri_e_avg*weight)/tot_ene*100, 
         cumpop = cumsum(hh_size*weight) / tot_pop*100, 
         cumexp = cumsum(expense2007MER*weight),
         cumexp_pct = cumsum(expense2007MER*weight) / tot_exp*100) %>%
  mutate(pop_decile = floor(cumpop/10))
pop_cut <- match(1:9, IND_summary$pop_decile)  # find decile boundaries

# Cumulative energy vs expenditure plot
qplot(expense2007MER, cumene, data=IND_summary, geom="auto", xlab="Annual expenditure [USD 2007MER] (All India)", ylab="TJ") +
  geom_hline(yintercept=IND_summary$cumene[pop_cut], alpha=0.4, linetype=2)

# Expenditure GINI plot
qplot(cumpop, cumexp_pct, data=IND_summary, geom="line", xlab="Cum population (All India)", ylab="Cum pct expenditure") 
qplot(cumexp_pct, cumene, data=IND_summary, geom="line", xlab="Cum expenditure (All India)", ylab="Cum pct energy") 

attach(IND_summary)
GINI_exp <- getGINI(cumpop, cumexp_pct)
detach(IND_summary)

# Sort again based on energy
IND_summary <- IND_summary %>% arrange(pri_e_avg) %>% 
  mutate(cumene = cumsum(pri_e_avg*weight)/tot_ene *100, cumpop = cumsum(hh_size*weight) / tot_pop * 100) 

# Energy GINI plot
qplot(cumpop, cumene, data=IND_summary, geom="line", xlab="Cum population (All India)", ylab="Cum pct energy") 

attach(IND_summary)
GINI_ene <- getGINI(cumpop, cumene)
detach(IND_summary)




# Incremental expenditure between IND1 and IND2

# Re-allocate 2004 FD vector following chng_pct_IND
IND_FD_HH_adj_2004 <- IND2_FD_AllHH_2004 * (chng_pct_IND + 1)
idx_inf <- which(is.infinite(chng_pct_IND))  # Identify rows with Inf adjustments
# For the sectors that were originally zero but were allocated non-zero values in the FD adjustment,
# we allocate the non-zero values proportionately to hh's total expenditure share.
r_HH <- colSums(IND2_FD_AllHH_2004)/sum(IND2_FD_AllHH_2004)  # ratio of each hh total to (unweighted) total
IND_FD_HH_adj_2004[idx_inf,] <- t(sapply(IND_FD_adj[idx_inf] * 1e6, # M.USD to USD
                                        function(x) x * r_HH / sum(r_HH * IND2_HH$weight))) * scaler_IND * IND2_con_grwth  
colnames(IND_FD_HH_adj_2004) <- substring(colnames(IND_FD_HH_adj_2004), 2)

# Re-allocate 2011 FD vector following chng_pct_IND
IND_FD_HH_adj_2011 <- IND_FD_AllHH_2011 * (chng_pct_IND + 1)
idx_inf <- which(is.infinite(chng_pct_IND))  # Identify rows with Inf adjustments
r_HH <- colSums(IND_FD_AllHH_2011)/sum(IND_FD_AllHH_2011)  # ratio of hh total to (unweighted) total
IND_FD_HH_adj_2011[idx_inf,] <- t(sapply(IND_FD_adj[idx_inf] * 1e6, # M.USD to USD
                                         function(x) x * r_HH / sum(r_HH * IND_HH$weight))) * scaler_IND * IND_con_grwth  
rm(r_HH)
gc()

# Get total primary energy for all HHs in two years
list[eHH_IND_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(1:164, 'IND', IND_FD_HH_adj_2011, IND_intensity)
list[eHH_IND_2004, eHH_sd_2004] <-  GetHHSectoralEnergyPerCap(1:164, 'IND2', IND_FD_HH_adj_2004, IND_intensity)

IND_HH_sum_2011 <- GetSummaryForPlot(eHH_IND_2011, IND_FD_HH_adj_2011, "IND", 0)
IND_HH_sum_2004 <- GetSummaryForPlot(eHH_IND_2004, IND_FD_HH_adj_2004, "IND2", 0)

# Get total primary energy for all HHs, but excluding cooking fuel (fuelwood, LPG) and coal
idx_cookingfuel <-  c(153, 157, 162)   
list[eHH_IND_2011_nosolid, eHH_sd_2011_nosolid] <-  GetHHSectoralEnergyPerCap(setdiff(ICP_all_idx,idx_cookingfuel), 'IND', IND_FD_HH_adj_2011, IND_intensity)
list[eHH_IND_2004_nosolid, eHH_sd_2004_nosolid] <-  GetHHSectoralEnergyPerCap(setdiff(ICP_all_idx,idx_cookingfuel), 'IND2', IND_FD_HH_adj_2004, IND_intensity)

IND_HH_sum_2011_nosolid <- GetSummaryForPlot(eHH_IND_2011_nosolid, IND_FD_HH_adj_2011, "IND", idx_cookingfuel)
IND_HH_sum_2004_nosolid <- GetSummaryForPlot(eHH_IND_2004_nosolid, IND_FD_HH_adj_2004, "IND2", idx_cookingfuel)

gini(IND_HH_sum_2011$intensity, IND_HH_sum_2011$weight)
gini(IND_HH_sum_2004$intensity, IND_HH_sum_2004$weight)
gini(IND_HH_sum_2011_nosolid$intensity, IND_HH_sum_2011_nosolid$weight)
gini(IND_HH_sum_2004_nosolid$intensity, IND_HH_sum_2004_nosolid$weight)


steps <- 100
a <- IND_HH %>% arrange(expenditure) %>% filter(!is.na(expenditure)) 
a <- a %>%
  mutate(cumpop = cumsum(weight*hh_size)/sum(a$weight*a$hh_size), 
         centile = cut(cumpop, breaks = seq(0, 1, 1/steps), labels=paste0("centile", 1:steps), include.lowest = TRUE, ordered=TRUE)) %>%
  group_by(centile) %>% summarise(mean_exp = weighted.mean(expenditure, weight, na.rm = TRUE)) %>% 
  mutate(cumexp = cumsum(mean_exp))
b <- IND2_HH %>% arrange(expenditure) %>% filter(!is.na(expenditure)) 
b <- b %>%
  mutate(cumpop = cumsum(weight*hh_size)/sum(b$weight*b$hh_size), 
         centile = cut(cumpop, breaks = seq(0, 1, 1/steps), labels=paste0("centile", 1:steps), include.lowest = TRUE, ordered=TRUE)) %>%
  group_by(centile) %>% summarise(mean_exp = weighted.mean(expenditure, weight, na.rm = TRUE))%>% 
  mutate(cumexp = cumsum(mean_exp))

breaks <- seq(0, 100, 100/steps)[-1]
qplot(breaks, a$cumexp/max(a$cumexp)*100, geom="line", xlab="Cum pop (All India)", ylab="Cum pct exp") 
qplot(breaks, b$cumexp/max(b$cumexp)*100, geom="line", xlab="Cum pop (All India)", ylab="Cum pct exp") 

cum_exp_diff <- (a$cumexp-b$cumexp)/max(a$cumexp-b$cumexp)*100
qplot(breaks, cum_exp_diff, geom="line", xlab="Cum% pop (All India)", ylab="Cum% exp increase") 
GINI_exp_diff <- getGINI(breaks, cum_exp_diff)

getGINI(breaks, a$cumexp/max(a$cumexp)*100)
getGINI(breaks, b$cumexp/max(b$cumexp)*100)



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



max.int <- 250
max.exp <- 30000
# 2011
qplot(totexpense2007MER, intensity, data=IND_HH_sum_2011 %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2011 [USD 2007MER] (Rural India)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=IND_HH_sum_2011_nosolid %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2011 [USD 2007MER] (Rural India)", ylab="MJ/USD w/o solid fuel & LPG", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=IND_HH_sum_2011 %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2011 [USD 2007MER] (Urban India)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=IND_HH_sum_2011_nosolid %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2011 [USD 2007MER] (Urban India)", ylab="MJ/USD w/o solid fuel & LPG", size=I(0.15)) +
  stat_smooth()

#2004
qplot(totexpense2007MER, intensity, data=IND_HH_sum_2004 %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2004 [USD 2007MER] (Rural India)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=IND_HH_sum_2004_nosolid %>% filter(urban==0), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2004 [USD 2007MER] (Rural India)", ylab="MJ/USD w/o solid fuel & LPG", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=IND_HH_sum_2004 %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2004 [USD 2007MER] (Urban India)", ylab="MJ/USD", size=I(0.15)) +
  stat_smooth()
qplot(totexpense2007MER, intensity, data=IND_HH_sum_2004_nosolid %>% filter(urban==1), geom="auto", xlim=c(0,max.exp), ylim=c(0,max.int), 
      xlab="Annual expenditure 2004 [USD 2007MER] (Urban India)", ylab="MJ/USD w/o solid fuel & LPG", size=I(0.15)) +
  stat_smooth()


# India: Sectoral comparison
ICP_food_idx <- 1:45
ICP_hhold_idx <- c(56:84, 138:151)  # Household goods/services
ICP_svc_idx <- 85:137   # Health, Transport, Communication, Recreation
ICP_fuel_idx <- 152:164
ICP_nosolid <- setdiff(152:164, c(153, 157, 162))
ICP_oth_idx <- 46:55

#2011
list[eHH_IND_food_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_food_idx, 'IND', IND_FD_HH_adj_2011, IND_intensity)
list[eHH_IND_hhold_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_hhold_idx, 'IND', IND_FD_HH_adj_2011, IND_intensity)
list[eHH_IND_svc_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_svc_idx, 'IND', IND_FD_HH_adj_2011, IND_intensity)
list[eHH_IND_fuel_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_fuel_idx, 'IND', IND_FD_HH_adj_2011, IND_intensity)
list[eHH_IND_oth_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_oth_idx, 'IND', IND_FD_HH_adj_2011, IND_intensity)

IND_HH_food_2011 <- GetSummaryForPlot(eHH_IND_food_2011, IND_FD_HH_adj_2011, "IND", setdiff(ICP_all_idx,ICP_food_idx)) %>%
  rename(En_food=pri_e_avg, I_food=intensity, Ex_food=expense2007MER)
IND_HH_hhold_2011 <- GetSummaryForPlot(eHH_IND_hhold_2011, IND_FD_HH_adj_2011, "IND", setdiff(ICP_all_idx,ICP_hhold_idx)) %>%
  select(hhid, En_hhold=pri_e_avg, I_hhold=intensity, Ex_hhold=expense2007MER)
IND_HH_svc_2011 <- GetSummaryForPlot(eHH_IND_svc_2011, IND_FD_HH_adj_2011, "IND", setdiff(ICP_all_idx,ICP_svc_idx)) %>%
  select(hhid, En_svc=pri_e_avg, I_svc=intensity, Ex_svc=expense2007MER)
IND_HH_fuel_2011 <- GetSummaryForPlot(eHH_IND_fuel_2011, IND_FD_HH_adj_2011, "IND", setdiff(ICP_all_idx,ICP_fuel_idx)) %>%
  select(hhid, En_fuel=pri_e_avg, I_fuel=intensity, Ex_fuel=expense2007MER)
IND_HH_oth_2011 <- GetSummaryForPlot(eHH_IND_oth_2011, IND_FD_HH_adj_2011, "IND", setdiff(ICP_all_idx,ICP_oth_idx)) %>%
  select(hhid, En_oth=pri_e_avg, I_oth=intensity, Ex_oth=expense2007MER)
IND_HH_sum_sector_2011_nosolid <- IND_HH_food_2011 %>% left_join(IND_HH_hhold_2011) %>% 
  left_join(IND_HH_svc_2011) %>% left_join(IND_HH_nosolid_2011) %>% left_join(IND_HH_oth_2011) %>%
  mutate(En_pri.tot = En_food+En_hhold+En_svc+En_nosol+En_oth)
# IND_HH_sum_sector_2011 <- IND_HH_food_2011 %>% left_join(IND_HH_hhold_2011) %>% 
# left_join(IND_HH_svc_2011) %>% left_join(IND_HH_fuel_2011) %>% left_join(IND_HH_oth_2011) %>%
# mutate(En_pri.tot = En_food+En_hhold+En_svc+En_fuel+En_oth)

list[eHH_IND_nosolid_2011, eHH_sd_2011] <-  GetHHSectoralEnergyPerCap(ICP_nosolid, 'IND', IND_FD_HH_adj_2011, IND_intensity)
IND_HH_nosolid_2011 <- GetSummaryForPlot(eHH_IND_nosolid_2011, IND_FD_HH_adj_2011, "IND", setdiff(ICP_all_idx,ICP_nosolid)) %>%
  select(hhid, En_nosol=pri_e_avg, I_nosol=intensity, Ex_nosol=expense2007MER)

IND_ene_2011 <- IND_HH_sum_sector_2011 %>% group_by(decile) %>%
  summarise_at(vars(starts_with("En_")), sum) %>%
  mutate_at(2:6, funs("pct" = ./En_pri.tot))
IND_ene_2011_nosolid <- IND_HH_sum_sector_2011_nosolid %>% group_by(decile) %>%
  summarise_at(vars(starts_with("En_")), sum) %>%
  mutate_at(2:6, funs("pct" = ./En_pri.tot))

#2004
list[eHH_IND_food_2004, eHH_sd_2004] <-  GetHHSectoralEnergyPerCap(ICP_food_idx, 'IND2', IND_FD_HH_adj_2004, IND_intensity)
list[eHH_IND_hhold_2004, eHH_sd_2004] <-  GetHHSectoralEnergyPerCap(ICP_hhold_idx, 'IND2', IND_FD_HH_adj_2004, IND_intensity)
list[eHH_IND_svc_2004, eHH_sd_2004] <-  GetHHSectoralEnergyPerCap(ICP_svc_idx, 'IND2', IND_FD_HH_adj_2004, IND_intensity)
list[eHH_IND_fuel_2004, eHH_sd_2004] <-  GetHHSectoralEnergyPerCap(ICP_fuel_idx, 'IND2', IND_FD_HH_adj_2004, IND_intensity)
list[eHH_IND_oth_2004, eHH_sd_2004] <-  GetHHSectoralEnergyPerCap(ICP_oth_idx, 'IND2', IND_FD_HH_adj_2004, IND_intensity)

IND_HH_food_2004 <- GetSummaryForPlot(eHH_IND_food_2004, IND_FD_HH_adj_2004, "IND2", setdiff(ICP_all_idx,ICP_food_idx)) %>%
  rename(En_food=pri_e_avg, I_food=intensity, Ex_food=expense2007MER)
IND_HH_hhold_2004 <- GetSummaryForPlot(eHH_IND_hhold_2004, IND_FD_HH_adj_2004, "IND2", setdiff(ICP_all_idx,ICP_hhold_idx)) %>%
  select(hhid, En_hhold=pri_e_avg, I_hhold=intensity, Ex_hhold=expense2007MER)
IND_HH_svc_2004 <- GetSummaryForPlot(eHH_IND_svc_2004, IND_FD_HH_adj_2004, "IND2", setdiff(ICP_all_idx,ICP_svc_idx)) %>%
  select(hhid, En_svc=pri_e_avg, I_svc=intensity, Ex_svc=expense2007MER)
IND_HH_fuel_2004 <- GetSummaryForPlot(eHH_IND_fuel_2004, IND_FD_HH_adj_2004, "IND2", setdiff(ICP_all_idx,ICP_fuel_idx)) %>%
  select(hhid, En_fuel=pri_e_avg, I_fuel=intensity, Ex_fuel=expense2007MER)
IND_HH_oth_2004 <- GetSummaryForPlot(eHH_IND_oth_2004, IND_FD_HH_adj_2004, "IND2", setdiff(ICP_all_idx,ICP_oth_idx)) %>%
  select(hhid, En_oth=pri_e_avg, I_oth=intensity, Ex_oth=expense2007MER)
IND_HH_sum_sector_2004 <- IND_HH_food_2004 %>% left_join(IND_HH_hhold_2004) %>% 
  left_join(IND_HH_svc_2004) %>% left_join(IND_HH_fuel_2004) %>% left_join(IND_HH_oth_2004) %>%
  mutate(En_pri.tot = En_food+En_hhold+En_svc+En_fuel+En_oth)
IND_ene_2004 <- IND_HH_sum_sector_2004 %>% group_by(decile) %>%
  summarise_at(vars(starts_with("En_")), sum) %>%
  mutate_at(2:6, funs("pct" = ./En_pri.tot))

write.table(IND_ene_2011_nosolid, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
write.table(IND_ene_2011, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
write.table(IND_ene_2004, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

rm(eHH_IND_food_2011, eHH_IND_hhold_2011, eHH_IND_svc_2011, eHH_IND_fuel_2011, 
   eHH_IND_food_2004, eHH_IND_hhold_2004, eHH_IND_svc_2004, eHH_IND_fuel_2004); gc()





# Check consumption vs expenditure
IND_tempfood <- selectDBdata(ID, ITEM, VAL_TOT, VAL_OWN, VAL_FREE, VAL_BAR, tables="IND1_FOOD") %>% data.table(key="id")
IND_tempfood %>% filter(val_own>16000)
IND_tempfood %>% filter(val_own>10000)
# For milk and rice, val_own is larger than val_tot

