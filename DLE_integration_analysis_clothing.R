
################################
### Clothing (incl. shoes)
################################

load(file="./Saved tables/BRA.tfei.icp.Rda")
load(file="./Saved tables/IND.tfei.icp.Rda")
load(file="./Saved tables/ZAF.tfei.icp.Rda")

load(file="./Saved tables/BRA.tfei.icp.elec.Rda")
load(file="./Saved tables/IND.tfei.icp.elec.Rda")
load(file="./Saved tables/ZAF.tfei.icp.elec.Rda")

load(file="./Saved tables/BRA.tfei.icp.non.elec.Rda")
load(file="./Saved tables/IND.tfei.icp.non.elec.Rda")
load(file="./Saved tables/ZAF.tfei.icp.non.elec.Rda")



### Load more data for Analysis

# Load IND_OTH_Alldata
load(file="./Saved tables/IND1_OTH_ALLdata.Rda")
IND_clothing <- IND_OTH_Alldata %>% filter(code >=350 & code <=376)
IND_footwear <- IND_OTH_Alldata %>% filter(code >=390 & code <=395)

# Load ZAF_OTH.raw
load(file="./Saved tables/ZAF_OTH_All.Rda")
ZAF_clothing <- ZAF_OTH.raw %>% mutate(code=as.numeric(code)) %>% filter(code >=3111100 & code <=3141200)
ZAF_footwear <- ZAF_OTH.raw %>% mutate(code=as.numeric(code)) %>% filter(code >=3211001 & code <=3213500)

# BRA_OTH_ALLdata
BRA_OTH_ALLdata <- selectDBdata(tables='BRA0_OTHCON') %>% left_join(data.frame(code=1:164, item=ICP_catnames))
load(file="./Saved tables/BRA_HH.Rda")
BRA_clothing <- BRA_OTH_ALLdata %>% filter(code >=48 & code <=52) 
BRA_footwear <- BRA_OTH_ALLdata %>% filter(code >=53 & code <=55) 





### 1. Total TEnergy for clothing

# Approach 1. Based on TPEI (either ICP or EXIO) and original expenditure from survey (EJ)
# ICP 50, 51, 54: only for purchases and no repairs (repair expenditure observations are not consistent across countries)
# I will assume just e
idx.clothing.exio <- 56:57 #55:57  # No textile
idx.clothing.icp <- c(50, 51)  # 50:55
idx.footwear.icp <- 54  # 50:55

# Average intensity (MJ/USD) - Not used
weighted.mean(colMeans(BRA.tfei.icp[, idx.clothing.icp]), weight=BRA_FD_ICP_usd2007[idx.clothing.icp, 1])
weighted.mean(colMeans(IND.tfei.icp[, idx.clothing.icp]), weight=IND_FD_ICP_usd2007[idx.clothing.icp, 1])
weighted.mean(colMeans(ZAF.tfei.icp[, idx.clothing.icp]), weight=ZAF_FD_ICP_usd2007[idx.clothing.icp, 1])

# Total energy/year (EJ/year) - Good to be based on household consumption (ICP)
tot.clothing.BRA <- rowSums(BRA.tfei.icp[, idx.clothing.icp] %*% diag(BRA_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6 / scaler_BRA
tot.clothing.IND <- rowSums(IND.tfei.icp[, idx.clothing.icp] %*% diag(IND_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6 / scaler_IND
tot.clothing.ZAF <- rowSums(ZAF.tfei.icp[, idx.clothing.icp] %*% diag(ZAF_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6 / scaler_ZAF

tot.footwear.BRA <- (BRA.tfei.icp[, idx.footwear.icp] * BRA_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6 / scaler_BRA
tot.footwear.IND <- (IND.tfei.icp[, idx.footwear.icp] * IND_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6 / scaler_IND
tot.footwear.ZAF <- (ZAF.tfei.icp[, idx.footwear.icp] * ZAF_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6 / scaler_ZAF

tot.clothing.elec.BRA <- rowSums(BRA.tfei.icp.elec[, idx.clothing.icp] %*% diag(BRA_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6 / scaler_BRA
tot.clothing.elec.IND <- rowSums(IND.tfei.icp.elec[, idx.clothing.icp] %*% diag(IND_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6 / scaler_IND
tot.clothing.elec.ZAF <- rowSums(ZAF.tfei.icp.elec[, idx.clothing.icp] %*% diag(ZAF_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6 / scaler_ZAF

tot.footwear.elec.BRA <- (BRA.tfei.icp.elec[, idx.footwear.icp] * BRA_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6 / scaler_BRA
tot.footwear.elec.IND <- (IND.tfei.icp.elec[, idx.footwear.icp] * IND_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6 / scaler_IND
tot.footwear.elec.ZAF <- (ZAF.tfei.icp.elec[, idx.footwear.icp] * ZAF_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6 / scaler_ZAF

tot.clothing.non.elec.BRA <- rowSums(BRA.tfei.icp.non.elec[, idx.clothing.icp] %*% diag(BRA_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6 / scaler_BRA
tot.clothing.non.elec.IND <- rowSums(IND.tfei.icp.non.elec[, idx.clothing.icp] %*% diag(IND_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6 / scaler_IND
tot.clothing.non.elec.ZAF <- rowSums(ZAF.tfei.icp.non.elec[, idx.clothing.icp] %*% diag(ZAF_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6 / scaler_ZAF

tot.footwear.non.elec.BRA <- (BRA.tfei.icp.non.elec[, idx.footwear.icp] * BRA_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6 / scaler_BRA
tot.footwear.non.elec.IND <- (IND.tfei.icp.non.elec[, idx.footwear.icp] * IND_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6 / scaler_IND
tot.footwear.non.elec.ZAF <- (ZAF.tfei.icp.non.elec[, idx.footwear.icp] * ZAF_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6 / scaler_ZAF


      # # Approach 2. Based on TPEI (.use) and individual hh expenditure (GJ/cap) - Better! But not used for simple integration
      # # Apply both FC_ICP: Original and adjusted
      # # Take the min and max from the two results for the DLE comparison
      # load("./Saved tables/BRA_intensities_val_BRA.use.Rda") #BRA_intensity.use
      # load("./Saved tables/BRA_FD_ICP_HH_adj_BR.Rda") #BRA_FD_ICP_HH_adj_BR
      # load("./Saved tables/BRA_FD_harmonized.Rda") #BRA_FD_ICP_AllHH
      # list[all_HH_c_BR, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity.use) # more reliable than IND_intensity
      # list[all_HH_c_BR, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'BRA', BRA_FD_ICP_HH_adj_BR, BRA.tfei.icp) # more reliable than IND_intensity
      # (SummarizeGJPerCap(all_HH_c_BR) * as.numeric(BRA_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)
      # list[all_HH_c_BR, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'BRA', BRA_FD_ICP_AllHH, BRA_intensity.use) # more reliable than IND_intensity
      # (SummarizeGJPerCap(all_HH_c_BR) * as.numeric(BRA_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)
      # 
      # load("./Saved tables/IND_intensities.use.Rda")
      # load("./Saved tables/IND_FD_ICP_HH_adj.Rda")
      # load("./Saved tables/IND_FD_harmonized.Rda") #IND_FD_ICP_AllHH
      # list[all_HH_c_IN, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'IND', IND_FD_ICP_HH_adj, IND_intensity.use) # more reliable than IND_intensity
      # (SummarizeGJPerCap(all_HH_c_IN) * as.numeric(IND_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)
      # list[all_HH_c_IN, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'IND', IND_FD_ICP_AllHH, IND_intensity.use) # more reliable than IND_intensity
      # (SummarizeGJPerCap(all_HH_c_IN) * as.numeric(IND_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)
      # 
      # load("./Saved tables/ZAF_intensities.use.Rda")
      # load("./Saved tables/ZAF_FD_ICP_HH_adj.Rda")
      # load("./Saved tables/ZAF_FD_harmonized.Rda") #ZAF_FD_ICP_AllHH
      # list[all_HH_c_ZA, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'ZAF', ZAF_FD_ICP_HH_adj, ZAF_intensity.use) # more reliable than IND_intensity
      # (SummarizeGJPerCap(all_HH_c_ZA) * as.numeric(ZAF_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)
      # list[all_HH_c_ZA, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'ZAF', ZAF_FD_ICP_AllHH, ZAF_intensity.use) # more reliable than IND_intensity
      # (SummarizeGJPerCap(all_HH_c_ZA) * as.numeric(ZAF_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)
      # 
      # rm(all_HH_c_BR, all_HH_c_IN, all_HH_c_ZA)
      # gc()




### 2. Deriving DLE threshold [kg/cap] for Clothing (and average too) 

# Derive from India data and apply the same $/kg (IND) to get kg/cap in BRA and ZAF

datapath <- "C:/Users/min/IIASA/DLE - Documents/WS3 - Documents/Trajectories/Clothing/"
weight.clothing <- read_xlsx(paste0(datapath, "Weights-IND.xlsx"), range="A1:J31") %>% select(CODE, `g/each - MIN`, `g/each - MAX`) %>% 
  rename(weight.min=`g/each - MIN`, weight.max=`g/each - MAX`, code=CODE)


### India
# No quantity for some items (code=372, 356, 357, 368, 375) -> Assume same avg price as 'shirts, T-shirts' (for decile1) (code=363)
price.shirt <- as.numeric(IND_clothing %>% filter(code==363) %>% mutate(price=val_tot/qty_tot) %>% 
                            left_join(IND_HH,  by = c("id" = "hhid")) %>% filter(decile=="decile1") %>%
                            summarise(price.shirt=weighted.mean(price, weight, na.rm=T)))

# Get total weight by hh in gram (and val_tot too)
clothing.all <- IND_clothing %>% inner_join(weight.clothing) %>% 
  mutate_cond(code %in% c(372, 356, 357, 368, 375), qty_tot=val_tot/price.shirt) %>% # fill in qty for no-price items
  mutate(weight.tot=qty_tot*weight.min, weight.tot.max=qty_tot*weight.max) %>% 
  mutate_cond(code==374, weight.tot=qty_tot) %>% # knitting wool
  group_by(id) %>% summarise(val_tot=sum(val_tot, na.rm=TRUE), weight.tot=sum(weight.tot, na.rm=TRUE), weight.tot.max=sum(weight.tot.max, na.rm=TRUE)) %>% 
  left_join(IND_HH, by = c("id" = "hhid")) 

# Get total weight by hh in gram (and val_tot too)
footwear.all <- IND_footwear %>% inner_join(weight.clothing) %>% 
  mutate(weight.tot=qty_tot*weight.min, weight.tot.max=qty_tot*weight.max) %>% 
  group_by(id) %>% summarise(val_tot=sum(val_tot, na.rm=TRUE), weight.tot=sum(weight.tot, na.rm=TRUE), weight.tot.max=sum(weight.tot.max, na.rm=TRUE)) %>% 
  left_join(IND_HH, by = c("id" = "hhid")) 


# DLE threshold [kg/cap] (g to kg) - only Flow
dle.clothing.pcap.IND <- clothing.all %>% summarise(median = weighted.median(weight.tot/hh_size, weight, na.rm=TRUE)/1000) %>% as.numeric() # 1.3 kg/cap
dle.footwear.pcap.IND <- footwear.all %>% summarise(median = weighted.median(weight.tot/hh_size, weight, na.rm=TRUE)/1000) %>% as.numeric()  # 0.9 kg/cap
# avg.clothing.pcap.IND <- clothing.all %>% summarise(mean = weighted.mean(weight.tot/hh_size, weight, na.rm=TRUE)/1000) %>% as.numeric() # 1.3 kg/cap
# avg.footwear.pcap.IND <- footwear.all %>% summarise(mean = weighted.mean(weight.tot/hh_size, weight, na.rm=TRUE)/1000) %>% as.numeric()  # 0.9 kg/cap
avg.clothing.pcap.IND <- clothing.all %>% 
  summarise(val_tot=sum(weight.tot*weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_tot/IND_pop_2007/1000) %>%
  select(kg.pcap) %>% as.numeric()
avg.footwear.pcap.IND <- footwear.all %>% 
  summarise(val_tot=sum(weight.tot*weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_tot/IND_pop_2007/1000) %>%
  select(kg.pcap) %>% as.numeric()

# Mean expenditure $/cap - IND - Not used
exp.clothing.pcap.IND <- clothing.all %>% summarise(median = weighted.median(val_tot/hh_size, weight, na.rm=TRUE)) %>% as.numeric() 
exp.footwear.pcap.IND <- footwear.all %>% summarise(median = weighted.median(val_tot/hh_size, weight, na.rm=TRUE)) %>% as.numeric() 

kg.pcap.dec <- clothing.all %>% group_by(decile) %>% summarise(clothing.per.cap = sum(weight.tot*weight, na.rm=TRUE) / sum(hh_size*weight, na.rm=TRUE) / 1000) 
kg.pcap.dec.max <- clothing.all %>% group_by(decile) %>% summarise(clothing.per.cap = sum(weight.tot.max*weight, na.rm=TRUE) / sum(hh_size*weight, na.rm=TRUE) / 1000) 

# Average price [$/kg] 2010USD ppp nationwide - Assume this ppp is same in all three countries
avg.price.clothing <- as.numeric(clothing.all %>% 
                                   summarise(avg.price = sum(val_tot*weight, na.rm=TRUE) / sum(weight.tot*weight, na.rm=TRUE) * 1000))
avg.price.footwear <- as.numeric(footwear.all %>% 
                                   summarise(avg.price = sum(val_tot*weight, na.rm=TRUE) / sum(weight.tot*weight, na.rm=TRUE) * 1000))
# Price per decile
avg.price.clothing.dec <- clothing.all %>% group_by(decile) %>%
                                   summarise(avg.price = sum(val_tot*weight, na.rm=TRUE) / sum(weight.tot*weight, na.rm=TRUE) * 1000)
avg.price.footwear.dec <- footwear.all %>% group_by(decile) %>%
  summarise(avg.price = sum(val_tot*weight, na.rm=TRUE) / sum(weight.tot*weight, na.rm=TRUE) * 1000)


### ZAF [kg/cap]

ZAF_clothing.all <- ZAF_clothing %>% left_join(ZAF_HH, by = c("id" = "hhid"))
ZAF_footwear.all <- ZAF_footwear %>% left_join(ZAF_HH, by = c("id" = "hhid"))

dle.clothing.pcap.ZAF <- ZAF_clothing.all %>%
  summarise(val_median=weighted.median(val_tot/hh_size, weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_median/avg.price.clothing) %>% 
  select(kg.pcap) %>% as.numeric()
dle.footwear.pcap.ZAF <- ZAF_footwear.all %>%
  summarise(val_median=weighted.median(val_tot/hh_size, weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_median/avg.price.footwear) %>% 
  select(kg.pcap) %>% as.numeric()

avg.clothing.pcap.ZAF <- ZAF_clothing.all %>%
  summarise(val_tot=sum(val_tot*weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_tot/ZAF_pop_2007/avg.price.clothing) %>%
  select(kg.pcap) %>% as.numeric()
avg.footwear.pcap.ZAF <- ZAF_footwear.all %>%
  summarise(val_tot=sum(val_tot*weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_tot/ZAF_pop_2007/avg.price.footwear) %>%
  select(kg.pcap) %>% as.numeric()

# Mean expenditure $ - ZAF
exp.clothing.pcap.ZAF <- ZAF_clothing.all %>%
  summarise(val_mean=weighted.median(val_tot/hh_size, weight, na.rm=TRUE))
exp.footwear.pcap.ZAF <- ZAF_footwear.all %>%
  summarise(val_mean=weighted.median(val_tot/hh_size, weight, na.rm=TRUE))




### BRA [kg/cap]

BRA_clothing.all <- BRA_clothing %>% left_join(BRA_HH, by = c("id" = "hhid"))
BRA_footwear.all <- BRA_footwear %>% left_join(BRA_HH, by = c("id" = "hhid"))

dle.clothing.pcap.BRA <- BRA_clothing.all %>%
  summarise(val_median=weighted.median(val_tot/hh_size, weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_median/avg.price.clothing) %>% 
  select(kg.pcap) %>% as.numeric()
dle.footwear.pcap.BRA <- BRA_footwear.all %>%
  summarise(val_median=weighted.median(val_tot/hh_size, weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_median/avg.price.footwear) %>% 
  select(kg.pcap) %>% as.numeric()

avg.clothing.pcap.BRA <- BRA_clothing %>% left_join(BRA_HH, by = c("id" = "hhid")) %>%
  summarise(val_tot=sum(val_tot*weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_tot/BRA_pop_2007/avg.price.clothing) %>%
  select(kg.pcap) %>% as.numeric()
avg.footwear.pcap.BRA <- BRA_footwear %>% left_join(BRA_HH, by = c("id" = "hhid")) %>%
  summarise(val_tot=sum(val_tot*weight, na.rm=TRUE)) %>% mutate(kg.pcap=val_tot/BRA_pop_2007/avg.price.footwear) %>%
  select(kg.pcap) %>% as.numeric()

# Mean expenditure $ - BRA
exp.clothing.pcap.BRA <- BRA_clothing.all %>%
  summarise(val_mean=weighted.mean(val_tot/hh_size, weight, na.rm=TRUE))
exp.footwear.pcap.BRA <- BRA_footwear.all %>%
  summarise(val_mean=weighted.mean(val_tot/hh_size, weight, na.rm=TRUE))



### Derive total kg of Clothing (to get MJ/kg)

tot.clothing.kg.IND <- avg.clothing.pcap.IND *IND_pop_2007
tot.footwear.kg.IND <- avg.footwear.pcap.IND *IND_pop_2007
tot.dle.clothing.kg.IND <- dle.clothing.pcap.IND *IND_pop_2007
tot.dle.footwear.kg.IND <- dle.footwear.pcap.IND *IND_pop_2007

tot.clothing.kg.ZAF <- avg.clothing.pcap.ZAF *ZAF_pop_2007
tot.footwear.kg.ZAF <- avg.footwear.pcap.ZAF *ZAF_pop_2007
tot.dle.clothing.kg.ZAF <- dle.clothing.pcap.ZAF *ZAF_pop_2007
tot.dle.footwear.kg.ZAF <- dle.footwear.pcap.ZAF *ZAF_pop_2007

tot.clothing.kg.BRA <- avg.clothing.pcap.BRA *BRA_pop_2007
tot.footwear.kg.BRA <- avg.footwear.pcap.BRA *BRA_pop_2007
tot.dle.clothing.kg.BRA <- dle.clothing.pcap.BRA *BRA_pop_2007
tot.dle.footwear.kg.BRA <- dle.footwear.pcap.BRA *BRA_pop_2007





clothing.lifetime <- 3 # https://eastonstewartsvilledrycleaner36.wordpress.com/2010/11/30/the-average-life-expectancy-of-clothing/
footwear.lifetime <- 3




### Derive TFEI (MJ/kg) by country

tfei.clothing <- list(IND=mean(tot.clothing.IND)*1e12/tot.clothing.kg.IND,  # [MJ/kg]
                      BRA=mean(tot.clothing.BRA)*1e12/tot.clothing.kg.BRA,  # [MJ/kg]
                      ZAF=mean(tot.clothing.ZAF)*1e12/tot.clothing.kg.ZAF)  # [MJ/kg]
tfei.footwear <- list(IND=mean(tot.footwear.IND)*1e12/tot.footwear.kg.IND,  # [MJ/kg]
                      BRA=mean(tot.footwear.BRA)*1e12/tot.footwear.kg.BRA,  # [MJ/kg]
                      ZAF=mean(tot.footwear.ZAF)*1e12/tot.footwear.kg.ZAF)  # [MJ/kg]
tfei.clothing.all <- list(IND=(mean(tot.footwear.IND)+mean(tot.clothing.IND))*1e12/(tot.clothing.kg.IND+tot.footwear.kg.IND), 
                       BRA=(mean(tot.footwear.BRA)+mean(tot.clothing.BRA))*1e12/(tot.footwear.kg.BRA+tot.clothing.kg.BRA),  # 
                       ZAF=(mean(tot.footwear.ZAF)+mean(tot.clothing.ZAF))*1e12/(tot.footwear.kg.ZAF+tot.clothing.kg.ZAF))  # [MJ/kg]

tfei.elec.clothing <- list(IND=mean(tot.clothing.elec.IND)*1e12/tot.clothing.kg.IND,  # [MJ/kg]
                      BRA=mean(tot.clothing.elec.BRA)*1e12/tot.clothing.kg.BRA,  # [MJ/kg]
                      ZAF=mean(tot.clothing.elec.ZAF)*1e12/tot.clothing.kg.ZAF)  # [MJ/kg]
tfei.elec.footwear <- list(IND=mean(tot.footwear.elec.IND)*1e12/tot.footwear.kg.IND,  # [MJ/kg]
                      BRA=mean(tot.footwear.elec.BRA)*1e12/tot.footwear.kg.BRA,  # [MJ/kg]
                      ZAF=mean(tot.footwear.elec.ZAF)*1e12/tot.footwear.kg.ZAF)  # [MJ/kg]
tfei.elec.clothing.all <- list(IND=(mean(tot.footwear.elec.IND)+mean(tot.clothing.elec.IND))*1e12/(tot.clothing.kg.IND+tot.footwear.kg.IND), 
                          BRA=(mean(tot.footwear.elec.ZAF)+mean(tot.clothing.elec.BRA))*1e12/(tot.footwear.kg.BRA+tot.clothing.kg.BRA),  # 
                          ZAF=(mean(tot.footwear.ZAF)+mean(tot.clothing.elec.ZAF))*1e12/(tot.footwear.kg.ZAF+tot.clothing.kg.ZAF))  # [MJ/kg]

tfei.non.elec.clothing <- as.list(unlist(tfei.clothing) - unlist(tfei.elec.clothing))
tfei.non.elec.footwear <- as.list(unlist(tfei.footwear) - unlist(tfei.elec.footwear))
tfei.non.elec.clothing.all <- as.list(unlist(tfei.clothing.all) - unlist(tfei.elec.clothing.all))


# 1. Based on the avg price ($/kg) from India
unit.clothing <- list(IND=dle.clothing.pcap.IND,  # [kg/cap]
                      BRA=dle.clothing.pcap.BRA,  
                      ZAF=dle.clothing.pcap.ZAF)
unit.footwear <- list(IND=dle.footwear.pcap.IND, 
                      BRA=dle.footwear.pcap.BRA,  
                      ZAF=dle.footwear.pcap.ZAF) 

# 2. Based on the kg-HDD relationship from Ed's data
unit.clothing <- list(IND=dle.clothing.pcap.IND,  # [kg/cap]
                      BRA=dle.clothing.pcap.BRA.HDD,  
                      ZAF=dle.clothing.pcap.ZAF.HDD)
unit.footwear <- list(IND=dle.footwear.pcap.IND, 
                      BRA=dle.footwear.pcap.BRA.HDD,  
                      ZAF=dle.footwear.pcap.ZAF.HDD) 


unit.clothing.all <- as.list(unlist(unit.clothing) + unlist(unit.footwear))

# Uncertainty
# tfei.clothing.range <- list(IND.min=min(tot.clothing.IND)*1e12/tot.clothing.kg.IND,  # [MJ/kg]
#                             IND.max=max(tot.clothing.IND)*1e12/tot.clothing.kg.IND,  # [MJ/kg]
#                             BRA.min=min(tot.clothing.BRA)*1e12/tot.clothing.kg.BRA,  # [MJ/kg]
#                             BRA.max=max(tot.clothing.BRA)*1e12/tot.clothing.kg.BRA,  # [MJ/kg]
#                             ZAF.min=min(tot.clothing.ZAF)*1e12/tot.clothing.kg.ZAF,
#                             ZAF.max=max(tot.clothing.ZAF)*1e12/tot.clothing.kg.ZAF)  # [MJ/kg]
# tfei.footwear.range <- list(IND.min=min(tot.footwear.IND)*1e12/tot.footwear.kg.IND,  # [MJ/kg]
#                             IND.max=max(tot.footwear.IND)*1e12/tot.footwear.kg.IND,
#                             BRA.min=min(tot.footwear.BRA)*1e12/tot.footwear.kg.BRA,  # [MJ/kg]
#                             BRA.max=max(tot.footwear.BRA)*1e12/tot.footwear.kg.BRA,  # [MJ/kg]
#                             ZAF.min=min(tot.footwear.ZAF)*1e12/tot.footwear.kg.ZAF,
#                             ZAF.max=max(tot.footwear.ZAF)*1e12/tot.footwear.kg.ZAF)  # [MJ/kg]
tfei.all.range <- list(IND.min=(min(tot.footwear.IND)+min(tot.clothing.IND))*1e12/(tot.clothing.kg.IND+tot.footwear.kg.IND),  # [MJ/kg]
                       IND.avg=(mean(tot.footwear.IND)+mean(tot.clothing.IND))*1e12/(tot.clothing.kg.IND+tot.footwear.kg.IND), 
                       IND.max=(max(tot.footwear.IND)+max(tot.clothing.IND))*1e12/(tot.clothing.kg.IND+tot.footwear.kg.IND), 
                       BRA.min=(min(tot.footwear.BRA)+min(tot.clothing.BRA))*1e12/(tot.footwear.kg.BRA+tot.clothing.kg.BRA),  # [MJ/kg]
                       BRA.avg=(mean(tot.footwear.BRA)+mean(tot.clothing.BRA))*1e12/(tot.footwear.kg.BRA+tot.clothing.kg.BRA),  # 
                       BRA.max=(max(tot.footwear.BRA)+max(tot.clothing.BRA))*1e12/(tot.footwear.kg.BRA+tot.clothing.kg.BRA),  # 
                       ZAF.min=(min(tot.footwear.ZAF)+min(tot.clothing.ZAF))*1e12/(tot.footwear.kg.ZAF+tot.clothing.kg.ZAF),
                       ZAF.avg=(mean(tot.footwear.ZAF)+mean(tot.clothing.ZAF))*1e12/(tot.footwear.kg.ZAF+tot.clothing.kg.ZAF),
                       ZAF.max=(max(tot.footwear.ZAF)+max(tot.clothing.ZAF))*1e12/(tot.footwear.kg.ZAF+tot.clothing.kg.ZAF))  # [MJ/kg]
tfei.elec.all.range <- list(IND.min=(min(tot.footwear.elec.IND)+min(tot.clothing.elec.IND))*1e12/(tot.clothing.kg.IND+tot.footwear.kg.IND),  # [MJ/kg]
                           IND.avg=(mean(tot.footwear.elec.IND)+mean(tot.clothing.elec.IND))*1e12/(tot.clothing.kg.IND+tot.footwear.kg.IND), 
                           IND.max=(max(tot.footwear.elec.IND)+max(tot.clothing.elec.IND))*1e12/(tot.clothing.kg.IND+tot.footwear.kg.IND), 
                           BRA.min=(min(tot.footwear.elec.BRA)+min(tot.clothing.elec.BRA))*1e12/(tot.footwear.kg.BRA+tot.clothing.kg.BRA),  # [MJ/kg]
                           BRA.avg=(mean(tot.footwear.elec.BRA)+mean(tot.clothing.elec.BRA))*1e12/(tot.footwear.kg.BRA+tot.clothing.kg.BRA),  # 
                           BRA.max=(max(tot.footwear.elec.BRA)+max(tot.clothing.elec.BRA))*1e12/(tot.footwear.kg.BRA+tot.clothing.kg.BRA),  # 
                           ZAF.min=(min(tot.footwear.elec.ZAF)+min(tot.clothing.elec.ZAF))*1e12/(tot.footwear.kg.ZAF+tot.clothing.kg.ZAF),
                           ZAF.avg=(mean(tot.footwear.elec.ZAF)+mean(tot.clothing.elec.ZAF))*1e12/(tot.footwear.kg.ZAF+tot.clothing.kg.ZAF),
                           ZAF.max=(max(tot.footwear.elec.ZAF)+max(tot.clothing.elec.ZAF))*1e12/(tot.footwear.kg.ZAF+tot.clothing.kg.ZAF))  # [MJ/kg]
tfei.non.elec.all.range <- list(IND.min=(min(tot.footwear.non.elec.IND)+min(tot.clothing.non.elec.IND))*1e12/(tot.clothing.kg.IND+tot.footwear.kg.IND),  # [MJ/kg]
                                IND.avg=(mean(tot.footwear.non.elec.IND)+mean(tot.clothing.non.elec.IND))*1e12/(tot.clothing.kg.IND+tot.footwear.kg.IND), 
                                IND.max=(max(tot.footwear.non.elec.IND)+max(tot.clothing.non.elec.IND))*1e12/(tot.clothing.kg.IND+tot.footwear.kg.IND), 
                                BRA.min=(min(tot.footwear.non.elec.BRA)+min(tot.clothing.non.elec.BRA))*1e12/(tot.footwear.kg.BRA+tot.clothing.kg.BRA),  # [MJ/kg]
                                BRA.avg=(mean(tot.footwear.non.elec.BRA)+mean(tot.clothing.non.elec.BRA))*1e12/(tot.footwear.kg.BRA+tot.clothing.kg.BRA),  # 
                                BRA.max=(max(tot.footwear.non.elec.BRA)+max(tot.clothing.non.elec.BRA))*1e12/(tot.footwear.kg.BRA+tot.clothing.kg.BRA),  # 
                                ZAF.min=(min(tot.footwear.non.elec.ZAF)+min(tot.clothing.non.elec.ZAF))*1e12/(tot.footwear.kg.ZAF+tot.clothing.kg.ZAF),
                                ZAF.avg=(mean(tot.footwear.non.elec.ZAF)+mean(tot.clothing.non.elec.ZAF))*1e12/(tot.footwear.kg.ZAF+tot.clothing.kg.ZAF),
                                ZAF.max=(max(tot.footwear.non.elec.ZAF)+max(tot.clothing.non.elec.ZAF))*1e12/(tot.footwear.kg.ZAF+tot.clothing.kg.ZAF))  # [MJ/kg]
write.table(t(tfei.elec.all.range), "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
write.table(t(tfei.non.elec.all.range), "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
