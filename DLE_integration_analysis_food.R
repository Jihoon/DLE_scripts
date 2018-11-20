# IND.food.tfei has annual kcal intake
load(file=paste0(getwd(), "/Saved tables/IND.food.tfei.Rda"))
# Why does this give so big EJ for total energy (>2 EJ), even with a subset of HHs, while tot.food.IND is around 0.6 EJ?
# Reason not known yet.
# For now, let's scale tot.food.IND with IND.food.tfei$EJ

load(file="./Saved tables/BRA.tfei.icp.Rda")
load(file="./Saved tables/IND.tfei.icp.Rda")
load(file="./Saved tables/ZAF.tfei.icp.Rda")

# kcal.proj has daily kcal intake.
kcal.proj = read_excel("../DLE trajectory/Data_FAO_2012_BICS_Narasimha.xlsx")
kcal.proj = kcal.proj %>% filter(Var %in% c("calTot", "pop")) %>% filter(Region!="CHN") %>%
  spread(Var, Val) %>% mutate(AnnTotCal=pop*calTot*365) %>% select(-Sector) # TotCal in 1e6 kcal
kcal.ssp2 = read_excel("../DLE trajectory/SSP_projection_Valin_BICS_2015.xlsx",
                      sheet="Data") %>% 
  filter(SSP=="SSP2" & Reg!="ChinaReg" & Foodtype=="TOTAL - unscaled") %>%
  select(-Diet_type, -Foodtype) %>% filter(Year >=2010 & Year <=2050) %>% 
  mutate(Reg=gsub("Reg", "", Reg)) %>% rename(Region=Reg) %>% 
  mutate(Region=gsub("SouthAfr", "ZAF", Region)) %>% 
  mutate(Region=gsub("India", "IND", Region)) %>% 
  mutate(Region=gsub("Brazil", "BRA", Region)) 


# Total energy/year (EJ/year) - Good to be based on household consumption (ICP)
idx.food.icp <- 1:45


# Approach #1
# in [EJ] = 1e12 MJ
tot.food.BRA <- mean(rowSums(BRA.tfei.icp[, idx.food.icp] %*% diag(BRA_FD_ICP_usd2007[idx.food.icp, 1]))) / 1e6 / scaler_BRA
tot.food.IND <- mean(rowSums(IND.tfei.icp[, idx.food.icp] %*% diag(IND_FD_ICP_usd2007[idx.food.icp, 1]))) / 1e6 / scaler_IND
tot.food.ZAF <- mean(rowSums(ZAF.tfei.icp[, idx.food.icp] %*% diag(ZAF_FD_ICP_usd2007[idx.food.icp, 1]))) / 1e6 / scaler_ZAF

tot.food.elec.BRA <- mean(rowSums(BRA.tfei.icp.elec[, idx.food.icp] %*% diag(BRA_FD_ICP_usd2007[idx.food.icp, 1]))) / 1e6 / scaler_BRA
tot.food.elec.IND <- mean(rowSums(IND.tfei.icp.elec[, idx.food.icp] %*% diag(IND_FD_ICP_usd2007[idx.food.icp, 1]))) / 1e6 / scaler_IND
tot.food.elec.ZAF <- mean(rowSums(ZAF.tfei.icp.elec[, idx.food.icp] %*% diag(ZAF_FD_ICP_usd2007[idx.food.icp, 1]))) / 1e6 / scaler_ZAF

# Test Approach #2
    # list[all_HH_f_IN, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'IND', IND_FD_ICP_AllHH, IND.tfei.icp) # more reliable than IND_intensity
    # (SummarizeGJPerCap(all_HH_f_IN) * (IND_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]
    # list[all_HH_f_ZA, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'ZAF', ZAF_FD_ICP_AllHH, ZAF.tfei.icp) # more reliable than IND_intensity
    # (SummarizeGJPerCap(all_HH_f_ZA) * (ZAF_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]
    # list[all_HH_f_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'BRA', BRA_FD_ICP_AllHH, BRA.tfei.icp) # more reliable than IND_intensity
    # (SummarizeGJPerCap(all_HH_f_BR) * (BRA_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]


# Scale the optm output with the aggregate value (tot.food.IND) because optm output seems too high because of mean of avg vs. avg of mean
# For the food base case, Total FE are scaled to match the value from EXIO.
# Alternative scenarios are scaled by the same ratio. 
IND.food.tfei$EJ <- IND.food.tfei$EJ * mean(tot.food.IND)/IND.food.tfei$EJ[1]
# Also scale kcal based on SSP2 assumption
# Base case divide by Hugo's SSP2 (2010) kcal (instead of our optimization outputs), which can be more reliable (w/o e.g. eat-out assumptions)
# Other opt scenarios are scaled at the same rate.
DLE.base.kcal.pcap <- IND.food.tfei$kcal[1]/365/IND_pop_2007
SSP2.base.kcal.pcap <- kcal.ssp2 %>% filter(Region=="IND" & Year==2010) %>% select(Val) %>% as.numeric()
IND.food.tfei$kcal <- IND.food.tfei$kcal * SSP2.base.kcal.pcap/DLE.base.kcal.pcap
# Update intensities
IND.food.tfei <- IND.food.tfei %>% mutate(ene.int=EJ/kcal*1e12, emi.int=gCO2e/kcal)  # MJ/kcal & g/kcal


# For BRA and ZAF, all scenarios are the same (no deficiency assumed)
BRA.base      <- tot.food.BRA      / (kcal.ssp2 %>% filter(Region=="BRA" & Year==2010)%>%select(Val) * BRA_pop_2007/1e12*365)
ZAF.base      <- tot.food.ZAF      / (kcal.ssp2 %>% filter(Region=="ZAF" & Year==2010)%>%select(Val) * ZAF_pop_2007/1e12*365)
BRA.base.elec <- tot.food.elec.BRA / (kcal.ssp2 %>% filter(Region=="BRA" & Year==2010)%>%select(Val) * BRA_pop_2007/1e12*365) 
ZAF.base.elec <- tot.food.elec.ZAF / (kcal.ssp2 %>% filter(Region=="ZAF" & Year==2010)%>%select(Val) * ZAF_pop_2007/1e12*365)

# For IND, intensities depend on scenarios. (MJ/kcal) 
IND.base <- IND.food.tfei %>% filter(name=="base") %>% select(ene.int) %>% as.numeric()
IND.devmin <- IND.food.tfei %>% filter(name=="dev_min") %>% select(ene.int) %>% as.numeric()    # will be achieved by 2030
IND.lctbhv <- IND.food.tfei %>% filter(name=="te_min_cap") %>% select(ene.int) %>% as.numeric() # will be achieved by 2050
IND.base.elec <- as.numeric(tot.food.elec.IND / IND.food.tfei$kcal[1]) * 1e12

IND.base.em <- IND.food.tfei %>% filter(name=="base") %>% select(emi.int) %>% as.numeric()
IND.devmin.em <- IND.food.tfei %>% filter(name=="dev_min") %>% select(emi.int) %>% as.numeric()    # will be achieved by 2030
IND.lctbhv.em <- IND.food.tfei %>% filter(name=="te_min_cap") %>% select(emi.int) %>% as.numeric() # will be achieved by 2050


# Set up adoption sequences for diff scenarios
# devmin is achieved by 2030 in ACCEL, te_min_cap is achieved by 2050 (in 40 years) in LCT.BHV
# FE intensity (MJ/kcal)
IND.seq.devmin <- c(seq(IND.base, IND.devmin, along.with=Year.base:Year.obs[3])[Year.obs[1:3]-Year.base+1], IND.devmin, IND.devmin)
IND.seq.lctbhv <- seq(IND.base, IND.lctbhv, along.with=Year.base:Year.end)[Year.obs-Year.base+1]
# Emissions intensity (g/kcal)
IND.seq.devmin.em <- c(seq(IND.base.em, IND.devmin.em, along.with=Year.base:Year.obs[3])[Year.obs[1:3]-Year.base+1], IND.devmin.em, IND.devmin.em)
IND.seq.lctbhv.em <- seq(IND.base.em, IND.lctbhv.em, along.with=Year.base:Year.end)[Year.obs-Year.base+1]


# Construct inputs for merge process (done in DLE_integration_Init.R)
tfei.food.base <- list(BRA= BRA.base, IND= IND.base, ZAF= ZAF.base)
tfei.food.accel <- list(BRA= BRA.base, IND= IND.seq.devmin, ZAF= ZAF.base)
tfei.food.lctbhv <- list(BRA= BRA.base, IND= IND.seq.lctbhv, ZAF= ZAF.base)

tfei.food.elec.base <- list(BRA= BRA.base.elec, IND= IND.base.elec, ZAF= ZAF.base.elec)
tfei.food.elec.accel <- list(BRA= BRA.base.elec,
                              IND= IND.seq.devmin * (tot.food.elec.IND/tot.food.IND),
                              ZAF= ZAF.base.elec)
tfei.food.elec.lctbhv <- list(BRA= BRA.base.elec,
                         IND= IND.seq.lctbhv * (tot.food.elec.IND/tot.food.IND),
                         ZAF= ZAF.base.elec)

tfei.food.non.elec.base <- mapply('-',  tfei.food.base, tfei.food.elec.base) 
tfei.food.non.elec.accel <- mapply('-',  tfei.food.accel, tfei.food.elec.accel)
tfei.food.non.elec.lctbhv <- mapply('-',  tfei.food.lctbhv, tfei.food.elec.lctbhv) 

names(tfei.food.non.elec.base) <- names(tfei.food.base)
names(tfei.food.non.elec.accel) <- names(tfei.food.accel)
names(tfei.food.non.elec.lctbhv) <- names(tfei.food.lctbhv)

# These units are kcal per capita per year
DLE.kcal.pcap <- 2162.835  # From our IND food analysis < DRI.pcap$cal.DRI_pcap
DRI.kcal.IND <- read.csv("C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Analysis/Food/DRI-india.csv") %>% filter(Nutrient=="calorie") %>%
  spread(Group, DRI) %>% select(male_adult, male_minor, female_adult, female_minor)

IND_HH_composition <-selectDBdata(tables='IND1_HH') %>% select(id, weight, hh_size, minor, male_adult, male_minor) %>% 
  mutate(female_adult = hh_size - minor - male_adult, female_minor = minor - male_minor) %>%
  summarise_at(vars(male_adult:female_minor), funs(sum(.*weight, na.rm=TRUE)))
BRA_HH_composition <-selectDBdata(tables='BRA1_HH') %>% select(id, weight, hh_size, minor, male_adult, male_minor) %>% 
  mutate(female_adult = hh_size - minor - male_adult, female_minor = minor - male_minor) %>%
  summarise_at(vars(male_adult:female_minor), funs(sum(.*weight, na.rm=TRUE)))
ZAF_HH_composition <-selectDBdata(tables='ZAF1_HH') %>% select(id, weight, hh_size, minor, male_adult, male_minor) %>% 
  mutate(female_adult = hh_size - minor - male_adult, female_minor = minor - male_minor) %>%
  summarise_at(vars(male_adult:female_minor), funs(sum(.*weight, na.rm=TRUE)))

unit.food.base <- list(BRA=weighted.mean(DRI.kcal.IND, w=BRA_HH_composition) * 365, 
                       IND=weighted.mean(DRI.kcal.IND, w=IND_HH_composition) * 365, 
                       ZAF=weighted.mean(DRI.kcal.IND, w=ZAF_HH_composition) * 365)

# Emission intensity DF
IND.intensity.em <- rbind(
  data.frame(Scenario="DLE.ACCEL", Year=Year.obs, Food=IND.seq.devmin.em),
  data.frame(Scenario="DLE.ACCEL.LCT.BHV", Year=Year.obs, Food=IND.seq.lctbhv.em),
  data.frame(Scenario="DLE.BAU", Year=Year.obs, Food=IND.base.em)
) %>% mutate(Country="IND") %>% select(Scenario, Country, Year, Food)
