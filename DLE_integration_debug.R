##############################

# We try to track back the version of data (e.g. intensities) and
# seek for the replicability of results mainly in the Nature Energy paper.

# There are various input data sets.
# 1. IO table (EXIO2 (2007) vs. EXIO3 (2010 selected))
# 2. Valuation data (same as above)
# 3. Energy extension (Arkaitz)


# Aug 8, 2019

##############################


EXIO3_path_old = "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Data/IO/EXIOBASE/EXIOBASE3/IOT_bug_w_India/"
EXIO3_path = "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Data/IO/EXIOBASE/EXIOBASE3/IOT_bug_w_TROA/"
EXIO3_path_fix = "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Data/IO/EXIOBASE/EXIOBASE3/EnvExt/EnvExt_NEU_1995-2015_14Nov/"


### First, compare tfei of EXIO sectors (health and edu)

idx.education.exio <- 174
idx.health.exio <- 175


# IO.year = 2008 # 2010 # 2007 #
# 
# 
# raw.S <- read.csv(paste0(EXIO3_path, "S_", IO.year, ".csv"), header = FALSE)    # Stressor (Intensity)
# raw.st <- read.csv(paste0(EXIO3_path, "st_", IO.year, ".csv"), header = FALSE)  # Total stressor
# raw.S <- read.csv(paste0(EXIO3_path_old, "S_", IO.year, ".csv"), header = FALSE)    # Stressor (Intensity)
# raw.st <- read.csv(paste0(EXIO3_path_old, "st_", IO.year, ".csv"), header = FALSE)  # Total stressor
# raw.S <- read.csv(paste0(EXIO3_path_fix, "S_", IO.year, ".csv"), header = FALSE)    # Stressor (Intensity)
# raw.st <- read.csv(paste0(EXIO3_path_fix, "st_", IO.year, ".csv"), header = FALSE)  # Total stressor


list[tfei.exio, tfei.elec, tfei.non.elec, tfei.sub, tnei.exio] <- HarmonizeEXIO3ExtensionFormat(raw.st) # tpei.nature, tpei.USE, tpei.SUPL,
list[dfei.exio, dfei.elec, dfei.non.elec, dfei.sub, dnei.exio] <- HarmonizeEXIO3ExtensionFormat(raw.S) # dpei.nature, dpei.USE, dpei.SUPL, 



# Current tfei.exio is for 2010 EXIO 2010 Valuation (Save after running read.csv & HarmonizeEXIO3ExtensionFormat)
save(tfei.exio, file="./Saved tables/tfei.exio.EXIO3.2010.Rda")
save(dfei.exio, file="./Saved tables/dfei.exio.EXIO3.2010.Rda")

save(tfei.exio, file="./Saved tables/tfei.exio.EXIO3.2010.fix.Rda")
save(dfei.exio, file="./Saved tables/dfei.exio.EXIO3.2010.fix.Rda")
# save(tfei.exio, file="./Saved tables/tfei.exio.EXIO3.2007.Rda")
save(tfei.exio, file="./Saved tables/tfei.exio.EXIO3.2008.Rda")
save(dfei.exio, file="./Saved tables/dfei.exio.EXIO3.2008.Rda")

save(tfei.exio, file="./Saved tables/tfei.exio.EXIO3.2007.Rda")
save(dfei.exio, file="./Saved tables/dfei.exio.EXIO3.2007.Rda")

save(tfei.exio, file="./Saved tables/tfei.exio.EXIO3.2008.fix.Rda")
save(dfei.exio, file="./Saved tables/dfei.exio.EXIO3.2008.fix.Rda")
# save(tfei.exio, file="./Saved tables/tfei.exio.EXIO3.2007.old.Rda")
# save(dfei.exio, file="./Saved tables/dfei.exio.EXIO3.2008.old.Rda")
# save(tfei.exio, file="./Saved tables/tfei.exio.EXIO3.2008.old.Rda")

load(file="./Saved tables/tfei.exio.EXIO3.2010.Rda")
load(file="./Saved tables/tfei.exio.EXIO3.2010.fix.Rda")
# load(file="./Saved tables/tfei.exio.EXIO3.2007.Rda")
load(file="./Saved tables/tfei.exio.EXIO3.2008.Rda")
load(file="./Saved tables/dfei.exio.EXIO3.2008.Rda")
load(file="./Saved tables/tfei.exio.EXIO3.2008.fix.Rda")
load(file="./Saved tables/dfei.exio.EXIO3.2008.fix.Rda")

# load(file="./Saved tables/tfei.exio.EXIO3.2007.old.Rda")
# load(file="./Saved tables/dfei.exio.EXIO3.2008.old.Rda")
# load(file="./Saved tables/tfei.exio.EXIO3.2008.old.Rda")

load(file="./Saved tables/L_inverse_EXIO3_2007.Rda")
# load(file="./Saved tables/L_inverse_EXIO3_2007.old.Rda")
# load(file="./Saved tables/L_inverse_EXIO3_2008.old.Rda")
load(file="./Saved tables/L_inverse_EXIO3_2010.Rda")


L_inverse <- read.csv(paste0(EXIO3_path, paste0("L_", IO.year, ".csv")), header = FALSE)
# save(L_inverse, file=paste0("./Saved tables/L_inverse_EXIO3_2007.old.Rda"))
save(L_inverse, file=paste0("./Saved tables/L_inverse_EXIO3_2007.Rda"))
# save(L_inverse, file=paste0("./Saved tables/L_inverse_EXIO3_2008.old.Rda"))
save(L_inverse, file=paste0("./Saved tables/L_inverse_EXIO3_2008.Rda"))



### 1. All up-to-date run (2010)
IO.year <- 2010
load(file="./Saved tables/tfei.exio.EXIO3.2010.fix.Rda")
load(file="./Saved tables/dfei.exio.EXIO3.2010.fix.Rda")
load(file="./Saved tables/L_inverse_EXIO3_2010.Rda")


### 2. All up-to-date run (2008)
IO.year <- 2008
load(file="./Saved tables/tfei.exio.EXIO3.2008.fix.Rda")
load(file="./Saved tables/dfei.exio.EXIO3.2008.fix.Rda")
load(file="./Saved tables/L_inverse_EXIO3_2008.Rda")

### 3. Old ways: mix of 2008 and 2007 IO
IO.year <- 2007
# This needs to go back to the old valuation files from EXIO2. (with the old path)
source("rIPFP - Valuation.R")
load(file="./Saved tables/dfei.exio.EXIO3.2008.fix.Rda")
load(file="./Saved tables/tfei.exio.EXIO3.2008.fix.Rda")
load(file="./Saved tables/L_inverse_EXIO3_2007.Rda")
# 
load(file="./Saved tables/dfei.exio.EXIO3.2008.old.Rda")
load(file="./Saved tables/tfei.exio.EXIO3.2008.old.Rda")
load(file="./Saved tables/L_inverse_EXIO3_2007.old.Rda")

load(file="./Saved tables/dfei.exio.EXIO3.2008.Rda")
load(file="./Saved tables/tfei.exio.EXIO3.2008.Rda")

CES.year = 2010
##################################################################################################
###  Recalculate the conversion factors based on IO.year

# Deflate currency in 2010 to 2007 (EXIO)
CPI <- WDI(country = c("IN", "BR", "FR", "ZA"), indicator = "FP.CPI.TOTL", start = IO.year, end = 2015, extra = FALSE, cache = NULL)

# For now let's assume both health and edu thresholds are 2010 PPP $ (Health is 2012 PPP$.)
# CPI_ratio_IND.edu <- as.numeric(CPI %>% filter(year==2010 & iso2c=='IN') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==IO.year & iso2c=='IN') %>% select(FP.CPI.TOTL))
# CPI_ratio_BRA.edu <- as.numeric(CPI %>% filter(year==2010 & iso2c=='BR') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==IO.year & iso2c=='BR') %>% select(FP.CPI.TOTL))
# CPI_ratio_ZAF.edu <- as.numeric(CPI %>% filter(year==2010 & iso2c=='ZA') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==IO.year & iso2c=='ZA') %>% select(FP.CPI.TOTL))
# CPI_ratio_IND.hth <- as.numeric(CPI %>% filter(year==2012 & iso2c=='IN') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==IO.year & iso2c=='IN') %>% select(FP.CPI.TOTL))
# CPI_ratio_BRA.hth <- as.numeric(CPI %>% filter(year==2012 & iso2c=='BR') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==IO.year & iso2c=='BR') %>% select(FP.CPI.TOTL))
# CPI_ratio_ZAF.hth <- as.numeric(CPI %>% filter(year==2012 & iso2c=='ZA') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==IO.year & iso2c=='ZA') %>% select(FP.CPI.TOTL))
CPI_ratio_IND <- as.numeric(CPI %>% filter(year==CES.year & iso2c=='IN') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==IO.year & iso2c=='IN') %>% select(FP.CPI.TOTL))
CPI_ratio_BRA <- as.numeric(CPI %>% filter(year==CES.year & iso2c=='BR') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==IO.year & iso2c=='BR') %>% select(FP.CPI.TOTL))
CPI_ratio_ZAF <- as.numeric(CPI %>% filter(year==CES.year & iso2c=='ZA') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==IO.year & iso2c=='ZA') %>% select(FP.CPI.TOTL))

PPP_cty = WDI(country = c("IN", "BR", "ZA"), indicator = c("PA.NUS.PPP", "PA.NUS.PRVT.PP"), start = CES.year, end = CES.year, extra = FALSE, cache = NULL)
PPP_IND <- as.numeric(PPP_cty %>% filter(country=="India") %>% select(PA.NUS.PRVT.PP))
PPP_BRA <- as.numeric(PPP_cty %>% filter(country=="Brazil") %>% select(PA.NUS.PRVT.PP))
PPP_ZAF <- as.numeric(PPP_cty %>% filter(country=="South Africa") %>% select(PA.NUS.PRVT.PP))

# Exchange rate (MER) [LCU/$]
EXR_EUR <- WDI(country = "XC", indicator = "PA.NUS.FCRF", start = IO.year, end = IO.year, extra = FALSE, cache = NULL)
EXR_EUR <- EXR_EUR %>% rename(r=PA.NUS.FCRF)
EXR_cty <- WDI(country = c("IN", "BR", "ZA"), indicator = "PA.NUS.FCRF", start = IO.year, end = IO.year, extra = FALSE, cache = NULL)

EXR_IND <- as.numeric(EXR_cty %>% filter(country=="India") %>% select(PA.NUS.FCRF))
EXR_BRA <- as.numeric(EXR_cty %>% filter(country=="Brazil") %>% select(PA.NUS.FCRF))
EXR_ZAF <- as.numeric(EXR_cty %>% filter(country=="South Africa") %>% select(PA.NUS.FCRF))


# HH Consumption in India 2007 [US$]
HH_CON <- WDI(country = c("IN", "BR", "ZA"), 
              indicator = c(#"NE.CON.PETC.CD", 
                "NE.CON.PRVT.CD", 
                # "NE.CON.PETC.CN", 
                "NE.CON.PRVT.KD"), 
              start = 2004, end = 2011, extra = FALSE, cache = NULL)
BRA_con_grwth <- as.numeric(HH_CON %>% filter(year==2008 & iso2c=='BR') %>% select(NE.CON.PRVT.KD) / 
                              HH_CON %>% filter(year==IO.year & iso2c=='BR') %>% select(NE.CON.PRVT.KD))
IND_con_grwth <- as.numeric(HH_CON %>% filter(year==2011 & iso2c=='IN') %>% select(NE.CON.PRVT.KD) / 
                              HH_CON %>% filter(year==IO.year & iso2c=='IN') %>% select(NE.CON.PRVT.KD))
ZAF_con_grwth <- as.numeric(HH_CON %>% filter(year==2010 & iso2c=='ZA') %>% select(NE.CON.PRVT.KD) / 
                              HH_CON %>% filter(year==IO.year & iso2c=='ZA') %>% select(NE.CON.PRVT.KD))

# Imports/Exports of goods and services (% of GDP)
WDI(country = c("IN", "BR"), indicator = c("NE.IMP.GNFS.ZS", "NE.EXP.GNFS.ZS"), start = 2007, end = 2007, extra = FALSE, cache = NULL)


Popul <- WDI(country = c("IN", "BR", "FR", "ZA"), indicator = "SP.POP.TOTL", start = 2007, end = 2015, extra = FALSE, cache = NULL)

BRA_pop_io.yr <- Popul %>% rename(pop=SP.POP.TOTL) %>% filter(iso2c=="BR" & year==IO.year) %>% select(pop) %>% as.numeric()#1.9e8
IND_pop_io.yr <- Popul %>% rename(pop=SP.POP.TOTL) %>% filter(iso2c=="IN" & year==IO.year) %>% select(pop) %>% as.numeric()#1.159e9
ZAF_pop_io.yr <- Popul %>% rename(pop=SP.POP.TOTL) %>% filter(iso2c=="ZA" & year==IO.year) %>% select(pop) %>% as.numeric()#1.159e9
##################################################################################################






# Intensity in MJ/EUR MER (IO.year: tfei.exio) -> MJ/USD PPP (svy year)
# sum(tfei.exio[,BRA_idx_ex[idx.education.exio]]) / (EXR_BRA * CPI_ratio_BRA.edu / PPP_BRA) * EXR_EUR$r #
# sum(tfei.exio[,IND_idx_ex[idx.education.exio]]) / (EXR_IND * CPI_ratio_IND.edu / PPP_IND) * EXR_EUR$r #
# sum(tfei.exio[,ZAF_idx_ex[idx.education.exio]]) / (EXR_ZAF * CPI_ratio_ZAF.edu / PPP_ZAF) * EXR_EUR$r #
# sum(tfei.exio[,BRA_idx_ex[idx.health.exio]]) / (EXR_BRA * CPI_ratio_BRA.hth / PPP_BRA) * EXR_EUR$r #
# sum(tfei.exio[,IND_idx_ex[idx.health.exio]]) / (EXR_IND * CPI_ratio_IND.hth / PPP_IND) * EXR_EUR$r #
# sum(tfei.exio[,ZAF_idx_ex[idx.health.exio]]) / (EXR_ZAF * CPI_ratio_ZAF.hth / PPP_ZAF) * EXR_EUR$r #
colSums(tfei.exio[,BRA_idx_ex[c(idx.education.exio,idx.health.exio)]]) / (EXR_BRA * CPI_ratio_BRA / PPP_BRA) #* EXR_EUR$r #
colSums(tfei.exio[,IND_idx_ex[c(idx.education.exio,idx.health.exio)]]) / (EXR_IND * CPI_ratio_IND / PPP_IND) #* EXR_EUR$r #
colSums(tfei.exio[,ZAF_idx_ex[c(idx.education.exio,idx.health.exio)]]) / (EXR_ZAF * CPI_ratio_ZAF / PPP_ZAF) #* EXR_EUR$r #

# L_inverse is required here.
TFEI.EXIO.ts.debug <- sapply(DLE.countries, TFEI.ApplyKeyTechImprovement.EXIO, dfei=dfei.exio, simplify=FALSE, USE.NAMES = TRUE) # Returns a list
DLE.intensity.debug <- FormatDLEInputTable.EXIO(c("Education", "Health"), "Intensity", carr="total", 
                         TFEI.EXIO.ts.debug, c(idx.education.exio, idx.health.exio))

# Adjust Health to GTAP (?)
tpei.health.GTAP <- data.frame(IND=3.82, BRA=2.1, ZAF=5.1)  # MJ/USD
tpei.health.exio <- colSums(tnei.exio[,c(IND_idx_ex[idx.health.exio], BRA_idx_ex[idx.health.exio], ZAF_idx_ex[idx.health.exio])])
names(tpei.health.exio) <- names(tpei.health.GTAP)
ratio.GTAP <- tpei.health.GTAP / tpei.health.exio

DLE.intensity.debug <- DLE.intensity.debug %>% left_join(ratio.GTAP %>% gather(Country, Ratio)) %>% mutate(Health=Health*Ratio) %>% select(-Ratio) 




##################################################################################################
###  Recalculate FD vectors from the survey
n_CES_fuel <- dim(DLE_fuel_sector_Q)[2]



IND_FD_code <- IND_FD[-grep("taxes", IND_FD$item, ignore.case = TRUE), ]

# Merge sector code info
IND_FD_code <- merge(IND_FD_code[1:(dim(IND_FD_code)[1]-n_CES_fuel),], IND_map %>% select(CODE, item=ITEM_DLE), 
                     by="item", all.x = TRUE) %>% arrange(CODE) %>%
  rbind(IND_FD[-(1:(dim(IND_FD)[1]-n_CES_fuel)),]%>% mutate(CODE=999))    # Fuels are temprarily assigned to 999. (not used)

# Replace NAs with zeros
IND_FD_code[is.na(IND_FD_code)] <- 0


######################################
### Convert CES rows into ICP rows ###
######################################

# Need to separatly handle the fuel rows  (in USD PPP 2010)
# Fianlly get 164 harmonized ICP rows for all 
IND_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD_code[1:(dim(IND_FD_code)[1]-n_CES_fuel),2:12]) %>%
  rbind(IND_FD_code[-(1:(dim(IND_FD_code)[1]-n_CES_fuel)),2:12]) # for all deciles and total
IND_FD_ICP <- as.matrix(IND_FD_ICP)

# Deciles
IND_FD_ICP_svy.yr <- IND_FD_ICP * PPP_IND / CPI_ratio_IND / EXR_IND / 1e6 # to M.USD at IO.year (MER)  - consumption quantity at the survey year (IND1:2011)
IND_FD_ICP_io.yr <- IND_FD_ICP_svy.yr / IND_con_grwth # M.USD 2007 (MER)  - consumption quantity at the common IO year (EXIO2:2007)
scaler_IND <- sum(IND_FD_ICP_io.yr[,1]) / sum(get_purch_price(IND_fd_exio, "IN"))

#################
### 2. Brazil ###
#################

#### 2.1 Brazil - Inflation & exchange rate

# BRA1 is for 2008-2009
BRA_FD <- data.frame(item=ICP_catnames) %>% left_join(BRA_FD)  # Join to make items consistent with the standardized names and order
BRA_FD[is.na(BRA_FD)] <- 0
BRA_FD_ICP_io.yr <- as.matrix(BRA_FD[,2:12] * PPP_BRA / CPI_ratio_BRA / EXR_BRA / 1e6 / BRA_con_grwth) # to M.USD 2007 (MER)
scaler_BRA <- sum(BRA_FD_ICP_io.yr[,1]) / sum(get_purch_price(BRA_fd_exio, "BR"))


#############################
### 3.1.1. ZAF1 by decile ###
#############################

# Remove tax observations from DB
ZAF_FD_code <- ZAF_FD[-grep("taxes|VAT ", ZAF_FD$item, ignore.case = TRUE), ]

ZAF_FD_code <- ZAF_FD_code[1:(dim(ZAF_FD_code)[1]-n_CES_fuel),] %>% left_join(ZAF_map %>% select(CODE, item=ITEM_DLE), by="item") %>% 
  arrange(CODE) %>%
  rbind(ZAF_FD[-(1:(dim(ZAF_FD)[1]-n_CES_fuel)),] %>% mutate(CODE=999))    # Fuels are temprarily assigned to 999. (not used)

# Replace NAs with zeros
ZAF_FD_code[is.na(ZAF_FD_code)] <- 0

######################################
### Convert CES rows into ICP rows ###
######################################

# Need to separatly handle the fuel rows  (in USD PPP 2010)
# Fianlly get 164 harmonized ICP rows for all 
ZAF_FD_ICP <- t(CES_ICP_ZAF) %*% as.matrix(ZAF_FD_code[1:(dim(ZAF_FD_code)[1]-n_CES_fuel),2:12]) %>%
  rbind(ZAF_FD_code[-(1:(dim(ZAF_FD_code)[1]-n_CES_fuel)),2:12]) # for all deciles and total
ZAF_FD_ICP <- as.matrix(ZAF_FD_ICP)

# Deciles
ZAF_FD_ICP_svy.yr <- ZAF_FD_ICP * PPP_ZAF / CPI_ratio_ZAF / EXR_ZAF / 1e6 # USD 2010 (PPP) to M.USD 2007 (MER)
ZAF_FD_ICP_io.yr <- ZAF_FD_ICP_svy.yr / ZAF_con_grwth
scaler_ZAF <- sum(ZAF_FD_ICP_io.yr[,1]) / sum(get_purch_price(ZAF_fd_exio, "ZA"))

##################################################################################################


# Get intensity by ICP cat
# DeriveIntensities requires *_ICP_io.yr for run.

n_draw <- 50
list[BRA.tfei.icp, BRA.alloc, NC_BRA_val_BRA, BRA_FD_adj_val_BRA] <- DeriveIntensities('BRA', 'final', final.intensity.mat=tfei.exio)
list[IND.tfei.icp, IND_alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND', 'final', final.intensity.mat=tfei.exio)
list[ZAF.tfei.icp, ZAF_alloc, NC_ZAF, ZAF_FD_adj] <- DeriveIntensities('ZAF', 'final', final.intensity.mat=tfei.exio)

# save(BRA.tfei.icp, file="./Saved tables/BRA.tfei.icp.2010.Rda")
# save(IND.tfei.icp, file="./Saved tables/IND.tfei.icp.2010.Rda")
# save(ZAF.tfei.icp, file="./Saved tables/ZAF.tfei.icp.2010.Rda")
save(BRA.tfei.icp, file="./Saved tables/BRA.tfei.icp.2008.fix.2007.IO.Rda")
save(IND.tfei.icp, file="./Saved tables/IND.tfei.icp.2008.fix.2007.IO.Rda")
save(ZAF.tfei.icp, file="./Saved tables/ZAF.tfei.icp.2008.fix.2007.IO.Rda")




# Then run food/clothing script to get total intensities




### Add Clothing and Food to the debug table
# Food - total
DLE.intensity.debug <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.base, scenario="DLE.BAU", carr="total", table=DLE.intensity.debug)
DLE.intensity.debug <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.accel, scenario="DLE.ACCEL", carr="total", table=DLE.intensity.debug)
DLE.intensity.debug <- AddComponentToDLETable("Food", tabl="Intensity", val.list=tfei.food.lctbhv, scenario="DLE.ACCEL.LCT.BHV", carr="total", table=DLE.intensity.debug)
DLE.intensity.debug <- AddComponentToDLETable("Clothing", tabl="Intensity", val.list=tfei.clothing.all, scenario="DLE.BAU", carr="total", table=DLE.intensity.debug)

# save(DLE.intensity.debug, file="./Saved tables/DLE.IO.intensities.2010.Rda")
save(DLE.intensity.debug, file="./Saved tables/DLE.IO.intensities.2008.fix.2007.IO.Rda")
