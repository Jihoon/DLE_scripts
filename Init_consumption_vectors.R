###############################################
### Organize final demand for our analyses  ###
###############################################

# Use inputs (IND_FD_ALL, IND_FD, BRA_FD_ALL, BRA_FD) directly from the database read
# IND_FD_ALL & BRA_FD_ALL are cast to data.table for faster manipulations.

# Harmonized fuel types
# item
# 1                        Biogas
# 2  Charcoal/coal/briquette/coke
# 3                        Diesel
# 4                   Electricity
# 5                       Ethanol
# 6      Firewood and other fuels
# 7                 Other biomass
# 8           Fuel oil, generator
# 9                      Gasoline
# 10                     Kerosene
# 11                          LPG
# 12                  Natural gas
# 13         Other household fuel 

# Number of fuel types in 13 DLE-harmonized classification
n_CES_fuel <- dim(DLE_fuel_sector_Q)[2]


################
### 1. India ###
################

################
### 1.1 IND1 ###
################

# Convert to IND final demand matrix in ICP classification

#############################
### 1.1.1. IND1 by decile ###
#############################

# Remove tax observations from DB
IND_FD_code <- IND_FD[-grep("taxes", IND_FD$item, ignore.case = TRUE), ]

# Merge sector code info
IND_FD_code <- merge(IND_FD_code[1:(dim(IND_FD_code)[1]-n_CES_fuel),], IND_map %>% select(CODE, item=ITEM_DLE), 
                    by="item", all.x = TRUE) %>% arrange(CODE) %>%
               rbind(IND_FD[-(1:(dim(IND_FD)[1]-n_CES_fuel)),]%>% mutate(CODE=999))    # Fuels are temprarily assigned to 999. (not used)

# Replace NAs with zeros
IND_FD_code[is.na(IND_FD_code)] <- 0


#################################
### 1.1.2. IND1 by household  ###
#################################

# Read in original IND_FD_ALL
load(file="./Saved tables/IND_AllHHConsump.Rda")
IND_FD_ALL <- IND_FD_ALL[-grep("taxes", IND_FD_ALL$item, ignore.case = TRUE), ]

IND_FD_ALL <- merge(IND_FD_ALL[1:(dim(IND_FD_ALL)[1]-n_CES_fuel),], IND_map %>% select(CODE, item=ITEM_DLE), 
                    by="item", all.x = TRUE) %>% arrange(CODE) %>%
              rbind(IND_FD_ALL[-(1:(dim(IND_FD_ALL)[1]-n_CES_fuel)),] %>% mutate(CODE=999))
IND_FD_ALL[is.na(IND_FD_ALL)] <- 0


######################################
### Convert CES rows into ICP rows ###
######################################

# Need to separatly handle the fuel rows  (in USD PPP 2010)
# Fianlly get 164 harmonized ICP rows for all 
IND_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD_code[1:(dim(IND_FD_code)[1]-n_CES_fuel),2:12]) %>%
  rbind(IND_FD_code[-(1:(dim(IND_FD_code)[1]-n_CES_fuel)),2:12]) # for all deciles and total
IND_FD_ICP <- as.matrix(IND_FD_ICP)

# Total final demand in ICP cat for all HH (not scaled up to Nat Acc level yet) (USD 2010 PPP as in DLE DB)
load(file="./Saved tables/IND_AllHH_w_CODE.Rda")
# IND_FD_ICP_AllHH <- crossprod(CES_ICP_IND, as.matrix(IND_FD_ALL[1:(dim(IND_FD_ALL)[1]-n_CES_fuel),-c(1,2), with=FALSE])) %>%
#   rbind(as.matrix(IND_FD_ALL[-(1:(dim(IND_FD_ALL)[1]-n_CES_fuel)),-c(1,2) , with=FALSE]))
# replace with eigenMapMatMult() later
IND_FD_ICP_AllHH <- eigenMapMatMult(t(CES_ICP_IND), as.matrix(IND_FD_ALL[1:(dim(IND_FD_ALL)[1]-n_CES_fuel), -c("item", "CODE"), with=FALSE])) %>%
  rbind(as.matrix(IND_FD_ALL[-(1:(dim(IND_FD_ALL)[1]-n_CES_fuel)), -c("item", "CODE"), with=FALSE]))

# In the end, all we need is this ICP matrices
rm(IND_FD, IND_FD_code, IND_FD_ALL)
gc()


##########################################
### Adjust FD based on the indicators  ###
##########################################

#### 1.1 India - Real consumption growth

# IND1 is for 2010-2011
# Not used now (10.18.2016)
# IND_FD_code$COICOP1 <- as.numeric(IND_FD_code$COICOP1)
# IND2_FD_code$COICOP1 <- as.numeric(IND2_FD_code$COICOP1)
# a <- aggregate(. ~ COICOP1, data=IND_FD_code[,2:14], sum)
# b <- aggregate(. ~ COICOP1, data=IND2_FD_code[,2:14], sum)
# 
# consumption_growth <- (a[,2:12]/b[,2:12])^(1/7) # general consumption growth rate in 7 years by decile
# consumption_growth_DE <- (IND_DE[,2:12]/IND2_DE[,2:12])^(1/7) # direct energy consumption growth rate in 7 years by decile
# consumption_growth_FD_DE <- (IND_FD_DE[,2:12]/IND2_FD_DE[,2:12])^(1/7) # consumption growth rate in 7 years by decile

#### 1.2 India - Inflation & exchange rate

# Deciles
IND_FD_ICP_usd2011 <- IND_FD_ICP * PPP_IND / CPI_ratio_IND / EXR_IND / 1e6 # to M.USD 2007 (MER)
IND_FD_ICP_usd2007 <- IND_FD_ICP_usd2011 / IND_con_grwth
# IND_FD_ICP_usd2007 <- IND_FD_ICP_usd2011 / ((consumption_growth^4)[c(as.numeric(icp_ntnu$COICOP1)[1:151],rep(4,n_CES_fuel)),])
# This scaling by consumption_growth will go obsolete and simply use IND_con_grwth.

# All households
IND_FD_AllHH_2011 <- IND_FD_ICP_AllHH * PPP_IND / CPI_ratio_IND / EXR_IND    # From USD 2010 PPP to USD 2007 (MER)
IND_FD_ICP_AllHH <- IND_FD_AllHH_2011 / IND_con_grwth   # to USD 2007 (MER)   # An estimate for hh consumption in 2007




#################
### 2. Brazil ###
#################

#### 2.1 Brazil - Inflation & exchange rate

# BRA1 is for 2008-2009
BRA_FD <- data.frame(item=ICP_catnames) %>% left_join(BRA_FD)  # Join to make items consistent with the standardized names and order
BRA_FD[is.na(BRA_FD)] <- 0
BRA_FD_ICP_usd2007 <- as.matrix(BRA_FD[,2:12] * PPP_BRA / CPI_ratio_BRA / EXR_BRA / 1e6 / BRA_con_grwth) # to M.USD 2007
# BRA1_FD_ICP_usd2007 <- cbind(BRA1_FD[,1], as.matrix(BRA1_FD[,2:12] * PPP_BRA / CPI_ratio_BRA / EXR_BRA / 1e6 / BRA_con_grwth)) # to M.USD 2007

# FD for all households
BRA_FD_ICP_AllHH <- data.frame(item=ICP_catnames) %>% left_join(BRA_FD_ALL)
BRA_FD_ICP_AllHH <- as.matrix(BRA_FD_ICP_AllHH[,-1]) * PPP_BRA / CPI_ratio_BRA / EXR_BRA / BRA_con_grwth # to USD 2007
# BRA_FD_ICP_AllHH[,-1] <- NAer(BRA_FD_ICP_AllHH[,-1]) # Faster than 
BRA_FD_ICP_AllHH[is.na(BRA_FD_ICP_AllHH)] <- 0

save(IND_FD_ICP_AllHH, file="./Saved tables/IND_FD_harmonized.Rda")
save(BRA_FD_ICP_AllHH, file="./Saved tables/BRA_FD_harmonized.Rda")
save(IND_FD_ICP_usd2007, file="./Saved tables/IND_FD_ICP_usd2007.Rda")
save(BRA_FD_ICP_usd2007, file="./Saved tables/BRA_FD_ICP_usd2007.Rda")
load(file="./Saved tables/IND_FD_ICP_usd2007.Rda")
load(file="./Saved tables/BRA_FD_ICP_usd2007.Rda")
load(file="./Saved tables/IND_FD_harmonized.Rda")



# Set Scalers
scaler_IND <- sum(IND_FD_ICP_usd2007[,1]) / sum(get_purch_price(IND_fd_exio, "IN"))
# scaler2_IND <- sum(IND2_FD_ICP_usd2007[,1]) / sum(get_purch_price(IND2_fd_exio, "IN"))
scaler_IND2 <- scaler_IND
init_FD_IND <- IND_FD_ICP_usd2007[,1] / scaler_IND

scaler_BRA <- sum(BRA_FD_ICP_usd2007[,1]) / sum(get_purch_price(BRA_fd_exio, "BR"))
init_FD_BRA <- BRA_FD_ICP_usd2007[,1] / scaler_BRA


# Enable this if IND2 analysis is needed
# source("Init_consumption_vectors_IND2.R")


#######################
### 3. South Africa ###
#######################

#############################
### 3.1.1. ZAF1 by decile ###
#############################

# Remove tax observations from DB
ZAF_FD_code <- ZAF_FD[-grep("taxes|VAT ", ZAF_FD$item, ignore.case = TRUE), ]

# Merge sector code info
# ZAF_FD_code <- merge(ZAF_FD_code[1:(dim(ZAF_FD_code)[1]-n_CES_fuel),], ZAF_map %>% select(CODE, item=ITEM_DLE), 
#                      by="item", all.x = TRUE) %>% 
#   arrange(CODE) %>%
#   rbind(ZAF_FD[-(1:(dim(ZAF_FD)[1]-n_CES_fuel)),] %>% mutate(CODE=999))    # Fuels are temprarily assigned to 999. (not used)
ZAF_FD_code <- ZAF_FD_code[1:(dim(ZAF_FD_code)[1]-n_CES_fuel),] %>% left_join(ZAF_map %>% select(CODE, item=ITEM_DLE), by="item") %>% 
  arrange(CODE) %>%
  rbind(ZAF_FD[-(1:(dim(ZAF_FD)[1]-n_CES_fuel)),] %>% mutate(CODE=999))    # Fuels are temprarily assigned to 999. (not used)

# Replace NAs with zeros
ZAF_FD_code[is.na(ZAF_FD_code)] <- 0


#################################
### 3.1.2. ZAF1 by household  ###
#################################

# Read in original IND_FD_ALL
load(file="./Saved tables/ZAF_AllHHConsump.Rda")
ZAF_FD_ALL <- ZAF_FD_ALL[-grep("taxes|VAT ", ZAF_FD_ALL$item, ignore.case = TRUE), ]

ZAF_FD_ALL <- merge(ZAF_FD_ALL[1:(dim(ZAF_FD_ALL)[1]-n_CES_fuel),], ZAF_map %>% select(CODE, item=ITEM_DLE), 
                    by="item", all.x = TRUE) %>% 
  arrange(CODE) %>% select(item, CODE, everything()) %>%
  rbind(ZAF_FD_ALL[-(1:(dim(ZAF_FD_ALL)[1]-n_CES_fuel)),] %>% mutate(CODE=999))
ZAF_FD_ALL[is.na(ZAF_FD_ALL)] <- 0


######################################
### Convert CES rows into ICP rows ###
######################################

# Need to separatly handle the fuel rows  (in USD PPP 2010)
# Fianlly get 164 harmonized ICP rows for all 
ZAF_FD_ICP <- t(CES_ICP_ZAF) %*% as.matrix(ZAF_FD_code[1:(dim(ZAF_FD_code)[1]-n_CES_fuel),2:12]) %>%
  rbind(ZAF_FD_code[-(1:(dim(ZAF_FD_code)[1]-n_CES_fuel)),2:12]) # for all deciles and total
ZAF_FD_ICP <- as.matrix(ZAF_FD_ICP)

# Total final demand in ICP cat for all HH (not scaled up to Nat Acc level yet) (USD 2010 PPP as in DLE DB)
# load(file="./Saved tables/ZAF_AllHH_w_CODE.Rda")
ZAF_FD_ICP_AllHH <- eigenMapMatMult(t(CES_ICP_ZAF), as.matrix(ZAF_FD_ALL[1:(dim(ZAF_FD_ALL)[1]-n_CES_fuel), -c("item", "CODE"), with=FALSE])) %>%
  rbind(as.matrix(ZAF_FD_ALL[-(1:(dim(ZAF_FD_ALL)[1]-n_CES_fuel)), -c("item", "CODE"), with=FALSE]))

# In the end, all we need is this ICP matrices
rm(ZAF_FD, ZAF_FD_code, ZAF_FD_ALL)
gc()


##########################################
### Adjust FD based on the indicators  ###
##########################################

#### 3.1 South Africa - Inflation & exchange rate

# Deciles
ZAF_FD_ICP_usd2011 <- ZAF_FD_ICP * PPP_ZAF / CPI_ratio_ZAF / EXR_ZAF / 1e6 # to M.USD 2007 (MER)
ZAF_FD_ICP_usd2007 <- ZAF_FD_ICP_usd2011 / ZAF_con_grwth
# ZAF_FD_ICP_usd2007 <- ZAF_FD_ICP_usd2011 / ((consumption_growth^4)[c(as.numeric(icp_ntnu$COICOP1)[1:151],rep(4,n_CES_fuel)),])
# This scaling by consumption_growth will go obsolete and simply use ZAF_con_grwth.

# All households
ZAF_FD_AllHH_2011 <- ZAF_FD_ICP_AllHH * PPP_ZAF / CPI_ratio_ZAF / EXR_ZAF    # From USD 2010 PPP to USD 2007 (MER)
ZAF_FD_ICP_AllHH <- ZAF_FD_AllHH_2011 / ZAF_con_grwth   # to USD 2007 (MER)   # An estimate for hh consumption in 2007

save(ZAF_FD_ICP_AllHH, file="./Saved tables/ZAF_FD_harmonized.Rda")
save(ZAF_FD_ICP_usd2007, file="./Saved tables/ZAF_FD_ICP_usd2007.Rda")
load(file="./Saved tables/ZAF_FD_ICP_usd2007.Rda")
load(file="./Saved tables/ZAF_FD_harmonized.Rda")