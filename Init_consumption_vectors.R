###############################################
### Organize final demand for our analyses  ###
###############################################

# Use inputs (IND_FD_ALL, IND_FD, BRA_FD_ALL, BRA_FD) directly from the database read
# IND_FD_ALL & BRA_FD_ALL are cast to data.table for faster manipulations.


################
### 1. India ###
################

################
### 1.1 IND1 ###
################

# Convert to IND final demand matrix in ICP classification

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
### 1.1.1. IND1 by household  ###
#################################

# Read in original IND_FD_ALL
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_AllHHConsump.Rda")
IND_FD_ALL <- IND_FD_ALL[-grep("taxes", IND_FD_ALL$item, ignore.case = TRUE), ]

IND_FD_ALL <- merge(IND_FD_ALL[1:(dim(IND_FD_ALL)[1]-n_CES_fuel),], IND_map %>% select(CODE, item=ITEM_DLE), 
                    by="item", all.x = TRUE) %>% arrange(CODE) %>%
              rbind(IND_FD_ALL[-(1:(dim(IND_FD_ALL)[1]-n_CES_fuel)),] %>% mutate(CODE=999))
IND_FD_ALL[is.na(IND_FD_ALL)] <- 0


################
### 1.2 IND2 ###
################

# Note1: need to add COICOP1 info to get annual hh consumption increase rates. 
# -> Not used any more (10.18.2016). Simplify and use just the hh consumption increase from WDI.

# Note2: Some item names are different from IND1. So we need to harmonize for comparison.
#        Also need to replace CODE field values to apply the WB CES-COICOP mapping (made for IND1).

#############################
### 1.2.1. IND2 by decile ###
#############################

# Same as above
IND2_FD_code <- IND2_FD[-grep("taxes", IND2_FD$item, ignore.case = TRUE), ]
IND2_FD_code <- merge(IND2_FD_code[1:(dim(IND2_FD_code)[1]-n_CES_fuel),], IND_map %>% select(CODE, item=ITEM_DLE), 
                      by="item", all.x = TRUE) %>% arrange(CODE) %>%
                rbind(IND2_FD[-(1:(dim(IND2_FD)[1]-n_CES_fuel)),]%>% mutate(CODE=999))
# Since IND_map is created based on IND1, this merge operation generates NAs in CODE for IND2-only items
    
      # Sub-process: Identify IND2-only items and assign IND1 CODE
  
      # Identify the items only in IND2 and not in IND1
      missing <- IND2_FD_code %>% filter(is.na(CODE))
      
      # Save the 'missing' items temporarily and match COICOP and ICP_SEQ values used in IND1 (Manually)
      # Because this can narrow down the set I need to consider for CODE matching.
      write.table(missing, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
      
      # The mapping is saved to "For IND2-IND1 matching.xlsx"
      missing_cat <- read.xlsx("H:/MyDocuments/IO work/Bridging/CES-COICOP/For IND2-IND1 matching.xlsx", 1)
      
      # Bring in IND1 CODE values matching the COICOP and ICP_SEQ
      # This is necessary to consistenty apply the WB mapping.
      missing_cat <- missing_cat %>% select(item, ICP_SEQ, COICOP1, COICOP2) %>% 
        left_join(IND_map %>% select(CODE, ICP_SEQ, COICOP1, COICOP2, Surv_Heading) 
                  %>% mutate(COICOP1=as.numeric(COICOP1), COICOP2=as.numeric(COICOP2)))
      write.table(missing_cat, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
      # Saved to the second sheet in "For IND2-IND1 matching.xlsx"
      
      # Final IND2-only items with IND1 CODE assigned 
      missing_cat <- read.xlsx("H:/MyDocuments/IO work/Bridging/CES-COICOP/For IND2-IND1 matching.xlsx", 2) %>% select(item, CODE)

# Consolidate the CODE field for all IND2 items 
IND2_FD_code <- IND2_FD_code %>% left_join(missing_cat, by="item")
IND2_FD_code <- IND2_FD_code %>% mutate(CODE.x = ifelse(is.na(CODE.x), CODE.y, CODE.x)) %>% 
  select(-CODE.y) %>% rename(CODE = CODE.x)

# As a result of this CODE consolidation, IND2_FD_code has items with the same CODEs. 
# I need to summarize them to consistently convert them to ICP classifcation.
a <- IND2_FD_code %>% group_by(CODE) %>% summarise_if(is.numeric, sum)

# And I need to bring in all unique CODE values from IND1 and have zero values for IND1-only CODE items.
# Because we are simply going to matrix-multiplicate for ICP harmonization below.
item_code <- IND_map %>% select(item=ITEM_DLE, CODE)  # All CODE-item pairs from IND1
IND2_FD_code <- item_code %>% left_join(a) %>% rbind(IND2_FD_code[-(1:(dim(IND2_FD_code)[1]-n_CES_fuel)),])  # Add fuel rows separately
IND2_FD_code <- IND2_FD_code %>% arrange(CODE)  # Now in the identical order as IND_FD_code
IND2_FD_code[is.na(IND2_FD_code)] <- 0


#################################
### 1.2.2. IND2 by household  ###
#################################

# Read in IND2_FD_ALL
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_AllHHConsump.Rda")
IND2_FD_ALL <- IND2_FD_ALL[-grep("taxes", IND2_FD_ALL$item, ignore.case = TRUE), ]
names(IND2_FD_ALL)[-1] <- paste0("H", names(IND2_FD_ALL)[-1])  # No numerical column names

# For the consideration for the running time, treat nonfuel separately (smaller)
idx_nonfuel <- 1:(dim(IND2_FD_ALL)[1]-n_CES_fuel)

# Do multiple operations above in this dplyr statement (takes somewhat long)
IND2_FD_nonfuel <- IND2_FD_ALL %>% slice(idx_nonfuel) %>%
  left_join(IND_map %>% select(CODE, ITEM_DLE), by=c("item" = "ITEM_DLE")) %>% 
  left_join(data.table(missing_cat), by="item") %>% mutate(CODE.x = ifelse(is.na(CODE.x), CODE.y, CODE.x)) %>%
  select(-CODE.y) %>% rename(CODE = CODE.x) 

# Set it as data.table and set the key
setDT(IND2_FD_nonfuel, key="CODE") 
save(IND2_FD_nonfuel, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_HHnonFuel_w_CODE.Rda")

# Combine the fuel rows (with CODE 999)
# This still does not have IND1-only items.
l <- list(IND2_FD_nonfuel, IND2_FD_ALL[-idx_nonfuel,] %>% mutate(CODE=999))
IND2_FD_ALL <- rbindlist(l)
IND2_FD_ALL[is.na(IND2_FD_ALL)] <- 0

# Bring CODE to the front
IND_FD_ALL <- IND_FD_ALL %>% select(item, CODE, everything())
IND2_FD_ALL <- IND2_FD_ALL %>% select(item, CODE, everything())

setDT(IND_FD_ALL, key="CODE")
setDT(IND2_FD_ALL, key="CODE")
save(IND_FD_ALL, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_AllHH_w_CODE.Rda")
save(IND2_FD_ALL, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_AllHH_w_CODE.Rda") # 340 rows w/ Code

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_AllHH_w_CODE.Rda") 
IND_only <- setdiff(IND_FD_ALL$CODE, IND2_FD_ALL$CODE)    # CODE missing in IND2 (IND1-only items)
IND2_dup <- as.numeric(names(which(table(IND2_FD_ALL$CODE) > 1)))    # more than one observations per CODE. Need to aggregate in IND2

# To reduce running time, I found dividing IND2_FD_ALL is faster.
a <- IND2_FD_ALL %>% select(1:40000)
a1 <- IND2_FD_ALL %>% select(1:2,40001:80000)
a2 <- IND2_FD_ALL %>% select(1:2,80001:124645)
# Aggregate IND2 FD into the code classification of IND1 (using data.table summarising method) -> faster
system.time(a <- a[, lapply(.SD, sum, na.rm=TRUE), by = "CODE", .SDcols=-c("item")])
system.time(a1 <- a1[, lapply(.SD, sum, na.rm=TRUE), by = "CODE", .SDcols=-c("item")])
system.time(a2 <- a2[, lapply(.SD, sum, na.rm=TRUE), by = "CODE", .SDcols=-c("item")])

# combine a, a1, a2
# This still does not have IND1-only items.
IND2_in_IND1 <- cbind.dt.simple(a, a1 %>% select(-1), a2 %>% select(-1))
save(IND2_in_IND1, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_in_IND1.Rda") 

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_in_IND1.Rda") 

setDT(IND2_in_IND1, key="CODE")
setDT(item_code, key="CODE")
# IND2_in_IND1 <- item_code %>% left_join(IND2_in_IND1)
# b <- data.table::merge(item_code, IND2_in_IND1, all.x=TRUE)
IND2_in_IND1 <- IND2_in_IND1[item_code]   # merge data.tables (left join) - FASTER!   # 331 rows w/o fuels
setcolorder(IND2_in_IND1, c("item", "CODE", names(IND2_in_IND1)[-c(1,length(IND2_in_IND1))])) 

l <- list(IND2_in_IND1, IND2_FD_ALL[(dim(IND2_FD_ALL)[1]-n_CES_fuel+1):dim(IND2_FD_ALL)[1], ])
IND2_FD_ALL <- rbindlist(l)
# IND2_FD_ALL[is.na(IND2_FD_ALL)] <- 0
for (i in seq_along(IND2_FD_ALL))  set(IND2_FD_ALL, i=which(is.na(IND2_FD_ALL[[i]])), j=i, value=0)  # Replace NAs to 0

save(IND2_FD_ALL, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_AllHH_compatible.Rda") # 344 rows w/ Code (same as IND1)


######################################
### Convert CES rows into ICP rows ###
######################################

# Need to separatly handle the fuel rows  (in USD PPP 2010)
# Fianlly get 164 harmonized ICP rows for all 
IND_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD_code[1:(dim(IND_FD_code)[1]-n_CES_fuel),2:12]) %>%
  rbind(IND_FD_code[-(1:(dim(IND_FD_code)[1]-n_CES_fuel)),2:12]) # for all deciles and total
IND_FD_ICP <- as.matrix(IND_FD_ICP)

IND2_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND2_FD_code[1:(dim(IND2_FD_code)[1]-n_CES_fuel),-c(1,2)]) %>%
  rbind(IND2_FD_code[-(1:(dim(IND2_FD_code)[1]-n_CES_fuel)),-c(1,2)]) # for all deciles and total
IND2_FD_ICP <- as.matrix(IND2_FD_ICP)

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_AllHH_w_CODE.Rda")
IND_FD_ICP_AllHH <- crossprod(CES_ICP_IND, as.matrix(IND_FD_ALL[1:(dim(IND_FD_ALL)[1]-n_CES_fuel),-c(1,2), with=FALSE])) %>%
  rbind(as.matrix(IND_FD_ALL[-(1:(dim(IND_FD_ALL)[1]-n_CES_fuel)),-c(1,2) , with=FALSE]))

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_AllHH_compatible.Rda")
IND2_FD_ICP_AllHH <- crossprod(CES_ICP_IND, as.matrix(IND2_FD_ALL[1:(dim(IND2_FD_ALL)[1]-n_CES_fuel),-c(1,2), with=FALSE])) %>%
  rbind(as.matrix(IND2_FD_ALL[-(1:(dim(IND2_FD_ALL)[1]-n_CES_fuel)),-c(1,2) , with=FALSE]))

# In the end, all we need is this ICP matrices
rm(IND_FD, IND_FD_code, IND_FD_ALL, IND_FD_ALL_ICP)
rm(IND2_FD, IND_FD2_code, IND2_FD_ALL, IND2_FD_ALL_ICP)
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
IND2_FD_ICP_usd2004 <- IND2_FD_ICP * PPP_IND / CPI_ratio_IND / EXR_IND / 1e6 # to M.USD 2007 (MER)
IND_FD_ICP_usd2007 <- IND_FD_ICP_usd2011 / IND_con_grwth
# IND_FD_ICP_usd2007 <- IND_FD_ICP_usd2011 / ((consumption_growth^4)[c(as.numeric(icp_ntnu$COICOP1)[1:151],rep(4,n_CES_fuel)),])
# This scaling by consumption_growth will go obsolete and simply use IND_con_grwth.

# All households
IND_FD_AllHH_2011 <- IND_FD_ICP_AllHH * PPP_IND / CPI_ratio_IND / EXR_IND 
IND2_FD_AllHH_2004 <- IND2_FD_ICP_AllHH * PPP_IND / CPI_ratio_IND / EXR_IND  # to USD 2007 (MER)
IND_FD_ICP_AllHH <- IND_FD_AllHH_2011 / IND_con_grwth   # to USD 2007 (MER)   # An estimate for hh consumption in 2007


#### 2.1 Brazil - Inflation & exchange rate

# BRA1 is for 2008-2009
BRA_FD <- data.frame(item=ICP_catnames) %>% left_join(BRA_FD)  # Join to make items consistent with the standardized names and order
BRA_FD[is.na(BRA_FD)] <- 0
BRA_FD_ICP_usd2007 <- as.matrix(BRA_FD[,2:12] * PPP_BRA / CPI_ratio_BRA / EXR_BRA / 1e6 / BRA_con_grwth) # to M.USD 2007
BRA1_FD_ICP_usd2007 <- cbind(BRA1_FD[,1], as.matrix(BRA1_FD[,2:12] * PPP_BRA / CPI_ratio_BRA / EXR_BRA / 1e6 / BRA_con_grwth)) # to M.USD 2007

# FD for all households
BRA_FD_ICP_AllHH <- data.frame(item=ICP_catnames) %>% left_join(BRA_FD_ALL)
BRA_FD_ICP_AllHH <- as.matrix(BRA_FD_ICP_AllHH[,-1]) * PPP_BRA / CPI_ratio_BRA / EXR_BRA / BRA_con_grwth # to USD 2007
# BRA_FD_ICP_AllHH[,-1] <- NAer(BRA_FD_ICP_AllHH[,-1]) # Faster than 
BRA_FD_ICP_AllHH[is.na(BRA_FD_ICP_AllHH)] <- 0

save(IND_FD_ICP_AllHH, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_FD_harmonized.Rda")
save(BRA_FD_ICP_AllHH, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_FD_harmonized.Rda")

load( file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_FD_harmonized.Rda")
load( file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_FD_harmonized.Rda")

#### 3.1 France - Inflation 

# This is in 109 NTNU COICOP classifications.
a <- WDI(country = "FR", indicator = "NE.CON.PETC.KD", start = 2007, end = 2011, extra = FALSE, cache = NULL)
consumption_growth_FR <- a$NE.CON.PETC.KD[1]/a$NE.CON.PETC.KD[5]
FRA_FD_ICP_usd2007 <- as.matrix(fd_decile_FRA / CPI_ratio_FRA) / consumption_growth_FR # M.EUR to M.USD 2007 [length: 109]

GetTotalEmbeddedEnergy <- function(country='IN') {

  if(country=='ALL') {
    cty_idx_fd <- 1:dim(final_demand)[2]
  }
  else {
    cty_place <- which(exio_ctys==country)
    cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
    cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  }
  
  emb_energy <- indirect_E_int %*% as.matrix(final_demand[,cty_idx_fd])
  emb_energy <- cbind(emb_energy, rowSums(emb_energy))
  
  # country <- countrycode(country, "iso2c", "iso3c")
  # pop2007 <- eval(parse(text=paste0(country, "_pop_2007")))
  
  # return(colSums(emb_energy)/pop2007*1000)  # in GJ/capita
  return(colSums(emb_energy))  # in TJ
}

global_total <- 0
for (i in exio_ctys) {
  country_total <- GetTotalEmbeddedEnergy(i)[8]
  global_total <- global_total + country_total
  print(paste0(i, ' ', country_total))
}
