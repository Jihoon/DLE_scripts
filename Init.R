options(java.parameters = "-Xmx8g") 
library(RJDBC)
library(data.table)
library(tidyr)
library(openxlsx)
library(XLConnect)
library(Surrogate)
library(ggplot2)
library(stringr)
library(plyr)
library(dplyr)
library(pastecs)
library(countrycode)
library(scatterplot3d)
library(rgl)
library(car)
library(shape)
library(graphics)
library(Surrogate)
library(fields)
library(WDI)

# This library needed to do multiple returns from functions
library(devtools)  
source_url("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

# Run RAS and construct final matrix in original dimension
library(mipfp)

setwd("H:/MyDocuments/IO work/DLE_scripts")

#################
### Constants ###
#################

### EXIO country order ###

exio_ctys <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
               "FR", "GR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
               "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", 
               "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW", 
               "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")
n_draw <- 2000
D_val_uncertainty <- 1  # or 1 : Whether to include uncertainty analysis for valuation mtx - margins and tax rates
draw_count <- 1

# DLE DB in PPP 2010$ (PPP in terms of private consumption)
# EXIO in MER 2007
# Need this PPP rate to go back to local currency in 2010
# [LCU/$]
PPP_IND = WDI(country = "IN", indicator = c("PA.NUS.PPP", "PA.NUS.PRVT.PP"), start = 2010, end = 2010, extra = FALSE, cache = NULL)
PPP_IND <- PPP_IND$PA.NUS.PRVT.PP

# Inflation
CPI <- WDI(country = c("IN", "FR"), indicator = "FP.CPI.TOTL", start = 2007, end = 2011, extra = FALSE, cache = NULL)
CPI<- CPI %>% rename(cpi=FP.CPI.TOTL)
attach(CPI)
CPI_ratio_IND <- cpi[year==2010 & iso2c=='IN'] / cpi[year==2007 & iso2c=='IN']
CPI_ratio_FRA <- cpi[year==2011 & iso2c=='FR'] / cpi[year==2007 & iso2c=='FR']
detach(CPI)

# Exchange rate (MER) [LCU/$]
EXR_EUR <- WDI(country = "XC", indicator = "PA.NUS.FCRF", start = 2007, end = 2007, extra = FALSE, cache = NULL)
EXR_EUR <- EXR_EUR %>% rename(r=PA.NUS.FCRF)
EXR_IND <- WDI(country = "IN", indicator = "PA.NUS.FCRF", start = 2007, end = 2007, extra = FALSE, cache = NULL)
EXR_IND <- EXR_IND %>% rename(r=PA.NUS.FCRF)

# HH Consumption in India 2007 [US$]
HH_CON <- WDI(country = "IN", indicator = c("NE.CON.PETC.CD", "NE.CON.PRVT.CD", "NE.CON.PETC.CN", "NE.CON.PRVT.CN"), start = 2007, end = 2011, extra = FALSE, cache = NULL)
HH_CON <- HH_CON %>% rename(hhcon=NE.CON.PRVT.CD)
HH_CON[,4:7] <- HH_CON[,4:7]/1e6 #[million US$]



#####################################################
### Read in (CES-Pseudo COICOP) mappings from WB  ###
#####################################################

### Read in ICP heading number following NTNU 109 mapping (not 100%, some ICP headings are aggregated) ###
Mapping <- system.file("ICP_SEQ.xlsx", package = "XLConnect")
wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/Worldbank/ICP_SEQ.xls")
# I added 'Sheet2' and fixed some mis-categorizations for my needs.
icp_seq <- XLConnect::readWorksheet(wb, sheet="Sheet2", header=TRUE, startRow=2, startCol=1, endCol=1, forceConversion=T)
icp_cat <- XLConnect::readWorksheet(wb, sheet="Sheet2", header=FALSE, startRow=3, startCol=3, endCol=4, forceConversion=T)
NTNU <- XLConnect::readWorksheet(wb, sheet="Sheet2", header=TRUE, startRow=2, startCol=7, endCol=8, forceConversion=T)
icp_ntnu <-cbind(icp_seq, icp_cat, NTNU)
names(icp_ntnu)[2:3] <- c("COICOP1","COICOP2")
names(icp_ntnu)[5] <- "ICP_Heading"

source("Process_WB.R")  # Read in the function 'processWBscript' and resulting mtxs for 4 countries

# Issue: I still need to match with our CES DB and final NTNU 109 classification
#        How to combine fuel consumption and other (food etc)
#       -> We decided to follow ICP headings from the WB and bridge this ICP classification to EXIO.


#########################################
### Read in COICOP-EXIO Qual mapping  ###
#########################################

# Issue: This is to be replaced by 'bridge_ICP_EXIO_q' (currently in Map_CES_COICOP.R).
#       But bridge_COICOP_EXIO_q is used as a base for contructing bridge_ICP_EXIO_q.
#       I will move the scripts here (or call from here) once it is being used
n_sector_coicop <- 109

# Mapping <- system.file("COICOP3_EXIO_bridge.xlsx", package = "XLConnect")
wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Uncertainty/COICOP3_EXIO_bridge.xlsx")

# Qualitative mapping (0 or 1)
bridge_COICOP_EXIO_q <- XLConnect::readWorksheet(wb, sheet="Qual_DK+FR", header=FALSE, 
                                                 startRow=3, endRow=2+n_sector_coicop, startCol=2, forceConversion=T)

# Compare Gibran's updated qual mapping
# wb <- XLConnect::loadWorkbook("C:/Users/min/Dropbox/HH environmental consumption/NTNU/product classification/COICOP3_EXIO_bridge.xlsx")
# bridge_ntnu <- XLConnect::readWorksheet(wb, sheet="qualitative 0_1", header=FALSE, 
#                                                  startRow=3, endRow=2+n_sector_coicop, startCol=3, forceConversion=T)

# Final COICOP classification with 109 headings
COICOP_catnames2 <- XLConnect::readWorksheet(wb, sheet="Qual_DK+FR", header=FALSE, startRow=3, endRow=2+n_sector_coicop, startCol=2, endCol=2)
EX_catnames <- XLConnect::readWorksheet(wb, sheet="Qual_DK+FR", header=FALSE, startRow=2, endRow=2, startCol=3)

# Issue: This qual mapping may change depending on countries, which we need to tackle then.

##############################################
###        Read in EXIO matrices           ###
##############################################

source("EXIO_init.R")



##########################################
### Read in function 'get_basic_price' ###
##########################################

# Currently does this only for FRA

source("Valuation.R")



#################################################
### Read in function 'Bridging_uncertainty.R' ###
#################################################

# The uniform random draw routine based on a qual mapping

source("Bridging_uncertainty.R")  



############################################################
### Read final demand vector from each country's CES DB  ###
############################################################

xlcFreeMemory()
source("P:/ene.general/DecentLivingEnergy/Surveys/Generic function to access database.R")
source("Read_final_demand_from_DB.R")
source("Read_direct_energy_from_DB.R")

# Read total FD for all population
# dim: n_CES_sector x 2 (or 11 for deciles)

# India

IND_FD <- readFinalDemandfromDBbyDecile('IND1')
IND2_FD <- readFinalDemandfromDBbyDecile('IND2')

list[IND_DE, IND_FD_DE] <- readDirectEnergyfromDBbyDecile('IND1')
list[IND2_DE, IND2_FD_DE] <- readDirectEnergyfromDBbyDecile('IND2')


# France - No DB, only summary from Lucas

wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Uncertainty/2011_FRHHBS_per_decile.xlsx")

n_sector_coicop <- 109  # Num of COICOP sectors
n_col <- 11  # Num of columns (10 deciles + avg)

# Final demand per HH [Euro/HH]
fd_decile_FRA <- XLConnect::readWorksheet(wb, sheet="ValueDecile", header=T, forceConversion=T) %>% select(2:(n_col+1))
n_hh_FRA <- 26058600 # interpolated from https://www.ined.fr/en/everything_about_population/data/france/couples-households-families/households/

fd_decile_FRA <- fd_decile_FRA * n_hh_FRA / 1e6 # [M.EUR]
fd_decile_FRA[,2:11] <- fd_decile_FRA[,2:11]/10 # 1/10 for each decile

# For later use
# IND2_FD <- readFinalDemandfromDB('IND2')
# IDN_FD <- readFinalDemandfromDB('IDN1')
# BRA_FD <- readFinalDemandfromDB('BRA1')
# ZAF_FD <- readFinalDemandfromDB('ZAF1')



##############################################
###       Generate CES-ICP mapping         ###
##############################################

# Read in CES code tables, fix some mis-mappings from WB, and create CES_ICP_IDN, CES_ICP_IND, etc.
# Then I can do 
# IND_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD_code[,2])
# to get FD in ICP classification.

source("Map_CES_COICOP.R")
source("Init_consumption_vectors.R")



##############################################
###     Read in ICP-EXIO Qual mapping      ###
##############################################

# This is already excuted and saved in a file.
# Don't need to run everytime.

source("Generate_base_ICP-EXIO_mapping.R")
n_sector_icp <- 151  # Num of ICP sectors



##############################################
###     Read in ICP-EXIO Qual mapping      ###
##############################################

# This matrix is modified externally manually based on the resulting csv from running Generate_base_ICP-EXIO_mapping.R
# to fine-allocate mostly for food-subsectors.
# The result is in H:\MyDocuments\IO work\Bridging\CES-COICOP\ICP_EXIO_Qual_Edited.xlsx
# Manually changed cells are colored in green in the xlsx file.
# Two types of manual changes
#   1. ICP item disaggregation info further details (meat -> poultry)
#   2. Some positive EXIO FD values do not match to any ICP sectors. (e.g. stone from EXIO mapped to household maintenance in ICP)
#     => can be checked by cbind(names(qual_map)[colConst_init!=0 & colSums(qual_map_init)==0], colConst_init[colConst_init!=0 & colSums(qual_map_init)==0])

wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/ICP_EXIO_Qual_Edited.xlsx")
# bridge_ICP_EXIO_q  <- XLConnect::readWorksheet(wb, "ICP_EXIO_Qual", header=TRUE, forceConversion=T, endRow=152, endCol=201)
# No yellow corrections
bridge_ICP_EXIO_q  <- XLConnect::readWorksheet(wb, "ICP_EXIO_Q_nochange", header=TRUE, forceConversion=T, endRow=152, endCol=201) 
ICP_catnames <- bridge_ICP_EXIO_q[,1]


##############################################
###    Set up environment for RAS run      ###
##############################################

source("Bridge_RAS.R")



#########################################
### Get EXIO FD vectors for countries ###
#########################################

# Get IND final demand from EXIO [M.EUR]
IND_place <- which(exio_ctys=="IN")
IND_idx_fd <- seq(7*(IND_place-1)+1, 7*IND_place)   # 7 final demand columns per country
IND_fd <- matrix(final_demand[,IND_idx_fd[1]], nrow=200)
# dim_fd <- dim(final_demand)
IND_fd_exio <- rowSums(IND_fd) # Sum all HH FD across countries
IND_fd_exio_imp <- rowSums(IND_fd[,-IND_place]) # Sum all HH FD across countries

# Get FRA final demand from EXIO [M.EUR]
FRA_place <- which(exio_ctys=="FR")
FRA_idx_fd <- seq(7*(FRA_place-1)+1, 7*FRA_place)   # 7 final demand columns per country
FRA_fd <- matrix(final_demand[,FRA_idx_fd[1]], nrow=200)
# dim_fd <- dim(final_demand)
FRA_fd_exio <- rowSums(FRA_fd) # Sum all HH FD across countries
FRA_fd_exio_imp <- rowSums(FRA_fd[,-FRA_place]) # Sum all HH FD across countries

# exio_fd <- list(IND_fd_exio, FRA_fd_exio)
# names(exio_fd) <- c('IND', 'FRA')



# # names(fd_decile)
# 
# soc_transfer_ratio <- fd_decile[,14:24]
# names(soc_transfer_ratio) <- names(fd_decile)[2:12]
# 
# fd_with_soctr <- fd_decile[,2:12] * (1+soc_transfer_ratio)  # Final demand including social transfer allocation
# # fd_with_soctr_flat <- diag((1+soc_transfer_ratio[,1])) %*% as.matrix(fd_decile[,2:12])    # Final demand including social transfer allocation
# fd_with_soctr_flat <- fd_decile[,2:12] + soc_transfer_ratio[,1]*fd_decile[,2]   # Flat means identical $/person, I guess.
# 
# 
# 
# ##################################
# ### Read in CES-COICOP mapping ### (Specifically for IND for now)
# ##################################
# 
# # Using India CES for French example for now 
# # Update: Received FRA consumption in NTNT 109 classification, so FRA doesn't use this
# # Issue: bridge_CES_COICOP may be replaced with the IND mtx from WB (after comparison)
# # Issue: This is likely to be replaced by the WB mapping.
# 
# Mapping <- system.file("IND_CES-COICOP_mapping.xlsx", package = "XLConnect")
# wb <- loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/IND_CES-COICOP_mapping.xlsx")
# 
# n_sector_coicop <- 109  # Num of COICOP sectors
# n_row <- 347  # Num of CES items
# bridge_CES_COICOP <- readWorksheet(wb, "Sheet2", header=FALSE, startRow=2, endRow=1+n_row, 
#                         startCol=3, endCol=2+n_sector_coicop, forceConversion=T)
# 
# CES_catnames <- readWorksheet(wb, "Sheet2", header=FALSE, startRow=2, endRow=1+n_row, startCol=1, endCol=1)
# # COICOP_catnames1 <- readWorksheet(wb, "Sheet2", header=FALSE, startRow=2, endRow=1, startCol=3, endCol=2+n_sector_coicop)
# 
# # Order rows of bridge mtx in the alphabatic order of CES item names
# bridge_CES_COICOP <- bridge_CES_COICOP[order(CES_catnames),]