# This file needs to be run once at the very beginning of an analysis


#################
### Constants ###
#################

### EXIO country order ###

exio_ctys <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
               "FR", "GR", 
               "HR", # Added new at EXIO3
               "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
               "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", 
               "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW", 
               "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")

num.cty <- length(exio_ctys)
exio.len <- length(exio_ctys)*200
exio.fd.len <- length(exio_ctys)*7

n_draw <- 1000
D_val_uncertainty <- 0  # or 1 : Whether to include uncertainty analysis for valuation mtx - margins and tax rates
draw_count <- 1
options(digits=3)

# DLE DB in PPP 2010$ (PPP in terms of private consumption)
# EXIO in MER 2007
# Need this PPP rate to go back to local currency in 2010
# [LCU/$]
PPP_cty = WDI(country = c("IN", "BR", "ZA"), indicator = c("PA.NUS.PPP", "PA.NUS.PRVT.PP"), start = 2010, end = 2010, extra = FALSE, cache = NULL)
PPP_IND <- as.numeric(PPP_cty %>% filter(country=="India") %>% select(PA.NUS.PRVT.PP))
PPP_BRA <- as.numeric(PPP_cty %>% filter(country=="Brazil") %>% select(PA.NUS.PRVT.PP))
PPP_ZAF <- as.numeric(PPP_cty %>% filter(country=="South Africa") %>% select(PA.NUS.PRVT.PP))

# Inflation
# Deflate currency in 2010 to 2007 (EXIO)
CPI <- WDI(country = c("IN", "BR", "FR", "ZA"), indicator = "FP.CPI.TOTL", start = 2007, end = 2015, extra = FALSE, cache = NULL)

CPI_ratio_IND <- as.numeric(CPI %>% filter(year==2010 & iso2c=='IN') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==2007 & iso2c=='IN') %>% select(FP.CPI.TOTL))
CPI_ratio_BRA <- as.numeric(CPI %>% filter(year==2010 & iso2c=='BR') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==2007 & iso2c=='BR') %>% select(FP.CPI.TOTL))
CPI_ratio_ZAF <- as.numeric(CPI %>% filter(year==2010 & iso2c=='ZA') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==2007 & iso2c=='ZA') %>% select(FP.CPI.TOTL))


# Exchange rate (MER) [LCU/$]
EXR_EUR <- WDI(country = "XC", indicator = "PA.NUS.FCRF", start = 2007, end = 2007, extra = FALSE, cache = NULL)
EXR_EUR <- EXR_EUR %>% rename(r=PA.NUS.FCRF)
EXR_cty <- WDI(country = c("IN", "BR", "ZA"), indicator = "PA.NUS.FCRF", start = 2007, end = 2007, extra = FALSE, cache = NULL)

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
                              HH_CON %>% filter(year==2007 & iso2c=='BR') %>% select(NE.CON.PRVT.KD))
IND_con_grwth <- as.numeric(HH_CON %>% filter(year==2011 & iso2c=='IN') %>% select(NE.CON.PRVT.KD) / 
                              HH_CON %>% filter(year==2007 & iso2c=='IN') %>% select(NE.CON.PRVT.KD))
ZAF_con_grwth <- as.numeric(HH_CON %>% filter(year==2010 & iso2c=='ZA') %>% select(NE.CON.PRVT.KD) / 
                              HH_CON %>% filter(year==2007 & iso2c=='ZA') %>% select(NE.CON.PRVT.KD))
# IND2_con_grwth <- as.numeric(HH_CON %>% filter(year==2004 & iso2c=='IN') %>% select(NE.CON.PRVT.KD) / 
#                               HH_CON %>% filter(year==2007 & iso2c=='IN') %>% select(NE.CON.PRVT.KD))

# Imports/Exports of goods and services (% of GDP)
WDI(country = c("IN", "BR"), indicator = c("NE.IMP.GNFS.ZS", "NE.EXP.GNFS.ZS"), start = 2007, end = 2007, extra = FALSE, cache = NULL)


Popul <- WDI(country = c("IN", "BR", "FR", "ZA"), indicator = "SP.POP.TOTL", start = 2007, end = 2015, extra = FALSE, cache = NULL)

BRA_pop_2007 <- Popul %>% rename(pop=SP.POP.TOTL) %>% filter(iso2c=="BR" & year==2007) %>% select(pop) %>% as.numeric()#1.9e8
IND_pop_2007 <- Popul %>% rename(pop=SP.POP.TOTL) %>% filter(iso2c=="IN" & year==2007) %>% select(pop) %>% as.numeric()#1.159e9
ZAF_pop_2007 <- Popul %>% rename(pop=SP.POP.TOTL) %>% filter(iso2c=="ZA" & year==2007) %>% select(pop) %>% as.numeric()#1.159e9



##############################################
###        Read in EXIO matrices           ###
##############################################

# Takes long time to run. 
# Some .Rda files are already created to save time.
# source("EXIO_init.R")

# EXIO_init_load.R derives energy intensity matrices.
# But since EXIO3, these matrices are read in 'import_EXIO_FE_extension.R'
# Now these intensities are based on 2008 IEA balance and fed into dfei.exio and tfei.exio.
# Using 2008 instead of 2007 because the Indian final electricity consumption breakdown into industries doesn't exist until 2007.
source("EXIO_init_load.R") # EXIO2 read
source("DLE_integration_functions.R")  
source("Import_EXIO3_FE_extension.R")  # Incorporate EXIO3

# For EXIO3, L_inverse/final_demand/tot_demand should be updated.



#########################################
### Get EXIO FD vectors for countries ###
#########################################

# Get IND final demand from EXIO [M.EUR to M.USD]
IND_place <- which(exio_ctys=="IN")
IND_idx_fd <- seq(7*(IND_place-1)+1, 7*IND_place)   # 7 final demand columns per country
IND_idx_ex <- seq(200*(IND_place-1)+1, 200*IND_place)   # 200 EXIO comodities
IND_idx_ex.i <- seq(163*(IND_place-1)+1, 163*IND_place)   # 163 EXIO industries
IND_fd_ex <- matrix(final_demand[,IND_idx_fd[1]], nrow=200) / EXR_EUR$r  # to M.USD (2007 MER)
IND_fd_exio <- rowSums(IND_fd_ex) # Sum all HH FD across countries
IND_fd_exio_imp <- rowSums(IND_fd_ex[,-IND_place]) # Sum all HH FD across countries

# Get BRA final demand from EXIO [M.EUR to M.USD]
BRA_place <- which(exio_ctys=="BR")
BRA_idx_fd <- seq(7*(BRA_place-1)+1, 7*BRA_place)   # 7 final demand columns per country
BRA_idx_ex <- seq(200*(BRA_place-1)+1, 200*BRA_place)   # 7 final demand columns per country
# Issue: This 'final_demand' for BRA gives too small values for electricity expenditure.
# Instead I can use the column from 'BR_output.xls' file.
# BRA_fd_ex <- matrix(final_demand[,BRA_idx_fd[1]], nrow=200)
# BRA_fd_exio <- rowSums(BRA_fd_ex) # Sum all HH FD across countries
# BRA_fd_exio_imp <- rowSums(BRA_fd_ex[,-BRA_place]) # Sum all HH FD across countries
BRA_fd_ex <- read_excel("H:/MyDocuments/IO work/Valuation/BR_output.xls", sheet="usebptot", skip=14, col_names=FALSE)
# Issue: Brazil FD has zero education expediture. (reasons unknown)
# Simply replace the zero with the values found on actual BRA IO 
BRA_fd_exio <- as.matrix(BRA_fd_ex[1:200,169]) 
BRA_fd_exio[174] <- 15600  # M Euro
BRA_fd_exio <- BRA_fd_exio / EXR_EUR$r  # to M.USD 2007
# The value 15600 is from H:\MyDocuments\IO work\Bridging\CES-COICOP\BRA IO FD comparison.xlsx

# Get ZAF final demand from EXIO [M.EUR to M.USD]
ZAF_place <- which(exio_ctys=="ZA")
ZAF_idx_fd <- seq(7*(ZAF_place-1)+1, 7*ZAF_place)   # 7 final demand columns per country
ZAF_idx_ex <- seq(200*(ZAF_place-1)+1, 200*ZAF_place)   # 200 EXIO comodities
ZAF_idx_ex.i <- seq(163*(ZAF_place-1)+1, 163*ZAF_place)   # 163 EXIO industries
ZAF_fd_ex <- matrix(final_demand[,ZAF_idx_fd[1]], nrow=200) / EXR_EUR$r  # to M.USD (2007 MER)
ZAF_fd_exio <- rowSums(ZAF_fd_ex) # Sum all HH FD across countries



#########################################
### Read in COICOP-EXIO Qual mapping  ###
#########################################

# Issue: This is to be replaced by 'bridge_ICP_EXIO_q' (currently in Map_CES_COICOP.R).
#       But bridge_COICOP_EXIO_q is used as a base for contructing bridge_ICP_EXIO_q.
#       I will move the scripts here (or call from here) once it is being used
n_sector_coicop <- 109

# Mapping <- system.file("COICOP3_EXIO_bridge.xlsx", package = "XLConnect")
# wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Uncertainty/COICOP3_EXIO_bridge.xlsx")
wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/COICOIP_EXIO_Qual_UN_Edited.xlsx")

# Qualitative mapping (0 or 1)
# bridge_COICOP_EXIO_q <- XLConnect::readWorksheet(wb, sheet="Qual_DK+FR", header=FALSE, 
#                                                  startRow=3, endRow=2+n_sector_coicop, startCol=2, forceConversion=T)
bridge_COICOP_EXIO_q <- XLConnect::readWorksheet(wb, sheet="COICOIP_EXIO_Qual_UN", header=FALSE, 
                                                 startRow=2, endRow=1+n_sector_coicop, startCol=1, endCol= 201, forceConversion=T)

# Compare Gibran's updated qual mapping
# wb <- XLConnect::loadWorkbook("C:/Users/min/Dropbox/HH environmental consumption/NTNU/product classification/COICOP3_EXIO_bridge.xlsx")
# bridge_ntnu <- XLConnect::readWorksheet(wb, sheet="qualitative 0_1", header=FALSE, 
#                                                  startRow=3, endRow=2+n_sector_coicop, startCol=3, forceConversion=T)

# Final COICOP classification with 109 headings
COICOP_catnames2 <- XLConnect::readWorksheet(wb, sheet="COICOIP_EXIO_Qual_UN", header=FALSE, startRow=2, endRow=1+n_sector_coicop, startCol=1, endCol=1)
EX_catnames <- XLConnect::readWorksheet(wb, sheet="COICOIP_EXIO_Qual_UN", header=FALSE, startRow=1, endRow=1, startCol=2)

# Issue: This qual mapping may change depending on countries, which we need to tackle then.



#####################################################
###     Treating CES fuel sectors differently     ###
#####################################################

wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/CES_fuel_EXIO.xlsx")
bridge_fuel_EXIO_q  <- XLConnect::readWorksheet(wb, "Sheet1", header=TRUE, forceConversion=T, 
                                                startRow=2, startCol=3, endCol=202) 
DLE_fuel_sector_Q  <- XLConnect::readWorksheet(wb, "Sheet2", header=TRUE, forceConversion=T, 
                                               startRow=2, startCol=3) 
DLE_fuelnames_std  <- XLConnect::readWorksheet(wb, "Sheet1", header=TRUE, forceConversion=T, 
                                                startRow=2, startCol=2, endCol=2) 
DLE_fuel_sector_Q[is.na(DLE_fuel_sector_Q)] <- 0
bridge_fuel_EXIO_q[is.na(bridge_fuel_EXIO_q)] <- 0
names(bridge_fuel_EXIO_q) <- EX_catnames
row.names(bridge_fuel_EXIO_q) <- DLE_fuelnames_std[,1]
names(DLE_fuelnames_std) <- "item"



############################################################
### Read final demand vector from each country's CES DB  ###
############################################################
# source("P:/ene.general/DecentLivingEnergy/Surveys/Scripts/00 Load required packages.R")
# source("P:/ene.general/DecentLivingEnergy/Surveys/Scripts/01 Load generic helper functions.R")
source("P:/ene.general/DecentLivingEnergy/Surveys/Generic function to access database.R")
# source("P:/ene.general/DecentLivingEnergy/Surveys/Scripts/Functions for building Oracle DB tables.R")

source("Read_final_demand_from_DB.R")

source("Read_direct_energy_from_DB.R")

# Read total FD for all population
# dim: n_CES_sector x 2 (or 11 for deciles)

# Comprehensive fuel sectors (union of all DBs)
DLE_fuel_types <- ConstructyFuelTypeSet() %>% arrange(fuel)

# Reading and constructing matrices
# source("Read_DLE_DB.R") 

# IND
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_FD.Rda")
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_HH.Rda")
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_AllHHConsump.Rda")
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_FUEL_Alldata.Rda") # IND_FUEL_Alldata
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_FD_harmonized.Rda") # IND_FD_ICP_AllHH

# ZAF
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/ZAF_FD.Rda")
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/ZAF_HH.Rda")
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/ZAF_AllHHConsump.Rda")


##############################################
###     Read in ICP-EXIO Qual mapping      ###
##############################################

# This is already excuted and saved in a file.
# Don't need to run everytime.

# source("Generate_base_ICP-EXIO_mapping.R")
n_sector_icp <- 151  # Num of ICP sectors
n_sector_icp_fuel <- n_sector_icp + dim(DLE_fuelnames_std)[1]



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

# wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/ICP_EXIO_Qual_Edited.xlsx")
# bridge_ICP_EXIO_q  <- XLConnect::readWorksheet(wb, "ICP_EXIO_Q_nochange", header=TRUE, forceConversion=T, endRow=152, endCol=201) 

wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/ICP_EXIO_Qual_UN_Edited.xlsx")
bridge_ICP_EXIO_q  <- XLConnect::readWorksheet(wb, "ICP_EXIO_Qual_UN2", header=TRUE, 
                                               forceConversion=T, endCol=201) 
ICP_catnames <- bridge_ICP_EXIO_q[,1]



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

source("rIPFP - Process_WB.R")  # Read in the function 'processWBscript' and resulting mtxs for 4 countries

# Issue: I still need to match with our CES DB and final NTNU 109 classification
#        How to combine fuel consumption and other (food etc)
#       -> We decided to follow ICP headings from the WB and bridge this ICP classification to EXIO.



##############################################
###       Generate CES-ICP mapping         ###
##############################################

# Read in CES code tables, fix some mis-mappings from WB, and create CES_ICP_IDN, CES_ICP_IND, etc.
# Then I can do 
# IND_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD_code[,2])
# to get FD in ICP classification.

source("rIPFP - Map_CES_COICOP.R")
# source("Init_consumption_vectors.R")  # Run once to generate and save those vectors
source("Load_init_data.R")  






##########################################
### Read in function 'get_basic_price' ###
##########################################

source("rIPFP - Valuation.R")
# source("Load_init_data.R")  



#################################################
### Read in function 'Bridging_uncertainty.R' ###
#################################################

# The uniform random draw routine based on a qual mapping

source("rIPFP - Bridging_uncertainty.R")  



#################################################
### Read in function 'Bridging_uncertainty.R' ###
#################################################

source("rIPFP - Functions_for_intensity_analysis.R")  



##############################################
###    Set up environment for RAS run      ###
##############################################

source("rIPFP - Bridge_RAS.R")




##############################################
###    Run analysis etc.!     ###
##############################################

# Analysis_for_paper.R is the main file for analysis

ICP_food_idx <- 1:45
ICP_hhold_idx <- c(56:84, 138:151)  # Household goods/services
ICP_svc_idx <- 85:137   # Health, Transport, Communication, Recreation
ICP_fuel_idx <-152:164
ICP_all_idx <- 1:164

