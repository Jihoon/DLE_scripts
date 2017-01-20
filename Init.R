# This file needs to be run once at the very beginning of an analysis


#################
### Constants ###
#################

### EXIO country order ###

exio_ctys <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
               "FR", "GR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
               "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", 
               "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW", 
               "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")
n_draw <- 1000
D_val_uncertainty <- 0  # or 1 : Whether to include uncertainty analysis for valuation mtx - margins and tax rates
draw_count <- 1
options(digits=3)

# DLE DB in PPP 2010$ (PPP in terms of private consumption)
# EXIO in MER 2007
# Need this PPP rate to go back to local currency in 2010
# [LCU/$]
PPP_cty = WDI(country = c("IN", "BR"), indicator = c("PA.NUS.PPP", "PA.NUS.PRVT.PP"), start = 2010, end = 2010, extra = FALSE, cache = NULL)
PPP_IND <- as.numeric(PPP_cty %>% filter(country=="India") %>% select(PA.NUS.PRVT.PP) )
PPP_BRA <- as.numeric(PPP_cty %>% filter(country=="Brazil") %>% select(PA.NUS.PRVT.PP) )

# Inflation
# Deflate currency in 2010 to 2007 (EXIO)
CPI <- WDI(country = c("IN", "BR", "FR"), indicator = "FP.CPI.TOTL", start = 2007, end = 2010, extra = FALSE, cache = NULL)

CPI_ratio_IND <- as.numeric(CPI %>% filter(year==2010 & iso2c=='IN') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==2007 & iso2c=='IN') %>% select(FP.CPI.TOTL))
CPI_ratio_BRA <- as.numeric(CPI %>% filter(year==2010 & iso2c=='BR') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==2007 & iso2c=='BR') %>% select(FP.CPI.TOTL))


# Exchange rate (MER) [LCU/$]
EXR_EUR <- WDI(country = "XC", indicator = "PA.NUS.FCRF", start = 2007, end = 2007, extra = FALSE, cache = NULL)
EXR_EUR <- EXR_EUR %>% rename(r=PA.NUS.FCRF)
EXR_cty <- WDI(country = c("IN", "BR"), indicator = "PA.NUS.FCRF", start = 2007, end = 2007, extra = FALSE, cache = NULL)
EXR_IND <- as.numeric(EXR_cty %>% filter(country=="India") %>% select(PA.NUS.FCRF))
EXR_BRA <- as.numeric(EXR_cty %>% filter(country=="Brazil") %>% select(PA.NUS.FCRF))

# HH Consumption in India 2007 [US$]
HH_CON <- WDI(country = c("IN", "BR"), indicator = c("NE.CON.PETC.CD", "NE.CON.PRVT.CD", "NE.CON.PETC.CN", "NE.CON.PRVT.KD"), 
              start = 2004, end = 2011, extra = FALSE, cache = NULL)
BRA_con_grwth <- as.numeric(HH_CON %>% filter(year==2008 & iso2c=='BR') %>% select(NE.CON.PRVT.KD) / 
                              HH_CON %>% filter(year==2007 & iso2c=='BR') %>% select(NE.CON.PRVT.KD))
IND_con_grwth <- as.numeric(HH_CON %>% filter(year==2011 & iso2c=='IN') %>% select(NE.CON.PRVT.KD) / 
                              HH_CON %>% filter(year==2007 & iso2c=='IN') %>% select(NE.CON.PRVT.KD))
IND2_con_grwth <- as.numeric(HH_CON %>% filter(year==2004 & iso2c=='IN') %>% select(NE.CON.PRVT.KD) / 
                              HH_CON %>% filter(year==2007 & iso2c=='IN') %>% select(NE.CON.PRVT.KD))

WDI(country = c("IN", "BR"), indicator = c("NE.IMP.GNFS.ZS", "NE.EXP.GNFS.ZS"), start = 2007, end = 2007, extra = FALSE, cache = NULL)

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


##############################################
###        Read in EXIO matrices           ###
##############################################

# Takes long time to run. 
# Some .Rda files are already created to save time.
# source("EXIO_init.R")



##########################################
### Read in function 'get_basic_price' ###
##########################################

source("Valuation.R")



#################################################
### Read in function 'Bridging_uncertainty.R' ###
#################################################

# The uniform random draw routine based on a qual mapping

source("Bridging_uncertainty.R")  



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

source("Read_final_demand_from_DB.R")
source("Read_direct_energy_from_DB.R")

# Read total FD for all population
# dim: n_CES_sector x 2 (or 11 for deciles)

# Comprehensive fuel sectors (union of all DBs)
DLE_fuel_types <- ConstructyFuelTypeSet() %>% arrange(fuel)

# India
IND_HH_Alldata <-selectDBdata(tables='IND1_HH')
IND_FOOD_Alldata <-selectDBdata(tables='IND1_FOOD')
save(IND_HH_Alldata, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_HH_All.Rda")
save(IND_FOOD_Alldata, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_Food_All.Rda")

IND_FD <- readFinalDemandfromDBbyDecile('IND1')
IND2_FD <- readFinalDemandfromDBbyDecile('IND2')
BRA_FD <- readFinalDemandfromDBbyDecile('BRA0')
BRA1_FD <- readFinalDemandfromDBbyDecile('BRA1')

list[IND_FD_ALL, IND_HH] <- readFinalDemandfromDBAllHH('IND1')
IND_FD_ALL <- data.table(IND_FD_ALL)
IND_HH <- data.table(IND_HH, key="hhid")
setorder(IND_HH, hhid)

list[IND2_FD_ALL, IND2_HH] <- readFinalDemandfromDBAllHH('IND2')
IND2_FD_ALL <- data.table(IND2_FD_ALL)
IND2_HH <- data.table(IND2_HH, key="hhid")
setorder(IND2_HH, hhid)

list[BRA_FD_ALL, BRA_HH] <- readFinalDemandfromDBAllHH('BRA0')
BRA_FD_ALL <- data.table(BRA_FD_ALL)
BRA_HH <- data.table(BRA_HH, key="hhid")
setorder(BRA_HH, hhid)

save(IND_FD_ALL, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_AllHHConsump.Rda")
# save(IND_FD_ALL, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_AllHHConsump_prcadj.Rda")
save(IND2_FD_ALL, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_AllHHConsump.Rda")
save(BRA_FD_ALL, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_AllHHConsump.Rda")
save(IND_HH, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_HH.Rda")
save(IND2_HH, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_HH.Rda")
save(BRA_HH, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_HH.Rda")





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



##############################################
###    Set up environment for RAS run      ###
##############################################

source("Bridge_RAS.R")



#########################################
### Get EXIO FD vectors for countries ###
#########################################

# Get IND final demand from EXIO [M.EUR to M.USD]
IND_place <- which(exio_ctys=="IN")
IND_idx_fd <- seq(7*(IND_place-1)+1, 7*IND_place)   # 7 final demand columns per country
IND_idx_ex <- seq(200*(IND_place-1)+1, 200*IND_place)   # 7 final demand columns per country
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

