# Assumption
# 1. We have seven extensions.
# TMAR: Marine transport
# TAVI: Air transport
# TROA: Road transport
# TRAI: Rail transport
# TOTH: Other transport
# NTRA: Non-transport (industry + other)
# NENE: Non-energy use

### Pasted from EXIO_init_load.R
# path_iot <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/mrIOT_PxP_ita_coefficient_version2.2.2/"
# path_sut <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/mrSUT_version2.2.2/"
# 
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/L_inverse.Rda")
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/indirect_E_int.Rda")
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/tot_use.Rda")
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/final_demand.Rda")
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/tot_demand.Rda")
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/materials.Rda")
# 
# 
# ### Indexing energy carriers (from "materials" matrix)
# nature_input_idx            <- 1:19   # number of rows for E-carrier use after removing the row headers
# emission_energy_carrier_idx <- 20:70   # number of rows for E-carrier use after removing the row headers
# energy_carrier_supply_idx   <- 71:139   # number of rows for E-carrier use after removing the row headers
# energy_carrier_use_idx      <- 140:208   # number of rows for E-carrier use after removing the row headers
# elec_use_idx                <- 184:195   # number of rows for E-carrier use after removing the row headers
# steam_use_idx               <- 201        # Column index for steam/hot water
# renew_elec_use_idx          <- c(187, 188, 191:194)   # number of rows for renewable E-carrier use after removing the row headers
# nonrenew_elec_use_idx       <- setdiff(elec_use_idx, renew_elec_use_idx)
# gasol_use_idx               <- c(160, 161, 181)   # number of rows for E-carrier use after removing the row headers
# elec_supply_idx             <- 115:126   # number of rows for E-carrier use after removing the row headers
# 
# captive_input_idx           <- c(4, 5, 26, 27, 12, 2)  # Coal, diesel, natural gas, biomass (bagass) index in energy_carrier_use block
# 
# 
# ##############################
# ### Intensity calculations ###
# ##############################
# 
# 
# ####### Temporary solution : Ignore all energy sector columns (make their use all zero)
# ### Note: This loses all own use and non-energy use from energy sectors (around 10% of world average).
# 
# materials.header <- read.table(paste(path_iot, "mrMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", nrows=2)
# energy_sector_idx_ex  <- c(64:85, 91:95, 128:146, 148, 176:182)    # Column index for energy sectors (excluding pulp and extraction sectors)
# fossil.elec.idx.ex    <- c(128, 129, 133)    # Column index for energy sectors (excluding pulp and extraction sectors)
# elec.idx.ex           <- 128:141    # Column index for electricity sectors 
# renew.elec.idx.ex     <- c(131:132, 135:138)    # Column index for renewable electricity sectors 
# nonrenew.elec.idx.ex  <- setdiff(elec.idx.ex, renew.elec.idx.ex)
# energy.carrier.idx.ex <- c(20:32, 64:85, 91:95, 128:146, 148, 176:182)    # Column index for energy sectors (excluding pulp and extraction sectors)
# captive_sector_idx_ex <- c(108, 36, 104, 90, 28, 62, 101, 50, 88, 89, 55)    # Column index for major captive generation sectors (Just for India for now)
# captive_sector_names  <- EX_catnames[captive_sector_idx_ex]
# # Aluminum1/2, steel, chemicals, petroleum(crude oil), paper, cement, sugar, fertilizer1/2, textiles
# 
# 
# # Names of energy carriers
# carrier.name.fin <- gsub("Energy Carrier Use ", "", as.character(material.name[energy_carrier_use_idx]))
# carrier.name.pr <- gsub("Nature Inputs: ", "", as.character(material.name[nature_input_idx]))
# primary.in.use <- carrier.name.fin %in% carrier.name.pr
# primary.in.use[1:2] <- TRUE # Biomass
# carrier.name.fin[primary.in.use]

# Different extensions from Arkaitz
tot.TMAR
tot.TAVI
tot.TROA
tot.TRAI
tot.TOTH
tot.INDR <- tot.NTRA # Industry + Agriculture + Fishing + Commercial
tot.NENE
tot.finalE <- list(tot.TMAR, tot.TAVI, tot.TROA, tot.TRAI, tot.TOTH, tot.INDR, tot.NENE)

####### Derive energy intensities #######
# All indirect intensity matrices are in MJ/EUR

# 1.1 embodied primary energy intensity (nature input)
y <- 1/tot_demand
y[is.infinite(y)] <- 0 
DFEI.TMAR <- as.matrix(tot.TMAR) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
DFEI.TAVI <- as.matrix(tot.TAVI) %*% diag(y)   
DFEI.TROA <- as.matrix(tot.TROA) %*% diag(y)   
DFEI.TRAI <- as.matrix(tot.TRAI) %*% diag(y)   
DFEI.TOTH <- as.matrix(tot.TOTH) %*% diag(y)   
DFEI.INDR <- as.matrix(tot.NTRA) %*% diag(y)   
DFEI.NENE <- as.matrix(tot.NENE) %*% diag(y)   
# DFEI.EXIO <- lapply(tot.finalE, function(x) {as.matrix(x) %*% diag(y)})

# TFEI EXIO (69x9600): Not used
TFEI.TMAR <- eigenMapMatMult(DFEI.TMAR, as.matrix(L_inverse)) 
TFEI.TAVI <- eigenMapMatMult(DFEI.TAVI, as.matrix(L_inverse)) 
TFEI.TROA <- eigenMapMatMult(DFEI.TROA, as.matrix(L_inverse)) 
TFEI.TRAI <- eigenMapMatMult(DFEI.TRAI, as.matrix(L_inverse)) 
TFEI.TOTH <- eigenMapMatMult(DFEI.TOTH, as.matrix(L_inverse)) 
TFEI.INDR <- eigenMapMatMult(DFEI.INDR, as.matrix(L_inverse)) 
TFEI.NENE <- eigenMapMatMult(DFEI.NENE, as.matrix(L_inverse)) 
# TFEI.EXIO <- lapply(DFEI.EXIO, function(x) {eigenMapMatMult(x, as.matrix(L_inverse))})

DFEI.INDR.elec <- DFEI.INDR[elec_rows,]
DFEI.INDR.nonelec <- DFEI.INDR[-elec_rows,]
TFEI.INDR.elec <- TFEI.INDR[elec_rows,]
TFEI.INDR.nonelec <- TFEI.INDR[-elec_rows,]



# Run_rIPFP just once (save time) - Adopted from DeriveIntensities()
n_draw <- 10

countries <- c('IND', 'BRA', 'ZAF')
final_alloc_list <- sapply(countries, Run_rIPFP, qual_map_init=bridge_ICP_EXIO_q[,-1], simplify=FALSE)
names(final_alloc_list) <- unlist(countries)

# Assumption: The efficiency gains are applied equally globaly.
TFEI.ApplyKeyTechImprovement <- function(country='IND', dfei, coeff=c(steel=0.27, alu=0.45, cement=0.8, brick=0.8)) {
	exio.index <- c(104, 108, 100, 101) # Same order as coeff
	exio.index.global <- as.vector(sapply(seq(0,9400,200), function(x) x+exio.index, simplify = "array"))
	n_years <- Year.end - Year.base
	coeff.ann <- coeff^(1/n_years)
	coeff.obs <- lapply(Year.obs, function(x) coeff.ann^(x-Year.base)) # Coefficients for observation years
	names(coeff.obs) <- paste0('Y',Year.obs)
	
	update.DFEI.eff <- function(coef) {
	  coef <- rep(coef, 48) # Copying for 48 exio regions
	  dfei[, exio.index.global] <- dfei[, exio.index.global] %*% diag(coef)
	  return(dfei)}
	
	DFEI.obs <- lapply(coeff.obs, update.DFEI.eff)
	TFEI.obs <- lapply(DFEI.obs, function(d) {eigenMapMatMult(d, as.matrix(L_inverse)) })
	
	TFEI.icp.draw <- lapply(TFEI.obs, SetupSectorIntensities,  # TFEI.obs input as final.intensity.mat
	            mapping_list=final_alloc_list[[country]][[1]], not_conv_idx=final_alloc_list[[country]][[2]], country=countrycode(country,"iso3c", "iso2c"),
	            type='final', pri.intensity.mat=indirect_E_int)
	
	TFEI.icp.mean <- sapply(TFEI.icp.draw, colMeans)
	
	return(data.frame(name=ICP_catnames,TFEI.icp.mean))
}

TFEI.ApplyKeyTechImprovement.EXIO <- function(country='IND', dfei=DFEI.INDR.elec, coeff=c(steel=0.27, alu=0.45, cement=0.8, brick=0.8)) {
  exio.index <- c(104, 108, 100, 101) # Same order as coeff
  exio.index.global <- as.vector(sapply(seq(0,9400,200), function(x) x+exio.index, simplify = "array"))
  n_years <- Year.end - Year.base
  coeff.ann <- coeff^(1/n_years)
  Year.obs <- c(Year.base, seq(2020, Year.end, 10))
  coeff.obs <- lapply(Year.obs, function(x) coeff.ann^(x-Year.base)) # Coefficients for observation years
  names(coeff.obs) <- paste0('Y',Year.obs)
  
  update.DFEI.eff <- function(coef) {
    coef <- rep(coef, 48) # Copying for 48 exio regions
    dfei[, exio.index.global] <- dfei[, exio.index.global] %*% diag(coef)
    return(dfei)}
  
  DFEI.obs <- lapply(coeff.obs, update.DFEI.eff)
  TFEI.obs <- lapply(DFEI.obs, function(d) {eigenMapMatMult(d, as.matrix(L_inverse)) })
  
  cty_place <- which(exio_ctys==countrycode(country, "iso3c", "iso2c"))
  cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  TFEI.exio.allregion <- sapply(TFEI.obs, colMeans)[cty_idx,]
  
  return(data.frame(name=t(EX_catnames),TFEI.exio.allregion))
}

# calls
# TFEI.ICP.IND.TMAR <- TFEI.ApplyKeyTechImprovement("IND", DFEI.TMAR) #x7
# TFEI.ICP.BRA.TMAR <- TFEI.ApplyKeyTechImprovement("BRA", DFEI.TMAR) #x7
# TFEI.ICP.ZAF.TMAR <- TFEI.ApplyKeyTechImprovement("ZAF", DFEI.TMAR) #x7
# Summurized as following:
# TFEI.ICP.IND <- lapply(DFEI.EXIO, function(x) {TFEI.ApplyKeyTechImprovement("IND", x)})
# TFEI.ICP.BRA <- lapply(DFEI.EXIO, function(x) {TFEI.ApplyKeyTechImprovement("BRA", x)})
# TFEI.ICP.ZAF <- lapply(DFEI.EXIO, function(x) {TFEI.ApplyKeyTechImprovement("ZAF", x)})

a <- TFEI.ApplyKeyTechImprovement("IND", p_energy_int.prirow, coeff=c(0.1, 0.1, 0.1, 0.1)) #x7

TFEI.EXIO.ts <- sapply(countries, TFEI.ApplyKeyTechImprovement.EXIO, dfei=p_energy_int.prirow, simplify=FALSE, USE.NAMES = TRUE) # Returns a list
# TFEI.IND <- TFEI.ApplyKeyTechImprovement.EXIO("IND", p_energy_int.prirow) 
# TFEI.BRA <- TFEI.ApplyKeyTechImprovement.EXIO("BRA", p_energy_int.prirow) 
# TFEI.ZAF <- TFEI.ApplyKeyTechImprovement.EXIO("ZAF", p_energy_int.prirow) 
# a <- TFEI.ApplyKeyTechImprovement("BRA", p_energy_int.prirow, coeff=c(0.1, 0.1, 0.1, 0.1)) #x7
# a <- TFEI.ApplyKeyTechImprovement("ZAF", p_energy_int.prirow, coeff=c(0.1, 0.1, 0.1, 0.1)) #x7
b <- a %>% mutate(r.improv=Y2050/Y2008) %>% filter(name=="")

# Health/Education TFEI (long format)
idx.HE <- 174:175
a <- do.call("rbind", lapply(seq_along(TFEI.EXIO.ts), function(x, name, i) {
  temp <- data.frame(t(x[[i]][idx.HE,-1])) 
  names(temp) <- c("Education", "Health")
  temp <- temp %>% mutate(Country=name[i], Year=names(x[[i]])[-1])
  return(temp)
  }, name=names(TFEI.EXIO.ts), x=TFEI.EXIO.ts))



# Return EXIO indirect intensity in MJ/USD2007
GetSpecificEXIOSectorIntensity <- function(cty, exio_sect) {
  cty_place <- which(exio_ctys==cty)
  # cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  cty_idx_ex <- seq(200*(cty_place-1)+1, 200*cty_place)   # 7 final demand columns per country
  ex_idx <- which(EX_catnames==exio_sect)
  
  int <- colSums(indirect_E_int[,cty_idx_ex])[ex_idx] * EXR_EUR$r   # MJ/EUR to MJ/USD2007
  
  return(int)
}