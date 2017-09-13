# Read in neccessary EXIO tables
# And derive indirect energy intensities per EXIO sector

# setwd("H:/MyDocuments/IO work/")

path_iot <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/mrIOT_PxP_ita_coefficient_version2.2.2/"
path_sut <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/mrSUT_version2.2.2/"

# From IoT folder
final_demand <- read.table(paste(path_iot, "mrFinalDemand_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
final_demand.name <- read.table(paste(path_iot, "mrFinalDemand_version2.2.2.txt", sep=""), 
                                header=FALSE, sep="\t", dec=".", skip=1, nrow=1,  stringsAsFactors=FALSE)[4:10]
final_demand <- final_demand[,c(-1,-2,-3)]

L_inverse <- read.table(paste(path_iot, "L_inverse.txt", sep=""), header=FALSE, sep=",", dec=".")

factor_input <- read.table(paste(path_iot, "mrFactorInputs_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
factor_input <- factor_input[,c(-1,-2)]

# iot <- read.table(paste(path_iot, "mrIot_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
# iot <- iot[,c(-1,-2,-3)]

supplym <- read.table(paste(path_sut, "mrSupply_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".")

fd_materials <- read.table(paste(path_iot, "mrFDMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
fd_materials <- fd_materials[,c(-1,-2)]

# Material extension with more energy carrier resolution from NTNU (ver 2.2.0)
# However these extensions are in TJ unit, which need to be divided by total use by product to get intensities.
materials <- read.table(paste(path_iot, "mrMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
material.name <- materials[,1]
materials <- materials[,c(-1,-2)]
# materials_reduc <- read.table(paste(path_iot, "mrMaterials_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
# materials_reduc <- materials_reduc[,c(-1,-2)]

# final_demand_material <- read.table(paste(path_iot, "mrFDMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
# final_demand_material <- final_demand_material[,c(-1,-2)]

emissions <- read.table(paste(path_iot, "mrEmissions_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", 
                        skip=2, nrows=85, stringsAsFactors = FALSE)
emissions <- emissions %>% select(-V2, -V3) %>% filter(grepl('CH4|CO2|N2O', V1)) 
GHG_item <- emissions$V1
emissions <- emissions %>% select(-V1)

path_iot_2.3 <- 'C:/Users/min/SharePoint/DLE - Documents/IO/Data - EXIOBASE/extension2.3.0/'
emissions_2.3 <- read.table(paste(path_iot_2.3, "mrEmissions_pxp_version2.3.0.txt", sep=""), header=FALSE, sep="\t", dec=".", 
                        skip=2, nrows=204, stringsAsFactors = FALSE)
emissions_2.3 <- emissions_2.3 %>% select(-V3) %>% filter(V2==" air") %>% filter(grepl('CH4|CO2|N2O', V1)) 
GHG_item_2.3 <- emissions_2.3$V1
emissions_2.3 <- emissions_2.3 %>% select(-V1, -V2)
View(cbind(GHG_item_2.3, emissions_2.3[,IND_idx_ex]))
View(cbind(GHG_item_2.3, emissions_2.3[,BRA_idx_ex]))
View(cbind(GHG_item, emissions[,IND_idx_ex]))
View(cbind(GHG_item, emissions[,BRA_idx_ex]))

# From SUT folder
tot_use <- read.table(paste(path_sut, "mrUse_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
tot_use <- tot_use[,c(-1,-2,-3)]

# Get total use by product 
tot_demand <- rowSums(final_demand) + rowSums(tot_use)
tot_demand <- rowSums(final_demand) + rowSums(tot_use)
b <- rowSums(final_demand[,-seq(7, 336, 7)]) # Excluding export for global demand
fd.sum <- data.frame(name=t(EX_catnames), ind.use=rowSums(tot_use[IND_idx_ex,]), 
                     hh.fd=rowSums(final_demand[IND_idx_ex,seq(1, 336, 7)]),
                     oth.fd=rowSums(final_demand[IND_idx_ex,-seq(1, 336, 7)])) %>% 
  mutate(tot.use = hh.fd + oth.fd + ind.use)

TJ_per_MTOE <- 41870
TWh_per_MTOE <- 11.63




### Intensity calculations ###

# Indexing energy carriers
nature_input_idx <- 1:19   # number of rows for E-carrier use after removing the row headers
emission_energy_carrier_idx <- 20:70   # number of rows for E-carrier use after removing the row headers
energy_carrier_supply_idx <- 71:139   # number of rows for E-carrier use after removing the row headers
energy_carrier_use_idx <- 140:208   # number of rows for E-carrier use after removing the row headers

elec_use_idx <- 184:195   # number of rows for E-carrier use after removing the row headers
elec_supply_idx <- 115:126   # number of rows for E-carrier use after removing the row headers

energy_pri_carrier_use_idx <- c(140:154, 187:188, 191:194)   # number of rows for E-carrier use after removing the row headers
energy_sec_carrier_use_idx <- energy_carrier_use_idx[!(energy_carrier_use_idx %in% energy_pri_carrier_use_idx)]


# indirect primary energy intensity
energy_use <- materials[nature_input_idx,]  # The energy extension has not intensities but total consumptions.
y <- 1/tot_demand
y[is.infinite(y)] <- 0 
energy_int <- as.matrix(energy_use) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
indirect_E_int <- energy_int %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1


# indirect emission intensity
em_int <- as.matrix(emissions_2.3) %*% diag(y) / 1e6  # Derive energy intensities by dividing by total demand per sector kg/M.EUR = mg/EUR to kg/EUR
indirect_em_int <- as.matrix(em_int) %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1   g/EUR


# Indirect final energy intensity
####### Temporary solution : Ignore all energy sector columns (make their use all zero)
### Note: This loses all own use and non-energy use from energy sectors (around 10% of world average).

materials.header <- read.table(paste(path_iot, "mrMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", nrows=2)
energy_sector_idx_ex <- c(60, 64:85, 91:95, 128:146, 148, 176:182)    # Column index for energy sectors

# 1. Energy carrier use
carrier.name <- gsub("Energy Carrier Use ", "", as.character(material.name[energy_carrier_use_idx]))
carrier.name.pr <- material.name[nature_input_idx]

tot.useE.sect <- materials[energy_carrier_use_idx,]
tot.supplE.sect <- materials[energy_carrier_supply_idx,]
tot.fdE.sect <- fd_materials[energy_carrier_use_idx,]

# compare global total supply and use
ene.sum <- data.frame(carrier=carrier.name, sup=rowSums(tot.supplE.sect), use=rowSums(tot.useE.sect), fd.use=rowSums(tot.fdE.sect[, -seq(7, 336, 7)]), 
           tot.use=rowSums(tot.useE.sect)+rowSums(tot.fdE.sect[, -seq(7, 336, 7)])) %>% 
       mutate(diff=sup-tot.use, error=diff/sup)
sum(ene.sum$sup)
sum(ene.sum$tot.use)
sum(energy_use)

# Dealing with direct energy use from HH
# tot.useE.sect.hhfd <- fd_materials[energy_carrier_use_idx, seq(1, 336, 7)]   # Final demand w/o export 69x48
# carrier.map <- read_excel("H:/MyDocuments/Analysis/Final energy/EXIO-carrier-product-mapping.xlsx", sheet=1, skip=0, col_names=TRUE)
# diago <- function(vec) {
#   mat <- matrix(0, 69, 200)
#   mat[data.matrix(carrier.map %>% select(Carrier, Prod))] <- vec
#   return(mat)
# }
# 
# # Energy extension (69x9600) for total HH energy use by carrier
# carrier.prod <- matrix(, nrow = 69, ncol=0)
# for(i in 1:48) {
#   temp <- diago(tot.useE.sect.hhfd[,i])
#   carrier.prod <- cbind(carrier.prod, temp)
# }
# 
# # FD for fuel items
# tot.sect.hhfd <- final_demand[, seq(1, 336, 7)]
# tot.sect.hhfd <- apply(tot.sect.hhfd, 2, function(x) {rowSums(matrix(x, nrow=200))})  # 200x48
# colnames(tot.sect.hhfd) <- exio_ctys

# Get HH direct energy intensity by dividing TJ by M.EUR
# fd.mat <- do.call(rbind, replicate(dim(carrier.prod)[1], as.vector(tot.sect.hhfd), simplify=FALSE)) # for easy division
# int.hh <- carrier.prod / fd.mat  # TJ / $  69x9600
# int.hh[is.nan(int.hh)] <- 0  # mostly for non-fuel items
# int.hh[is.infinite(int.hh)] <- 0  # Some fuels have non-zero TJ with 0 expenditure. (WRONG!! EXIO)

# energy_sector_ex <- names(tot.useE.sect)[energy_sector_idx_ex] 
energy_sector_idx_ex <- as.vector(sapply(seq(0,9400,200), function(x) x+energy_sector_idx_ex, simplify = "array"))
a <- tot.useE.sect
a[setdiff(1:dim(a)[1], (elec_use_idx - first(energy_carrier_use_idx) + 1)), energy_sector_idx_ex] <- 0  # Energy sector assumed to have only electricty as final energy
tot.prim.use <- tot.useE.sect - a  # Another way to get primary energy intensity. (This does not include primary inputs to captive generation. (So underestimated))

# Numerical matrix - use total
f_energy_use <- a   
row.names(f_energy_use) <- carrier.name
sum(f_energy_use)

# Header and name column added as a data.frame (mainly for visualization)
f_energy_use.df <- rbindlist(list(materials.header[,-1], data.frame(carrier.name, f_energy_use)), use.names=FALSE, fill=FALSE, idcol=NULL) # In TJ
energy_use.df <- rbindlist(list(materials.header[,-1], data.frame(carrier.name.pr, energy_use)), use.names=FALSE, fill=FALSE, idcol=NULL) # In TJ


# 2. Emission relevent carrier use
# carrier.name.em <- gsub("Emission Relevant Energy Carrier ", "", as.character(material.name[emission_energy_carrier_idx]))
# tot.emissE.sect <- materials[emission_energy_carrier_idx,]
# tot.emissE.sect.fd <- fd_materials[emission_energy_carrier_idx, -seq(7, 336, 7)]   # Final demand w/o export
# a <- tot.emissE.sect
# a[, energy_sector_idx_ex] <- 0
# 
# # Numerical matrix - use emission
# f_energy_use.emiss <- a
# f_energy_use.emiss.name <- data.frame(name=carrier.name) %>% left_join(data.frame(name=carrier.name.em, f_energy_use.emiss))
# f_energy_use.emiss.name.fd <- data.frame(name=carrier.name) %>% left_join(data.frame(name=carrier.name.em, tot.emissE.sect.fd))
# 
# # Header and name column added as a data.frame (mainly for visualization)
# f_energy_use.emiss.df <- rbindlist(list(materials.header[,-1], f_energy_use.emiss.name), 
#                                    use.names=FALSE, fill=FALSE, idcol=NULL) # In TJ


# view(data.frame(carrier.name, use=rowSums(f_energy_use), fd.use =rowSums(tot.useE.sect.fd), 
#                 use.emission=rowSums(f_energy_use.emiss.name[, -1], na.rm=TRUE), fd.use.emission=rowSums(f_energy_use.emiss.name.fd[, -1], na.rm=TRUE)))

# Derive intensity
f_energy_int <- as.matrix(f_energy_use) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
indirect_fE_int <- f_energy_int %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1

# Compare intensities direct vs indirect, primary vs final
view(cbind(t(EX_catnames), colSums(f_energy_int[,IND_idx_ex]), colSums(energy_int[,IND_idx_ex]), 
           colSums(indirect_fE_int[,IND_idx_ex]), colSums(indirect_E_int[,IND_idx_ex])))

# Energy use vs. Demand
view(cbind(c(carrier.name, "td"), rbind(f_energy_use[,IND_idx_ex], tot_demand[IND_idx_ex])))
view(cbind(carrier.name, f_energy_int[,IND_idx_ex]))

# Test - Derive primary intensity from total primary carrier use (tot.prim.use)
p_energy_int <- as.matrix(tot.prim.use) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
indirect_pE_int <- p_energy_int %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1


# Check energy total
sum(f_energy_use) + sum(tot.useE.sect.fd)  # All final energy use (global) = 407e3 PJ (without non-energy use of energy sector), IEA 2007 has 384e3 PJ (including own use).
sum(energy_use)  # All primary energy use (global) = 483e3 PJ, IEA has 500e3 PJ (after excluding import = 706 - 200).








# To clean up the memory
save(L_inverse, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/L_inverse.Rda")
save(indirect_E_int, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/indirect_E_int.Rda")
save(tot_use, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/tot_use.Rda")
save(supplym, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/supplym.Rda")
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/L_inverse.Rda")
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/indirect_E_int.Rda")
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/tot_use.Rda")
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/supplym.Rda")
rm(L_inverse, tot_use, supplym, materials_reduc)
rm(val_AT_rand, val_FR_rand, val_IN_rand)
rm(final_alloc_list_FRA, final_alloc_list_FRA_all, result_FRA, result_FRA_all)
rm(eHH, all_HH_f, all_HH_fl, eHH_cap)
gc()

# Return EXIO indirect intensity in MJ/USD2007
GetSpecificEXIOSectorIntensity <- function(cty, exio_sect) {
  cty_place <- which(exio_ctys==cty)
  # cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  cty_idx_ex <- seq(200*(cty_place-1)+1, 200*cty_place)   # 7 final demand columns per country
  ex_idx <- which(EX_catnames==exio_sect)
  
  int <- colSums(indirect_E_int[,cty_idx_ex])[ex_idx] * EXR_EUR$r   # MJ/EUR to MJ/USD2007
  
  return(int)
}
