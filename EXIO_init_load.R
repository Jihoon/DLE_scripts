### This is solely about EXIOBASE2.

### Created to 
# 1. avoid running EXIO_init when starting
# 2. incorporate energy intensity initiation

path_iot <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/EXIOBASE2/mrIOT_PxP_ita_coefficient_version2.2.2/"
path_sut <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/EXIOBASE2/mrSUT_version2.2.2/"

TJ_per_MTOE <- 41870
TWh_per_MTOE <- 11.63

load(file="./Saved tables/L_inverse.Rda")
# load(file="./Saved tables/indirect_E_int.Rda")
load(file="./Saved tables/tot_use.Rda")
# load(file="./Saved tables/supplym.Rda")
load(file="./Saved tables/final_demand.Rda")
load(file="./Saved tables/tot_demand.Rda")
load(file="./Saved tables/materials.Rda")
load(file="./Saved tables/fd_materials.Rda")



### Indexing energy carriers (from "materials" matrix)
nature_input_idx            <- 1:19   # number of rows for E-carrier use after removing the row headers
emission_energy_carrier_idx <- 20:70   # number of rows for E-carrier use after removing the row headers
energy_carrier_supply_idx   <- 71:139   # number of rows for E-carrier use after removing the row headers
energy_carrier_use_idx      <- 140:208   # number of rows for E-carrier use after removing the row headers
elec_use_idx                <- 184:195   # number of rows for E-carrier use after removing the row headers
steam_use_idx               <- 201        # Column index for steam/hot water
renew_elec_use_idx          <- c(187, 188, 191:194)   # number of rows for renewable E-carrier use after removing the row headers
nonrenew_elec_use_idx       <- setdiff(elec_use_idx, renew_elec_use_idx)
gasol_use_idx               <- c(160, 161, 181)   # number of rows for E-carrier use after removing the row headers
elec_supply_idx             <- 115:126   # number of rows for E-carrier use after removing the row headers

captive_input_idx           <- c(4, 5, 26, 27, 12, 2)  # Coal, diesel, natural gas, biomass (bagass) index in energy_carrier_use block

# energy_pri_carrier_use_idx <- c(140:154, 187:188, 191:194)   # number of rows for E-carrier use after removing the row headers
# energy_sec_carrier_use_idx <- energy_carrier_use_idx[!(energy_carrier_use_idx %in% energy_pri_carrier_use_idx)]


##############################
### Intensity calculations ###
##############################


####### Temporary solution : Ignore all energy sector columns (make their use all zero)
### Note: This loses all own use and non-energy use from energy sectors (around 10% of world average).

material.name <- (read.table(paste(path_iot, "mrMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2))[,1]

energy_sector_idx_ex  <- c(64:85, 91:95, 128:146, 148, 176:182)    # Column index for energy sectors (excluding pulp and extraction sectors)
fossil.elec.idx.ex    <- c(128, 129, 133)    # Column index for energy sectors (excluding pulp and extraction sectors)
elec.idx.ex           <- 128:141    # Column index for electricity sectors 
renew.elec.idx.ex     <- c(131:132, 135:138)    # Column index for renewable electricity sectors 
nonrenew.elec.idx.ex  <- setdiff(elec.idx.ex, renew.elec.idx.ex)
energy.carrier.idx.ex <- c(20:32, 64:85, 91:95, 128:146, 148, 176:182)    # Column index for energy sectors (excluding pulp and extraction sectors)
# captive_sector_idx_ex <- c(108, 36, 104, 90, 28, 62, 101, 50, 88, 89, 55)    # Column index for major captive generation sectors (Just for India for now)
# captive_sector_names  <- EX_catnames[captive_sector_idx_ex]
# Aluminum1/2, steel, chemicals, petroleum(crude oil), paper, cement, sugar, fertilizer1/2, textiles



### Dealing with captive generation (India only now)
# Crude estimates based on literature
captive_pri <- read.xlsx("H:/MyDocuments/Analysis/Final energy/Captive power India/generation estimates by sector.xlsx", 
                         sheet=2, rowNames=TRUE, cols=1:7, rows=15:26)

# Names of energy carriers
carrier.name.fin <- gsub("Energy Carrier Use ", "", as.character(material.name[energy_carrier_use_idx]))
carrier.name.pr <- gsub("Nature Inputs: ", "", as.character(material.name[nature_input_idx]))
primary.in.use <- carrier.name.fin %in% carrier.name.pr
primary.in.use[1:2] <- TRUE # Biomass
carrier.name.fin[primary.in.use]





### Below was for initial attempt to extract FE extenstion from existing EXIO2 extension.
### Not used any more after EXIO3 extension was developed. 

# # Different sections of energy extension
# tot.priE.sect <- materials[nature_input_idx,]  # The energy extension has not intensities but total consumptions.
# tot.useE.sect <- materials[energy_carrier_use_idx,]
# tot.supplE.sect <- materials[energy_carrier_supply_idx,]
# tot.fdE.sect <- fd_materials[energy_carrier_use_idx, -seq(7, exio.fd.len, 7)] # Excluding exports
# tot.fdE.elec <- fd_materials[elec_use_idx,] 
# tot.useE.gasol <- materials[gasol_use_idx,]
# tot.useE.elec <- materials[elec_use_idx,]
# tot.supplE.elec <- materials[elec_supply_idx,]
# 
# # Interim check
# # compare global total supply and use
# # ene.sum <- data.frame(carrier=carrier.name.fin, sup=rowSums(tot.supplE.sect), use=rowSums(tot.useE.sect), fd.use=rowSums(tot.fdE.sect[, -seq(7, exio.fd.len, 7)]), 
# #            tot.use=rowSums(tot.useE.sect)+rowSums(tot.fdE.sect[, -seq(7, exio.fd.len, 7)])) %>% 
# #        mutate(diff=sup-tot.use, error=diff/sup)
# # sum(ene.sum$sup)
# # sum(ene.sum$tot.use)
# # sum(tot.priE.sect)
# 
# # Organize the energy carrier use block to remove primary values
# energy_sector_idx_all <- as.vector(sapply(seq(0,exio.len-200,200), function(x) x+energy_sector_idx_ex, simplify = "array"))
# captive_sector_idx_all <- as.vector(sapply(seq(0,exio.len-200,200), function(x) x+captive_sector_idx_ex, simplify = "array"))
# a <- tot.useE.sect
# b <- tot.useE.sect
# # a: removing primary energy uses (based on rough assumptions) from use block, leaving final energy
# # Energy sector assumed to have only electricty as final energy = All non-elec energy for energy sectors is transformative.
# a[setdiff(1:dim(a)[1], (c(elec_use_idx, steam_use_idx) - first(energy_carrier_use_idx) + 1)), energy_sector_idx_all] <- 0  # zeroing energy sectors (column-wise) except evident final energy (elec and steam)
# a[setdiff(1:dim(a)[1], (c(elec_use_idx, steam_use_idx) - first(energy_carrier_use_idx) + 1)), captive_sector_idx_all] <- 0 # zeroing captive sectors
# 
# b[setdiff(1:dim(a)[1], (c(nonrenew_elec_use_idx, steam_use_idx) - first(energy_carrier_use_idx) + 1)), energy_sector_idx_all] <- 0  # zeroing primary and secondary inputs to energy sectors (including renewable electricity)
# b[setdiff(1:dim(a)[1], (c(nonrenew_elec_use_idx, steam_use_idx) - first(energy_carrier_use_idx) + 1)), captive_sector_idx_all] <- 0   # zeroing primary and secondary inputs to captive sectors
# 
# 
# 
# ################ THIS NOT USED in the end ################
# # # Attempt to subtract captive generation from the 'energy carrier use' block (using IPFP)
# # # with sectors sum = captive generation total, carrier sum following EXIO total ratio (with matched total)
# # # Why IPFP? 
# # # 1. Captive gen GWh > total energy use (for some sectors)
# # # (Aluminum captive generation is so high in the data, but the sector's total energy use in EXIO extension is even lower than that.)
# # library(mipfp)
# # IND_ene_cap <- a[captive_input_idx, 200*(IND_place-1) + captive_sector_idx_ex]   # Total energy for captive generation sectors/carrier
# # # Finding: IPFP cannot give proper solution in this case. 
# # names(IND_ene_cap) <- EX_catnames[captive_sector_idx_ex]
# # rownames(IND_ene_cap) <- names(captive_pri)
# # sc <- sum(captive_pri)/sum(IND_ene_cap)
# # IND_total_capgen <- Ipfp(captive_pri, list(1,2), list(rowSums(captive_pri), rowSums(IND_ene_cap)*sc))$x.hat #* sum(captive_pri)
# # IND_total_capgen <- Ipfp(captive_pri, list(1,2), list(colSums(IND_ene_cap)*sc, colSums(captive_pri)))$x.hat #* sum(captive_pri)
# # IND_total_capgen[IND_total_capgen<1e-3] <- 0
# # t(IND_ene_cap) - IND_total_capgen
# #####################################################
# 
# # Try just erasing those sectors/carriers (only for India for now)
# # All of these selected energy carriers for captive is primary.
# # a[captive_input_idx, 200*(IND_place-1) + captive_sector_idx_ex] <- 0
# 
# # Numerical matrix - final energy use total
# tot.finE.sect <- a   
# row.names(tot.finE.sect) <- carrier.name.fin
# 
# # Total primary energy carved out of energy carrier use block
# tot.prim.use <- tot.useE.sect - a  # Another way to get primary energy intensity. 
# tot.prim.use.renewELrow <- tot.useE.sect - b  # All primary and secondary incl. renewable elec
# 
# ### Primary only for electricy sectors (for comparison with tot.prim.use)
# elec.sector.idx.all <- as.vector(sapply(seq(0,exio.len-200,200), function(x) x+elec.idx.ex, simplify = "array"))
# tot.prim.use.elec <- tot.prim.use
# tot.prim.use.elec[, -elec.sector.idx.all] <- 0  # This will include all secondary
# 
# tot.prim.use.elec.no2nd <- tot.prim.use.elec
# tot.prim.use.elec.no2nd[-which(primary.in.use), ] <- 0 # making secondary zero
# 
# tot.useE.sect.pri <- tot.useE.sect # This is not just for EXIO electricity sectors but all sectors with primary inputs
# tot.useE.sect.pri[-which(primary.in.use), ] <- 0 # selecting only primary carrier rows from Use block
# 
# ### Primary only for electricy sectors (now including renewable elec as primary input). Only for EXIO electricy columns
# tot.prim.use.renewELrow.Elcol <- tot.prim.use.renewELrow
# tot.prim.use.renewELrow.Elcol[, -elec.sector.idx.all] <- 0
# 
# tot.prim.use.renewELrow.Elcol.no2nd <- tot.prim.use.renewELrow.Elcol
# tot.prim.use.renewELrow.Elcol.no2nd[-which(primary.in.use), ] <- 0 # making secondary zero
# 
# 
# 
# 
# ####### Derive energy intensities #######
# # All indirect intensity matrices are in MJ/EUR
# 
# # 1.1 embodied primary energy intensity (nature input)
# y <- 1/tot_demand
# y[is.infinite(y)] <- 0 
# energy_int <- as.matrix(tot.priE.sect) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# # indirect_E_int <- energy_int %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1
# indirect_E_int <- eigenMapMatMult(energy_int, as.matrix(L_inverse)) # faster
# 
# energy_int.nobio <- energy_int[-18:-19,]   # Try removing biomass
# # indirect_E_int.nobio <- energy_int.nobio %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1
# indirect_E_int.nobio <- eigenMapMatMult(energy_int.nobio, as.matrix(L_inverse)) # faster
# 
# # 1.2 embodied primary energy intensity (tot.prim.use)
# # Test - Derive primary intensity from total primary carrier use (tot.prim.use)
# p_energy_int <- as.matrix(tot.prim.use) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# # indirect_pE_int <- p_energy_int %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1
# indirect_pE_int <- eigenMapMatMult(p_energy_int, as.matrix(L_inverse)) # faster
# 
# # 1.3 embodied primary energy intensity (tot.prim.use)
# p_energy_int.elec <- as.matrix(tot.prim.use.elec) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# indirect_pE_int.elec <- eigenMapMatMult(p_energy_int.elec, as.matrix(L_inverse)) # faster
# 
# # # 1.4 embodied primary energy intensity (tot.prim.use) after removing all secondary carriers
# # p_energy_int.elec.no2nd <- as.matrix(tot.prim.use.elec.no2nd) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# # indirect_pE_int.elec.no2nd <- eigenMapMatMult(p_energy_int.elec.no2nd, as.matrix(L_inverse)) # faster
# 
# # 1.5 embodied primary energy intensity (any primary carriers)
# # May need to consider loss between nature input and use blocks
# p_energy_int.prirow <- as.matrix(tot.useE.sect.pri) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# indirect_pE_int.elec.prirow <- eigenMapMatMult(p_energy_int.prirow, as.matrix(L_inverse)) # faster
# 
# # # 1.6 embodied primary energy intensity (tot.prim.use.renewelec)
# # # Test - Derive primary intensity from total primary carrier use (tot.prim.use)
# # p_energy_int.renewELrow <- as.matrix(tot.prim.use.renewELrow) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# # # indirect_pE_int <- p_energy_int %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1
# # indirect_pE_int.renewELrow <- eigenMapMatMult(p_energy_int.renewELrow, as.matrix(L_inverse)) # faster
# # 
# # # 1.7 embodied primary energy intensity (tot.prim.use)
# # p_energy_int.renewELrow.Elcol <- as.matrix(tot.prim.use.renewELrow.Elcol) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# # indirect_pE_int.renewELrow.Elcol <- eigenMapMatMult(p_energy_int.renewELrow.Elcol, as.matrix(L_inverse)) # faster
# # 
# # # 1.8 embodied primary energy intensity (tot.prim.use) after removing all secondary carriers
# # p_energy_int.renewELrow.Elcol.no2nd <- as.matrix(tot.prim.use.renewELrow.Elcol.no2nd) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# # indirect_pE_int.renewELrow.Elcol.no2nd <- eigenMapMatMult(p_energy_int.renewELrow.Elcol.no2nd, as.matrix(L_inverse)) # faster
# 
# 
# 
# 
# # 2. final energy (embodied)
# f_energy_int <- as.matrix(tot.finE.sect) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# indirect_fE_int <- eigenMapMatMult(f_energy_int, as.matrix(L_inverse)) # faster
# 
# # 3. electricity (embodied)
# elec_int <- as.matrix(tot.useE.elec) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# indirect_El_int <- eigenMapMatMult(elec_int, as.matrix(L_inverse)) # faster
# 
# # 4. total use block (embodied)
# totuse_int <- as.matrix(tot.useE.sect) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# indirect_use_int <- eigenMapMatMult(totuse_int, as.matrix(L_inverse)) # faster
# 
# # 5. gasoline (embodied)
# gasol_int <- as.matrix(tot.useE.gasol) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# indirect_gasol_int <- eigenMapMatMult(gasol_int, as.matrix(L_inverse)) # faster
# 
# 
# # AUX. indirect emission intensity (I don't use it now)
# # em_int <- as.matrix(emissions_2.3) %*% diag(y) / 1e6  # Derive energy intensities by dividing by total demand per sector kg/M.EUR = mg/EUR to kg/EUR
# # indirect_em_int <- as.matrix(em_int) %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1   g/EUR
# 
# 
# # Energy use vs. Demand
# # view(cbind(c(carrier.name.fin, "td"), rbind(tot.finE.sect[,IND_idx_ex], tot_demand[IND_idx_ex])))
# # view(cbind(carrier.name.fin, f_energy_int[,IND_idx_ex]))
# 
# # Compare elec share (direct vs. indirect intensities) - relying on use block...
# # This shows that we cannot simply assume direct electricity share is similar to indirect electricity share.
# # view(data.frame(sector=EX_catnames,tot.dir=colSums(totuse_int[,IND_idx_ex]), elec.dir=colSums(elec_int[,IND_idx_ex]), 
# #                 tot.ind=colSums(indirect_use_int[,IND_idx_ex]), elec.ind=colSums(indirect_El_int[,IND_idx_ex])) %>%
# #        mutate(share.elec.dir=elec.dir/tot.dir, share.elec.ind=elec.ind/tot.ind)) 
# 
# # Total Primary vs. Final 
# # This again shows that some EXIO sectors (e.g. meat) have too high final energy consumption. (TP < TF)
# view(data.frame(sector=EX_catnames, dir.pri=colSums(p_energy_int.prirow[,IND_idx_ex]), dir.fin=colSums(f_energy_int[,IND_idx_ex]),
#                 tot.pri=colSums(indirect_pE_int.elec.prirow[,IND_idx_ex]), tot.fin=colSums(indirect_fE_int[,IND_idx_ex])) %>%
#        mutate(share.dir=dir.fin/dir.pri, share.tot=tot.fin/tot.pri))
# view(data.frame(sector=EX_catnames, dir.pri=colSums(p_energy_int.prirow[,IND_idx_ex]), dir.el=colSums(elec_int[,IND_idx_ex]),
#                 tot.pri=colSums(indirect_pE_int.elec.prirow[,IND_idx_ex]), tot.el=colSums(indirect_El_int[,IND_idx_ex])) %>%
#        mutate(share.dir=dir.el/dir.pri, share.tot=tot.el/tot.pri))
# 
# 
# # Check energy total
# sum(tot.finE.sect) + sum(tot.fdE.sect)  # All final energy use (global) = 407e3 PJ (without non-energy use of energy sector), IEA 2007 has 384e3 PJ (including own use).
# sum(tot.priE.sect)  # All primary energy use (global) = 483e3 PJ, IEA has 500e3 PJ (after excluding import = 706 - 200).
# sum(tot.prim.use)   # All primary energy use from use side (global) = 398e3 PJ
# 
# 
# # rm(L_inverse, tot_use, supplym, materials_reduc)
# # rm(val_AT_rand, val_FR_rand, val_IN_rand)
# # rm(final_alloc_list_FRA, final_alloc_list_FRA_all, result_FRA, result_FRA_all)
# # rm(eHH, all_HH_f, all_HH_fl, eHH_cap)
# # gc()
# 
# # Return EXIO indirect intensity in MJ/USD2007
# GetSpecificEXIOSectorIntensity <- function(cty, exio_sect) {
#   cty_place <- which(exio_ctys==cty)
#   # cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
#   cty_idx_ex <- seq(200*(cty_place-1)+1, 200*cty_place)   # 7 final demand columns per country
#   ex_idx <- which(EX_catnames==exio_sect)
#   
#   int <- colSums(indirect_E_int[,cty_idx_ex])[ex_idx] * EXR_EUR$r   # MJ/EUR to MJ/USD2007
#   
#   return(int)
# }

