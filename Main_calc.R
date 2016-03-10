setwd("H:/MyDocuments/IO work/DLE_scripts")

# Focusing on France case

# Read in data
source("Init.R")

### Mapping CES-COICOP ###

# Total final demand converted to COICOP classification
# fd_tot_coicop <- t(bridge_CES_COICOP)%*%fd_tot$FD_TOT   # Using India CES for placeholder


# Get a random draw of a mapping
n_draw <- 500

# Constants
N_hh_FR <- 27568000 # Number of households in 2012
                      # https://www.ined.fr/en/everything_about_population/data/france/couples-households-families/households/
         # 25253000 # Number of households in 2005 
                      # https://en.wikipedia.org/wiki/List_of_countries_by_number_of_households
koe_to_MJ <- 42.868   # Unit conversion
BTU_to_KJ <- 0.94782  #  Conversion factors from https://www.eia.gov/cfapps/ipdbproject/docs/units.cfm
E_capita_FR <- 4117 * koe_to_MJ # MJ per capita in 2007 
                      # http://data.worldbank.org/indicator/EG.USE.PCAP.KG.OE?page=1
totE_pri_FR <- 11.221 * 10^9 * BTU_to_KJ * 1000  # Total MJ
                      
inten_FR <- 5014 * BTU_to_KJ * 1000 / 0.8  # Primary https://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=92&pid=46&aid=2&cid=FR,&syid=2007&eyid=2011&unit=BTUPUSDM
                                      # 0.8 = Exchange rate http://www.x-rates.com/average/?from=USD&to=EUR&amount=1&year=2005

# EXIO indexing
FR_place <- which(exio_ctys=="FR")
FR_idx <- seq(200*(FR_place-1)+1, 200*FR_place)  # 200 EXIO commodities per country
FR_idx_fd <- seq(7*(FR_place-1)+1, 7*FR_place)   # 7 final demand columns per country
  
# Defence spending allocation to hh
FR_fd_tot <- final_demand[FR_idx, FR_idx_fd]
names(FR_fd_tot) <- c("HH", "NPISH", "Gov", "GFCF", "Inven", "Valuable", "Export")
idx_defence <- grep("Public", EX_catnames, ignore.case=TRUE)
FR_exp_defence <- FR_fd_tot$Gov[idx_defence]
FR_defence_alloc_prop <- FR_exp_defence * 1e6 * colSums(fd_exio) / sum(colSums(fd_exio)[2:11]) / (N_hh_FR/10)
FR_defence_alloc_flat <- rep(FR_exp_defence / N_hh_FR, 11) * 1e6

null_demand <- matrix(0, 9600, 11)  # A vector to be filled with final demand values

# Dimension : n_draw x n_deciles
totE_per_hh <- vector()
intensity <- vector()
totE <- vector()

totE_per_hh_sc <- vector()
intensity_sc <- vector()
totE_sc <- vector()

totE_per_hh_sc_flat <- vector()
intensity_sc_flat <- vector()
totE_sc_flat <- vector()

totE_per_hh_sc_def <- vector()
intensity_sc_def <- vector()
totE_sc_def <- vector()

totE_per_hh_sc_def_f <- vector()
intensity_sc_def_f <- vector()
totE_sc_def_f <- vector()

totE_per_hh_sc_def_imp <- vector()
intensity_sc_def_imp <- vector()
totE_sc_def_imp <- vector()

totE_per_hh_sc_def_dom <- vector()
intensity_sc_def_dom <- vector()
totE_sc_def_dom <- vector()


### Mapping COICOP-EXIO ###

bridge_COICOP_EXIO <- get_bridge_COICOP_EXIO(bridge_COICOP_EXIO_q, n_draw)

# for each draw of bridging (COICOP-EXIO)
for (i in 1:n_draw) {
  
  # Converting COICOP vector to EXIO vector
  # Final hh demand with different elements allocated (by decile)
  fd_exio <- bridge_COICOP_EXIO[[i]] %*% as.matrix(fd_decile[,2:12])
  fd_sc_exio <- bridge_COICOP_EXIO[[i]] %*% as.matrix(fd_with_soctr)  # Because fd_with_soctr is extended, this can inflate errors.
  fd_sc_exio_f <- bridge_COICOP_EXIO[[i]] %*% as.matrix(fd_with_soctr_flat)  
  fd_sc_exio_def <- fd_sc_exio    # Proportional to income
  fd_sc_exio_def_f <- fd_sc_exio_f  # Flat
   
  # For public good e.g. defence in EXIO 173: "Public administration and defence services; compulsory social security services"
  # This is already an EXIO sector. So we don't have to go through COICOP-EXIO conversion.
  # Then, it can be added at this point after the conversion.
  fd_sc_exio_def[idx_defence,] <- FR_defence_alloc_prop
  fd_sc_exio_def_f[idx_defence,] <- FR_defence_alloc_flat
  
  FR_fd <- null_demand
  FR_sc_fd <- null_demand
  FR_sc_fd_flat <- null_demand
  FR_sc_fd_def <- null_demand
  FR_sc_fd_def_f <- null_demand
  
  FR_fd[FR_idx,] <- get_basic_price(fd_exio, "FR")
  FR_sc_fd[FR_idx,] <- get_basic_price(fd_sc_exio, "FR")
  FR_sc_fd_flat[FR_idx,] <- get_basic_price(fd_sc_exio_f, "FR")
  FR_sc_fd_def[FR_idx,] <- get_basic_price(fd_sc_exio_def, "FR")
  FR_sc_fd_def_f[FR_idx,] <- get_basic_price(fd_sc_exio_def_f, "FR")
  
  FR_energy <- indirect_E_int %*% FR_fd   # indirect energy use from the supply chains
  FR_energy_sc <- indirect_E_int %*% FR_sc_fd 
  FR_energy_sc_flat <- indirect_E_int %*% FR_sc_fd_flat  
  FR_energy_sc_def <- indirect_E_int %*% FR_sc_fd_def   
  FR_energy_sc_def_f <- indirect_E_int %*% FR_sc_fd_def_f  
  
  # Example of disaggregating domestic and imported embodied energy
  a <- null_demand
  b <- null_demand
  a[FR_idx,] <- (as.matrix(L_inverse) %*% FR_sc_fd_def)[FR_idx,]  
  b[-FR_idx,] <- (as.matrix(L_inverse) %*% FR_sc_fd_def)[-FR_idx,]  
  FR_energy_sc_def_dom <- indirect_E_int %*% a  
  FR_energy_sc_def_imp <- indirect_E_int %*% b   
  
  # Embodied energy total per hh and intensity 
  # Only hh consumption
  totE_per_hh <- rbind(totE_per_hh, colSums(FR_energy)) # Total indirect energy/hh by decile
  intensity <- rbind(intensity, colSums(FR_energy)/colSums(fd_decile[,2:12])) # indirect energy intensity by decile [MJ/EUR]
  totE <- rbind(totE, colSums(FR_energy)*N_hh_FR/10)  # Total indirect energy by decile
  
  # hh con + SocTr (prop)
  totE_per_hh_sc <- rbind(totE_per_hh_sc, colSums(FR_energy_sc)) # Total indirect energy/hh by decile
  intensity_sc <- rbind(intensity_sc, colSums(FR_energy_sc)/colSums(fd_sc_exio)) # indirect energy intensity by decile [MJ/EUR]
  totE_sc <- rbind(totE_sc, colSums(FR_energy_sc)*N_hh_FR/10)  # Total indirect energy by decile
  
  # hh con + SocTr (flat)
  totE_per_hh_sc_flat <- rbind(totE_per_hh_sc_flat, colSums(FR_energy_sc_flat)) # Total indirect energy/hh by decile
  intensity_sc_flat <- rbind(intensity_sc_flat, colSums(FR_energy_sc_flat)/colSums(fd_sc_exio_f)) # indirect energy intensity by decile [MJ/EUR]
  totE_sc_flat <- rbind(totE_sc_flat, colSums(FR_energy_sc_flat)*N_hh_FR/10)  # Total indirect energy by decile
  
  # hh con + SocTr (prop) + defense (prop)
  totE_per_hh_sc_def <- rbind(totE_per_hh_sc_def, colSums(FR_energy_sc_def)) # Total indirect energy/hh by decile
  intensity_sc_def <- rbind(intensity_sc_def, colSums(FR_energy_sc_def)/colSums(fd_sc_exio_def)) # indirect energy intensity by decile [MJ/EUR]
  totE_sc_def <- rbind(totE_sc_def, colSums(FR_energy_sc_def)*N_hh_FR/10)  # Total indirect energy by decile
  
  # hh con + SocTr (flat) + defense (flat)
  totE_per_hh_sc_def_f <- rbind(totE_per_hh_sc_def_f, colSums(FR_energy_sc_def_f)) # Total indirect energy/hh by decile
  intensity_sc_def_f <- rbind(intensity_sc_def_f, colSums(FR_energy_sc_def_f)/colSums(fd_sc_exio_def_f)) # indirect energy intensity by decile [MJ/EUR]
  totE_sc_def_f <- rbind(totE_sc_def_f, colSums(FR_energy_sc_def_f)*N_hh_FR/10)  # Total indirect energy by decile
  
    # hh con + SocTr (prop) + defense (prop) : imported
    totE_per_hh_sc_def_imp <- rbind(totE_per_hh_sc_def_imp, colSums(FR_energy_sc_def_imp)) # Total indirect energy/hh by decile
    intensity_sc_def_imp <- rbind(intensity_sc_def_imp, colSums(FR_energy_sc_def_imp)/colSums(fd_sc_exio_def)) # indirect energy intensity by decile [MJ/EUR]
    totE_sc_def_imp <- rbind(totE_sc_def_imp, colSums(FR_energy_sc_def_imp)*N_hh_FR/10)  # Total indirect energy by decile
    
    # hh con + SocTr (prop) + defense (prop) : domestic
    totE_per_hh_sc_def_dom <- rbind(totE_per_hh_sc_def_dom, colSums(FR_energy_sc_def_dom)) # Total indirect energy/hh by decile
    intensity_sc_def_dom <- rbind(intensity_sc_def_dom, colSums(FR_energy_sc_def_dom)/colSums(fd_sc_exio_def)) # indirect energy intensity by decile [MJ/EUR]
    totE_sc_def_dom <- rbind(totE_sc_def_dom, colSums(FR_energy_sc_def_dom)*N_hh_FR/10)  # Total indirect energy by decile
  
}

figure_path = "H:/MyDocuments/IO work/France/"
png(filename = paste(figure_path, "1.1 Energy consumption per hh by decile (without SocTr).png", sep=""), width = 781, height = 553, units = "px")
boxplot(totE_per_hh[,2:11], ylab ="Total indirect energy per hh [MJ]", range=0)
dev.off()

png(filename = paste(figure_path, "1.2 Energy intensity by decile (without SocTr).png", sep=""), width = 781, height = 553, units = "px")
boxplot(intensity[,2:11], ylab ="Indirect energy intensity [MJ/EUR]", range=0)
dev.off()
# boxplot(totE[,2:11]/10^6, ylab ="Total indirect energy [TJ]")

png(filename = paste(figure_path, "2.1 Energy consumption per hh by decile (incl. SocTr by quintile).png", sep=""), width = 781, height = 553, units = "px")
boxplot(totE_per_hh_sc[,2:11], ylab ="Total indirect energy per hh incl. SocTr[MJ]", range=0)
dev.off()

png(filename = paste(figure_path, "2.2 Energy intensity by decile (incl. SocTr by quintile).png", sep=""), width = 781, height = 553, units = "px")
boxplot(intensity_sc[,2:11], ylab ="Indirect energy intensity incl. SocTr [MJ/EUR]", range=0)
dev.off()

png(filename = paste(figure_path, "3.1 Energy consumption per hh by decile (incl. SocTr flat alloc).png", sep=""), width = 781, height = 553, units = "px")
boxplot(totE_per_hh_sc_flat[,2:11], ylab ="Total indirect energy per hh incl. SocTr flat alloc [MJ]", range=0)
dev.off()

png(filename = paste(figure_path, "3.2 Energy intensity by decile (incl. SocTr flat alloc).png", sep=""), width = 781, height = 553, units = "px")
boxplot(intensity_sc_flat[,2:11], ylab ="Indirect energy intensity incl. SocTr flat alloc [MJ/EUR]", range=0)
dev.off()

png(filename = paste(figure_path, "4.1 Energy consumption per hh by decile (incl. SocTr+defence prop alloc).png", sep=""), width = 781, height = 553, units = "px")
boxplot(totE_per_hh_sc_def[,2:11], ylab ="Total indirect energy per hh incl. SocTr+defence prop alloc [MJ]", range=0)
dev.off()

png(filename = paste(figure_path, "4.2 Energy intensity by decile (incl. SocTr+defence prop alloc).png", sep=""), width = 781, height = 553, units = "px")
boxplot(intensity_sc_def[,2:11], ylab ="Indirect energy intensity incl. SocTr+defence prop alloc [MJ/EUR]", range=0)
dev.off()

png(filename = paste(figure_path, "5.1 Energy consumption per hh by decile (incl. SocTr+defence flat alloc).png", sep=""), width = 781, height = 553, units = "px")
boxplot(totE_per_hh_sc_def_f[,2:11], ylab ="Total indirect energy per hh incl. SocTr+defence flat alloc [MJ]", range=0)
dev.off()

png(filename = paste(figure_path, "5.2 Energy intensity by decile (incl. SocTr+defence flat alloc).png", sep=""), width = 781, height = 553, units = "px")
boxplot(intensity_sc_def_f[,2:11], ylab ="Indirect energy intensity incl. SocTr+defence flat alloc [MJ/EUR]", range=0)
dev.off()

# Domestic/imported disaggregation
png(filename = paste(figure_path, "4.1.1 Domestic embodied energy by decile (incl. SocTr+defence prop alloc).png", sep=""), width = 781, height = 553, units = "px")
boxplot(intensity_sc_def_dom[,2:11], ylab ="Total domestic embodied  energy per hh incl. SocTr+defence prop alloc [MJ]", range=0)
dev.off()

png(filename = paste(figure_path, "4.1.2 Imported embodied energy by decile (incl. SocTr+defence prop alloc).png", sep=""), width = 781, height = 553, units = "px")
boxplot(intensity_sc_def_imp[,2:11], ylab ="Total imported embodied energy per hh incl. SocTr+defence prop alloc [MJ]", range=0)
dev.off()

png(filename = paste(figure_path, "4.2.1 Domestic embodied energy int. by decile (incl. SocTr+defence prop alloc).png", sep=""), width = 781, height = 553, units = "px")
boxplot(intensity_sc_def_dom[,2:11], ylab ="Domestic embodied energy intensity incl. SocTr+defence prop alloc [MJ/EUR]", range=0)
dev.off()

png(filename = paste(figure_path, "4.2.2 Imported embodied energy int. by decile (incl. SocTr+defence prop alloc).png", sep=""), width = 781, height = 553, units = "px")
boxplot(intensity_sc_def_imp[,2:11], ylab ="Imported embodied energy intensity incl. SocTr+defence prop alloc [MJ/EUR]", range=0)
dev.off()

# Get intensities by COICOP consumption sector
ind_intensity <- vector()
null_demand_int <- matrix(0, 9600, n_sector_coicop)
SectoralE_per_hh <- vector()

for (i in 1:n_draw) {
  
  unit_exio <- bridge_COICOP_EXIO[[i]] %*% diag(n_sector_coicop)  # Identity mtx representing 1 EUR spending in each sector
  fd_exio <- bridge_COICOP_EXIO[[i]] %*% diag(fd_decile[,2])  # For Ensemble
  
  fd_unit <- null_demand_int
  fd_unit[FR_idx,] <- get_basic_price(unit_exio, "FR")
  
  FR_fd <- null_demand_int
  FR_fd[FR_idx,] <- get_basic_price(fd_exio, "FR")
  
  energy_int_FR <- indirect_E_int %*% fd_unit   # indirect energy use from the supply chains
  energy_tot <- indirect_E_int %*% FR_fd
    
  ind_intensity <- rbind(ind_intensity, colSums(energy_int_FR)) # Total indirect energy/hh by decile
  SectoralE_per_hh <- rbind(SectoralE_per_hh, colSums(energy_tot)) # Total indirect energy/hh by decile
}
png(filename = paste(figure_path, "Energy intensity by COICOP consumption category.png", sep=""), width = 781, height = 553, units = "px")
boxplot(ind_intensity, xlab ="COICOP sectors", ylab ="Energy intensity by consumption sector [MJ/EUR]", range=0)
dev.off()

png(filename = paste(figure_path, "Energy total per hh by COICOP consumption category.png", sep=""), width = 781, height = 553, units = "px")
boxplot(SectoralE_per_hh, xlab ="COICOP sectors", ylab ="Embodied energy by consumption sector per hh [MJ]", range=0)
dev.off()

# For NYC meeting slide
boxplot(ind_intensity[,-32], xlab ="COICOP sectors", ylab ="Energy intensity by consumption sector [MJ/EUR]", range=0)

# Basic comparison
total_indr_E <- mean(totE_per_hh[,1])*N_hh_FR/41.86e9   # 1 Tep = 41.86e9 J
total_indr_E_sc <- mean(totE_per_hh_sc[,1])*N_hh_FR/41.86e9

# Descriptive stat
library(pastecs)
a <- data.frame((stat.desc(ind_intensity)))
names(a) <- COICOP_catnames2[,1]
intensity_by_sector <- t(a)[,c(4:6, 8:9, 13)]

a <- data.frame((stat.desc(SectoralE_per_hh)))
names(a) <- COICOP_catnames2[,1]
energy_by_sector_per_hh <- t(a)[,c(4:6, 8:9, 13)]

a<-cbind(intensity_by_sector, energy_by_sector_per_hh)
write.csv(a, file = paste(figure_path, "summary_FRA.csv", sep=""), row.names = TRUE)
