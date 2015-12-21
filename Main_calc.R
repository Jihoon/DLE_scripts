setwd("H:/MyDocuments/IO work/DLE_scripts")

# Focusing on France case

# Read in data
source("Init.R")

### Mapping CES-COICOP ###

# Total final demand converted to COICOP classification
# fd_tot_coicop <- t(bridge_CES_COICOP)%*%fd_tot$FD_TOT   # Using India CES for placeholder


# Get a random draw of a mapping
n_draw <- 1000

N_hh_FR <- 25253000 # in 2005 https://en.wikipedia.org/wiki/List_of_countries_by_numrow(ber_of_households
koe_to_MJ <- 42.868
E_capita_FR <- 4117 * koe_to_MJ # MJ in 2007 http://data.worldbank.org/indicator/EG.USE.PCAP.KG.OE?page=1
totE_pri_FR <- 11.221 * 10^9 * 0.94782 * 1000  # Conversion factors from https://www.eia.gov/cfapps/ipdbproject/docs/units.cfm
inten_FR <- 5014 * 0.94782 * 1000 / 0.8  # Primary https://www.eia.gov/cfapps/ipdbproject/iedindex3.cfm?tid=92&pid=46&aid=2&cid=FR,&syid=2007&eyid=2011&unit=BTUPUSDM
                                      # Exchange rate http://www.x-rates.com/average/?from=USD&to=EUR&amount=1&year=2005
FR_place <- which(exio_ctys=="FR")
FR_idx <- seq(200*FR_place, 200*(FR_place+1)-1)
null_demand <- matrix(0, 9600, 11)

totE_per_hh <- vector()
intensity <- vector()
totE <- vector()

totE_per_hh_sc <- vector()
intensity_sc <- vector()
intensity_sc1 <- vector()
totE_sc <- vector()

totE_per_hh_sc_flat <- vector()
intensity_sc_flat <- vector()
totE_sc_flat <- vector()


### Mapping COICOP-EXIO ###

# for each draw of bridging (COICOP-EXIO)
for (i in 1:n_draw) {
  bridge_COICOP_EXIO <- get_bridge_COICOP_EXIO()
  
  fd_exio <- t(bridge_COICOP_EXIO) %*% as.matrix(fd_decile[,2:12])
  fd_sc_exio <- t(bridge_COICOP_EXIO) %*% as.matrix(fd_with_soctr)  # Because fd_with_soctr is extended, this can inflate errors.
  fd_sc_exio_f <- t(bridge_COICOP_EXIO) %*% as.matrix(fd_with_soctr_flat)  # Because fd_with_soctr is extended, this can inflate errors.
  
  FR_fd <- null_demand
  FR_sc_fd <- null_demand
  FR_sc_fd_flat <- null_demand
  
  FR_fd[FR_idx,] <- get_basic_price(fd_exio, "FR")
  FR_sc_fd[FR_idx,] <- get_basic_price(fd_sc_exio, "FR")
  FR_sc_fd_flat[FR_idx,] <- get_basic_price(fd_sc_exio_f, "FR")
  
  energy_FR <- indirect_E_int %*% FR_fd   # indirect energy use from the supply chains
  energy_sc_FR <- indirect_E_int %*% FR_sc_fd   # indirect energy use from the supply chains
  energy_sc_FR_flat <- indirect_E_int %*% FR_sc_fd_flat   # indirect energy use from the supply chains
  
  energy_FR[,1] <- rowSums(energy_FR[,2:11])
  # colSums(energy_FR)*c(N_hh_FR, rep(N_hh_FR/10 , 10))
  
  totE_per_hh <- rbind(totE_per_hh, colSums(energy_FR)) # Total indirect energy/hh by decile
  intensity <- rbind(intensity, colSums(energy_FR)/colSums(fd_decile[,2:12])) # indirect energy intensity by decile [MJ/EUR]
  totE <- rbind(totE, colSums(energy_FR)*N_hh_FR/10)  # Total indirect energy by decile
  
  totE_per_hh_sc <- rbind(totE_per_hh_sc, colSums(energy_sc_FR)) # Total indirect energy/hh by decile
  intensity_sc <- rbind(intensity_sc, colSums(energy_sc_FR)/colSums(fd_decile[,2:12])) # indirect energy intensity by decile [MJ/EUR]
  intensity_sc1 <- rbind(intensity_sc1, colSums(energy_sc_FR)/colSums(fd_with_soctr)) # indirect energy intensity by decile [MJ/EUR]
  totE_sc <- rbind(totE_sc, colSums(energy_sc_FR)*N_hh_FR/10)  # Total indirect energy by decile
  
  totE_per_hh_sc_flat <- rbind(totE_per_hh_sc_flat, colSums(energy_sc_FR_flat)) # Total indirect energy/hh by decile
  intensity_sc_flat <- rbind(intensity_sc_flat, colSums(energy_sc_FR_flat)/colSums(fd_decile[,2:12])) # indirect energy intensity by decile [MJ/EUR]
  # intensity_sc1 <- rbind(intensity_sc1, colSums(energy_sc_FR)/colSums(fd_with_soctr)) # indirect energy intensity by decile [MJ/EUR]
  totE_sc_flat <- rbind(totE_sc_flat, colSums(energy_sc_FR_flat)*N_hh_FR/10)  # Total indirect energy by decile
}

# f1 <- ggplot(data = totE_per_hh, aes(x = X, y = Y, group = group) )  # lesion becomes a classifying 
boxplot(totE_per_hh[,2:11], ylab ="Total indirect energy per hh [MJ]")
boxplot(intensity[,2:11], ylab ="Indirect energy intensity [MJ/EUR]")
# boxplot(totE[,2:11]/10^6, ylab ="Total indirect energy [TJ]")

boxplot(totE_per_hh_sc[,2:11], ylab ="Total indirect energy per hh incl. SocTr[MJ]")
boxplot(intensity_sc[,2:11], ylab ="Indirect energy intensity incl. SocTr [MJ/EUR]")
boxplot(intensity_sc1[,2:11], ylab ="Indirect energy intensity incl. SocTr [MJ/EUR]")

boxplot(totE_per_hh_sc_flat[,2:11], ylab ="Total indirect energy per hh incl. SocTr flat alloc [MJ]")
boxplot(intensity_sc_flat[,2:11], ylab ="Indirect energy intensity incl. SocTr flat alloc [MJ/EUR]")

# Get intensities by COICOP consumption sector
ind_intensity <- vector()
null_demand_int <- matrix(0, 9600, 109)

for (i in 1:n_draw) {
  bridge_COICOP_EXIO <- get_bridge_COICOP_EXIO()
  
  unit_exio <- t(bridge_COICOP_EXIO) %*% diag(n_sector_coicop)
  fd_unit <- null_demand_int
  fd_unit[FR_idx,] <- get_basic_price(unit_exio, "FR")
  
  energy_int_FR <- indirect_E_int %*% fd_unit   # indirect energy use from the supply chains
  
  ind_intensity <- rbind(ind_intensity, colSums(energy_int_FR)) # Total indirect energy/hh by decile
}
boxplot(ind_intensity, ylab ="Energy intensity by consumption sector [MJ/EUR]")