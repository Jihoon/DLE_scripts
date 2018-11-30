# Read in neccessary EXIO tables
# And derive indirect energy intensities per EXIO sector

# setwd("H:/MyDocuments/IO work/")

path_iot <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/mrIOT_PxP_ita_coefficient_version2.2.2/"
path_sut <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/mrSUT_version2.2.2/"

EXIO3_path = "H:/MyDocuments/Analysis/Final energy/Arkaitz/IOT/"

# From IoT folder

### final_demand

# 1. From EXIO2
final_demand <- read.table(paste(path_iot, "mrFinalDemand_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
final_demand.name <- read.table(paste(path_iot, "mrFinalDemand_version2.2.2.txt", sep=""),
                                header=FALSE, sep="\t", dec=".", skip=1, nrow=1,  stringsAsFactors=FALSE)[4:10]
final_demand <- final_demand[,c(-1,-2,-3)]

### Leontief inverse: L

# 1. From EXIO2
L_inverse <- read.table(paste(path_iot, "L_inverse.txt", sep=""), header=FALSE, sep=",", dec=".")


### Other EXIO data

factor_input <- read.table(paste(path_iot, "mrFactorInputs_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
factor_input <- factor_input[,c(-1,-2)]

iot <- read.table(paste(path_iot, "mrIot_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
iot <- iot[,c(-1,-2,-3)]

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

final_demand_material <- read.table(paste(path_iot, "mrFDMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
final_demand_material <- final_demand_material[,c(-1,-2)]

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
gwp <- c(CH4=34, N20=298)
emissions_2.3[c(2,5,8,11),] <- emissions_2.3[c(2,5,8,11),] * gwp["CH4"]
emissions_2.3[c(3,6,9),] <- emissions_2.3[c(3,6,9),] * gwp["N20"]

# From SUT folder
# 1. From EXIO2
tot_use <- read.table(paste(path_sut, "mrUse_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
tot_use <- tot_use[,c(-1,-2,-3)]

# Get total use by product 
tot_demand <- rowSums(final_demand) + rowSums(tot_use)  # X vector (global)
b <- rowSums(final_demand[,-seq(7, exio.fd.len, 7)]) # Excluding (export for global demand = 0)
fd.sum <- data.frame(name=EX_catnames, ind.use=rowSums(tot_use[IND_idx_ex,]), 
                     hh.fd=rowSums(final_demand[IND_idx_ex,seq(1, exio.fd.len, 7)]),
                     oth.fd=rowSums(final_demand[IND_idx_ex,-seq(1, exio.fd.len, 7)])) %>% 
  mutate(tot.use = hh.fd + oth.fd + ind.use)


# To clean up the memory
save(L_inverse, file="./Saved tables/L_inverse.Rda")
save(iot, file="./Saved tables/iot.Rda")
save(indirect_E_int, file="./Saved tables/indirect_E_int.Rda")
save(tot_use, file="./Saved tables/tot_use.Rda")
save(supplym, file="./Saved tables/supplym.Rda")
save(final_demand, file="./Saved tables/final_demand.Rda")
save(tot_demand, file="./Saved tables/tot_demand.Rda")
save(materials, file="./Saved tables/materials.Rda")
save(fd_materials, file="./Saved tables/fd_materials.Rda")
save(indirect_pE_int.elec.prirow, file="./Saved tables/indirect_pE_int.elec.prirow.Rda")
