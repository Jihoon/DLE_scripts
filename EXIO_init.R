# Read in neccessary EXIO tables
# And derive indirect energy intensities per EXIO sector

setwd("H:/MyDocuments/IO work/")

path_iot <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/mrIOT_PxP_ita_coefficient_version2.2.2/"
path_sut <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/mrSUT_version2.2.2/"

# From IoT folder
L_inverse <- read.table(paste(path_iot, "L_inverse.txt", sep=""), header=FALSE, sep=",", dec=".")
factor_input <- read.table(paste(path_iot, "mrFactorInputs_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
final_demand <- read.table(paste(path_iot, "mrFinalDemand_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
iot <- read.table(paste(path_iot, "mrIot_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)

# Material extension with more energy carrier resolution from NTNU (ver 2.2.0)
# However these extensions are in TJ unit, which need to be divided by total use by product to get intensities.
materials <- read.table(paste(path_iot, "mrMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
final_demand_material <- read.table(paste(path_iot, "mrFDMaterials_version2.2.0.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)

# From SUT folder
tot_use <- read.table(paste(path_sut, "mrUse_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)

# Remove first few columns with names
factor_input <- factor_input[,c(-1,-2)]
final_demand <- final_demand[,c(-1,-2,-3)]
final_demand_material <- final_demand_material[,c(-1,-2)]
materials <- materials[,c(-1,-2)]
iot <- iot[,c(-1,-2,-3)]
tot_use <- tot_use[,c(-1,-2,-3)]

# Get total use by product 
tot_demand <- rowSums(final_demand) + rowSums(tot_use)

energy_carrier_use_idx <- 140:208   # number of rows for E-carrier use after removing the row headers
energy_use <- materials[energy_carrier_use_idx,]  # The energy extension has not intensities but total consumptions.
y <- 1/tot_demand
y[is.infinite(y)] <- 0 
energy_int <- as.matrix(energy_use) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector
indirect_E_int <- energy_int %*% as.matrix(L_inverse)   # (intensity by sector) * (I-A)^-1
