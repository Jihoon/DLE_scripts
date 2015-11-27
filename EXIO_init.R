setwd("H:/MyDocuments/IO work/")

path <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/mrIOT_PxP_ita_coefficient_version2.2.2/"

L_inverse <- read.table(paste(path, "L_inverse.txt", sep=""), header=F, sep=",", dec=".")
factor_input <- read.table(paste(path, "mrFactorInputs_version2.2.2.txt", sep=""), header=F, sep="\t", dec=".", skip=2)
final_demand <- read.table(paste(path, "mrFinalDemand_version2.2.2.txt", sep=""), header=F, sep="\t", dec=".", skip=2)
final_demand_material <- read.table(paste(path, "mrFDMaterials_version2.2.2.txt", sep=""), header=F, sep="\t", dec=".", skip=2)
materials <- read.table(paste(path, "mrMaterials_version2.2.2.txt", sep=""), header=F, sep="\t", dec=".", skip=2)
iot <- read.table(paste(path, "mrIot_version2.2.2.txt", sep=""), header=F, sep="\t", dec=".", skip=2)

factor_input <- factor_input[,c(-1,-2)]
final_demand <- final_demand[,c(-1,-2,-3)]
final_demand_material <- final_demand_material[,c(-1,-2)]
materials <- materials[,c(-1,-2)]
iot <- iot[,c(-1,-2,-3)]

temp <- as.matrix(materials[1:4,])%*%as.matrix(L_inverse)