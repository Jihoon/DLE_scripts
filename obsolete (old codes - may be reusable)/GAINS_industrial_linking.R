
path_iot_ind <- "P:/ene.general/DecentLivingEnergy/IO/Data - EXIOBASE/mrIOT_IxI_fpa_coefficient_version2.2.2/"

EXIOind <- read.table(paste(path_iot_ind, "mrFinalDemand_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)[1:163,2]
write.table(EXIOind, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)



write.table(EX_catnames, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
