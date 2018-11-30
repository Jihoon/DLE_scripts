a <- colSums((indirect_E_int)[,FR_idx])
b <- colSums((energy_int)[,FR_idx])
intensities <- data.frame(EX_catnames, b, a)
names(intensities) <- c("EXIOsector", "DirectInt", "IndirInt")
