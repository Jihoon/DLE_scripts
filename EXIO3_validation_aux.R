
### Additional outputs for FE meeting w/ Volker & Keywan 27/4/2018
sects <- 1:200 
names(sects) <- EX_catnames
# sects <- c(Beef=43, Pork=44, Poultry=45) # Cement, Steel, Aluminum # 
# sects <- c(Cement=101, Steel=104, Alu=108) # Cement, Steel, Aluminum
elec.idx <- grep("Electricity ", carrier.name.fin)
cty.idx.ex <- ZAF_idx_ex

NENE <- c(colSums(tfei.sub.bug[[1]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub.bug[[1]][-elec.idx, cty.idx.ex[sects]]))
NTRA <- c(colSums(tfei.sub.bug[[2]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub.bug[[2]][-elec.idx, cty.idx.ex[sects]]))
TAVI <- c(colSums(tfei.sub.bug[[3]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub.bug[[3]][-elec.idx, cty.idx.ex[sects]]))
TMAR <- c(colSums(tfei.sub.bug[[4]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub.bug[[4]][-elec.idx, cty.idx.ex[sects]]))
TOTH <- c(colSums(tfei.sub.bug[[5]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub.bug[[5]][-elec.idx, cty.idx.ex[sects]]))
TRAI <- c(colSums(tfei.sub.bug[[6]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub.bug[[6]][-elec.idx, cty.idx.ex[sects]]))
TROA <- c(colSums(tfei.sub.bug[[7]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub.bug[[7]][-elec.idx, cty.idx.ex[sects]]))
TOTL.F <- c(colSums(tfei.exio.bug[elec.idx,cty.idx.ex[sects]]),colSums(tfei.exio.bug[-elec.idx,cty.idx.ex[sects]]))
TOTL.P.prod <- c(colSums(tpei.nature.bug[,cty.idx.ex[sects]]))
TOTL.P.use <- c(colSums(tpei.USE.bug[,cty.idx.ex[sects]]))

sum.out <- data.frame(NENE, NTRA, TAVI, TMAR, TOTH, TRAI, TROA, TOTL.F, TOTL.P.prod, TOTL.P.use)
row.names(sum.out) <- c(paste0(names(sects), ".elec"), paste0(names(sects), ".non-elec"))
write.table(sum.out, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)

# a <- sum.out[1:3,] + sum.out[4:6,]
a <- sum.out[1:length(sects),] + sum.out[(length(sects)+1):(2*length(sects)),]
a <- a %>% mutate(EXIO = t(EX_catnames)) %>% select(EXIO, everything())
write.table(a, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)



### Additional outputs for FE meeting w/ Volker & Keywan 27/4/2018
sects <- 1:200 
names(sects) <- EX_catnames
# sects <- c(Beef=43, Pork=44, Poultry=45) # Cement, Steel, Aluminum # 
# sects <- c(Cement=101, Steel=104, Alu=108) # Cement, Steel, Aluminum
elec.idx <- grep("Electricity ", carrier.name.fin)
cty.idx.ex <- IND_idx_ex

NENE <- c(colSums(dfei.sub[[1]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub[[1]][-elec.idx, cty.idx.ex[sects]]))
NTRA <- c(colSums(dfei.sub[[2]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub[[2]][-elec.idx, cty.idx.ex[sects]]))
TAVI <- c(colSums(dfei.sub[[3]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub[[3]][-elec.idx, cty.idx.ex[sects]]))
TMAR <- c(colSums(dfei.sub[[4]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub[[4]][-elec.idx, cty.idx.ex[sects]]))
TOTH <- c(colSums(dfei.sub[[5]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub[[5]][-elec.idx, cty.idx.ex[sects]]))
TRAI <- c(colSums(dfei.sub[[6]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub[[6]][-elec.idx, cty.idx.ex[sects]]))
TROA <- c(colSums(dfei.sub[[7]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub[[7]][-elec.idx, cty.idx.ex[sects]]))
TOTL.F <- c(colSums(dfei.exio[elec.idx,cty.idx.ex[sects]]),colSums(dfei.exio[-elec.idx,cty.idx.ex[sects]]))
TOTL.P.prod <- c(colSums(dpei.nature[,cty.idx.ex[sects]]))
TOTL.P.use <- c(colSums(dpei.USE[,cty.idx.ex[sects]]))

sum.out <- data.frame(NENE, NTRA, TAVI, TMAR, TOTH, TRAI, TROA, TOTL.F, TOTL.P.prod, TOTL.P.use)
row.names(sum.out) <- c(paste0(names(sects), ".elec"), paste0(names(sects), ".non-elec"))
write.table(sum.out, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)

# a <- sum.out[1:3,] + sum.out[4:6,]
a <- sum.out[1:length(sects),] + sum.out[(length(sects)+1):(2*length(sects)),]
a <- a %>% mutate(EXIO = t(EX_catnames)) %>% select(EXIO, everything())
write.table(a, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)



### Additional outputs for FE meeting w/ Volker & Keywan 27/4/2018
sects <- 1:200 
names(sects) <- EX_catnames
# sects <- c(Beef=43, Pork=44, Poultry=45) # Cement, Steel, Aluminum # 
# sects <- c(Cement=101, Steel=104, Alu=108) # Cement, Steel, Aluminum
elec.idx <- grep("Electricity ", carrier.name.fin)
cty.idx.ex <- IND_idx_ex

NENE <- c(colSums(dfei.sub.bug[[1]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub.bug[[1]][-elec.idx, cty.idx.ex[sects]]))
NTRA <- c(colSums(dfei.sub.bug[[2]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub.bug[[2]][-elec.idx, cty.idx.ex[sects]]))
TAVI <- c(colSums(dfei.sub.bug[[3]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub.bug[[3]][-elec.idx, cty.idx.ex[sects]]))
TMAR <- c(colSums(dfei.sub.bug[[4]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub.bug[[4]][-elec.idx, cty.idx.ex[sects]]))
TOTH <- c(colSums(dfei.sub.bug[[5]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub.bug[[5]][-elec.idx, cty.idx.ex[sects]]))
TRAI <- c(colSums(dfei.sub.bug[[6]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub.bug[[6]][-elec.idx, cty.idx.ex[sects]]))
TROA <- c(colSums(dfei.sub.bug[[7]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub.bug[[7]][-elec.idx, cty.idx.ex[sects]]))
TOTL.F <- c(colSums(dfei.exio.bug[elec.idx,cty.idx.ex[sects]]),colSums(dfei.exio.bug[-elec.idx,cty.idx.ex[sects]]))
TOTL.P.prod <- c(colSums(dpei.nature.bug[,cty.idx.ex[sects]]))
TOTL.P.use <- c(colSums(dpei.USE.bug[,cty.idx.ex[sects]]))

sum.out <- data.frame(NENE, NTRA, TAVI, TMAR, TOTH, TRAI, TROA, TOTL.F, TOTL.P.prod, TOTL.P.use)
row.names(sum.out) <- c(paste0(names(sects), ".elec"), paste0(names(sects), ".non-elec"))
write.table(sum.out, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)

# a <- sum.out[1:3,] + sum.out[4:6,]
a <- sum.out[1:length(sects),] + sum.out[(length(sects)+1):(2*length(sects)),]
a <- a %>% mutate(EXIO = t(EX_catnames)) %>% select(EXIO, everything())
write.table(a, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)


# Compare old buggy versions of EXIO3 extensions
raw.F.2008.old = read.csv(paste0(EXIO3_path_old, "F_2008.csv"), header = FALSE)
raw.F.2008.bug = read.csv(paste0(EXIO3_path, "F_2008.csv"), header = FALSE)
raw.S.2008.old = read.csv(paste0(EXIO3_path_old, "S_2008.csv"), header = FALSE)
raw.S.2008.bug = read.csv(paste0(EXIO3_path, "S_2008.csv"), header = FALSE)
                              
list[dfe.exio_old, dfe.elec_old, dfe.non.elec_old, dfe.sub_old, dpe.nature_old, dpe.USE_old, de.SUPL_old, dne.exio_old] <- 
  HarmonizeEXIO3ExtensionFormat(read.csv(paste0(EXIO3_path_old, "F_2008.csv"), header = FALSE))
list[dfe.exio_bug, dfe.elec_bug, dfe.non.elec_bug, dfe.sub_bug, dpe.nature_bug, dpe.USE_bug, de.SUPL_bug, dne.exio_bug] <- 
  HarmonizeEXIO3ExtensionFormat(read.csv(paste0(EXIO3_path, "F_2008.csv"), header = FALSE))

sum(dfe.exio_old[,IND_idx_ex])
sum(dfe.exio_bug[,IND_idx_ex])
sum(dfe.exio[,IND_idx_ex])

sum(raw.F.2008.old[idx.FE.NTRA,IND_idx_ex])
sum(raw.F.2008.bug[idx.FE.NTRA,IND_idx_ex])
sum(raw.F.2008[idx.FE.NTRA,IND_idx_ex])

list[dfei.exio_old, dfei.elec_old, dfei.non.elec_old, dfei.sub_old, dpei.nature_old, dpei.USE_old, dei.SUPL_old, dnei.exio_old] <- 
  HarmonizeEXIO3ExtensionFormat(read.csv(paste0(EXIO3_path_old, "S_2008.csv"), header = FALSE))
list[dfei.exio_bug, dfei.elec_bug, dfei.non.elec_bug, dfei.sub_bug, dpei.nature_bug, dpei.USE_bug, dei.SUPL_bug, dnei.exio_bug] <- 
  HarmonizeEXIO3ExtensionFormat(read.csv(paste0(EXIO3_path, "S_2008.csv"), header = FALSE))

sum(dfei.exio_old[,IND_idx_ex])
sum(dfei.exio_bug[,IND_idx_ex])
sum(dfei.exio[,IND_idx_ex])

list[tfei.exio_old, tfei.elec_old, tfei.non.elec_old, tfei.sub_old, tpei.nature_old, tpei.USE_old, tei.SUPL_old, tnei.exio_old] <- 
  HarmonizeEXIO3ExtensionFormat(read.csv(paste0(EXIO3_path_old, "st_2008.csv"), header = FALSE))
list[tfei.exio_bug, tfei.elec_bug, tfei.non.elec_bug, tfei.sub_bug, tpei.nature_bug, tpei.USE_bug, tei.SUPL_bug, tnei.exio_bug] <- 
  HarmonizeEXIO3ExtensionFormat(read.csv(paste0(EXIO3_path, "st_2008.csv"), header = FALSE))

sum(tfei.exio_old[,IND_idx_ex])
sum(tfei.exio_bug[,IND_idx_ex])
sum(tfei.exio[,IND_idx_ex])