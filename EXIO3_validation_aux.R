
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