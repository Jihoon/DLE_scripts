
### Arkaitz/Richard Question on EXIO

# exio.industry <- 86:96 # 104:Steel, 108:Alu, 101: Cement, 100: Brick, 86:96: Chemicals, 97:103: non-matallic mineral, 106:115: non-ferrous metal
# exio.industry <- 97:103 # 104:Steel, 108:Alu, 101: Cement, 100: Brick, 86:96: Chemicals, 97:103: non-matallic mineral, 106:115: non-ferrous metal
exio.industry <- 106:115 # 104:Steel, 108:Alu, 101: Cement, 100: Brick, 86:96: Chemicals, 97:103: non-matallic mineral, 106:115: non-ferrous metal

# compare.ei <- data.frame(carrier.name.fin, 
#                          IND.DFE=dfe.exio[,IND_idx_ex[exio.industry]], 
#                          IND.DNE=dne.exio[,IND_idx_ex[exio.industry]], 
#                          IND.DFEI=dfei.exio[,IND_idx_ex[exio.industry]], 
#                          IND.TFEI=tfei.exio[,IND_idx_ex[exio.industry]], 
#                          IND.TPEI=tpei.nature[,IND_idx_ex[exio.industry]], 
#                          IND.TPEI.USE=tpei.USE[,IND_idx_ex[exio.industry]],
#                          IND.TPEI.net=tnei.exio[,IND_idx_ex[exio.industry]], 
#                          
#                          BRA.DFE=dfe.exio[,BRA_idx_ex[exio.industry]], 
#                          BRA.DNE=dne.exio[,BRA_idx_ex[exio.industry]], 
#                          BRA.DFEI=dfei.exio[,BRA_idx_ex[exio.industry]], 
#                          BRA.TFEI=tfei.exio[,BRA_idx_ex[exio.industry]], 
#                          BRA.TPEI=tpei.nature[,BRA_idx_ex[exio.industry]], 
#                          BRA.TPEI.USE=tpei.USE[,BRA_idx_ex[exio.industry]], 
#                          BRA.TPEI.net=tnei.exio[,BRA_idx_ex[exio.industry]], 
#                          
#                          ZAF.DFE=dfe.exio[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.DNE=dne.exio[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.DFEI=dfei.exio[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.TFEI=tfei.exio[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.TPEI=tpei.nature[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.TPEI.USE=tpei.USE[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.TPEI.net=tnei.exio[,ZAF_idx_ex[exio.industry]])
compare.ei <- data.frame(carrier.name.fin, 
                         IND.DFE=dfe.exio[,IND_idx_ex[exio.industry]], 
                         BRA.DFE=dfe.exio[,BRA_idx_ex[exio.industry]], 
                         ZAF.DFE=dfe.exio[,ZAF_idx_ex[exio.industry]])
compare.ei <- compare.ei %>% rbind(data.frame(carrier.name.fin="Elec.total", t(colSums(compare.ei[idx_elec,-1])))) %>%
  rbind(data.frame(carrier.name.fin="FE.total", t(colSums(compare.ei[,-1]))))
names(compare.ei) <- c("Carrier", paste("IND", EX_catnames[exio.industry]), paste("BRA", EX_catnames[exio.industry]), paste("ZAF", EX_catnames[exio.industry]))

# view(compare.ei)
if (exio.industry[1]==86) {
  write.csv(compare.ei, "H:/MyDocuments/Analysis/Final energy/Arkaitz/For Debug/IND-BRA-ZAF Chemical and petrochemical.csv")
} else if (exio.industry[1]==97) {
  write.csv(compare.ei, "H:/MyDocuments/Analysis/Final energy/Arkaitz/For Debug/IND-BRA-ZAF Non-matallic minerals.csv")
} else {
  write.csv(compare.ei, "H:/MyDocuments/Analysis/Final energy/Arkaitz/For Debug/IND-BRA-ZAF Non-ferrous metals.csv")
}






### Additional outputs for FE meeting 27/4/2018
# sects <- c(Beef=43, Pork=44, Poultry=45) # Cement, Steel, Aluminum
sects <- c(Cement=101, Steel=104, Alu=108) # Cement, Steel, Aluminum
elec.idx <- grep("Electricity ", carrier.name.fin)
cty.idx.ex <- BRA_idx_ex

NENE <- c(colSums(tfei.sub[[1]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub[[1]][-elec.idx, cty.idx.ex[sects]]))
NTRA <- c(colSums(tfei.sub[[2]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub[[2]][-elec.idx, cty.idx.ex[sects]]))
# NTRA <- c(colSums(dfei.sub[[2]][elec.idx, cty.idx.ex[sects]]),colSums(dfei.sub[[2]][-elec.idx, cty.idx.ex[sects]]))
TAVI <- c(colSums(tfei.sub[[3]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub[[3]][-elec.idx, cty.idx.ex[sects]]))
TMAR <- c(colSums(tfei.sub[[4]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub[[4]][-elec.idx, cty.idx.ex[sects]]))
TOTH <- c(colSums(tfei.sub[[5]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub[[5]][-elec.idx, cty.idx.ex[sects]]))
TRAI <- c(colSums(tfei.sub[[6]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub[[6]][-elec.idx, cty.idx.ex[sects]]))
TROA <- c(colSums(tfei.sub[[7]][elec.idx, cty.idx.ex[sects]]),colSums(tfei.sub[[7]][-elec.idx, cty.idx.ex[sects]]))
TOTL.F <- c(colSums(tfei.exio[elec.idx,cty.idx.ex[sects]]),colSums(tfei.exio[-elec.idx,cty.idx.ex[sects]]))
TOTL.P.prod <- c(colSums(tpei.nature[,cty.idx.ex[sects]]))
TOTL.P.use <- c(colSums(tpei.USE[,cty.idx.ex[sects]]))

sum.out <- data.frame(NENE, NTRA, TAVI, TMAR, TOTH, TRAI, TROA, TOTL.F, TOTL.P.prod, TOTL.P.use)
row.names(sum.out) <- c(paste0(names(sects), ".elec"), paste0(names(sects), ".non-elec"))
write.table(sum.out, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)

a <- sum.out[1:3,] + sum.out[4:6,]
row.names(a) <- names(sects)
write.table(a, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)

rowSums(tfei.elec[elec.idx, cty.idx.ex[sects]])/sum(tfei.elec[elec.idx, cty.idx.ex[sects]])

# Compare elec shares of countries
elec.share <- data.frame(matrix(ncol=3))
names(elec.share) <- names(sects)
a <- data.frame(name=carrier.name.fin, tfei.exio[,IND_idx_ex[sects]]) %>% mutate(elec=ifelse(row_number() %in% elec.idx, 1, 0) ) %>%
  group_by(elec) %>% summarise_if(is.numeric, sum) %>% rbind(colSums(.)) 
names(a)[-1] <- names(sects)
elec.share <- rbind(elec.share, a[2,-1]/a[3,-1])

a <- data.frame(name=carrier.name.fin, tfei.exio[,BRA_idx_ex[sects]]) %>% mutate(elec=ifelse(row_number() %in% elec.idx, 1, 0) ) %>%
  group_by(elec) %>% summarise_if(is.numeric, sum) %>% rbind(colSums(.)) 
names(a)[-1] <- names(sects)
elec.share <- rbind(elec.share, a[2,-1]/a[3,-1])

a <- data.frame(name=carrier.name.fin, tfei.exio[,ZAF_idx_ex[sects]]) %>% mutate(elec=ifelse(row_number() %in% elec.idx, 1, 0) ) %>%
  group_by(elec) %>% summarise_if(is.numeric, sum) %>% rbind(colSums(.)) 
names(a)[-1] <- names(sects)
elec.share <- rbind(elec.share, a[2,-1]/a[3,-1])


# Try GER and USA
GER_place <- which(exio_ctys=="DE")
GER_idx_ex <- seq(200*(GER_place-1)+1, 200*GER_place)   # 200 EXIO comodities
a <- data.frame(name=carrier.name.fin, tfei.exio[,GER_idx_ex[sects]]) %>% mutate(elec=ifelse(row_number() %in% elec.idx, 1, 0) ) %>%
  group_by(elec) %>% summarise_if(is.numeric, sum) %>% rbind(colSums(.)) 
names(a)[-1] <- names(sects)
elec.share <- rbind(elec.share, a[2,-1]/a[3,-1])

USA_place <- which(exio_ctys=="US")
USA_idx_ex <- seq(200*(USA_place-1)+1, 200*USA_place)   # 200 EXIO comodities
a <- data.frame(name=carrier.name.fin, tfei.exio[,USA_idx_ex[sects]]) %>% mutate(elec=ifelse(row_number() %in% elec.idx, 1, 0) ) %>%
  group_by(elec) %>% summarise_if(is.numeric, sum) %>% rbind(colSums(.)) 
names(a)[-1] <- names(sects)
elec.share <- rbind(elec.share, a[2,-1]/a[3,-1])

CHN_place <- which(exio_ctys=="CN")
CHN_idx_ex <- seq(200*(CHN_place-1)+1, 200*CHN_place)   # 200 EXIO comodities
a <- data.frame(name=carrier.name.fin, tfei.exio[,CHN_idx_ex[sects]]) %>% mutate(elec=ifelse(row_number() %in% elec.idx, 1, 0) ) %>%
  group_by(elec) %>% summarise_if(is.numeric, sum) %>% rbind(colSums(.)) 
names(a)[-1] <- names(sects)
elec.share <- rbind(elec.share, a[2,-1]/a[3,-1])

elec.share <- elec.share %>% slice(-1) %>% mutate(country=c("IND", "BRA", "ZAF", "GER", "USA", "CHN")) %>% select(country, everything())
write.table(elec.share, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)


# Derive elec share and f/p raio for all countries
elec.share <- data.frame(matrix(ncol=3))
names(elec.share) <- names(sects)
p.to.f.prod <- data.frame(matrix(ncol=3))
names(p.to.f.prod) <- names(sects)
p.to.f.use <- data.frame(matrix(ncol=3))
names(p.to.f.use) <- names(sects)
for (i in 1:49) {
  cty.idx.ex <- seq(200*(i-1)+1, 200*i)
  a <- data.frame(name=carrier.name.fin, tfei.exio[,cty.idx.ex[sects]]) %>% mutate(elec=ifelse(row_number() %in% elec.idx, 1, 0) ) %>%
    group_by(elec) %>% summarise_if(is.numeric, sum) %>% rbind(colSums(.)) 
  names(a)[-1] <- names(sects)
  elec.share <- rbind(elec.share, a[2,-1]/a[3,-1])
  p.to.f.prod <- rbind(p.to.f.prod, a[3,-1] / c(colSums(tpei.nature[,cty.idx.ex[sects]])))
  p.to.f.use <- rbind(p.to.f.use, a[3,-1] / c(colSums(tpei.USE[,cty.idx.ex[sects]])))
}

# par(mfrow = c(7,7), mar = c(0, 0, 1, 0))
# for (i in 1:49) {
par(mfrow = c(3,4), mar = c(2, 1, 2, 1))
for (i in cty_set) {
  plot(as.numeric(elec.share[i+1,]), type="o", col="blue", ylim=c(0,1), axes=FALSE, ann=FALSE)
  axis(1, at=1:3, lab=c("Cement","Steel","Alu"))
  axis(2, las=1, at=seq(0, 1, 0.1))
  # box()
  grid(NA, 5, lwd = 1)
  lines(as.numeric(p.to.f.use[i+1,]), type="o", pch=22, lty=2, col="red")
  title(countrycode(exio_ctys[i], "iso2c", "iso3c"))
}
legend(1, 1, c("Electricity share","Final to Primary"), cex=1.0,
col=c("blue","red"), pch=21:22, lty=1:2)
dev.off()

# Countries with nice-looking trend
cty_set <-  which(exio_ctys %in% c("BE", "SI", "CZ", "LV", "NL", "IN", "HU", "CN", "EE", "TW", "KR", "ZA"))

