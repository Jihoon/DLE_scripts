EXIO3_path = "H:/MyDocuments/Analysis/Final energy/Arkaitz/IOT/"

raw.S <- read.csv(paste0(EXIO3_path, "S_2008.csv"), header = FALSE)    # Stressor (Intensity)
raw.st <- read.csv(paste0(EXIO3_path, "st_2008.csv"), header = FALSE)  # Total stressor
raw.S.2007 <- read.csv(paste0(EXIO3_path, "S_2007.csv"), header = FALSE)    # Stressor (Intensity)
raw.st.2007 <- read.csv(paste0(EXIO3_path, "st_2007.csv"), header = FALSE)  # Total stressor

raw.Y <- read.csv(paste0(EXIO3_path, "Y_2007.csv"), header = FALSE)  # Final demand
raw.F <- read.csv(paste0(EXIO3_path, "F_2007.csv"), header = FALSE)
raw.F.2008 <- read.csv(paste0(EXIO3_path, "F_2008.csv"), header = FALSE)
raw.F_hh <- read.csv(paste0(EXIO3_path, "F_hh_2007.csv"), header = FALSE)
raw.x <- unlist(read.csv(paste0(EXIO3_path, "x_2007.csv"), header = FALSE))
raw.x.2008 <- unlist(read.csv(paste0(EXIO3_path, "x_2008.csv"), header = FALSE))

label.S <- read.xls(paste0(EXIO3_path, "labs_S_2011.xls"), header = FALSE)[,1:2] %>% rename(name = V1, unit = V2) # slice(1413:1707)

# idx.FE <- grep("Energy Carrier Net", label.S$name)
idx.FE.NENE <- grep("NENE", label.S$name)
idx.FE.NTRA <- grep("NTRA", label.S$name)
idx.FE.TAVI <- grep("TAVI", label.S$name)
idx.FE.TMAR <- grep("TMAR", label.S$name)
idx.FE.TOTH <- grep("TOTH", label.S$name)
idx.FE.TRAI <- grep("TRAI", label.S$name)
idx.FE.TROA <- grep("TROA", label.S$name)
idx.FE.LOSS <- grep("LOSS", label.S$name)
idx.PE.nature <- grep("Nature Inputs", label.S$name)
idx.USE <- grep("Energy Carrier Use", label.S$name)
idx.SUPL <- grep("Energy Carrier Supply ", label.S$name)
idx.FE <- c(idx.FE.NENE, idx.FE.NTRA, idx.FE.TAVI, idx.FE.TMAR, idx.FE.TOTH, idx.FE.TRAI, idx.FE.TROA)

idx.Elec.carrier <- grep("Electricity by ", carrier.name.fin) # Among the 69 carriers

# Arrange intensity outputs (with 69 carriers)
list[tfei.exio, tfei.elec, tfei.non.elec, tfei.sub, tpei.nature, tpei.USE, tpei.SUPL, tnei.exio] <- HarmonizeEXIO3ExtensionFormat(raw.st)
list[dfei.exio, dfei.elec, dfei.non.elec, dfei.sub, dpei.nature, dpei.USE, dpei.SUPL, dnei.exio] <- HarmonizeEXIO3ExtensionFormat(raw.S)
list[dfe.exio, dfe.elec, dfe.non.elec, dfe.sub, dpe.nature, dpe.USE, de.SUPL, dne.exio] <- HarmonizeEXIO3ExtensionFormat(raw.F.2008)






### No need to run for init
### Additional outputs for FE meeting 27/4/2018
# sects <- c(Beef=43, Pork=44, Poultry=45) # Cement, Steel, Aluminum
sects <- c(Cement=101, Steel=104, Alu=108) # Cement, Steel, Aluminum
elec.idx <- grep("Electricity ", carrier.name.fin)
cty.idx.ex <- ZAF_idx_ex

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

