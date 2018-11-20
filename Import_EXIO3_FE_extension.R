EXIO3_path_old = "H:/MyDocuments/Analysis/Final energy/Arkaitz/IOT_bug_w_India/"
EXIO3_path = "H:/MyDocuments/Analysis/Final energy/Arkaitz/IOT_bug_w_TROA/"
EXIO3_path_fix = "H:/MyDocuments/Analysis/Final energy/Arkaitz/IOT/EnvExt_NEU_1995-2015_new/"

# Monetary Mtx EXIO3
# raw.Y <- read.csv(paste0(EXIO3_path, "Y_2007.csv"), header = FALSE)  # Final demand
# raw.x <- unlist(read.csv(paste0(EXIO3_path, "x_2007.csv"), header = FALSE))
# raw.x.2008 <- unlist(read.csv(paste0(EXIO3_path, "x_2008.csv"), header = FALSE))

### Before TROA bug-fix

# Satellites
raw.S.2008 <- read.csv(paste0(EXIO3_path, "S_2008.csv"), header = FALSE)    # Stressor (Intensity)
raw.st.2008 <- read.csv(paste0(EXIO3_path, "st_2008.csv"), header = FALSE)  # Total stressor
raw.S.2007 <- read.csv(paste0(EXIO3_path, "S_2007.csv"), header = FALSE)    # Stressor (Intensity)
raw.st.2007 <- read.csv(paste0(EXIO3_path, "st_2007.csv"), header = FALSE)  # Total stressor

raw.F.2007 <- read.csv(paste0(EXIO3_path, "F_2007.csv"), header = FALSE)
raw.F.2008 <- read.csv(paste0(EXIO3_path, "F_2008.csv"), header = FALSE)
# raw.F_hh <- read.csv(paste0(EXIO3_path, "F_hh_2007.csv"), header = FALSE)
# raw.F_hh.2008 <- read.csv(paste0(EXIO3_path, "F_hh_2008.csv"), header = FALSE)

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
idx.NE <- c(idx.FE, idx.FE.LOSS)

idx.Elec.carrier <- grep("Electricity by ", carrier.name.fin) # Among the 69 carriers


### TROA bug-fix 
# These are different dimensions from those above, because Arkaitz sent in different formats.
# It is easier to replace this FE part in the older format in order to keep all other info (PE).

label.Arkz <- read_xlsx(paste0(EXIO3_path_fix, "NEU_products_IIASA.xlsx"), col_names = FALSE) %>% rename(carrier=X__1, num=X__2)
carriers.Arkz <- gsub("Energy Carrier Net ", "", label.Arkz$carrier)
n_carriers.Arkz <- length(carriers.Arkz)
carriers.RWood <- c(
  # carriers.Arkz %in% gsub("Energy Carrier Net NENE ", "", label.S$name[idx.FE.NENE]),
  # carriers.Arkz %in% gsub("Energy Carrier Net NTRA ", "", label.S$name[idx.FE.NTRA]),
  # carriers.Arkz %in% gsub("Energy Carrier Net TAVI ", "", label.S$name[idx.FE.TAVI]),
  # carriers.Arkz %in% gsub("Energy Carrier Net TMAR ", "", label.S$name[idx.FE.TMAR]),
  # carriers.Arkz %in% gsub("Energy Carrier Net TOTH ", "", label.S$name[idx.FE.TOTH]),
  # carriers.Arkz %in% gsub("Energy Carrier Net TRAI ", "", label.S$name[idx.FE.TRAI]),
  # carriers.Arkz %in% gsub("Energy Carrier Net TROA ", "", label.S$name[idx.FE.TROA]),
  # carriers.Arkz %in% gsub("Energy Carrier Net LOSS ", "", label.S$name[idx.FE.LOSS]))
  sapply(gsub("Energy Carrier Net NENE ", "", label.S$name[idx.FE.NENE]), function(x) {which(x==carriers.Arkz)}),
  sapply(gsub("Energy Carrier Net NTRA ", "", label.S$name[idx.FE.NTRA]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz,
  sapply(gsub("Energy Carrier Net TAVI ", "", label.S$name[idx.FE.TAVI]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*2,
  sapply(gsub("Energy Carrier Net TMAR ", "", label.S$name[idx.FE.TMAR]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*3,
  sapply(gsub("Energy Carrier Net TOTH ", "", label.S$name[idx.FE.TOTH]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*4,
  sapply(gsub("Energy Carrier Net TRAI ", "", label.S$name[idx.FE.TRAI]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*5,
  sapply(gsub("Energy Carrier Net TROA ", "", label.S$name[idx.FE.TROA]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*6,
  sapply(gsub("Energy Carrier Net LOSS ", "", label.S$name[idx.FE.LOSS]), function(x) {which(x==carriers.Arkz)})+n_carriers.Arkz*7)

View(cbind(rep(label.Arkz$carrier, 8)[carriers.RWood], as.character(label.S$name[idx.NE])))

raw.F.2007[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "F_2007.csv"), header = FALSE)[carriers.RWood,]
raw.F.2008[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "F_2008.csv"), header = FALSE)[carriers.RWood,]
raw.S.2007[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "S_2007.csv"), header = FALSE)[carriers.RWood,]
raw.S.2008[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "S_2008.csv"), header = FALSE)[carriers.RWood,]
raw.st.2008[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "st_2008.csv"), header = FALSE)[carriers.RWood,]




# Arrange intensity outputs (with 69 carriers)
list[tfei.exio, tfei.elec, tfei.non.elec, tfei.sub, tpei.nature, tpei.USE, tpei.SUPL, tnei.exio] <- HarmonizeEXIO3ExtensionFormat(raw.st.2008)
list[dfei.exio, dfei.elec, dfei.non.elec, dfei.sub, dpei.nature, dpei.USE, dpei.SUPL, dnei.exio] <- HarmonizeEXIO3ExtensionFormat(raw.S.2008)
list[dfe.exio, dfe.elec, dfe.non.elec, dfe.sub, dpe.nature, dpe.USE, de.SUPL, dne.exio] <- HarmonizeEXIO3ExtensionFormat(raw.F.2008)
# list[dfe.exio.hh, dfe.elec.hh, dfe.non.elec.hh, dfe.sub.hh, dpe.nature.hh, 
#      dpe.USE.hh, de.SUPL.hh, dne.exio.hh] <- HarmonizeEXIO3ExtensionFormat(raw.F_hh.2008)

# tfei.exio.noTRP <- ConstructCustomTEI.EXIO(dfei.exio, trp_idx) # I can use tfei.sub$NTRA instead.
