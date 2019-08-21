EXIO3_path = "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Data/IO/EXIOBASE/EXIOBASE3/IOT_bug_w_India/"

raw.S <- read.csv(paste0(EXIO3_path, "S_2008.csv"), header = FALSE)    # Stressor (Intensity)
raw.st <- read.csv(paste0(EXIO3_path, "st_2008.csv"), header = FALSE)  # Total stressor
# raw.S.2007 <- read.csv(paste0(EXIO3_path, "S_2007.csv"), header = FALSE)    # Stressor (Intensity)
# raw.st.2007 <- read.csv(paste0(EXIO3_path, "st_2007.csv"), header = FALSE)  # Total stressor

raw.Y <- read.csv(paste0(EXIO3_path, "Y_2007.csv"), header = FALSE)  # Final demand
raw.F <- read.csv(paste0(EXIO3_path, "F_2007.csv"), header = FALSE)
# raw.F.2008 <- read.csv(paste0(EXIO3_path, "F_2008.csv"), header = FALSE)
raw.F_hh <- read.csv(paste0(EXIO3_path, "F_hh_2007.csv"), header = FALSE)
raw.x <- unlist(read.csv(paste0(EXIO3_path, "x_2007.csv"), header = FALSE))
# raw.x.2008 <- unlist(read.csv(paste0(EXIO3_path, "x_2008.csv"), header = FALSE))

label.S <- read_xls(paste0(EXIO3_path, "labs_S_2011.xls"), col_names = FALSE)[,1:2] %>% rename(name = ...1, unit = ...2) # slice(1413:1707)

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
# list[dfe.exio, dfe.elec, dfe.non.elec, dfe.sub, dpe.nature, dpe.USE, de.SUPL, dne.exio] <- HarmonizeEXIO3ExtensionFormat(raw.F.2008)




