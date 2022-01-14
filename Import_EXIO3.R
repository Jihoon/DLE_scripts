# EXIO3_path_old for satellite format
EXIO3_path_old = "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Data/IO/EXIOBASE/EXIOBASE3/IOT_bug_w_India/"

# EXIO3_path for A matrix or other base IO matrices
EXIO3_path = "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Data/IO/EXIOBASE/EXIOBASE3/IOT_bug_w_TROA/"

# EXIO3_path_fix for energy satellite
EXIO3_path_fix = "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Data/IO/EXIOBASE/EXIOBASE3/EnvExt/EnvExt_NEU_1995-2015_20190114/" 

# Monetary Mtx EXIO3
# raw.Y <- read.csv(paste0(EXIO3_path, "Y_2007.csv"), header = FALSE)  # Final demand
# raw.x <- unlist(read.csv(paste0(EXIO3_path, "x_2007.csv"), header = FALSE))
# raw.x.2008 <- unlist(read.csv(paste0(EXIO3_path, "x_2008.csv"), header = FALSE))

### Before TROA bug-fix

# Satellites (Still loading it becasue non-energy materials may be used some day)
# raw.S.2008 <- read.csv(paste0(EXIO3_path, "S_2008.csv"), header = FALSE)    # Stressor (Intensity)
# raw.st.2008 <- read.csv(paste0(EXIO3_path, "st_2008.csv"), header = FALSE)  # Total stressor
# raw.S.2007 <- read.csv(paste0(EXIO3_path, "S_2007.csv"), header = FALSE)    # Stressor (Intensity)
# raw.st.2007 <- read.csv(paste0(EXIO3_path, "st_2007.csv"), header = FALSE)  # Total stressor
# 
# raw.F.2007 <- read.csv(paste0(EXIO3_path, "F_2007.csv"), header = FALSE)
# raw.F.2008 <- read.csv(paste0(EXIO3_path, "F_2008.csv"), header = FALSE)

raw.S <- read.csv(paste0(EXIO3_path, "S_", IO.year, ".csv"), header = FALSE)    # Stressor (Intensity)
raw.st <- read.csv(paste0(EXIO3_path, "st_", IO.year, ".csv"), header = FALSE)  # Total stressor
raw.F <- read.csv(paste0(EXIO3_path, "F_", IO.year, ".csv"), header = FALSE)
# raw.V <- read.csv(paste0(EXIO3_path, "V_", IO.year, ".csv"), header = FALSE)

raw.F_hh <- read.csv(paste0(EXIO3_path, "F_hh_", IO.year, ".csv"), header = FALSE)
# raw.F_hh.2008 <- read.csv(paste0(EXIO3_path, "F_hh_2008.csv"), header = FALSE)

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
idx.NE <- c(idx.FE, idx.FE.LOSS)



### TROA bug-fix 
# These are different dimensions from those above, because Arkaitz sent in different formats.
# It is easier to replace this FE part in the older format in order to keep all other info (PE).

label.Arkz <- read_xlsx(paste0(EXIO3_path_fix, "NEU_products_IIASA.xlsx"), col_names = FALSE) %>% rename(carrier=...1, num=...2)
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


idx.Elec.carrier <- grep("Electricity by ", carriers.Arkz) # Among the 69 carriers

# View(cbind(rep(label.Arkz$carrier, 8)[carriers.RWood], as.character(label.S$name[idx.NE])))

# New fixed version replaces the energy part of the extension.
# raw.F.2007[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "F_2007.csv"), header = FALSE)[carriers.RWood,]
# raw.F.2008[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "F_2008.csv"), header = FALSE)[carriers.RWood,]
# raw.S.2007[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "S_2007.csv"), header = FALSE)[carriers.RWood,]
# raw.S.2008[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "S_2008.csv"), header = FALSE)[carriers.RWood,]
# raw.st.2008[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "st_2008.csv"), header = FALSE)[carriers.RWood,]

raw.F[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "F_pxp_", IO.year, ".csv"), header = FALSE)[carriers.RWood,]
raw.S[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "S_pxp_", IO.year, ".csv"), header = FALSE)[carriers.RWood,]
raw.st[idx.NE,] <- read.csv(paste0(EXIO3_path_fix, "st_pxp_", IO.year, ".csv"), header = FALSE)[carriers.RWood,]




# Arrange intensity outputs (with 69 carriers) - fei: MJ/EUR
list[tfei.exio, tfei.elec, tfei.non.elec, tfei.sub, tnei.exio] <- HarmonizeEXIO3ExtensionFormat(raw.st) # tpei.nature, tpei.USE, tpei.SUPL,
list[dfei.exio, dfei.elec, dfei.non.elec, dfei.sub, dnei.exio] <- HarmonizeEXIO3ExtensionFormat(raw.S) # dpei.nature, dpei.USE, dpei.SUPL, 
list[dfe.exio, dfe.elec, dfe.non.elec, dfe.sub, dne.exio] <- HarmonizeEXIO3ExtensionFormat(raw.F) # dpe.nature, dpe.USE, de.SUPL, 
list[dfe.exio.hh, dfe.elec.hh, dfe.non.elec.hh, dfe.sub.hh, dne.exio.hh] <- HarmonizeEXIO3ExtensionFormat(raw.F_hh)

# tfei.exio.noTRP <- ConstructCustomTEI.EXIO(dfei.exio, trp_idx) # I can use tfei.sub$NTRA instead.

# Save the results for DLE upscaling

EXIO3_FD <- read.csv(paste0(EXIO3_path, "Y_", IO.year, ".csv"), header = FALSE)
EXIO3_FD.hh <- as.matrix(EXIO3_FD[,seq(1, dim(EXIO3_FD)[2], 7)])
EXIO3_data <- list(tfei.exio, tfei.elec, tfei.non.elec, tfei.sub, tnei.exio,
                     dfei.exio, dfei.elec, dfei.non.elec, dfei.sub, dnei.exio,
                     dfe.exio, dfe.elec, dfe.non.elec, dfe.sub, dne.exio,
                     dfe.exio.hh, dfe.elec.hh, dfe.non.elec.hh, dfe.sub.hh, dne.exio.hh,
                     EXIO3_FD.hh)
names(EXIO3_data) <- c("tfei.exio", "tfei.elec", "tfei.non.elec", "tfei.sub", "tnei.exio", 
                         "dfei.exio", "dfei.elec", "dfei.non.elec", "dfei.sub", "dnei.exio", 
                         "dfe.exio", "dfe.elec", "dfe.non.elec", "dfe.sub", "dne.exio", 
                         "dfe.exio.hh", "dfe.elec.hh", "dfe.non.elec.hh", "dfe.sub.hh", "dne.exio.hh",
                         "EXIO3_FD.hh")
EXIO3_sect_reg <- list(EXIO_sector = EX_catnames, EXIO_region = exio_ctys)
save(EXIO3_data, file = paste0("P:/ene.general/DecentLivingEnergy/DLE_scaleup/Data/IO/EXIO3_energy_ext_", as.character(IO.year), ".Rdata"))
save(EXIO3_sect_reg, file = paste0("P:/ene.general/DecentLivingEnergy/DLE_scaleup/Data/IO/EXIO3_sectors_regions.Rdata"))



#### Auxiliary script (may not be used) ####


# Then we need to overwrite L_inverse, tot_demand, final_demand, and the country list with EXIO3 data.
exio_ctys <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
               "FR", "GR", 
               "HR", # Added new at EXIO3
               "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
               "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", 
               "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW", 
               "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")

num.cty <- length(exio_ctys)
exio.len <- length(exio_ctys)*200
exio.fd.len <- length(exio_ctys)*7

# Get IND final demand from EXIO [M.EUR to M.USD]
IND_place <- which(exio_ctys=="IN")
IND_idx_fd <- seq(7*(IND_place-1)+1, 7*IND_place)   # 7 final demand columns per country
IND_idx_ex <- seq(200*(IND_place-1)+1, 200*IND_place)   # 200 EXIO comodities

# Get BRA final demand from EXIO [M.EUR to M.USD]
BRA_place <- which(exio_ctys=="BR")
BRA_idx_fd <- seq(7*(BRA_place-1)+1, 7*BRA_place)   # 7 final demand columns per country
BRA_idx_ex <- seq(200*(BRA_place-1)+1, 200*BRA_place)   # 7 final demand columns per country

# Get ZAF final demand from EXIO [M.EUR to M.USD]
ZAF_place <- which(exio_ctys=="ZA")
ZAF_idx_fd <- seq(7*(ZAF_place-1)+1, 7*ZAF_place)   # 7 final demand columns per country
ZAF_idx_ex <- seq(200*(ZAF_place-1)+1, 200*ZAF_place)   # 200 EXIO comodities



# Overwrite these with values from EXIO3
L_inverse <- read.csv(paste0(EXIO3_path, paste0("L_", IO.year, ".csv")), header = FALSE)
final_demand <- read.csv(paste0(EXIO3_path, paste0("Y_", IO.year, ".csv")), header = FALSE)
tot_output <- read.csv(paste0(EXIO3_path, paste0("x_", IO.year, ".csv")), header = FALSE)
tot_demand <- rowSums(tot_output) 

save(L_inverse, file=paste0("./Saved tables/L_inverse_EXIO3_", IO.year, ".Rda"))
load(file=paste0("./Saved tables/L_inverse_EXIO3_", IO.year, ".Rda")) # L_inverse


# *_fd_exio being used in rIPFP
IND_fd_ex <- matrix(final_demand[,IND_idx_fd[1]], nrow=200) / EXR_EUR$r  # to M.USD (2007 MER)
IND_fd_exio <- rowSums(IND_fd_ex) # Sum all HH FD across countries

BRA_fd_ex <- matrix(final_demand[,BRA_idx_fd[1]], nrow=200) / EXR_EUR$r  # to M.USD (2007 MER)
BRA_fd_exio <- rowSums(BRA_fd_ex) # Sum all HH FD across countries

ZAF_fd_ex <- matrix(final_demand[,ZAF_idx_fd[1]], nrow=200) / EXR_EUR$r  # to M.USD (2007 MER)
ZAF_fd_exio <- rowSums(ZAF_fd_ex) # Sum all HH FD across countries

