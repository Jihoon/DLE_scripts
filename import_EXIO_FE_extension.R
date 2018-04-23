IOT_path = "H:/MyDocuments/Analysis/Final energy/Arkaitz/IOT/"

raw.S <- read.csv(paste0(IOT_path, "S_2007.csv"), header = FALSE)
raw.F <- read.csv(paste0(IOT_path, "F_2007.csv"), header = FALSE)
raw.F_hh <- read.csv(paste0(IOT_path, "F_hh_2007.csv"), header = FALSE)
raw.x <- unlist(read.csv(paste0(IOT_path, "x_2007.csv"), header = FALSE))

label.S <- read.xls(paste0(IOT_path, "labs_S_2011.xls"), header = FALSE)[,1:2] %>% rename(name = V1, unit = V2) # slice(1413:1707)

# idx.FE <- grep("Energy Carrier Net", label.S$name)
idx.FE.NENE <- grep("NENE", label.S$name)
idx.FE.NTRA <- grep("NTRA", label.S$name)
idx.FE.TAVI <- grep("TAVI", label.S$name)
idx.FE.TMAR <- grep("TMAR", label.S$name)
idx.FE.TOTH <- grep("TOTH", label.S$name)
idx.FE.TRAI <- grep("TRAI", label.S$name)
idx.FE.TROA <- grep("TROA", label.S$name)
idx.FE.LOSS <- grep("LOSS", label.S$name)
idx.FE <- c(idx.FE.NENE, idx.FE.NTRA, idx.FE.TAVI, idx.FE.TMAR, idx.FE.TOTH, idx.FE.TRAI, idx.FE.TROA)


tot.NENE.raw <- data.frame(name=label.S$name[idx.FE.NENE], mat=raw.F[idx.FE.NENE, ]) %>% 
  mutate(name=gsub("Energy Carrier Net NENE ", "", name))
tot.NTRA.raw <- data.frame(name=label.S$name[idx.FE.NTRA], mat=raw.F[idx.FE.NTRA, ]) %>% 
  mutate(name=gsub("Energy Carrier Net NTRA ", "", name))
tot.TAVI.raw <- data.frame(name=label.S$name[idx.FE.TAVI], mat=raw.F[idx.FE.TAVI, ]) %>% 
  mutate(name=gsub("Energy Carrier Net TAVI ", "", name))
tot.TMAR.raw <- data.frame(name=label.S$name[idx.FE.TMAR], mat=raw.F[idx.FE.TMAR, ]) %>% 
  mutate(name=gsub("Energy Carrier Net TMAR ", "", name))
tot.TOTH.raw <- data.frame(name=label.S$name[idx.FE.TOTH], mat=raw.F[idx.FE.TOTH, ]) %>% 
  mutate(name=gsub("Energy Carrier Net TOTH ", "", name))
tot.TRAI.raw <- data.frame(name=label.S$name[idx.FE.TRAI], mat=raw.F[idx.FE.TRAI, ]) %>% 
  mutate(name=gsub("Energy Carrier Net TRAI ", "", name))
tot.TROA.raw <- data.frame(name=label.S$name[idx.FE.TROA], mat=raw.F[idx.FE.TROA, ]) %>% 
  mutate(name=gsub("Energy Carrier Net TROA ", "", name))

tot.NENE <- data.frame(name=carrier.name.fin) %>% full_join(tot.NENE.raw) %>% slice(1:69) %>% select(-name)
tot.NTRA <- data.frame(name=carrier.name.fin) %>% full_join(tot.NTRA.raw) %>% slice(1:69) %>% select(-name)
tot.TAVI <- data.frame(name=carrier.name.fin) %>% full_join(tot.TAVI.raw) %>% slice(1:69) %>% select(-name)
tot.TMAR <- data.frame(name=carrier.name.fin) %>% full_join(tot.TMAR.raw) %>% slice(1:69) %>% select(-name)
tot.TOTH <- data.frame(name=carrier.name.fin) %>% full_join(tot.TOTH.raw) %>% slice(1:69) %>% select(-name)
tot.TRAI <- data.frame(name=carrier.name.fin) %>% full_join(tot.TRAI.raw) %>% slice(1:69) %>% select(-name)
tot.TROA <- data.frame(name=carrier.name.fin) %>% full_join(tot.TROA.raw) %>% slice(1:69) %>% select(-name)
    
tot.NENE[is.na(tot.NENE)] <- 0
tot.NTRA[is.na(tot.NTRA)] <- 0
tot.TAVI[is.na(tot.TAVI)] <- 0
tot.TMAR[is.na(tot.TMAR)] <- 0
tot.TOTH[is.na(tot.TOTH)] <- 0
tot.TRAI[is.na(tot.TRAI)] <- 0
tot.TROA[is.na(tot.TROA)] <- 0

tot.FE <- tot.NENE + tot.NTRA + tot.TAVI + tot.TMAR + tot.TOTH + tot.TRAI + tot.TROA
y <- 1/raw.x
y[is.infinite(y)] <- 0 
dfei.exio <- as.matrix(tot.FE) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
tfei.exio <- eigenMapMatMult(dfei.exio, as.matrix(L_inverse)) # faster



sum(tot.NENE.raw[,-1], tot.NTRA.raw[,-1], tot.TAVI.raw[,-1], 
    tot.TMAR.raw[,-1], tot.TOTH.raw[,-1], tot.TRAI.raw[,-1], 
    tot.TROA.raw[,-1],na.rm=TRUE)
sum(tot.FE) / sum(raw.F[idx.FE, ]) # % after losing sugar, manure, etc.

sum(raw.F_hh[idx.FE, ]) + sum(raw.F[idx.FE, ]) # Compare with IEA total final consumption


add <- function(x) Reduce("+", x)

tot.INDR <- tot.NTRA # Industry + Agriculture + Fishing + Commercial


label.all <- gsub("NTRA ", "", label.S$name[idx.FE.NTRA])
data.frame(name=label.all) %>% full_join(data.frame(name=gsub("NENE ", "", label.S$name[idx.FE.NENE]), 1:length(idx.FE.NENE)))
a <- data.frame(name=carrier.name.fin, old=1:length(carrier.name.fin)) %>% 
  full_join(data.frame(name=gsub("Energy Carrier Net NTRA ", "", label.S$name[idx.FE.NTRA]), new=1:length(idx.FE.NTRA)))
view(a)
