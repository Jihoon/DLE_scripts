library(readxl)

COICOP_CPC <- read_excel("H:/MyDocuments/IO work/Bridging/UN correspondence tables/COICOP-CPC.xls")
names(COICOP_CPC)[c(2,4)] <- c("Part_COICOP", "Part_CPC")
CPC_ISIC <- read.table("H:/MyDocuments/IO work/Bridging/UN correspondence tables/CPCV1_0_ISIC_REV_3_correspondence.txt",
                         header=TRUE, sep=",", as.is=T)
# ISIC_names <- read.table("H:/MyDocuments/IO work/Bridging/UN correspondence tables/ISIC_Rev_3_english_structure.txt",
#            header=TRUE, sep="\t", quote="", as.is=T)
# names(ISIC_names)[1] <- "lv_ISIC"
# a <- strsplit(ISIC_names[,1], "\t")
# a <- matrix(unlist(a), ncol=2, byrow=TRUE)

names(COICOP_CPC)[c(1,3)] <- c("lv_COICOP", "lv_CPC")
names(CPC_ISIC)[c(1,3)] <- c("lv_CPC", "lv_ISIC")
# COICOP_CPC$lv_CPC <- as.numeric(COICOP_CPC$lv_CPC)

CPC_ISIC$lv_CPC <- formatC(CPC_ISIC$lv_CPC, width=5, flag="0")
CPC_ISIC$lv_ISIC <- formatC(CPC_ISIC$lv_ISIC, width=5, flag="0")

lvls_CPC <- sort(unique(COICOP_CPC$lv_CPC))
lvls_COICOP <- sort(unique(COICOP_CPC$lv_COICOP))
lvls_ISIC <- sort(unique(CPC_ISIC$lv_ISIC))

# Note 1: CPC_ISIC mapping has more CPC categories (832) than COICOP_CPC (2038). (Of course because COICOP is only for consumer consumption.)
#   Left join below will remove those non-COICOP related CPC items.
# Note 2: Six items in COICOP_CPC don't exist in CPC_ISIC. 39110 39217 39260 39280 64100 83631
#   These are '39 - Wastes or scraps', an aggregate item '64100' for '07.3.5 - Combined passenger transport', and 
#   '83631' not existing in CPC 1.0 for 'Sale of advertising space in print media' (from CPC 1.1).

n_CPC <- length(lvls_CPC)
n_COICOP <- length(lvls_COICOP)
n_ISIC <- length(lvls_ISIC)

options(stringsAsFactors = FALSE)
idx_CPC <- data.frame(lv_CPC=lvls_CPC, CPC = 1:n_CPC)
idx_COICOP <- data.frame(lv_COICOP=lvls_COICOP, COICOP = 1:n_COICOP)
idx_ISIC <- data.frame(lv_ISIC=lvls_ISIC, ISIC = 1:n_ISIC)

# COICOP_CPC$lv_COICOP <- factor(COICOP_CPC$lv_COICOP)
# COICOP_CPC$lv_CPC <- factor(COICOP_CPC$lv_CPC)
# CPC_ISIC$lv_CPC <- factor(CPC_ISIC$lv_CPC)
# CPC_ISIC$lv_ISIC <- factor(CPC_ISIC$lv_ISIC)

COICOP_CPC <- COICOP_CPC %>% left_join(idx_COICOP, by="lv_COICOP" ) %>% left_join(idx_CPC, by="lv_CPC")
CPC_ISIC <- CPC_ISIC %>% left_join(idx_ISIC, by="lv_ISIC" ) %>% left_join(idx_CPC, by="lv_CPC") %>% filter(!is.na(CPC))

m_COICOP_CPC <- matrix(0, n_COICOP, n_CPC)
m_CPC_ISIC<- matrix(0, n_CPC, n_ISIC)
Q_UN <- matrix(0, n_COICOP, n_ISIC)  # Final mapping

rownames(m_COICOP_CPC) <- lvls_COICOP
colnames(m_COICOP_CPC) <- lvls_CPC
rownames(m_CPC_ISIC) <- lvls_CPC
colnames(m_CPC_ISIC) <-lvls_ISIC
rownames(Q_UN) <- lvls_COICOP
colnames(Q_UN) <- lvls_ISIC

m_COICOP_CPC[as.matrix(COICOP_CPC[c("COICOP", "CPC")])] <- 1
m_CPC_ISIC[as.matrix(CPC_ISIC[c("CPC", "ISIC")])] <- 1

for (i in 1:n_COICOP) {
  if (sum(m_COICOP_CPC[i,])==0) {next()}
  a <- m_CPC_ISIC[which(m_COICOP_CPC[i,]==1), , drop=FALSE] # Matching rows in m_CPC_ISIC for the i-th COICOP item
  Q_UN[i,] <- as.numeric(apply(a, 2, function(x) { Reduce("|", x) }))
}

# Remove unnecessary COICOP rows(117 -> 109)
Q_UN['10.1.0',] <- as.numeric(apply(Q_UN[c('10.1.0','10.2.0','10.3.0','10.4.0','10.5.0'),], 2, function(x) { Reduce("|", x) }))
Q_UN <- Q_UN[!rownames(Q_UN) %in% c('02.3.0', '12.2.0', '12.5.1', '12.6.1'),] 
Q_UN <- Q_UN[!rownames(Q_UN) %in% c('10.2.0', '10.3.0', '10.4.0', '10.5.0'),] 

# Harmonize ISIC Rev3 with EXIO classification
ISICrev3 <- read.table("H:/MyDocuments/IO work/Bridging/UN correspondence tables/ISIC_Rev_3_english_structure.txt",
                       header=TRUE, sep="\t", quote = "", as.is = TRUE)
ISICrev3$Code <- as.numeric(ISICrev3$Code)
ISICrev3 <- ISICrev3[ISICrev3$Code > 1000 & !is.na(ISICrev3$Code),]

ISICrev3$Code <- formatC(ISICrev3$Code, width=5, flag="0")
temp <- data.frame(Code = lvls_ISIC[1:9], Description = c(
           'Growing of cereals and other crops n.e.c.',
           'Growing of vegetables, horticultural specialties and nursery products',
           'Growing of fruit, nuts, beverage and spice crops',
           'Farming of cattle, sheep, goats, horses, asses, mules and hinnies; dairy farming',
           'Other animal farming; production of animal products n.e.c.',
           # 'Growing of crops combined with farming of animals (mixed farming)',
           'Agricultural and animal husbandry service activities, except veterinary activities',
           'Hunting, trapping and game propagation including related service activities',
           'Forestry, logging and related service activities',
           'Fishing, operation of fish hatcheries and fish farms; service activities incidental to fishing'))
ISICrev3 <- rbind(temp, ISICrev3)
names(ISICrev3)[1] <- "lv_ISIC"

# write.table(ISICrev3, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

ISIC_EXIO <- read_excel("H:/MyDocuments/IO work/Bridging/UN correspondence tables/ISIC_EXIO_mapping.xlsx", 3)
ISIC_EXIO <- ISIC_EXIO %>% gather(name, ISIC, -EXIO) %>% select(-name) %>% arrange(ISIC) %>% filter(!is.na(ISIC))
names(ISIC_EXIO) <- c("lv_EXIO", "lv_ISIC")
ISIC_EXIO$lv_ISIC <- formatC(ISIC_EXIO$lv_ISIC, width=5, flag="0")

foo <- function(x) {  # For each row of Q_UN
  ISIC_match <- names(which(x==1))   # Get codes of mapped ISIC sectors
  a <- matrix(0, 1, 200)
  a[ISIC_EXIO$lv_EXIO[ISIC_EXIO$lv_ISIC %in% ISIC_match]] <- 1   # Set the corresponding EXIO cells mapped to those ISIC sectors
  return(a)
}

Q_UN_EXIO <- t(apply(Q_UN, 1, foo))
colnames(Q_UN_EXIO) <- EX_catnames

# Special treatment needed for '07.3.5 - Combined passenger transport'
# CPC_ISIC is missing the mapping, ending up with zero row for 07.3.5 in Q_UN_EXIO[68,]
a <- Q_UN_EXIO[rownames(Q_UN_EXIO) %in% c('07.3.1','07.3.2','07.3.3','07.3.4'),]
Q_UN_EXIO[rownames(Q_UN_EXIO)=='07.3.5',] <- as.numeric(apply(a, 2, function(x) { Reduce("|", x) }))

# So now I have bridge_COICOP_EXIO_q, bridge_ICP_EXIO_q, and Q_UN_EXIO.
bridge_coicop_exio <- data.frame(Q_UN_EXIO)
names(bridge_coicop_exio) <- EX_catnames  
row.names(bridge_coicop_exio) <- as.matrix(COICOP_catnames2)

# This file is used to create ICP-EXIO mapping after some manual editing
write.csv(bridge_coicop_exio, "H:/MyDocuments/IO work/Bridging/CES-COICOP/COICOIP_EXIO_Qual_UN.csv")

# Fuel rows replaced by the standardized DLE fuel sectors
# But we don't have (for FRA) any detailed info about further fuel breakdowns
# Q_UN_EXIO <- as.matrix(Q_UN_EXIO[-c(32:36,61),] %>% rbind(bridge_fuel_EXIO_q))
