#####################
# Main run of rIPFP #
#####################

# Expenditure in cells
n_draw <- 10

# D_val_uncertainty <- 1
# list[result_IND, NC_IND] <- Run_rIPFP(bridge_ICP_EXIO_q[,-1], "IND")
D_val_uncertainty <- 0
list[result_IND_noVal, NC_IND_noVal] <- Run_rIPFP(bridge_ICP_EXIO_q[,-1], "IND")
list[result_BRA_noVal, NC_BRA_noVal] <- Run_rIPFP(bridge_ICP_EXIO_q[,-1], "BRA")

# D_val_uncertainty <- 1
# list[result_FRA, NC_FRA] <- Run_rIPFP(bridge_COICOP_EXIO_q[,-1], "FRA")
# D_val_uncertainty <- 0
# list[result_FRA_noVal, NC_FRA_noVal] <- Run_rIPFP(bridge_COICOP_EXIO_q[,-1], "FRA")

# Allocation ratio in cells - summing up to 1 each row
func1 <- function (x) {
  a <- diag(1/rowSums(x))
  a[is.infinite(a)] <- 0
  x <- a %*% x
}

# final_alloc_list_FRA_noVal <- lapply(result_FRA_noVal, func1)
final_alloc_list_IND_noVal <- lapply(result_IND_noVal, func1)
final_alloc_list_BRA_noVal <- lapply(result_BRA_noVal, func1)



###############################################
### Calculate ICP Sectoral energy intensity ###
###############################################

# in MJ/EUR
xlcFreeMemory()

# Including val mtx uncertainty
# India
alloc_nonRAS <- get_bridge_COICOP_EXIO(bridge_ICP_EXIO_q[,-1], n_draw)

D_val_uncertainty <- 0
IND_inten_RAS_noVal <- SetupSectorIntensities(final_alloc_list_IND_noVal, NC_IND_noVal, "IN")
IND_inten_nonRAS_noVal <- SetupSectorIntensities(alloc_nonRAS, NC_IND_noVal, "IN")

# Brazil
D_val_uncertainty <- 0
BRA_inten_RAS_noVal <- SetupSectorIntensities(final_alloc_list_BRA_noVal, NC_BRA_noVal, "BR")
BRA_inten_nonRAS_noVal <- SetupSectorIntensities(alloc_nonRAS, NC_BRA_noVal, "BR")

# France
# D_val_uncertainty <- 1
alloc_nonRAS <- get_bridge_COICOP_EXIO(bridge_COICOP_EXIO_q[,-1], n_draw)

D_val_uncertainty <- 0
FRA_inten_RAS_noVal <- SetupSectorIntensities(final_alloc_list_FRA_noVal, NC_FRA_noVal, "FR")
FRA_inten_nonRAS_noVal <- SetupSectorIntensities(alloc_nonRAS, NC_FRA_noVal, "FR")


# Replacing intensity values for ICP sectors with 0 expenditure
# Note: There are cases where ICP expenditure is zero for a sector from a country.
# In the RAS approach, we do not get intensity numbers because no expenditure is assigned and thus no emission.
# In this case, I copy intensities from the non-RAS estimation.
no_expense_IND <- which((rowSums(bridge_ICP_EXIO_q[,-1])!=0) & (IND_FD_ICP_usd2007[,1]==0))
no_expense_IND <- no_expense_IND[!(no_expense_IND %in% grep("UNBR", ICP_catnames))]   # Remove UNBR items
IND_inten_RAS_combined_noVal <- IND_inten_RAS_noVal
IND_inten_RAS_combined_noVal[,no_expense_IND] <- IND_inten_nonRAS_noVal[,no_expense_IND]

no_expense_BRA <- which((rowSums(bridge_ICP_EXIO_q[,-1])!=0) & (BRA_FD_ICP_usd2007[,1]==0))
no_expense_BRA <- no_expense_BRA[!(no_expense_IND %in% grep("UNBR", ICP_catnames))]   # Remove UNBR items
BRA_inten_RAS_combined_noVal <- BRA_inten_RAS_noVal
BRA_inten_RAS_combined_noVal[,no_expense_BRA] <- BRA_inten_nonRAS_noVal[,no_expense_BRA]

no_expense_FRA <- which((rowSums(bridge_COICOP_EXIO_q[,-1])!=0) & (FRA_FD_ICP_usd2007[,1]==0))
FRA_inten_RAS_combined_noVal <- FRA_inten_RAS_noVal
FRA_inten_RAS_combined_noVal[,no_expense_FRA] <- FRA_inten_nonRAS_noVal[,no_expense_FRA]






####################
### Sanity check ###
####################

# 1. Electricity allocation results
elec_alloc <- do.call("rbind", lapply(final_alloc_list_BRA_noVal, '[', 155,))  # was 63 in COICOP
colnames(elec_alloc) <- EX_catnames
elec_alloc<-(elec_alloc)[,128:141]
colnames(elec_alloc) <- gsub('Electricity by ', '', colnames(elec_alloc))
colnames(elec_alloc) <- c("coal","gas","nuclear","hydro","wind","oil","biomass/waste","PV","solar thermal","tidal",
                          "geothermal","nec","transmission","distr/trade")

elec_gen <- elec_alloc[,1:12]
a <- sweep(elec_gen, 1, rowSums(elec_gen), '/')

opar <- par() 
par(mar=c(8.5,4.1,2.1,2.1))
boxplot(elec_alloc, ylab ="Shares", range=0, axes = TRUE, las = 2)
title(xlab = "EXIO electricity sectors", line = 6.5)
boxplot(a, ylab ="Generation shares", range=0, axes = TRUE, las = 2)
title(xlab = "EXIO electricity sectors", line = 6.5)
par(opar)

# 2. Check HH energy consumption / Cap / Yr
# hh consumption [$2007] = 6.898717e+11
# per-cap energy
IND_pop_2007 <- 1.159e9
a <- WDI(country = "IN", indicator = "NE.CON.PETC.CD", start = 2007, end = 2011, extra = FALSE, cache = NULL)
a$NE.CON.PETC.CD[5] * EXR_EUR$r * median(int_by_decile_IN[,1]) / 1e3 / IND_pop_2007

# Total primary energy consumption  
# https://www.eia.gov/cfapps/ipdbproject/IEDIndex3.cfm?tid=44&pid=44&aid=2
# TPEC -> 19 QBtu = 479 Mtoe (2007)
2.005E+19 / 1e9 / IND_pop_2007
# http://www.statista.com/statistics/265582/primary-energy-consumption-in-india/
# 420.3 Mtoe(2007)
1.75971204E+19 / 1e9 / IND_pop_2007
# http://www.statista.com/statistics/265582/primary-energy-consumption-in-india/
# 575 Mtoe
2.40741E+19 / 1e9 / IND_pop_2007
# 622 Mtoe from http://www.iea.org/sankey/#?c=India&s=Balance
2.6041E+19 / 1e9 / IND_pop_2007

# France
FRA_pop_2007 <- 64e6
# https://www.google.at/search?q=primary+energy+consumption+france&espv=2&biw=1920&bih=911&tbm=isch&tbo=u&source=univ&sa=X&ved=0ahUKEwiXnpScuLHMAhXBIcAKHaWoAV0QsAQIIQ&dpr=1#imgrc=h_RwRGCAT_rZyM%3A
# 250 Mtoe (2007)
1.0467E+19 / 1e9 / FRA_pop_2007

# per-cap energy
a <- WDI(country = "FR", indicator = "NE.CON.PETC.CD", start = 2007, end = 2011, extra = FALSE, cache = NULL)
a$NE.CON.PETC.CD[5] * EXR_EUR$r * median(int_by_decile_FR[,1]) / 1e3 / FRA_pop_2007

BRA_pop_2007 <- 1.9e8
# https://www.eia.gov/cfapps/ipdbproject/IEDIndex3.cfm?tid=44&pid=44&aid=2
# TPEC -> 10 QBtu = 252 Mtoe (2007) = 1.06E+19 J
# TPEC -> 275 Mtoe (2007) from http://www.iea.org/sankey/#?c=Brazil&s=Balance
1.15E+19 / 1e9 / BRA_pop_2007

# Compare rowSum sizes between CES and RASed one
a <- cbind(bridge_ICP_EXIO_q[,1], rowSums(final_RAS_list[[1]]), IND_FD_ICP_usd2007[,1], IND_FD_ICP_usd2007[,1] > rowSums(final_RAS_list[[1]]))

idx_anomaly <- IND_FD_ICP_usd2007[,1] > rowSums(final_RAS_list[[1]])
anomaly <- data.frame(bridge_ICP_EXIO_q[idx_anomaly,1], IND_FD_ICP_usd2007[,1][idx_anomaly], rowSums(final_RAS_list[[1]])[idx_anomaly])
names(anomaly) <- c("SV_sector", "SV_exp", "RAS_exp")
write.table(anomaly, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

b <- cbind(qual_map_init, final_RAS_list[[1]])
exio_anomal <- apply(b[idx_anomaly,], 1, function(x) {cbind(names(which(x==1)), x[which(x==1)+200])})
# exio_anomal <- apply(qual_map_init[idx_anomaly,], 1, function(x) {cbind(names(which(x==1)), colConst_init[which(x==1)])})
a <- do.call("rbind", lapply(exio_anomal, '[', ,))

write.table(a, "clipboard", sep="\t", row.names = FALSE, col.names = FALSE)

# Let's see the 'Gas' item from CES (IND)
do.call("rbind", lapply(final_RAS_list, '[', 64,))
a<- cbind(names(bridge_ICP_EXIO_q)[2:201], final_RAS_list[[1]][63,], final_RAS_list[[1]][63,]/sum(final_RAS_list[[1]][63,]))


#################
###  Outputs  ###
#################

# Summary table
library(pastecs)
colnames(IND_inten_RAS_combined) <- rownames(bridge_icp_exio)
colnames(FRA_inten_RAS_combined) <- COICOP_catnames2[,1]

IND_int_summary_combined <- stat.desc(IND_inten_RAS_combined) 
IND_int_summary_combined <- t(IND_int_summary_combined[c(4,5,9,13),]) %>% round(digits=2) %>% format(digits = 2, nsmall = 1, scientific=FALSE)
FRA_int_summary_combined <- stat.desc(FRA_inten_RAS_combined) 
FRA_int_summary_combined <- t(FRA_int_summary_combined[c(4,5,9,13),]) %>% round(digits=2) %>% format(digits = 2, nsmall = 1, scientific=FALSE)
BRA_int_summary_combined <- stat.desc(BRA_inten_RAS_combined_noVal) 
BRA_int_summary_combined <- t(BRA_int_summary_combined[c(4,5,9,13),]) %>% round(digits=2) %>% format(digits = 2, nsmall = 1, scientific=FALSE)
# int_summary_combined <- round(int_summary_combined, digits = 2)
# int_summary_combined <- format(int_summary_combined, digits = 2, nsmall = 1, scientific=FALSE)

# int_summary <- stat.desc(inten_RAS) 
# int_summary <- t(int_summary[c(4,5,9,13),]) %>% round(digits=2) %>% format(digits = 2, nsmall = 1, scientific=FALSE)

write.csv(IND_int_summary_combined, "India - Intensity summary without direct E.csv")
write.csv(FRA_int_summary_combined, "France - Intensity summary without direct E.csv")
write.csv(BRA_int_summary_combined, "France - Intensity summary without direct E.csv")
# write.csv(int_summary, "Intensity summary with direct E.csv")

intensities <- cbind(int_summary_combined, int_summary)
