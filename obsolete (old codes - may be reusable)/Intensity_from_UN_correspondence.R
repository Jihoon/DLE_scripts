n_draw <- 10

D_val_uncertainty <- 0

list[result_IND_all, NC_IND_all, FD_IND_adj] <- Run_rIPFP(bridge_ICP_EXIO_q[,-1], "IND")
final_alloc_list_IND_all <- lapply(result_IND_all, func1)

alloc_nonRAS <- get_bridge_COICOP_EXIO(bridge_ICP_EXIO_q[,-1], n_draw)
IND_inten_RAS_all <- SetupSectorIntensities(final_alloc_list_IND_all, NC_IND_all, "IN")
IND_nonRAS_all <- SetupSectorIntensities(alloc_nonRAS, NC_IND_all, "IN")

# Filling in sectors with no expenditures
no_expense_IND <- which((rowSums(bridge_ICP_EXIO_q[,-1])!=0) & (IND_FD_ICP_io.yr[,1]==0))
no_expense_IND <- no_expense_IND[!(no_expense_IND %in% grep("UNBR", ICP_catnames))]   # Remove UNBR items
IND_inten_comb_all <- IND_inten_RAS_all
IND_inten_comb_all[,no_expense_IND] <- IND_nonRAS_all[,no_expense_IND]

save(IND_inten_comb_all, file="./Saved tables/IND_intensities.Rda")



list[result_BRA_all, NC_BRA_all, FD_BRA_adj] <- Run_rIPFP(bridge_ICP_EXIO_q[,-1], "BRA")
final_alloc_list_BRA_all <- lapply(result_BRA_all, func1)

alloc_nonRAS <- get_bridge_COICOP_EXIO(bridge_ICP_EXIO_q[,-1], n_draw)
BRA_inten_RAS_all <- SetupSectorIntensities(final_alloc_list_BRA_all, NC_BRA_all, "BR")
BRA_nonRAS_all <- SetupSectorIntensities(alloc_nonRAS, NC_BRA_all, "BR")

# Filling in sectors with no expenditures
no_expense_BRA <- which((rowSums(bridge_ICP_EXIO_q[,-1])!=0) & (BRA_FD_ICP_io.yr[,1]==0))
no_expense_BRA <- no_expense_BRA[!(no_expense_BRA %in% grep("UNBR", ICP_catnames))]   # Remove UNBR items
BRA_inten_comb_all <- BRA_inten_RAS_all
BRA_inten_comb_all[,no_expense_BRA] <- BRA_nonRAS_all[,no_expense_BRA]




########################################
### Primary energy per HH and capita ###
########################################

### 1. India
load("./Saved tables/IND_FD_harmonized.Rda")
# n_draw x n_HH
IND_inten_comb_all[,157] <- 0  # Ignore fuelwood for now
xlcFreeMemory()
eHH <- t(IND_inten_comb_all %*% IND_FD_ICP_AllHH) / 1000 / scaler_IND # MJ to GJ 
xlcFreeMemory()
eHH <- data.table(hhid = colnames(IND_FD_ICP_AllHH), eHH)
setkey(eHH, hhid)
xlcFreeMemory()
eHH <- merge(eHH, IND_HH, by="hhid") 
setkey(eHH, hhid, consumption)
xlcFreeMemory()

load("./Saved tables/IND_GJperHH.Rda")
eHH_cap <- eHH
eHH_cap[, 2:(n_draw+1) := eHH_cap[,2:(n_draw+1), with=FALSE] / hh_size, with=FALSE]

# Average HH energy/cap and get SD
eHH_cap_avg <- data.table(hhid = eHH[,1, with=FALSE],
                          mean = apply(eHH[,2:(n_draw+1), with=FALSE], 1, mean) / eHH$hh_size, 
                          sd = apply(eHH[,2:(n_draw+1), with=FALSE], 1, sd) / eHH$hh_size, 
                          eHH[,(n_draw+2):dim(eHH)[2], with=FALSE])
names(eHH_cap_avg)[2] <- "V1"
save(eHH, file="./Saved tables/IND_GJperHH.Rda")
rm(eHH)
gc()

### 2. Brazil

load("./Saved tables/BRA_FD_harmonized.Rda")

xlcFreeMemory()
eHH <- t(BRA_inten_comb_all %*% BRA_FD_ICP_AllHH) / 1000 / scaler_BRA  # MJ to GJ per USD
xlcFreeMemory()
eHH <- data.table(hhid = colnames(BRA_FD_ICP_AllHH), eHH)
setkey(eHH, hhid)
xlcFreeMemory()
eHH <- merge(eHH, BRA_HH, by="hhid") 
setkey(eHH, hhid, consumption)
xlcFreeMemory()
# eHH_dec <- aggregate(.~decile, eHH, sum)

# load(eHH)
load("./Saved tables/BRA_GJperHH.Rda")
eHH_cap <- eHH
eHH_cap[, 2:(n_draw+1) := eHH_cap[,2:(n_draw+1), with=FALSE] / hh_size, with=FALSE]

# Average HH energy/cap and get SD
eHH_cap_avg <- data.table(hhid = eHH[,1, with=FALSE],
                          mean = apply(eHH[,2:(n_draw+1), with=FALSE], 1, mean) / eHH$hh_size, 
                          sd = apply(eHH[,2:(n_draw+1), with=FALSE], 1, sd) / eHH$hh_size, 
                          eHH[,(n_draw+2):dim(eHH)[2], with=FALSE])
names(eHH_cap_avg)[2] <- "V1"
save(eHH, file="./Saved tables/BRA_GJperHH.Rda")


# Sectoral 


ICP_food_idx <- 1:40
list[all_HH_f, mean_HH_f] <- GetHHSectoralEnergyPerCap(ICP_food_idx)
PlotIntensityHist.decile(all_HH_f, "V", xmax=40, .5)
PlotIntensityHist.decile(mean_HH_f, "sd", xmax=0.5, 0.001)

ICP_housing_idx <-56:84
list[all_HH_hs, mean_HH_hs] <- GetHHSectoralEnergyPerCap(ICP_housing_idx)
PlotIntensityHist.decile(all_HH_hs, "V", xmax=40, .5)
PlotIntensityHist.decile(mean_HH_hs, "sd", xmax=0.5, 0.001)

ICP_svc_idx <-85:151
list[all_HH_svc, mean_HH_svc] <- GetHHSectoralEnergyPerCap(ICP_svc_idx)
PlotIntensityHist.decile(all_HH_svc, "V", xmax=40, 0.5)
PlotIntensityHist.decile(mean_HH_svc, "sd", xmax=0.5, 0.001)

ICP_fuel_idx <-152:164
list[all_HH_fl, mean_HH_fl] <- GetHHSectoralEnergyPerCap(ICP_fuel_idx)
PlotIntensityHist.decile(all_HH_fl, "V", xmax=40, 0.5)
PlotIntensityHist.decile(mean_HH_fl, "sd", xmax=0.5, 0.001)



pdf(file = paste0(figure_path, "IIOA5.1 - IND energy per capita by decile.pdf"), width = 8, height = 8)
PlotIntensityHist.decile(eHH_cap, "V", xmax=100, 0.1)
dev.off()

pdf(file = paste0(figure_path, "IIOA5.2 - IND stdev by decile.pdf"), width = 8, height = 8)
PlotIntensityHist.decile(eHH_cap_avg, "sd", xmax=3)
dev.off()



##### Derive final values to plot #####
# I use IND_inten_comb_all instead of IND_inten_RAS_all because that can be more inclusive. 
# But the result will be identical, since IND_FD_ICP_io.yr will have zeros where IND_inten_RAS_all is zero.

# 1. Intensity ($) by decile (MJ/2007$ PPP)
totE_by_decile_IN_all <- IND_inten_comb_all %*% IND_FD_ICP_io.yr / scaler_IND   # n_draw X n_decile (11)
# int_by_decile_IN_all <- sweep(totE_by_decile_IN_all, 2, colSums(IND_FD_ICP_io.yr* EXR_IND$r / PPP_IND2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') * scaler_IND 

totE_by_decile_FR_all <- FRA_inten_comb_all %*% FRA_FD_ICP_io.yr / scaler_FRA   # n_draw X n_decile (11)
# int_by_decile_FR_all  <- sweep(totE_by_decile_FR_all, 2, colSums(FRA_FD_ICP_io.yr* EXR_EUR$r / PPP_FRA2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') *scaler_FRA

# 2. Intensity by capita  by decile
totE_per_cap_by_dec_IN <- 1000*cbind(totE_by_decile_IN_all[,1]/IND_pop_io.yr, totE_by_decile_IN_all[,2:11]/IND_pop_io.yr*10)
totE_per_cap_by_dec_FR <- 1000*cbind(totE_by_decile_FR_all[,1]/FRA_pop_io.yr, totE_by_decile_FR_all[,2:11]/FRA_pop_io.yr*10)

# 3. Food intensity ($)  by decile (MJ/2007$ PPP)
foodE_by_decile_IN <- IND_inten_comb_all[,1:40] %*% IND_FD_ICP_io.yr[1:40,] / scaler_IND   # n_draw X n_decile (11) in 1000 GJ
# food_int_by_decile_IN <- sweep(foodE_by_decile_IN, 2, colSums(IND_FD_ICP_io.yr[1:40,]* EXR_IND$r / PPP_IND2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') * scaler_IND

foodE_by_decile_FR <- FRA_inten_comb_all[,1:11] %*% FRA_FD_ICP_io.yr[1:11,] / scaler_FRA   # n_draw X n_decile (11) in 1000 GJ
# food_int_by_decile_FR <- sweep(foodE_by_decile_FR, 2, colSums(FRA_FD_ICP_io.yr[1:11,]* EXR_EUR$r / PPP_FRA2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') *scaler_FRA

#### Plotting

# 1. Decile total intensity ($)
pdf(file = paste0(figure_path, "IIOA1.1 - IND embodied energy intensity by decile.pdf"), width = 15, height = 8)
boxplot(int_by_decile_IN_all, ylab ="Average energy intensity [MJ/2007USD PPP]",range=0, axes = TRUE)
dev.off()

pdf(file = paste0(figure_path, "IIOA1.2 - FRA embodied energy intensity by decile.pdf"), width = 15, height = 8)
boxplot(int_by_decile_FR_all, ylab ="Average energy intensity [MJ/2007USD PPP]", range=0, axes = TRUE)
dev.off()

# 2. Decile total intensity ($)
pdf(file = paste0(figure_path, "IIOA2.1 - IND embodied energy intensity in food by decile.pdf"), width = 15, height = 8)
boxplot(food_int_by_decile_IN, ylab ="IND Avg food energy intensity [MJ/2007USD PPP]", range=0, axes = TRUE)
dev.off()

# boxplot(foodE_by_decile_FR[,2:11], ylab ="FRA food energy [GJ]", range=0, axes = TRUE)
# boxplot(foodE_by_decile_IN[,2:11], ylab ="IND food energy [GJ]", range=0, axes = TRUE)

pdf(file = paste0(figure_path, "IIOA2.2 - FRA embodied energy intensity in food by decile.pdf"), width = 15, height = 8)
boxplot(food_int_by_decile_FR, ylab ="FRA Avg food energy intensity [MJ/2007USD PPP]", range=0, axes = TRUE)
dev.off()

# 3. Decile total E per capita
pdf(file = paste0(figure_path, "IIOA3.1 - IND embodied energy per cap by decile.pdf"), width = 15, height = 8)
boxplot(totE_per_cap_by_dec_IN, ylab ="Embodied energy per capita [GJ/cap]",range=0, axes = TRUE)
dev.off()

pdf(file = paste0(figure_path, "IIOA3.2 - FRA embodied energy per cap by decile.pdf"), width = 15, height = 8)
boxplot(totE_per_cap_by_dec_FR, ylab ="Embodied energy per capita [GJ/cap]", range=0, axes = TRUE)
dev.off()

# Overlay
pdf(file = paste0(figure_path, "IIOA3.3 - embodied energy per cap by decile.pdf"), width = 15, height = 8)
plot(apply(totE_per_cap_by_dec_FR[,2:11], 2, mean), ylab ="Embodied energy per capita [GJ/cap]", ylim=c(0,200), 
     xlab="Decile", xaxt='n')
par(new=T)
plot(apply(totE_per_cap_by_dec_IN[,2:11], 2, mean), ylab ="Embodied energy per capita [GJ/cap]", ylim=c(0,200), 
     pch=19, xaxt='n', ann=FALSE)
abline(h=mean(totE_per_cap_by_dec_FR[,1]), col=3)
abline(h=mean(totE_per_cap_by_dec_IN[,1]), col=3)
legend(1.5, 200, c("FRA","IND"), pch=c(1,19))
axis(1, at=1:10)
par(new=F)
dev.off()

# Plot intensity ($) by sector
pdf(file = paste0(figure_path, "IIOA4.2 - FRA embodied energy intensity by COICOP sector.pdf"), width = 16, height = 9)
# FRA_inten_RAS_all[,35] <- 0
Plot_ICP_sectors(BRA_inten_RAS_all, no_expense_FRA, icp=0, 150, 
                 "Embodied energy intensity by COICOP consumption category: Brazil")
dev.off()


pdf(file = paste0(figure_path, "IIOA4.1 - IND embodied energy intensity by COICOP sector.pdf"), width = 16, height = 9)
# IND_inten_RAS_all[,65] <- 0
Plot_ICP_sectors(IND_inten_RAS_all, no_expense_IND, icp=1, 150, 
                 "Embodied energy intensity by COICOP consumption category: India")
dev.off()



##### Sanity check #####

TotEmbodEne <- sum(eHH_cap$V5 * eHH_cap$hh_size * eHH_cap$weight)  # in GJ
EneComparison <- data.frame(name=ICP_catnames, IND_int=apply(IND_inten_comb_all,2,mean), IND_exp=IND_FD_ICP_io.yr[,1], IND_GJ = 0, 
                                  BRA_int=apply(BRA_inten_comb_all,2,mean), BRA_exp=BRA_FD_ICP_io.yr[,1], BRA_GJ = 0)
EneComparison$IND_GJ <- EneComparison$IND_int * EneComparison$IND_exp *1000 # GJ
EneComparison$BRA_GJ <- EneComparison$BRA_int * EneComparison$BRA_exp *1000 # GJ
sum(EneComparison$IND_GJ) / IND_pop_io.yr
sum(EneComparison$BRA_GJ) / BRA_pop_io.yr



# For Julia
# For all the rows with ones in the Q mapping
library(pastecs)
colnames(IND_inten_comb_all) <- rownames(Q_UN_ICP_EXIO)
Impute_row <- which((rowSums(Q_UN_ICP_EXIO)!=0) & (IND_FD_ICP_io.yr[,1]==0))
IND_inten_comb_all <- IND_inten_RAS_all
IND_inten_comb_all[,Impute_row] <- IND_nonRAS_all[,Impute_row]

IND_int_summary_combined <- stat.desc(IND_inten_comb_all) 
IND_int_summary_combined <- t(IND_int_summary_combined[c(4,5,9,13),]) %>% round(digits=2) %>% 
  format(digits = 2, nsmall = 1, scientific=FALSE)
IND_int_summary_combined <- cbind(IND_int_summary_combined, 1:151 %in% Impute_row)
colnames(IND_int_summary_combined)[5] <- "NSS exp==0"

write.csv(IND_int_summary_combined, "India - Intensity summary without direct E.csv")

# Test
totE_sector <- t(IND_inten_comb_all) * IND_FD_ICP_io.yr[,1] / scaler_IND 
totE_sector <- rbind(totE_sector, colSums(totE_sector))
totE_summary <- t(stat.desc(t(totE_sector)))[,c(4,5,9,13)]
