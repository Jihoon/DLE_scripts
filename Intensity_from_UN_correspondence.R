n_draw <- 1000

# Q_all_ICP <- matrix(1, dim(bridge_ICP_EXIO_q[,-1])[1], dim(bridge_ICP_EXIO_q[,-1])[2])
# Q_all_COICOP <- matrix(1, dim(bridge_COICOP_EXIO_q[,-1])[1], dim(bridge_COICOP_EXIO_q[,-1])[2])

D_val_uncertainty <- 0
# list[result_IND_all, NC_IND_all] <- Run_rIPFP(Q_all_ICP, "IND")
# list[result_FRA_all, NC_FRA_all] <- Run_rIPFP(Q_all_COICOP, "FRA")
list[result_IND_all, NC_IND_all] <- Run_rIPFP(Q_UN_ICP_EXIO, "IND")
list[result_FRA_all, NC_FRA_all] <- Run_rIPFP(Q_UN_EXIO, "FRA")

final_alloc_list_FRA_all <- lapply(result_FRA_all, func1)
final_alloc_list_IND_all <- lapply(result_IND_all, func1)

D_val_uncertainty <- 0
IND_inten_RAS_all <- SetupSectorIntensities(final_alloc_list_IND_all, NC_IND_all, "IN")
FRA_inten_RAS_all <- SetupSectorIntensities(final_alloc_list_FRA_all, NC_FRA_all, "FR")
alloc_nonRAS <- get_bridge_COICOP_EXIO(Q_UN_ICP_EXIO, n_draw)
IND_nonRAS_all <- SetupSectorIntensities(alloc_nonRAS, NC_IND_all, "IN")
alloc_nonRAS <- get_bridge_COICOP_EXIO(Q_UN_EXIO, n_draw)
FRA_nonRAS_all <- SetupSectorIntensities(alloc_nonRAS, NC_FRA_all, "FR")

# Filling in sectors with no expenditures
no_expense_IND <- which((rowSums(Q_UN_ICP_EXIO)!=0) & (IND_FD_ICP_usd2007[,1]==0))
no_expense_IND <- no_expense_IND[!(no_expense_IND %in% grep("UNBR", ICP_catnames))]   # Remove UNBR items
no_expense_FRA <- which((rowSums(Q_UN_EXIO)!=0) & (FRA_FD_ICP_usd2007[,1]==0))

IND_inten_comb_all <- IND_inten_RAS_all
FRA_inten_comb_all <- FRA_inten_RAS_all
IND_inten_comb_all[,no_expense_IND] <- IND_nonRAS_all[,no_expense_IND]
FRA_inten_comb_all[,no_expense_FRA] <- FRA_nonRAS_all[,no_expense_FRA]

# n_draw x n_HH
IND_inten_comb_all[,65] <- 0  # Ignore other fuel now
xlcFreeMemory()
eHH <- t(IND_inten_comb_all %*% (IND_FD_ICP_AllHH * EXR_EUR$r)) / 1000 # MJ to GJ
xlcFreeMemory()
eHH <- cbind(id = as.numeric(rownames(eHH)), eHH)
xlcFreeMemory()
eHH <- merge(eHH, IND_HH, by="id") %>% arrange(consumption)
xlcFreeMemory()
# eHH_dec <- aggregate(.~decile, eHH, sum)
xlcFreeMemory()
eHH_cap <- cbind(id=eHH[,1], eHH[,2:(n_draw+1)] / eHH$hh_size, eHH[,(n_draw+2):dim(eHH)[2]])

# Average HH energy/cap and get SD
eHH_cap_avg <- cbind(eHH[,1], mean = apply(eHH[,2:(n_draw+1)], 1, mean) / eHH$hh_size, 
                     sd=apply(eHH[,2:(n_draw+1)], 1, sd) / eHH$hh_size, eHH[,(n_draw+2):dim(eHH)[2]])
names(eHH_cap_avg)[2] <- "V1"
# eHH_cap_long <- eHH_cap[1:1000,c(1:1001,1007)] %>% gather(draw, ecap, -c(id, decile)) %>% arrange(id)
# eHH_cap_long$idchar <- as.character(eHH_cap_long$id)

# Sectoral 
GetHHSectoralEnergyPerCap <- function(idx) {
  xlcFreeMemory()
  eHH_sect <- t(IND_inten_comb_all[,idx] %*% (IND_FD_ICP_AllHH[idx,] * EXR_EUR$r)) / 1000 # MJ to GJ
  xlcFreeMemory()
  eHH_sect <- cbind(id = as.numeric(rownames(eHH_sect)), eHH_sect)
  xlcFreeMemory()
  eHH_sect <- merge(eHH_sect, IND_HH, by="id") %>% arrange(consumption)
  xlcFreeMemory()
  eHH_cap_sect <- cbind(id=eHH_sect[,1], eHH_sect[,2:(n_draw+1)] / eHH_sect$hh_size, eHH_sect[,(n_draw+2):dim(eHH_sect)[2]])
  eHH_cap_avg_sect <- cbind(eHH_cap_sect[,1], mean = apply(eHH_cap_sect[,2:(n_draw+1)], 1, mean) / eHH_cap_sect$hh_size, 
                            sd=apply(eHH_cap_sect[,2:(n_draw+1)], 1, sd) / eHH_cap_sect$hh_size, 
                            eHH_cap_sect[,(n_draw+2):dim(eHH_cap_sect)[2]])
  return(list(eHH_cap_sect, eHH_cap_avg_sect))
}
ICP_food_idx <- 1:40
list[all_HH_f, mean_HH_f] <- GetHHSectoralEnergyPerCap(ICP_food_idx)
PlotIntensityHist(all_HH_f, "V", xmax=40, 0.1)
PlotIntensityHist(mean_HH_f, "sd", xmax=0.5, 0.001)

ICP_housing_idx <-56:84
list[all_HH_hs, mean_HH_hs] <- GetHHSectoralEnergyPerCap(ICP_housing_idx)
PlotIntensityHist(all_HH_hs, "V", xmax=40, 0.1)
PlotIntensityHist(mean_HH_hs, "sd", xmax=0.5, 0.001)

ICP_svc_idx <-85:151
list[all_HH_svc, mean_HH_svc] <- GetHHSectoralEnergyPerCap(ICP_svc_idx)
PlotIntensityHist(all_HH_svc, "V", xmax=40, 0.1)
PlotIntensityHist(mean_HH_svc, "sd", xmax=0.5, 0.001)





# for (i in 1:10) {
#   summary(aov(ecap~id, eHH_cap_long[eHH_cap_long$decile==paste0('decile',i),]))
# }
# summary(aov(ecap~idchar, eHH_cap_long[eHH_cap_long$decile==paste0('decile',i),]))


pdf(file = paste0(figure_path, "IIOA5.1 - IND energy per capita by decile.pdf"), width = 8, height = 8)
PlotIntensityHist(eHH_cap, "V", xmax=150, 0.1)
dev.off()
# PlotIntensityHist(eHH_cap_avg, "V", xmax=200)
pdf(file = paste0(figure_path, "IIOA5.2 - IND stdev by decile.pdf"), width = 8, height = 8)
PlotIntensityHist(eHH_cap_avg, "sd", xmax=3)
dev.off()

# All 

PlotIntensityHist <- function (intens_HH, name="V", xmax, bin_size=0.1) {
  xlcFreeMemory()
  
  opar <- par() 
  # nb <- seq(0,xmax,(xmax/nbin))
  # breaks <- seq(0, max(a), bin_size)
  par(mfrow=c(10,1), oma = c(0, 0, 0, 0), mar= c(0, 0, 0, 0))
  
  for (i in 1:10) {
    a <- as.matrix(intens_HH %>% filter(decile==paste0("decile",i)) %>% select(starts_with(name)))
    # hist(a, seq(0, max(a)+bin_size, bin_size), xlim=c(0,xmax), main=NULL)
    w0 <- intens_HH$weight[intens_HH$decile==paste0("decile",i)]
    w1 <- w0[rep(1:length(w0), each=n_draw)]
    weighted.hist(a, w1, seq(0, max(a)+bin_size, bin_size), xlim=c(0,xmax), main=NULL)
  }

  par(opar)
}

##### Derive final values to plot #####
# I use IND_inten_comb_all instead of IND_inten_RAS_all because that can be more inclusive. 
# But the result will be identical, since IND_FD_ICP_usd2007 will have zeros where IND_inten_RAS_all is zero.

# 1. Intensity ($) by decile (MJ/2007$ PPP)
totE_by_decile_IN_all <- IND_inten_comb_all %*% (IND_FD_ICP_usd2007 * EXR_EUR$r) / scaler_IND   # n_draw X n_decile (11)
int_by_decile_IN_all <- sweep(totE_by_decile_IN_all, 2, colSums(IND_FD_ICP_usd2007* EXR_IND$r / PPP_IND2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') * scaler_IND 

totE_by_decile_FR_all <- FRA_inten_comb_all %*% (FRA_FD_ICP_usd2007* EXR_EUR$r) / scaler_FRA   # n_draw X n_decile (11)
int_by_decile_FR_all  <- sweep(totE_by_decile_FR_all, 2, colSums(FRA_FD_ICP_usd2007* EXR_EUR$r / PPP_FRA2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') *scaler_FRA

# 2. Intensity by capita  by decile
totE_per_cap_by_dec_IN <- 1000*cbind(totE_by_decile_IN_all[,1]/IND_pop_2007, totE_by_decile_IN_all[,2:11]/IND_pop_2007*10)
totE_per_cap_by_dec_FR <- 1000*cbind(totE_by_decile_FR_all[,1]/FRA_pop_2007, totE_by_decile_FR_all[,2:11]/FRA_pop_2007*10)

# 3. Food intensity ($)  by decile (MJ/2007$ PPP)
foodE_by_decile_IN <- IND_inten_comb_all[,1:40] %*% (IND_FD_ICP_usd2007[1:40,] * EXR_EUR$r) / scaler_IND   # n_draw X n_decile (11) in 1000 GJ
food_int_by_decile_IN <- sweep(foodE_by_decile_IN, 2, colSums(IND_FD_ICP_usd2007[1:40,]* EXR_IND$r / PPP_IND2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') * scaler_IND

foodE_by_decile_FR <- FRA_inten_comb_all[,1:11] %*% (FRA_FD_ICP_usd2007[1:11,] * EXR_EUR$r) / scaler_FRA   # n_draw X n_decile (11) in 1000 GJ
food_int_by_decile_FR <- sweep(foodE_by_decile_FR, 2, colSums(FRA_FD_ICP_usd2007[1:11,]* EXR_EUR$r / PPP_FRA2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') *scaler_FRA

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
FRA_inten_RAS_all[,35] <- 0
Plot_ICP_sectors(FRA_inten_RAS_all, no_expense_FRA, icp=0, 30, 
                 "Embodied energy intensity by COICOP consumption category: France")
dev.off()

# Plot_ICP_sectors(FRA_inten_comb_all, no_expense_FRA, icp=0, 180, 
#                  "Embodied energy intensity by COICOP consumption category: France (Imputed)")

pdf(file = paste0(figure_path, "IIOA4.1 - IND embodied energy intensity by COICOP sector.pdf"), width = 16, height = 9)
IND_inten_RAS_all[,65] <- 0
Plot_ICP_sectors(IND_inten_RAS_all, no_expense_IND, icp=1, 150, 
                 "Embodied energy intensity by COICOP consumption category: India")
dev.off()

# Plot_ICP_sectors(IND_inten_comb_all, no_expense_IND, icp=1, 360, 
#                  "Embodied energy intensity by COICOP consumption category: India (Imputed)")




# For Julia
# For all the rows with ones in the Q mapping
library(pastecs)
colnames(IND_inten_comb_all) <- rownames(Q_UN_ICP_EXIO)
Impute_row <- which((rowSums(Q_UN_ICP_EXIO)!=0) & (IND_FD_ICP_usd2007[,1]==0))
IND_inten_comb_all <- IND_inten_RAS_all
IND_inten_comb_all[,Impute_row] <- IND_nonRAS_all[,Impute_row]

IND_int_summary_combined <- stat.desc(IND_inten_comb_all) 
IND_int_summary_combined <- t(IND_int_summary_combined[c(4,5,9,13),]) %>% round(digits=2) %>% 
  format(digits = 2, nsmall = 1, scientific=FALSE)
IND_int_summary_combined <- cbind(IND_int_summary_combined, 1:151 %in% Impute_row)
colnames(IND_int_summary_combined)[5] <- "NSS exp==0"

write.csv(IND_int_summary_combined, "India - Intensity summary without direct E.csv")

# Test
totE_sector <- (t(IND_inten_comb_all) * (IND_FD_ICP_usd2007[,1] * EXR_EUR$r) / scaler_IND )
totE_sector <- rbind(totE_sector, colSums(totE_sector))
totE_summary <- t(stat.desc(t(totE_sector)))[,c(4,5,9,13)]
