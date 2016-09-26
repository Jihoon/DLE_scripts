#######################################
### Analysis for the JIE paper ########
#######################################

# This analysis is for the results to be presented in the paper.

# Brazil 
# Two dimensions
# 1. Valuation matrix: EXIO default VS. Brazil-specific (from Guilioto)
# 2. ICP FD adjustment: Original FD from survey VS. Adjusted FD matched to the national total

# India 
# 1. ICP FD adjustment: Original FD from survey VS. Adjusted FD matched to the national total


n_draw <- 500
D_val_uncertainty <- 0


###############################
# Derive sectoral intensities # 
###############################

# 1. Brazil #
# Two intensity sets based on two valuation matrices #

# 1.1. BRA - EXIO default

val_BR_EX <- get_valuation_mtx('BR', 0)

# Re-set val_mtx for further analysis
val_mtx <- list(val_FR, val_BR_EX, val_US, val_IN)
names(val_mtx) <- c('FR', 'BR', 'US', 'IN')

# All in M.USD 2007
BRA_fd_exio_pp_EX <- get_purch_price(BRA_fd_exio, "BR")
scaler_BRA <- sum(BRA_FD_ICP_usd2007[,1]) / sum(BRA_fd_exio_pp_EX)
init_FD_BRA <- BRA_FD_ICP_usd2007[,1] / scaler_BRA

list[BRA_intensity, BRA_alloc, NC_BRA_val_EXIO, BRA_FD_adj_val_EXIO] <- DeriveIntensities('BRA')
save(BRA_intensity, file="./Saved tables/BRA_intensities_val_EXIO.Rda")
save(BRA_alloc, file="./Saved tables/BRA_alloc_val_EXIO.Rda")


# 1.2. BRA - Brazil-specific valuation (from Guilioto)

attach(BRA_val)
val_BR_BR <- construct_val_mtx(as.matrix(SupBP), as.matrix(TrdMrg), as.matrix(TrpMrg), as.matrix(TotTaxSub))
detach(BRA_val)

# Re-set val_mtx for further analysis
val_mtx <- list(val_FR, val_BR_BR, val_US, val_IN)
names(val_mtx) <- c('FR', 'BR', 'US', 'IN')

BRA_fd_exio_pp_BR <- get_purch_price(BRA_fd_exio, "BR")
scaler_BRA <- sum(BRA_FD_ICP_usd2007[,1]) / sum(BRA_fd_exio_pp_BR)
init_FD_BRA <- BRA_FD_ICP_usd2007[,1] / scaler_BRA

list[BRA_intensity, BRA_alloc, NC_BRA_val_BRA, BRA_FD_adj_val_BRA] <- DeriveIntensities('BRA')
save(BRA_intensity, file="./Saved tables/BRA_intensities_val_BRA.Rda")
save(BRA_alloc, file="./Saved tables/BRA_alloc_val_BRA.Rda")
load( file="./Saved tables/BRA_intensities_val_BRA.Rda")
load( file="./Saved tables/BRA_alloc_val_BRA.Rda")

# Temporary run without any Valuation
inten_BRA_noVal <- SetupSectorIntensities(BRA_alloc, NC_BRA_val_BRA, countrycode("BRA","iso3c", "iso2c"))
save(inten_BRA_noVal, file="./Saved tables/BRA_intensities_noVal.Rda")


# 2. India #
# Valuation mtx fixed

IND_fd_exio_pp <- get_purch_price(IND_fd_exio, "IN")
scaler_IND <- sum(IND_FD_ICP_usd2007[,1]) / sum(IND_fd_exio_pp)
init_FD_IND <- IND_FD_ICP_usd2007[,1] / scaler_IND

list[IND_intensity, IND_alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND')
save(IND_intensity, file="./Saved tables/IND_intensities.Rda")
save(IND_alloc, file="./Saved tables/IND_alloc.Rda")
load( file="./Saved tables/IND_intensities.Rda")
load( file="./Saved tables/IND_alloc.Rda")

# Temporary run without any Valuation
inten_IND_noVal <- SetupSectorIntensities(IND_alloc, NC_IND, countrycode("IND","iso3c", "iso2c"))
save(inten_IND_noVal, file="./Saved tables/IND_intensities_noVal.Rda")



# Print sectoral intensities
pdf(file = paste0(figure_path, "BRA non-fuel intensity - Val EXIO.pdf"), width = 16, height = 9)
load("./Saved tables/BRA_intensities_val_EXIO.Rda")
PlotNonfuelIntensity(BRA_intensity, no_expense_BRA, 80, "Brazil w/ EXIO valuation")
dev.off()
pdf(file = paste0(figure_path, "BRA fuel intensity - Val EXIO.pdf"), width = 4, height = 9)
PlotFuelIntensity(BRA_intensity, no_expense_BRA, 400)
dev.off()

pdf(file = paste0(figure_path, "BRA non-fuel intensity - Val BRA.pdf"), width = 16, height = 9)
load("./Saved tables/BRA_intensities_val_BRA.Rda")
PlotNonfuelIntensity(BRA_intensity, no_expense_BRA, 80, "Brazil w/ own valuation")
dev.off()
pdf(file = paste0(figure_path, "BRA fuel intensity - Val BRA.pdf"), width = 4, height = 9)
PlotFuelIntensity(BRA_intensity, no_expense_BRA, 400)
dev.off()

pdf(file = paste0(figure_path, "IND non-fuel intensity - Val EXIO.pdf"), width = 16, height = 9)
load("./Saved tables/IND_intensities.Rda")
PlotNonfuelIntensity(IND_intensity, no_expense_IND, 80, "India")
dev.off()
pdf(file = paste0(figure_path, "IND fuel intensity - Val EXIO.pdf"), width = 4, height = 9)
PlotFuelIntensity(IND_intensity, no_expense_IND, 600)
dev.off()

#####################
# ICP FD adjustment #
#####################

# We have three vectors of total hh FD for Brazil.
# init_FD_BRA: Initial vector from the survey (scaled to M.USD 2007 MER)
# BRA_FD_adj_val_BRA: Adjusted by rIPFP based on Brazilian valuation mtx
# BRA_FD_adj_val_EXIO: Adjusted by rIPFP based on EXIO default valuation mtx

# Brazil
chng_pct_val_BRA <- (BRA_FD_adj_val_BRA - init_FD_BRA) / init_FD_BRA
chng_pct_val_BRA[is.nan(chng_pct_val_BRA)] <- 0
chng_pct_val_EXIO <- (BRA_FD_adj_val_EXIO - init_FD_BRA) / init_FD_BRA
chng_pct_val_EXIO[is.nan(chng_pct_val_EXIO)] <- 0

BRA_FD_ICP_HH_adj_BR <- BRA_FD_ICP_AllHH * (chng_pct_val_BRA + 1)
BRA_FD_ICP_HH_adj_EX <- BRA_FD_ICP_AllHH * (chng_pct_val_EXIO + 1)

# And two vectors for India
# init_FD_IND: Initial vector from the survey (scaled to M.USD 2007 MER)
# IND_FD_adj: Adjusted by rIPFP based on EXIO default valuation mtx

# India
chng_pct_IND <- (IND_FD_adj - init_FD_IND) / init_FD_IND
chng_pct_IND[is.nan(chng_pct_IND)] <- 0

IND_FD_ICP_HH_adj <- IND_FD_ICP_AllHH * (chng_pct_IND + 1)
# When there is an adjustment frmo 0 to non-zero values, we need to assign the non-zero values to all HH.
# I do it proportionately to match the weighted sum.
idx_inf <- which(is.infinite(chng_pct_IND))  # Identify rows with Inf adjustments
r_HH <- colSums(IND_FD_ICP_AllHH)/sum(IND_FD_ICP_AllHH)  # ratio of hh total to (unweighted) total
IND_FD_ICP_HH_adj[idx_inf,] <- t(sapply(IND_FD_adj[idx_inf] * 1e6, # M.USD to USD
                        function(x) x * r_HH / sum(r_HH * IND_HH$weight))) * scaler_IND   
rm(r_HH)
gc()
# scaler_IND needed since IND_FD_ICP_HH_adj is not scaled to match fd_exio



  
# Derive overall energy per capita for all HH for each scenario
ICP_all_idx <- 1:164
eHH_summary <- data.frame(dec = paste0('dec',1:10))

# Brazil

load("./Saved tables/BRA_intensities_val_EXIO.Rda")
list[eHH_BRA, eHH_sd]   <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'BRA', BRA_FD_ICP_AllHH, BRA_intensity)
a <- SummarizeGJPerCapByDecile(eHH_BRA)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist(eHH_BRA, "V", xmax=200, bin_size=0.1, linedata=a)
save(eHH_BRA, file="./Saved tables/BRA_ENEperCap_valEX_orgFD.Rda")

load("./Saved tables/BRA_intensities_val_BRA.Rda")
list[eHH_BRA, eHH_sd]   <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'BRA', BRA_FD_ICP_AllHH, BRA_intensity)
a <- SummarizeGJPerCapByDecile(eHH_BRA)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist(eHH_BRA, "V", xmax=200, 0.1, linedata=a)
save(eHH_BRA, file="./Saved tables/BRA_ENEperCap_valBR_orgFD.Rda")

load("./Saved tables/BRA_intensities_val_EXIO.Rda")
list[eHH_BRA, eHH_sd]   <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'BRA', BRA_FD_ICP_HH_adj_EX, BRA_intensity)
a <- SummarizeGJPerCapByDecile(eHH_BRA)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist(eHH_BRA, "V", xmax=200, 0.1, linedata=a)
save(eHH_BRA, file="./Saved tables/BRA_ENEperCap_valEX_adjFD.Rda")

load("./Saved tables/BRA_intensities_val_BRA.Rda")   # Reference case
list[eHH_BRA, eHH_sd] <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
a <- SummarizeGJPerCapByDecile(eHH_BRA)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist(eHH_BRA, "V", xmax=200, 0.1, linedata=a)
PlotMainHist(eHH_BRA, "V", xmax=200, 0.1, eHH_summary)
save(eHH_BRA, file="./Saved tables/BRA_ENEperCap_valBR_adjFD.Rda")

# No valuation case BRA
load("./Saved tables/BRA_intensities_noVal.Rda")
list[eHH_BRA_noVal, eHH_sd_noVal]   <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'BRA', BRA_FD_ICP_HH_adj_BR, inten_BRA_noVal)
a <- SummarizeGJPerCapByDecile(eHH_BRA_noVal)
# eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist(eHH_BRA_noVal, "V", xmax=200, bin_size=0.1, linedata=a)
save(eHH_BRA_noVal, file="./Saved tables/BRA_ENEperCap_noVal_adjFD.Rda")



# India

load("./Saved tables/IND_intensities.Rda")
list[eHH_IND, eHH_sd] <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'IND', IND_FD_ICP_AllHH, IND_intensity)
a <- SummarizeGJPerCapByDecile(eHH_IND)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist(eHH_IND, "V", xmax=100, 0.1, linedata=a)
save(eHH_IND, file="./Saved tables/IND_ENEperCap_orgFD.Rda")

# load("./Saved tables/IND_intensities.Rda")
list[eHH_IND, eHH_sd] <-  GetHHSectoralEnergyPerCap(ICP_all_idx, 'IND', IND_FD_ICP_HH_adj, IND_intensity)
a <- SummarizeGJPerCapByDecile(eHH_IND)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist(eHH_IND, "V", xmax=100, 0.1, linedata=a)
save(eHH_IND, file="./Saved tables/IND_ENEperCap_adjFD.Rda")

# No valuation case BRA
load("./Saved tables/IND_intensities_noVal.Rda")
list[eHH_IND_noVal, eHH_sd_noVal] <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'IND', IND_FD_ICP_HH_adj, inten_IND_noVal)
a <- SummarizeGJPerCapByDecile(eHH_IND_noVal)
# eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist(eHH_IND_noVal, "V", xmax=200, bin_size=0.1, linedata=a)
save(eHH_IND_noVal, file="./Saved tables/IND_ENEperCap_noVal_orgFD.Rda")



# Main decile plot for BRA and IND

pdf(file = paste0(figure_path, "Fig1.1 Primary energy-BRA.pdf"), width = 8, height = 10)
save(eHH_BRA, file="./Saved tables/BRA_ENEperCap_valBR_adjFD.Rda")
PlotMainHistBRA(eHH_BRA, "V", xmax=250, 0.5, eHH_summary)
title("Primany energy per capita by decile: Brazil", outer=T)
dev.off()

pdf(file = paste0(figure_path, "Fig1.2 Primary energy-IND.pdf"), width = 8, height = 10)
save(eHH_IND, file="./Saved tables/IND_ENEperCap_adjFD.Rda")
PlotMainHistIND(eHH_IND, "V", xmax=100, 0.2, eHH_summary)
title("Primany energy per capita by decile: India", outer=T)
dev.off()







# Sectoral energy per capita

load("./Saved tables/BRA_intensities_val_BRA.Rda")

ICP_food_idx <- 1:45
ICP_hhold_idx <- c(56:84, 138:151)  # Household goods/services
ICP_svc_idx <- 85:137   # Health, Transport, Communication, Recreation
ICP_fuel_idx <-152:164

sect_summary_BR <- data.frame(dec = paste0('dec',1:10))

list[all_HH_f_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
all_HH_f_BR <- all_HH_f_BR[which(colSums(BRA_FD_ICP_HH_adj_BR[ICP_food_idx,])!=0),]   # Remove HH with no food spending
save(all_HH_f_BR, file="./Saved tables/BRA_ENEperCap_food.Rda")
load("./Saved tables/BRA_ENEperCap_food.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_f_BR)
names(a) <- c("u_Food", "sd_Food")
sect_summary_BR <- cbind(sect_summary_BR, a)
PlotIntensityHist(all_HH_f_BR, "V", xmax=50, .2, drawline=TRUE, linedata=a, ticksize=1)
title('Food: Brazil', line = 2.5)
rm(all_HH_f_BR)
gc()

list[all_HH_hs_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_hhold_idx,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
save(all_HH_hs_BR, file="./Saved tables/BRA_ENEperCap_hhold.Rda")
load("./Saved tables/BRA_ENEperCap_hhold.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_hs_BR)
names(a) <- c("u_Hhold", "sd_Hhold")
sect_summary_BR <- cbind(sect_summary_BR, a)
PlotIntensityHist(all_HH_hs_BR, "V", xmax=50, .2, drawline=TRUE, linedata=a, ticksize=1)
# sd_hs <- sd_hs[sd_hs$sd!=0, ] 
# PlotIntensityHist(sd_hs, "sd", xmax=5, .0002, drawline=FALSE, ticksize=0.2)
title('Household goods and services: Brazil', line = 2.5)
rm(all_HH_hs_BR)
gc()

list[all_HH_svc_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_svc_idx,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
save(all_HH_svc_BR, file="./Saved tables/BRA_ENEperCap_svc.Rda")
load("./Saved tables/BRA_ENEperCap_svc.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_svc_BR)
names(a) <- c("u_SVC", "sd_SVC")
sect_summary_BR <- cbind(sect_summary_BR, a)
PlotIntensityHist(all_HH_svc_BR, "V", xmax=50, 0.2, drawline=TRUE, linedata=a, ticksize=1)
title('Communication, transportation, health, etc. : Brazil', line = 2.5)
rm(all_HH_svc_BR)
gc()

list[all_HH_fl_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_fuel_idx,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
save(all_HH_fl_BR, file="./Saved tables/BRA_ENEperCap_fuel.Rda")
load("./Saved tables/BRA_ENEperCap_fuel.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_fl_BR)
names(a) <- c("u_Fuel", "sd_Fuel")
sect_summary_BR <- cbind(sect_summary_BR, a)
PlotIntensityHist(all_HH_fl_BR, "V", xmax=100, 0.5, drawline=TRUE, linedata=a, ticksize=1)
title('Fuel: Brazil', line = 2.5)
rm(all_HH_fl_BR)
gc()


load("./Saved tables/IND_intensities.Rda")
sect_summary_IN <- data.frame(dec = paste0('dec',1:10))

list[all_HH_f_IN, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'IND', IND_FD_ICP_HH_adj, IND_intensity)
save(all_HH_f_IN, file="./Saved tables/IND_ENEperCap_food.Rda")
load("./Saved tables/IND_ENEperCap_food.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_f_IN)
names(a) <- c("u_Food", "sd_Food")
sect_summary_IN <- cbind(sect_summary_IN, a)
PlotIntensityHist(all_HH_f_IN, "V", xmax=10, .02, drawline=FALSE, ticksize=1)
title('Food: India', line = 2.5)
rm(all_HH_f_IN)
gc()

list[all_HH_hs_IN, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_hhold_idx,'IND', IND_FD_ICP_HH_adj, IND_intensity)
save(all_HH_hs_IN, file="./Saved tables/IND_ENEperCap_hhold.Rda")
load("./Saved tables/IND_ENEperCap_hhold.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_hs_IN)
names(a) <- c("u_Hhold", "sd_Hhold")
sect_summary_IN <- cbind(sect_summary_IN, a)
PlotIntensityHist(all_HH_hs_IN, "V", xmax=10, .02, drawline=FALSE, ticksize=1)
title('Household goods and services: India', line = 2.5)
rm(all_HH_hs_IN)
gc()

list[all_HH_svc_IN, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_svc_idx,'IND', IND_FD_ICP_HH_adj, IND_intensity)
save(all_HH_svc_IN, file="./Saved tables/IND_ENEperCap_svc.Rda")
load("./Saved tables/IND_ENEperCap_svc.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_svc_IN)
names(a) <- c("u_SVC", "sd_SVC")
sect_summary_IN <- cbind(sect_summary_IN, a)
PlotIntensityHist(all_HH_svc_IN, "V", xmax=10, .02, drawline=FALSE, ticksize=1)
title('Communication, transportation, health, etc. : India', line = 2.5)
rm(all_HH_svc_IN)
gc()

list[all_HH_fl_IN, sd_hs] <-GetHHSectoralEnergyPerCap(ICP_fuel_idx,'IND', IND_FD_ICP_HH_adj, IND_intensity)
save(all_HH_fl_IN, file="./Saved tables/IND_ENEperCap_fuel.Rda")
load("./Saved tables/IND_ENEperCap_fuel.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_fl_IN)
names(a) <- c("u_Fuel", "sd_Fuel")
sect_summary_IN <- cbind(sect_summary_IN, a)
PlotIntensityHist(all_HH_fl_IN, "V", xmax=10, .02, drawline=FALSE, ticksize=1)
title('Fuel: India', line = 2.5)
rm(all_HH_fl_IN)
gc()



# Consumption distribution by decile
pdf(file = paste0(figure_path, "Annual household consumption total - India.pdf"), width = 7, height = 9)
PlotIncomeByCountry(IND_HH, 30, .02, 2)
title('India in thousand USD (2010 PPP)', line = 2.5)
dev.off()

pdf(file = paste0(figure_path, "Annual household consumption total - Brazil.pdf"), width = 7, height = 9)
PlotIncomeByCountry(BRA_HH, 100, .05, 5)
title('Brazil in thousand USD (2010 PPP)', line = 2.5)
dev.off()

BRA_HH %>% group_by(decile) %>% summarise(quantile(expenditure, probs=0.05),quantile(expenditure, probs=0.95))



# View/compare FD vectors

IND_FD_compare <- data.frame(ICP_catnames, init=as.integer(init_FD_IND), IN_adj=as.integer(IND_FD_adj),
                             IN_pct=chng_pct_IND*100)
BRA_FD_compare <- data.frame(ICP_catnames, init=as.integer(init_FD_BRA), 
                             BR_adj=as.integer(BRA_FD_adj_val_BRA), BR_pct=chng_pct_val_BRA*100, 
                             EX_adj=as.integer(BRA_FD_adj_val_EXIO), EX_pct=chng_pct_val_EXIO*100)
View(IND_FD_compare)
View(BRA_FD_compare)
View(cbind(BRA_fd_exio, BRA_fd_exio_pp_EX, BRA_fd_exio_pp_BR, BRA_fd_exio_pp_BR/BRA_fd_exio_pp_EX, t(EX_catnames)))
View(cbind(IND_fd_exio, IND_fd_exio_pp, t(EX_catnames)))
View(eHH_summary)

sect_idx <- grep("household fuel", ICP_catnames, ignore.case = TRUE)
map_idx <- which(bridge_ICP_EXIO_q[sect_idx,-1]==1)
a <- do.call("rbind", lapply(final_alloc_list_BRA_all, '[', sect_idx,))
b <- do.call("rbind", lapply(alloc_nonRAS, '[', sect_idx,))
head(a)
head(b)



##### Sanity check #####
# National total 

TotEmbodEne_BRA <- mean(apply(eHH_BRA[,2:(dim(eHH_BRA)[2]-dim(BRA_HH)[2]), with=FALSE], 2, # in GJ
                              function(x) {sum(x * eHH_BRA$hh_size *eHH_BRA$weight)}))
TotEmbodEne_IND <- mean(apply(eHH_IND[,2:(dim(eHH_IND)[2]-dim(IND_HH)[2]), with=FALSE], 2, 
                              function(x) {sum(x * eHH_IND$hh_size *eHH_IND$weight)}))
TotEmbodEne_BRA / BRA_pop_2007  # Average GJ/capita for the country
TotEmbodEne_IND / IND_pop_2007

TotHHFD_BRA <- sum(apply(BRA_FD_ICP_AllHH, 1, function(x) {sum(x * eHH_BRA$weight)})) /1e6
TotHHFD_BRA <- sum(apply(BRA_FD_ICP_HH_adj_EX, 1, function(x) {sum(x * eHH_BRA$weight)})) /1e6
TotHHFD_IND <- sum(apply(IND_FD_ICP_AllHH, 1, function(x) {sum(x * eHH_IND$weight)})) /1e6
sum(IND_FD_ICP_usd2007[,1])
sum(BRA_FD_ICP_usd2007[,1])

# We see that Brazil GJ/capita become very high..
# Let's try to find out which sector it is..

for (i in 1:164) {
  list[GJPerCapBySector, sd_hs] <- GetHHSectoralEnergyPerCap(i,'BRA', BRA_FD_ICP_HH_adj_EX, BRA_intensity)
  print(paste("sect",i,min(GJPerCapBySector$V2),
              mean(GJPerCapBySector$V2),
              max(GJPerCapBySector$V2)))
}
for (i in 1:164) {
  list[GJPerCapBySector, sd_hs] <- GetHHSectoralEnergyPerCap(i,'BRA', BRA_FD_ICP_AllHH, BRA_intensity)
  print(paste("sect",i,min(GJPerCapBySector$V2),
              mean(GJPerCapBySector$V2),
              max(GJPerCapBySector$V2)))
}


### Look into ICP sector allocation ###

a <- IndirecIntensitiesByICPSect(155, "BR")
a <- IndirecIntensitiesByICPSect(155, "IN")
a <- IndirecIntensitiesByICPSect(60, "BR")  # water supply
a <- IndirecIntensitiesByICPSect(60, "IN")  # water supply
a <- IndirecIntensitiesByICPSect(79, "BR")  
a <- IndirecIntensitiesByICPSect(79, "IN")  
a <- IndirecIntensitiesByICPSect(103, "BR")  # Lubricant
a <- IndirecIntensitiesByICPSect(103, "IN")  # Lubricant
a <- IndirecIntensitiesByICPSect(157, "BR")  
a <- IndirecIntensitiesByICPSect(157, "IN")  
a <- IndirecIntensitiesByICPSect(158, "BR")
a <- IndirecIntensitiesByICPSect(158, "IN")
a <- IndirecIntensitiesByICPSect(163, "BR")  
a <- IndirecIntensitiesByICPSect(163, "IN")  
a <- IndirecIntensitiesByICPSect(76, "BR")
a <- IndirecIntensitiesByICPSect(76, "IN")


### Test MRIO aggregate sectors

# Try EXIO-EORA
eora.exio <- read_excel("../Bridging/EORA26-EXIO200.xlsx", sheet=2, skip=0, col_names=TRUE) %>% select(2:3)
eora.name <- read_excel("../Bridging/EORA26-EXIO200.xlsx", sheet=1, skip=1, col_names=FALSE) %>% select(2)
map.eora.exio <- matrix(0,nrow=26, ncol = 200)
map.eora.exio[cbind(eora.exio$EORA26, eora.exio$EXIO200)] <- 1  # 26x200

IND.fd.ex <- matrix(final_demand[,IND_idx_fd[1]], nrow=200)   # 200x48
BRA.fd.ex <- matrix(final_demand[,BRA_idx_fd[1]], nrow=200) 
BRA.fd.ex[174,BRA_place] <- 15600  # M Euro
BRA.fd.ex <- BRA.fd.ex

# There are EXIO sectors with 0 demand. But I still want to keep intensity values for these.
IND.fd.ex[which(rowSums(IND.fd.ex)==0),IND_place] <- 1
BRA.fd.ex[which(rowSums(BRA.fd.ex)==0),BRA_place] <- 1

tot.int <- matrix(colSums(indirect_E_int), nrow=200, byrow=FALSE) # 200x48

IND.tot <- rowSums(map.eora.exio %*% (tot.int*IND.fd.ex))
BRA.tot <- rowSums(map.eora.exio %*% (tot.int*BRA.fd.ex))
IND.totexp <- rowSums(map.eora.exio %*% IND.fd.ex)
BRA.totexp <- rowSums(map.eora.exio %*% BRA.fd.ex)
  
IND.int.eora <- IND.tot / IND.totexp * EXR_EUR$r  # MJ/USD
BRA.int.eora <- BRA.tot / BRA.totexp * EXR_EUR$r 

options("scipen" = 10)
View(cbind(eora.name,IND.int.eora,BRA.int.eora))

# Food only
load("./Saved tables/BRA_intensities_val_BRA.Rda")
mean.int.fdbev <- apply(BRA_intensity[,ICP_food_idx], 2, mean)
exp.food.allHH <- BRA_FD_ICP_HH_adj_BR[1:45,] / scaler_BRA 
exp.food.allHH <- cbind(BRA_HH[,c(1,2,5,8), with=FALSE], t(exp.food.allHH), colSums(exp.food.allHH))
names(exp.food.allHH)[dim(exp.food.allHH)[2]] <- "tot.expend.food"
setkey(exp.food.allHH, tot.expend.food)

BRA.agg <- exp.food.allHH[tot.expend.food>0] %>% group_by(decile) %>% summarise(pop=sum(weight*hh_size),exp=sum(weight*tot.expend.food))
BRA.agg.dec <- exp.food.allHH[tot.expend.food>0] %>% select(V1:V45, decile, weight) %>% group_by(decile) %>% 
  summarise_each(funs(sum(.*weight)), -weight)

int.fdbev.per.cap <- rowSums(data.matrix(BRA.agg.dec[,2:46]) %*% diag(mean.int.fdbev)) / 1e3 / BRA.agg$pop

# Aggregate GJ/USD for all food = 12.1        
BRA_adj_fdbev <- 1e6 * BRA_FD_ICP_usd2007 * (chng_pct_val_BRA + 1) / scaler_BRA # M.USD to USD
BRA_adj_fdbev.tot <- BRA_adj_fdbev[ICP_food_idx,1]  # = 1e6 * BRA_FD_adj_val_BRA[ICP_food_idx]
int.fdbev.per.usd <- sum(BRA_adj_fdbev.tot * mean.int.fdbev) / sum(BRA_adj_fdbev.tot)

# Apply it to all decile uniformly
BRA_adj_fdbev.dec <- BRA_adj_fdbev[ICP_food_idx,2:11]  
BRA_fdbev.ene.dec <- colSums(BRA_adj_fdbev.dec) * int.fdbev.per.usd / 1e3 / BRA.agg$pop

# int.fdbev.by.dec <- BRA.agg$exp * mean.int.fdbev / 1e3 / BRA.agg$pop

# int.fdbev.per.usd <- colSums(BRA_adj_fdbev * mean.int.fdbev) / colSums(BRA_adj_fdbev)

# agg.int.per.cap <- rowSums(data.matrix(BRA.agg.dec[,2:46])) * int.fdbev.per.usd / 1e3 / BRA.agg$pop

#################
### Functions ###
#################

IndirecIntensitiesByICPSect <- function(sect_idx, cty) {

  # sect_idx <- grep(ICP_sect_name, ICP_catnames, ignore.case = TRUE)
  print(ICP_catnames[sect_idx])
  map_idx <- which(bridge_ICP_EXIO_q[sect_idx,-1]==1)
  cty_alloc <- eval(parse(text=paste0(countrycode(cty,"iso2c", "iso3c"), "_alloc")))
  sect_alloc <- do.call("rbind", lapply(cty_alloc, '[', sect_idx,))
  
  cty_place <- which(exio_ctys==cty)
  # cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  # cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  
  all_bp_idx <- c(map_idx, trd_idx, trp_idx)
  own_ex_idx <- (cty_place-1)*200 + all_bp_idx
  alloc <- apply(sect_alloc[,all_bp_idx], 2, mean)
  
  indirec.int <- colSums(indirect_E_int[,own_ex_idx]) * EXR_EUR$r  # MJ/USD2007
  direc.int <- colSums(energy_int[,own_ex_idx]) * EXR_EUR$r  # MJ/USD2007
  
  comp <- rbind(as.integer(all_bp_idx), indirec.int, direc.int, alloc)
  
  print(map_idx)
  print(comp)  # MJ/Eur
  
  return(comp)
}

PlotIncomeByCountry <- function (f_HH, xmax, bin_size=0.1, tick_size = 1) {  # bin/tick size in kilo USD
  xlcFreeMemory()
  
  opar <- par() 
  
  par(mfrow=c(10,1), oma = c(0, 0, 0, 0), mar= c(2, 0, 0, 0))
  
  for (i in 1:10) {
    a <- as.matrix(f_HH %>% filter(decile==paste0("decile",i)) %>% select(consumption)) / 1000 # 1000 USD/yr
    w0 <- f_HH$weight[f_HH$decile==paste0("decile",i)]
    w1 <- w0[rep(1:length(w0), each=dim(a)[2])]   # dim(a)[2] instead of n_draw because of non-converge runs
    weighted.hist(a, w1, seq(0, max(a)+bin_size, bin_size), xlim=c(0,xmax), main=NULL, xaxis = FALSE)
    axis(side = 1, at = seq(0, xmax, tick_size), cex.axis=1.5)
  }
  
  par(opar)
}



PlotMainHistBRA <- function (intens_HH, name="V", xmax, bin_size=0.1, linedata, ticksize=10) {
  xlcFreeMemory()
  
  # opar <- par() 
  layout(matrix(1:11, ncol=1))
  
  par(oma = c(0, 0, 2, 0), mar= c(2, 0, 0, 0))
  
  for (i in 1:10) {
    a <- as.matrix(intens_HH %>% filter(decile==paste0("decile",i)) %>% select(starts_with(name)))
    w0 <- intens_HH$weight[intens_HH$decile==paste0("decile",i)]
    w1 <- w0[rep(1:length(w0), each=dim(a)[2])]   # dim(a)[2] instead of n_draw because of non-converge runs
    # weighted.hist(a, w1, seq(0, max(a)+bin_size, bin_size), xlim=c(0,xmax), freq=FALSE, main=NULL, xaxis = FALSE)
    d<- density(a, weights = w1); d$y <- d$y/sum(d$y) 
    plot(d, axes=FALSE, xlim=c(1,xmax), main=' ')
    axis(side = 1, at = seq(0,xmax,ticksize))
    axis(side = 2)
    
    abline(v=(seq(0,250,10)), col="lightgray", lty="dotted")
    
    m1 <- linedata[i,8]; m2 <- linedata[i,6]; m3 <- linedata[i,4];m4 <- linedata[i,2];
    sd1 <- linedata[i,9]; sd2 <- linedata[i,7]; sd3 <- linedata[i,5]; sd4 <- linedata[i,3];
    
    # Uncertainty bands
    polygon(x=c(m1-2*sd1, m1+2*sd1, m1+2*sd1, m1-2*sd1), y=c(0,0,1,1), col=rgb(1,0,0,0.3), border=NA)
    polygon(x=c(m2-2*sd2, m2+2*sd2, m2+2*sd2, m2-2*sd2), y=c(0,0,1,1), col=rgb(0,0,1,0.3), border=NA)
    polygon(x=c(m3-2*sd3, m3+2*sd3, m3+2*sd3, m3-2*sd3), y=c(0,0,1,1), col=rgb(0.3,0.8,0.3,0.3), border=NA)
    polygon(x=c(m4-2*sd4, m4+2*sd4, m4+2*sd4, m4-2*sd4), y=c(0,0,1,1), col=rgb(1,0,1,0.3), border=NA)
    
    # Mean lines
    abline(v=m1,lwd=1, col="red")    
    abline(v=m2,lwd=1, col="blue")    
    abline(v=m3,lwd=1, col=rgb(0.3,0.8,0.3,1))    
    abline(v=m4,lwd=1, col="purple")    
  }
  
  plot.new()
  legend(x="center", ncol=4, c("ownVal-adjFD", "defVal-adjFD", "ownVal-orgFD", "defVal-orgFD"), 
         lty = 1, lwd=2, col=c("red", "blue", rgb(0.3,0.8,0.3,1), "purple"), 
         y.intersp=0.4, cex = 1, bty = "n", 
         pt.bg = c(rgb(1,0,0,0.3),rgb(0,0,1,0.3),rgb(0.3,0.8,0.3,0.3),rgb(1,0,1,0.3)))
  
  # par(opar)
}


PlotMainHistIND <- function (intens_HH, name="V", xmax, bin_size=0.1, linedata, ticksize=10) {
  xlcFreeMemory()
  
  layout(matrix(1:11, ncol=1))
  par(oma = c(0, 0, 2, 0), mar= c(2, 0, 0, 0))
  
  for (i in 1:10) {
    a <- as.matrix(intens_HH %>% filter(decile==paste0("decile",i)) %>% select(starts_with(name)))
    w0 <- intens_HH$weight[intens_HH$decile==paste0("decile",i)]
    w1 <- w0[rep(1:length(w0), each=dim(a)[2])]   # dim(a)[2] instead of n_draw because of non-converge runs
    # weighted.hist(a, w1, seq(0, max(a)+bin_size, bin_size), xlim=c(0,xmax), freq=FALSE, main=NULL, xaxis = FALSE)
    d<- density(a, weights = w1); d$y <- d$y/sum(d$y) 
    plot(d, axes=FALSE, xlim=c(1,xmax), main=' ')
    axis(side = 1, at = seq(0,xmax,ticksize))
    axis(side = 2)
    
    abline(v=(seq(0,100,10)), col="lightgray", lty="dotted")
    
    m1 <- linedata[i,12]; m2 <- linedata[i,10];
    sd1 <- linedata[i,13]; sd2 <- linedata[i,11]; 
    
    # Uncertainty bands
    polygon(x=c(m1-2*sd1, m1+2*sd1, m1+2*sd1, m1-2*sd1), y=c(0,0,1,1), col=rgb(1,0,0,0.3), border=NA)
    polygon(x=c(m2-2*sd2, m2+2*sd2, m2+2*sd2, m2-2*sd2), y=c(0,0,1,1), col=rgb(0,0,1,0.3), border=NA)
    
    # Mean lines
    abline(v=m1,lwd=1, col="red")    
    abline(v=m2,lwd=1, col="blue")    
  }
  plot.new()
  legend(x="center", ncol=2, c("defVal-adjFD", "defVal-orgFD"), 
         lty = 1, lwd=2, col=c("red", "blue"), 
         y.intersp=0.4, cex = 1, bty = "n", 
         pt.bg = c(rgb(1,0,0,0.3),rgb(0,0,1,0.3)))
}

# Get by-decile summary (mean,sd) of any eHH
SummarizeGJPerCapByDecile <- function(eHH) {
  nColTot <- dim(eHH)[2]
  nColAdd <- dim(BRA_HH)[2]-1   # we do this because # of drawn columns are not equal to n_draw because of no convergences in rIPFP
  
  eHH_sum <- data.table(hhid = eHH[,1, with=FALSE],
                        avg = apply(eHH[,2:(nColTot-nColAdd), with=FALSE], 1, mean), # , with=FALSE all removed (data.table issue)
                        sd = apply(eHH[,2:(nColTot-nColAdd), with=FALSE], 1, sd),
                        eHH[,(nColTot-nColAdd+1):nColTot, with=FALSE])
  
  eHH_sum <- eHH_sum %>% group_by(decile) %>% summarise(u=weighted.mean(avg, weight), sd=weighted.mean(sd, weight))
  return(eHH_sum[,-1])
}


PlotNonfuelIntensity <- function(intensity_mtx, noexp, ymax, titlename) {
  
  divider <- c(2, 8, 14, 17, 22, 25, 28, 32, 37, 
               40, 47, 55, 65, 84, 95, 112, 116, 134, 135, 137)
  idx_section_name <- c(divider)+1
  section_name <- icp_ntnu$ICP_Heading[idx_section_name]
  section_name <- gsub("UNBR ", "", section_name)
  section_name[19] <- "Restaurants and hotels"
  
  nonFuelIntensity <- intensity_mtx[,1:151]
  
  n_sector_nonfuel <- 151
  noexp_nonfuel <- noexp[noexp<=151]
  
  # Non-fuel
  boxplot(nonFuelIntensity, range=0, ylim=c(0, ymax), axes = FALSE, add=FALSE)
  col_div <- c(par("usr")[1], divider+0.5, par("usr")[2])
  
  # Paint alternating colors
  for(i in 1:(length(col_div)-1)) { 
    color_bgn <- c("gray60", "gray15")[i %% 2]
    rect(col_div[i], par("usr")[3], col_div[i+1], par("usr")[4],col = color_bgn, border=FALSE)  
  }
  boxplot(nonFuelIntensity, ylab ="Primary energy intensity [MJ/2007USD]", 
          axes = FALSE, ylim=c(0, ymax), add=TRUE, cex.lab=1.3, range=0)
  # axis(side = 1, at = seq(1,n_sector_icp,10))
  title(xlab ="Consumption items", line=1, cex.lab=1.3) 
  axis(side = 2, at = seq(0,ymax,20), cex.axis=1.1)
  
  idx_section <- c(divider)+1
  
  text(idx_section-1, y=40, section_name, pos=4, offset=0.8, cex = 1, srt = 90)
  text(1:n_sector_nonfuel+0.5, y=apply(nonFuelIntensity, 2, max), 1:n_sector_nonfuel, pos=3, offset=1, cex = 0.7, srt = 90)
  text(noexp_nonfuel+0.9, y=apply(nonFuelIntensity[,noexp_nonfuel], 2, max), '+', pos=3, offset=2, cex = 1.2, srt = 90)
  title(titlename)
}
  
PlotFuelIntensity <- function(intensity_mtx, noexp, ymax) {
  # Fuel
  opar <- par()
  
  par(mar=c(10,3,1,2))
  FuelIntensity <- intensity_mtx[,152:164]
  noexp_fuel <- noexp[noexp>151]-151
  
  FuelLabel <- DLE_fuelnames_std$item
  FuelLabel[2] <- "Charcoal/coal/brisquette"
  FuelLabel[6] <- "Fuelwood"
  FuelLabel[8] <- "Fuel oil"
  
  boxplot(FuelIntensity, axes = FALSE, ylim=c(0, ymax), add=FALSE, cex.lab=1.3, range=0)
  
  axis(side = 2, at = seq(0,ymax,50), cex.axis=1.1)
  text(noexp_fuel, y=apply(FuelIntensity[,noexp_fuel], 2, max), '+', pos=3, offset=1, cex = 1.2, srt = 90)
  axis(side = 1, at = 1:dim(DLE_fuelnames_std)[1], labels=FuelLabel, las=2)
  
  par(opar)
}
