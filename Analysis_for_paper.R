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


n_draw <- 20
D_val_uncertainty <- 0


###############################
# Derive sectoral intensities # 
###############################

# 1. Brazil #
# Two intensity sets based on two valuation matrices #

# 1.1. BRA - EXIO default

# Re-set val_mtx for further analysis
# val_mtx <- list(val_BR, val_IN, val_ZA)
# names(val_mtx) <- c('BR', 'IN', 'ZA')

list[BRA_intensity, BRA_alloc, NC_BRA_val_EXIO, BRA_FD_adj_val_EXIO] <- DeriveIntensities('BRA')
save(BRA_intensity, file="./Saved tables/BRA_intensities.Rda")
save(BRA_alloc, file="./Saved tables/BRA_alloc.Rda")
save(BRA_FD_adj_val_EXIO, file="./Saved tables/BRA_FD_adj.Rda")


# # 1.2. BRA - Brazil-specific valuation (from Guilioto)
# 
# # Re-set val_mtx for further analysis
# val_mtx <- list(val_FR, val_BR_BR, val_US, val_IN, val_ZA)
# names(val_mtx) <- c('FR', 'BR', 'US', 'IN', 'ZA')
# 
# BRA_fd_exio_pp_BR <- get_purch_price(BRA_fd_exio, "BR")
# scaler_BRA <- sum(BRA_FD_ICP_io.yr[,1]) / sum(BRA_fd_exio_pp_BR)
# init_FD_BRA <- BRA_FD_ICP_io.yr[,1] / scaler_BRA
# 
# list[BRA_intensity, BRA_alloc, NC_BRA_val_BRA, BRA_FD_adj_val_BRA] <- DeriveIntensities('BRA', 'primary')
# # list[BRA_f.intensity, BRA_f.alloc, NC_f.BRA_val_BRA, BRA_f.FD_adj_val_BRA] <- DeriveIntensities('BRA', 'final')
# list[BRA_intensity.use, BRA_alloc.use, NC_BRA.use, BRA_FD_adj_val_BRA.use] <- DeriveIntensities('BRA', 'primary', pri.intensity.mat=indirect_pE_int.elec.prirow)
# 
# save(BRA_intensity, file="./Saved tables/BRA_intensity.Rda")
# # save(BRA_f.intensity, file="./Saved tables/BRA_f.intensity.Rda")
# save(BRA_alloc, file="./Saved tables/BRA_alloc_val_BRA.Rda")
# save(BRA_FD_adj_val_BRA, file="./Saved tables/BRA_FD_adj_val_BRA.Rda")
# 
# save(BRA_intensity.use, file="./Saved tables/BRA_intensities_val_BRA.use.Rda")
# save(BRA_FD_adj_val_BRA.use, file="./Saved tables/BRA_FD_adj_val_BRA.use.Rda")

load( file="./Saved tables/BRA_intensities.Rda")
load( file="./Saved tables/BRA_alloc.Rda")
load( file="./Saved tables/BRA_FD_adj.Rda")

# view(data.frame(ICP_catnames, tpei= as.numeric(format(colMeans(BRA.tpei.use.icp), digits=2, nsmall=2)), 
#                 tfei=as.numeric(format(colMeans(BRA.tfei.icp), digits=2, nsmall=2)),
#                 ratio=colMeans(BRA.tfei.icp)/colMeans(BRA.tpei.use.icp) ) %>% 
#        mutate(ratio=as.numeric(format(ratio, digits=2, nsmall=2))))

# Temporary run without any Valuation
inten_BRA_noVal <- SetupSectorIntensities(BRA_alloc, NC_BRA_val_BRA, countrycode("BRA","iso3c", "iso2c"))
save(inten_BRA_noVal, file="./Saved tables/BRA_intensities_noVal.Rda")


# 2. India #
# Valuation mtx fixed

list[IND_intensity, IND_alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND', 'primary')  # IND_FD_adj is already scaled up to match EXIO FD
list[IND_intensity.use, IND_alloc.use, NC_IND.use, IND_FD_adj.use] <- DeriveIntensities('IND', 'primary', pri.intensity.mat=indirect_pE_int.elec.prirow)

save(IND_intensity, file="./Saved tables/IND_intensities.Rda")
save(IND_alloc, file="./Saved tables/IND_alloc.Rda")
save(IND_FD_adj, file="./Saved tables/IND_FD_adj.Rda")

save(IND_intensity.use, file="./Saved tables/IND_intensities.use.Rda")

load(file="./Saved tables/IND_intensities.Rda")
load(file="./Saved tables/IND_intensities.use.Rda")
load(file="./Saved tables/IND_alloc.Rda")
load(file="./Saved tables/IND_FD_adj.Rda")

# Final E intensity
list[IND_f.intensity, IND_f.alloc, NC_f.IND, IND_f.FD_adj] <- DeriveIntensities('IND', 'final', colSums(indir.fin.eng.int.derived))
# We don't need this below for "int.e <- indir.fin.eng.int.derived" in SetupSectorIntensities, but we do for "int.e <- indirect_fE_int"
# Adding the energy intensity of the final $ spend on energy product
IND_f.intensity[,ICP_fuel_idx] <- sweep(IND_f.intensity[,ICP_fuel_idx], 2, IND.E.direct.int, `+`) 

view(data.frame(ICP_catnames, primary.int.emb= as.numeric(format(colMeans(IND_intensity), digits=2, nsmall=2)), 
                final.int.emb=as.numeric(format(colMeans(IND_f.intensity), digits=2, nsmall=2)),
                ratio=colMeans(IND_f.intensity)/colMeans(IND_intensity) ) %>% 
       mutate(ratio=as.numeric(format(ratio, digits=2, nsmall=2))))




# 3. South Africa
# Valuation mtx fixed

list[ZAF_intensity, ZAF_alloc, NC_ZAF, ZAF_FD_adj] <- DeriveIntensities('ZAF', 'primary')  # ZAF_FD_adj is already scaled up to match EXIO FD
save(ZAF_intensity, file="./Saved tables/ZAF_intensities.Rda")
save(ZAF_alloc, file="./Saved tables/ZAF_alloc.Rda")
save(ZAF_FD_adj, file="./Saved tables/ZAF_FD_adj.Rda")
load(file="./Saved tables/ZAF_intensities.Rda")
load(file="./Saved tables/ZAF_alloc.Rda")
load(file="./Saved tables/ZAF_FD_adj.Rda")

list[ZAF_intensity.use, ZAF_alloc.use, NC_ZAF.use, ZAF_FD_adj.use] <- DeriveIntensities('ZAF', 'primary', pri.intensity.mat=indirect_pE_int.elec.prirow)  # ZAF_FD_adj is already scaled up to match EXIO FD
save(ZAF_intensity.use, file="./Saved tables/ZAF_intensities.use.Rda")


no_expense_IND <- which((rowSums(bridge_ICP_EXIO_q[,-1])!=0) & (IND_FD_ICP_io.yr[,1]==0))
no_expense_IND <- no_expense_IND[!(no_expense_IND %in% grep("UNBR", ICP_catnames))]   # Remove UNBR items
no_expense_ZAF <- which((rowSums(bridge_ICP_EXIO_q[,-1])!=0) & (ZAF_FD_ICP_io.yr[,1]==0))
no_expense_ZAF <- no_expense_ZAF[!(no_expense_ZAF %in% grep("UNBR", ICP_catnames))]   # Remove UNBR items
no_expense_BRA <- which((rowSums(bridge_ICP_EXIO_q[,-1])!=0) & (BRA_FD_ICP_io.yr[,1]==0))
no_expense_BRA <- no_expense_BRA[!(no_expense_BRA %in% grep("UNBR", ICP_catnames))]   # Remove UNBR items


# Visualize/Compare
view(data.frame(ICP_catnames, 
                IND=as.numeric(format(colMeans(IND.tfei.icp), digits=2, nsmall=2)), 
                BRA=as.numeric(format(colMeans(BRA.tfei.icp), digits=2, nsmall=2)),
                ZAF=as.numeric(format(colMeans(ZAF.tfei.icp), digits=2, nsmall=2))))

ZAF.tfei.icp

###############################
# Plot sectoral intensities # 
###############################

figure_path <- "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Writing/IO uncertainty methodology/Final production/Figures/"
y_lim1 <- 80 # TJ/usd
y_lim2 <- 400 # TJ/usd

pdf(file = paste0(figure_path, "BRA sectoral intensity - Val EXIO.pdf"), width = 21, height = 10)
load("./Saved tables/BRA_intensities_val_EXIO.Rda")
par(fig=c(0,0.8,0,1))
p1 <- PlotNonfuelIntensity(BRA_intensity, no_expense_BRA, y_lim1, "Brazil w/ EXIO valuation")
par(fig=c(0.8,1,0,0.95), new=TRUE)
p2 <- PlotFuelIntensity(BRA_intensity, no_expense_BRA, y_lim2)
dev.off()

pdf(file = paste0(figure_path, "BRA sectoral intensity - Val BRA.pdf"), width = 21, height = 10)
load("./Saved tables/BRA_intensities_val_BRA.Rda")
par(fig=c(0,0.8,0,1))
p1 <- PlotNonfuelIntensity(BRA_intensity, no_expense_BRA, y_lim1, "Brazil w/ own valuation")
par(fig=c(0.8,1,0,0.95), new=TRUE)
p2 <- PlotFuelIntensity(BRA_intensity, no_expense_BRA, y_lim2)
dev.off()

pdf(file = paste0(figure_path, "IND sectoral intensity - Val EXIO.pdf"), width = 21, height = 10)
load("./Saved tables/IND_intensities.Rda")
par(fig=c(0,0.8,0,1))
p1 <- PlotNonfuelIntensity(IND_intensity, no_expense_IND, y_lim1, "India")
par(fig=c(0.8,1,0,0.95), new=TRUE)
p2 <- PlotFuelIntensity(IND_intensity, no_expense_IND, 600)
dev.off()

pdf(file = paste0(figure_path, "ZAF sectoral intensity - Val EXIO.pdf"), width = 21, height = 10)
load("./Saved tables/ZAF_intensities.Rda")
par(fig=c(0,0.8,0,1))
p1 <- PlotNonfuelIntensity(ZAF_intensity, no_expense_ZAF, y_lim1, "South Africa")
par(fig=c(0.8,1,0,0.95), new=TRUE)
p2 <- PlotFuelIntensity(ZAF_intensity, no_expense_ZAF, 600)
dev.off()

# A better total Primary energy based on use block
pdf(file = paste0(figure_path, "BRA sectoral intensity - Val BRA - Use.pdf"), width = 21, height = 10)
par(fig=c(0,0.8,0,1))
p1 <- PlotNonfuelIntensity(BRA_intensity.use, no_expense_BRA, y_lim1, "Brazil w/ own valuation based on Use")
par(fig=c(0.8,1,0,0.95), new=TRUE)
p2 <- PlotFuelIntensity(BRA_intensity.use, no_expense_BRA, y_lim2)
dev.off()

pdf(file = paste0(figure_path, "ZAF sectoral intensity - Val EXIO - Use.pdf"), width = 21, height = 10)
par(fig=c(0,0.8,0,1))
p1 <- PlotNonfuelIntensity(ZAF_intensity.use, no_expense_ZAF, y_lim1, "South Africa based on Use")
par(fig=c(0.8,1,0,0.95), new=TRUE)
p2 <- PlotFuelIntensity(ZAF_intensity.use, no_expense_ZAF, 600)
dev.off()

view(data.frame(icp=ICP_catnames, pri.avg=colMeans(IND_intensity), final.avg=colMeans(IND_f.intensity) ))
view(data.frame(exio=EX_catnames, pri.avg=colSums(indirect_E_int[, IND_idx_ex]), final.avg=colSums(indirect_fE_int[, IND_idx_ex])))

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
save(BRA_FD_ICP_HH_adj_BR, file="./Saved tables/BRA_FD_ICP_HH_adj_BR.Rda")
save(BRA_FD_ICP_HH_adj_EX, file="./Saved tables/BRA_FD_ICP_HH_adj_EX.Rda")


# And two FD vectors for India
# init_FD_IND: Initial vector from the survey (scaled to M.USD 2007 MER). Also scaled up by scaler_IND to match EXIO FD sum
# IND_FD_adj: Adjusted by rIPFP based on EXIO default valuation mtx

# India
chng_pct_IND <- (IND_FD_adj - init_FD_IND) / init_FD_IND
chng_pct_IND[is.nan(chng_pct_IND)] <- 0

IND_FD_ICP_HH_adj <- IND_FD_ICP_AllHH * (chng_pct_IND + 1) # IND_FD_ICP_AllHH and IND_FD_ICP_HH_adj are not scaled up yet by scaler_IND
# When there is an adjustment frmo 0 to non-zero values, we need to assign the non-zero values to all HH.
# I do it proportionately to match the weighted sum.
idx_inf <- which(is.infinite(chng_pct_IND))  # Identify rows with Inf adjustments
r_HH <- colSums(IND_FD_ICP_AllHH)/sum(IND_FD_ICP_AllHH)  # ratio of hh total to (unweighted) total
IND_FD_ICP_HH_adj[idx_inf,] <- t(sapply(IND_FD_adj[idx_inf] * 1e6, # M.USD to USD
                        function(x) x * r_HH / sum(r_HH * IND_HH$weight))) * scaler_IND   
rm(r_HH)
gc()
save(IND_FD_ICP_HH_adj, file="./Saved tables/IND_FD_ICP_HH_adj.Rda")

# scaler_IND needed since IND_FD_ICP_HH_adj is not scaled to match fd_exio

IND_FD_ICP_adj <- IND_FD_ICP_io.yr * (chng_pct_IND + 1)
r_dec <- colSums(IND_FD_ICP_io.yr[,-1])/sum(IND_FD_ICP_io.yr[,1])  # ratio of decile total to (unweighted) total
IND_FD_ICP_adj[idx_inf,-1] <- t(sapply(IND_FD_adj[idx_inf], # M.USD
                                        function(x) x * r_dec)) * scaler_IND   
IND_FD_ICP_adj[idx_inf,1] <- IND_FD_adj[idx_inf]
# temp <- IND_f.intensity %*% IND_FD_ICP_adj


# And two FD vectors for South Africa
# init_FD_ZAF: Initial vector from the survey (scaled to M.USD 2007 MER). Also scaled up by scaler_ZAF to match EXIO FD sum
# ZAF_FD_adj: Adjusted by rIPFP based on EXIO default valuation mtx

# South Africa
chng_pct_ZAF <- (ZAF_FD_adj - init_FD_ZAF) / init_FD_ZAF
chng_pct_ZAF[is.nan(chng_pct_ZAF)] <- 0

ZAF_FD_ICP_HH_adj <- ZAF_FD_ICP_AllHH * (chng_pct_ZAF + 1) # ZAF_FD_ICP_AllHH and ZAF_FD_ICP_HH_adj are not scaled up yet by scaler_ZAF
# When there is an adjustment frmo 0 to non-zero values, we need to assign the non-zero values to all HH.
# I do it proportionately to match the weighted sum.
idx_inf <- which(is.infinite(chng_pct_ZAF))  # Identify rows with Inf adjustments
r_HH <- colSums(ZAF_FD_ICP_AllHH)/sum(ZAF_FD_ICP_AllHH)  # ratio of hh total to (unweighted) total
ZAF_FD_ICP_HH_adj[idx_inf,] <- t(sapply(ZAF_FD_adj[idx_inf] * 1e6, # M.USD to USD
                                        function(x) x * r_HH / sum(r_HH * ZAF_HH$weight))) * scaler_ZAF   
rm(r_HH)
gc()
save(ZAF_FD_ICP_HH_adj, file="./Saved tables/ZAF_FD_ICP_HH_adj.Rda")
# scaler_ZAF needed since ZAF_FD_ICP_HH_adj is not scaled to match fd_exio

ZAF_FD_ICP_adj <- ZAF_FD_ICP_io.yr * (chng_pct_ZAF + 1)
r_dec <- colSums(ZAF_FD_ICP_io.yr[,-1])/sum(ZAF_FD_ICP_io.yr[,1])  # ratio of decile total to (unweighted) total
ZAF_FD_ICP_adj[idx_inf,-1] <- t(sapply(ZAF_FD_adj[idx_inf], # M.USD
                                       function(x) x * r_dec)) * scaler_ZAF   
ZAF_FD_ICP_adj[idx_inf,1] <- ZAF_FD_adj[idx_inf]
# temp <- ZAF_f.intensity %*% ZAF_FD_ICP_adj






#####################################################################  
### Derive overall energy per capita for all HH for each scenario ###
#####################################################################

eHH_summary <- data.frame(dec = paste0('dec',1:10))

# Brazil

load("./Saved tables/BRA_intensities_val_EXIO.Rda")
list[eHH_BRA, eHH_sd]   <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'BRA', BRA_FD_ICP_AllHH, BRA_intensity)
a <- SummarizeGJPerCapByDecile(eHH_BRA)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist.decile(eHH_BRA, "V", xmax=200, bin_size=0.1, linedata=a)
# save(eHH_BRA, file="./Saved tables/BRA_ENEperCap_valEX_orgFD.Rda")

load("./Saved tables/BRA_intensities_val_BRA.Rda")
list[eHH_BRA, eHH_sd]   <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'BRA', BRA_FD_ICP_AllHH, BRA_intensity)
a <- SummarizeGJPerCapByDecile(eHH_BRA)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist.decile(eHH_BRA, "V", xmax=200, 0.1, linedata=a)
# save(eHH_BRA, file="./Saved tables/BRA_ENEperCap_valBR_orgFD.Rda")

load("./Saved tables/BRA_intensities_val_EXIO.Rda")
list[eHH_BRA, eHH_sd]   <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'BRA', BRA_FD_ICP_HH_adj_EX, BRA_intensity)
a <- SummarizeGJPerCapByDecile(eHH_BRA)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist.decile(eHH_BRA, "V", xmax=200, 0.1, linedata=a)
# save(eHH_BRA, file="./Saved tables/BRA_ENEperCap_valEX_adjFD.Rda")

load("./Saved tables/BRA_intensities_val_BRA.Rda")   # Reference case
list[eHH_BRA, eHH_sd] <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
a <- SummarizeGJPerCapByDecile(eHH_BRA)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist.decile(eHH_BRA, "V", xmax=200, 0.1, linedata=a)
PlotMainHist(eHH_BRA, "V", xmax=200, 0.1, eHH_summary)
# save(eHH_BRA, file="./Saved tables/BRA_ENEperCap_valBR_adjFD.Rda")

# No valuation case BRA
load("./Saved tables/BRA_intensities_noVal.Rda")
list[eHH_BRA_noVal, eHH_sd_noVal]   <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'BRA', BRA_FD_ICP_HH_adj_BR, inten_BRA_noVal)
a <- SummarizeGJPerCapByDecile(eHH_BRA_noVal)
# eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist.decile(eHH_BRA_noVal, "V", xmax=200, bin_size=0.1, linedata=a)
# save(eHH_BRA_noVal, file="./Saved tables/BRA_ENEperCap_noVal_adjFD.Rda")



# India

load("./Saved tables/IND_intensities.Rda")
list[eHH_IND, eHH_sd] <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'IND', IND_FD_ICP_AllHH, IND_intensity)
a <- SummarizeGJPerCapByDecile(eHH_IND)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist.decile(eHH_IND, "V", xmax=100, 0.1, linedata=a)
# save(eHH_IND, file="./Saved tables/IND_ENEperCap_orgFD.Rda")

# load("./Saved tables/IND_intensities.Rda")
list[eHH_IND, eHH_sd] <-  GetHHSectoralEnergyPerCap(ICP_all_idx, 'IND', IND_FD_ICP_HH_adj, IND_intensity)
a <- SummarizeGJPerCapByDecile(eHH_IND)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist.decile(eHH_IND, "V", xmax=100, 0.1, linedata=a)
# save(eHH_IND, file="./Saved tables/IND_ENEperCap_adjFD.Rda")

# No valuation case IND
load("./Saved tables/IND_intensities_noVal.Rda")
list[eHH_IND_noVal, eHH_sd_noVal] <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'IND', IND_FD_ICP_HH_adj, inten_IND_noVal)
a <- SummarizeGJPerCapByDecile(eHH_IND_noVal)
# eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist.decile(eHH_IND_noVal, "V", xmax=200, bin_size=0.1, linedata=a)
# save(eHH_IND_noVal, file="./Saved tables/IND_ENEperCap_noVal_orgFD.Rda")


# South Africa

load("./Saved tables/ZAF_intensities.Rda")
list[eHH_ZAF, eHH_sd] <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'ZAF', ZAF_FD_ICP_AllHH, ZAF_intensity)
a <- SummarizeGJPerCapByDecile(eHH_ZAF)
SummarizeGJPerCap(eHH_ZAF)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist.decile(eHH_ZAF, "V", xmax=100, 0.1, linedata=a)

list[eHH_ZAF, eHH_sd] <- GetHHSectoralEnergyPerCap(ICP_all_idx, 'ZAF', ZAF_FD_ICP_HH_adj, ZAF_intensity.use)
a <- SummarizeGJPerCapByDecile(eHH_ZAF)
SummarizeGJPerCap(eHH_ZAF)
eHH_summary <- cbind(eHH_summary, a)
PlotIntensityHist.decile(eHH_ZAF, "V", xmax=100, 0.1, linedata=a)





### India Final energy intensities# Final energy trial
list[eHH_f.IND, eHH_f.sd] <-  GetHHSectoralEnergyPerCap(ICP_all_idx, 'IND', IND_FD_ICP_HH_adj, IND_f.intensity)
a <- SummarizeGJPerCapByDecile(eHH_f.IND)
PlotIntensityHist.decile(eHH_f.IND, "V", xmax=100, 0.1, linedata=a)

list[eHH_f.IND, eHH_f.sd] <-  GetHHSectoralEnergyPerCap(ICP_all_idx, 'IND', IND_FD_ICP_AllHH, IND_f.intensity)
a <- SummarizeGJPerCapByDecile(eHH_f.IND)
PlotIntensityHist.decile(eHH_f.IND, "V", xmax=100, 0.1, linedata=a)

# Sectoral
list[eHH_f.IND, eHH_f.sd] <-  GetHHSectoralEnergyPerCap(ICP_food_idx, 'IND', IND_FD_ICP_HH_adj, IND_f.intensity)
a <- SummarizeGJPerCapByDecile(eHH_f.IND)
PlotIntensityHist.decile(eHH_f.IND, "V", xmax=100, 0.1, linedata=a)

list[eHH_f.IND, eHH_f.sd] <-  GetHHSectoralEnergyPerCap(ICP_fuel_idx, 'IND', IND_FD_ICP_HH_adj, IND_f.intensity)
a <- SummarizeGJPerCapByDecile(eHH_f.IND)
PlotIntensityHist.decile(eHH_f.IND, "V", xmax=100, 0.1, linedata=a)

list[eHH_f.IND, eHH_f.sd] <-  GetHHSectoralEnergyPerCap(ICP_hhold_idx, 'IND', IND_FD_ICP_HH_adj, IND_f.intensity)
a <- SummarizeGJPerCapByDecile(eHH_f.IND)
PlotIntensityHist.decile(eHH_f.IND, "V", xmax=100, 0.1, linedata=a)


# Main decile plot for BRA and IND

pdf(file = paste0(figure_path, "Fig1.1 Primary energy-BRA rev.pdf"), width = 8, height = 10)
load(file="./Saved tables/BRA_ENEperCap_valBR_adjFD.Rda")
PlotMainHistBRA(eHH_BRA, "V", xmax=250, 0.5, eHH_summary)
title("Primany energy per capita by decile: Brazil", outer=T)
dev.off()

pdf(file = paste0(figure_path, "Fig1.2 Primary energy-IND rev.pdf"), width = 8, height = 10)
load(file="./Saved tables/IND_ENEperCap_adjFD.Rda")
PlotMainHistIND(eHH_IND, "V", xmax=100, 0.2, eHH_summary)
title("Primany energy per capita by decile: India", outer=T)
dev.off()






# Sectoral energy per capita

load("./Saved tables/BRA_intensities_val_BRA.Rda")

sect_summary_BR <- data.frame(dec = paste0('dec',1:10))

list[all_HH_f_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
all_HH_f_BR <- all_HH_f_BR[which(colSums(BRA_FD_ICP_HH_adj_BR[ICP_food_idx,])!=0),]   # Remove HH with no food spending
save(all_HH_f_BR, file="./Saved tables/BRA_ENEperCap_food.Rda")
load("./Saved tables/BRA_ENEperCap_food.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_f_BR)
names(a) <- c("u_Food", "sd_Food")
sect_summary_BR <- cbind(sect_summary_BR, a)
PlotIntensityHist.decile(all_HH_f_BR, "V", xmax=50, .2, drawline=TRUE, linedata=a, ticksize=1)
title('Food: Brazil', line = 2.5)
rm(all_HH_f_BR)
gc()

list[all_HH_hs_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_hhold_idx,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
save(all_HH_hs_BR, file="./Saved tables/BRA_ENEperCap_hhold.Rda")
load("./Saved tables/BRA_ENEperCap_hhold.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_hs_BR)
names(a) <- c("u_Hhold", "sd_Hhold")
sect_summary_BR <- cbind(sect_summary_BR, a)
PlotIntensityHist.decile(all_HH_hs_BR, "V", xmax=50, .2, drawline=TRUE, linedata=a, ticksize=1)
# sd_hs <- sd_hs[sd_hs$sd!=0, ] 
# PlotIntensityHist.decile(sd_hs, "sd", xmax=5, .0002, drawline=FALSE, ticksize=0.2)
title('Household goods and services: Brazil', line = 2.5)
rm(all_HH_hs_BR)
gc()

list[all_HH_svc_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_svc_idx,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
save(all_HH_svc_BR, file="./Saved tables/BRA_ENEperCap_svc.Rda")
load("./Saved tables/BRA_ENEperCap_svc.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_svc_BR)
names(a) <- c("u_SVC", "sd_SVC")
sect_summary_BR <- cbind(sect_summary_BR, a)
PlotIntensityHist.decile(all_HH_svc_BR, "V", xmax=50, 0.2, drawline=TRUE, linedata=a, ticksize=1)
title('Communication, transportation, health, etc. : Brazil', line = 2.5)
rm(all_HH_svc_BR)
gc()

list[all_HH_fl_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_fuel_idx,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity)
save(all_HH_fl_BR, file="./Saved tables/BRA_ENEperCap_fuel.Rda")
load("./Saved tables/BRA_ENEperCap_fuel.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_fl_BR)
names(a) <- c("u_Fuel", "sd_Fuel")
sect_summary_BR <- cbind(sect_summary_BR, a)
PlotIntensityHist.decile(all_HH_fl_BR, "V", xmax=100, 0.5, drawline=TRUE, linedata=a, ticksize=1)
title('Fuel: Brazil', line = 2.5)
rm(all_HH_fl_BR)
gc()


load("./Saved tables/IND_intensities.Rda")
sect_summary_IN <- data.frame(dec = paste0('dec',1:10))

list[all_HH_f_IN, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'IND', IND_FD_ICP_HH_adj, IND_intensity.use) # more reliable than IND_intensity
# save(all_HH_f_IN, file="./Saved tables/IND_ENEperCap_food.Rda")
# load("./Saved tables/IND_ENEperCap_food.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_f_IN)
SummarizeGJPerCap(all_HH_f_IN)
names(a) <- c("u_Food", "sd_Food")
sect_summary_IN <- cbind(sect_summary_IN, a)
PlotIntensityHist.decile(all_HH_f_IN, "V", xmax=100, .1, drawline=FALSE, ticksize=1)
title('Food: India', line = 2.5)
rm(all_HH_f_IN)
gc()

list[all_HH_hs_IN, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_hhold_idx,'IND', IND_FD_ICP_HH_adj, IND_intensity)
save(all_HH_hs_IN, file="./Saved tables/IND_ENEperCap_hhold.Rda")
load("./Saved tables/IND_ENEperCap_hhold.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_hs_IN)
names(a) <- c("u_Hhold", "sd_Hhold")
sect_summary_IN <- cbind(sect_summary_IN, a)
PlotIntensityHist.decile(all_HH_hs_IN, "V", xmax=100, .1, drawline=FALSE, ticksize=1)
title('Household goods and services: India', line = 2.5)
rm(all_HH_hs_IN)
gc()

list[all_HH_svc_IN, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_svc_idx,'IND', IND_FD_ICP_HH_adj, IND_intensity)
save(all_HH_svc_IN, file="./Saved tables/IND_ENEperCap_svc.Rda")
load("./Saved tables/IND_ENEperCap_svc.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_svc_IN)
names(a) <- c("u_SVC", "sd_SVC")
sect_summary_IN <- cbind(sect_summary_IN, a)
PlotIntensityHist.decile(all_HH_svc_IN, "V", xmax=10, .02, drawline=FALSE, ticksize=1)
title('Communication, transportation, health, etc. : India', line = 2.5)
rm(all_HH_svc_IN)
gc()

list[all_HH_fl_IN, sd_hs] <-GetHHSectoralEnergyPerCap(ICP_fuel_idx,'IND', IND_FD_ICP_HH_adj, IND_intensity)
list[all_HH_fl_IN, sd_hs] <-GetHHSectoralEnergyPerCap(155,'IND', IND_FD_ICP_HH_adj, IND_intensity)
list[all_HH_fl_IN, sd_hs] <-GetHHSectoralEnergyPerCap(155,'IND', IND_FD_ICP_HH_adj, IND_f.intensity)
save(all_HH_fl_IN, file="./Saved tables/IND_ENEperCap_fuel.Rda")
load("./Saved tables/IND_ENEperCap_fuel.Rda")
a <- SummarizeGJPerCapByDecile(all_HH_fl_IN)
names(a) <- c("u_Fuel", "sd_Fuel")
sect_summary_IN <- cbind(sect_summary_IN, a)
PlotIntensityHist.decile(all_HH_fl_IN, "V", xmax=30, .1, drawline=TRUE, linedata=a, ticksize=1)
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
View(cbind(BRA_fd_exio, BRA_fd_exio_pp_EX, BRA_fd_exio_pp_BR, BRA_fd_exio_pp_BR/BRA_fd_exio_pp_EX, EX_catnames))
View(cbind(IND_fd_exio, IND_fd_exio_pp, EX_catnames))
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
TotEmbodEne_IND <- mean(apply(eHH_IND[,2:(dim(eHH_IND)[2]-dim(IND_HH)[2]-2), with=FALSE], 2, 
                              function(x) {sum(x * eHH_IND$hh_size *eHH_IND$weight)}))
TotEmbodEne_IND <- sum(apply(eHH_IND[,2:(dim(eHH_IND)[2]-dim(IND_HH)[2]-2), with=FALSE], 1, mean) * eHH_IND$hh_size *eHH_IND$weight)
TotEmbodEne_BRA / BRA_pop_io.yr  # Average GJ/capita for the country
TotEmbodEne_IND / IND_pop_io.yr
TotEmbodEne_IND / sum(eHH_IND$hh_size *eHH_IND$weight)

# Food energy per capita
TotFoodEne_IND <- mean(apply(all_HH_f_IN[,2:(dim(all_HH_f_IN)[2]-dim(IND_HH)[2]-2), with=FALSE], 2, 
                              function(x) {sum(x * all_HH_f_IN$hh_size *all_HH_f_IN$weight)}))
TotFoodEne_IND / IND_pop_io.yr

TotHHFD_BRA <- sum(apply(BRA_FD_ICP_AllHH, 1, function(x) {sum(x * eHH_BRA$weight)})) /1e6
TotHHFD_BRA <- sum(apply(BRA_FD_ICP_HH_adj_EX, 1, function(x) {sum(x * eHH_BRA$weight)})) /1e6
TotHHFD_IND <- sum(apply(IND_FD_ICP_AllHH, 1, function(x) {sum(x * eHH_IND$weight)})) /1e6
sum(IND_FD_ICP_io.yr[,1])
sum(BRA_FD_ICP_io.yr[,1])

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

IND.fd.ex <- matrix(final_demand[,IND_idx_fd[1]], nrow=200)   # 200x49
BRA.fd.ex <- matrix(final_demand[,BRA_idx_fd[1]], nrow=200) 
BRA.fd.ex[174,BRA_place] <- 15600  # M Euro
BRA.fd.ex <- BRA.fd.ex

# There are EXIO sectors with 0 demand. But I still want to keep intensity values for these.
IND.fd.ex[which(rowSums(IND.fd.ex)==0),IND_place] <- 1
BRA.fd.ex[which(rowSums(BRA.fd.ex)==0),BRA_place] <- 1

tot.int <- matrix(colSums(indirect_E_int), nrow=200, byrow=FALSE) # 200x49

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
exp.food.allHH <- BRA_FD_ICP_HH_adj_BR[ICP_food_idx,] / scaler_BRA 
exp.food.allHH <- cbind(BRA_HH %>% select(hhid, weight, hh_size, decile), t(exp.food.allHH), colSums(exp.food.allHH))
names(exp.food.allHH)[dim(exp.food.allHH)[2]] <- "tot.expend.food"
setkey(exp.food.allHH, tot.expend.food)

BRA.agg <- exp.food.allHH[tot.expend.food>0] %>% group_by(decile) %>% summarise(pop=sum(weight*hh_size),exp=sum(weight*tot.expend.food))
BRA.agg.dec <- exp.food.allHH[tot.expend.food>0] %>% select(V1:V45, decile, weight) %>% group_by(decile) %>% 
  summarise_each(funs(sum(.*weight)), -weight)

int.fdbev.per.cap <- rowSums(data.matrix(BRA.agg.dec[,2:46]) %*% diag(mean.int.fdbev)) / 1e3 / BRA.agg$pop

# Aggregate GJ/USD for all food = 12.1        
BRA_adj_fdbev <- 1e6 * BRA_FD_ICP_io.yr * (chng_pct_val_BRA + 1) / scaler_BRA # M.USD to USD
BRA_adj_fdbev.tot <- BRA_adj_fdbev[ICP_food_idx,1]  # = 1e6 * BRA_FD_adj_val_BRA[ICP_food_idx]
int.fdbev.per.usd <- sum(BRA_adj_fdbev.tot * mean.int.fdbev) / sum(BRA_adj_fdbev.tot)

# Apply it to all decile uniformly
BRA_adj_fdbev.dec <- BRA_adj_fdbev[ICP_food_idx,2:11]  
BRA_fdbev.ene.dec <- colSums(BRA_adj_fdbev.dec) * int.fdbev.per.usd / 1e3 / BRA.agg$pop

# Energy use per capita
BRA_pop <- BRA_HH %>% group_by(decile) %>% summarise(pop=sum(weight*hh_size))
IND_pop <- IND_HH %>% group_by(decile) %>% summarise(pop=sum(weight*hh_size))

BRA_fuel_tot <- readFuelQuantDemandfromDBbyDecile('BRA0') %>% mutate_if(is.numeric, funs(./scaler_BRA))
BRA_fuel_tot_pcap <- data.frame(BRA_fuel_tot$item, as.matrix(BRA_fuel_tot[,2:11]) %*% diag(1/BRA_pop$pop), 
                                 as.matrix(BRA_fuel_tot[,12:21]) %*% diag(1/BRA_pop$pop))
names(BRA_fuel_tot_pcap) <- names(BRA_fuel_tot)[1:length(BRA_fuel_tot_pcap)]

IND_fuel_tot <- readFuelQuantDemandfromDBbyDecile('IND1') %>% mutate_if(is.numeric, funs(./scaler_IND))
IND_fuel_tot_pcap <- data.frame(IND_fuel_tot$item, as.matrix(IND_fuel_tot[,2:11]) %*% diag(1/IND_pop$pop), 
                                as.matrix(IND_fuel_tot[,12:21]) %*% diag(1/IND_pop$pop))
names(IND_fuel_tot_pcap) <- names(IND_fuel_tot)[1:length(IND_fuel_tot_pcap)]



## Check BRA sectoral expenditure by hh (because of hh with no food expenditures)
sum_expenditure <- data.frame(BRA_FD_ICP_AllHH) %>% 
  mutate(sect_exp = ifelse(row_number() <= 37, "Food", ifelse(row_number() >= 152, "Energy", "Other"))) %>% 
  group_by(sect_exp) %>% summarise_all(funs(sum)) 
BRA_hh_no_food <- sum_expenditure[,c(TRUE,as.numeric(sum_expenditure[2,-1])==0)]
exp.food.allHH[tot.expend.food==0] %>% group_by(decile) %>% summarise(pop=sum(weight*hh_size))


