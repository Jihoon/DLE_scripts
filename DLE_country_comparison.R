# Intensity comparison of countries for DLE gap identification 
# Now in primary energy terms

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_FD_ICP_usd2007.Rda")
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_FD_ICP_usd2007.Rda")
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/ZAF_FD_ICP_usd2007.Rda")

load("./Saved tables/BRA_intensities_val_BRA.Rda")   # Reference case
load("./Saved tables/IND_intensities.Rda")
load("./Saved tables/ZAF_intensities.Rda")

load(file="./Saved tables/BRA_intensities_val_BRA.use.Rda")
load(file="./Saved tables/IND_intensities.use.Rda")
load(file="./Saved tables/ZAF_intensities.use.Rda")


################################
### Health & Education: Directly from EXIO sector
################################

idx.education.exio <- 174
idx.health.exio <- 175

# Intensity
sum(indirect_pE_int.elec.prirow[,BRA_idx_ex[idx.health.exio]])
sum(indirect_pE_int.elec.prirow[,IND_idx_ex[idx.health.exio]])
sum(indirect_pE_int.elec.prirow[,ZAF_idx_ex[idx.health.exio]])

sum(indirect_pE_int.elec.prirow[,BRA_idx_ex[idx.education.exio]])
sum(indirect_pE_int.elec.prirow[,IND_idx_ex[idx.education.exio]])
sum(indirect_pE_int.elec.prirow[,ZAF_idx_ex[idx.education.exio]])

# Total
tot.health.BRA <- sum(indirect_pE_int.elec.prirow[,BRA_idx_ex[idx.health.exio]]) * BRA_fd_exio[idx.health.exio] / 1e6
tot.health.IND <- sum(indirect_pE_int.elec.prirow[,IND_idx_ex[idx.health.exio]]) * IND_fd_exio[idx.health.exio] / 1e6
tot.health.ZAF <- sum(indirect_pE_int.elec.prirow[,ZAF_idx_ex[idx.health.exio]]) * ZAF_fd_exio[idx.health.exio] / 1e6

tot.education.BRA <- sum(indirect_pE_int.elec.prirow[,BRA_idx_ex[idx.education.exio]]) * BRA_fd_exio[idx.education.exio] / 1e6
tot.education.IND <- sum(indirect_pE_int.elec.prirow[,IND_idx_ex[idx.education.exio]]) * IND_fd_exio[idx.education.exio] / 1e6
tot.education.ZAF <- sum(indirect_pE_int.elec.prirow[,ZAF_idx_ex[idx.education.exio]]) * ZAF_fd_exio[idx.education.exio] / 1e6

# Total per cap
tot.health.BRA / BRA_pop_2007 * 1e9
tot.health.IND / IND_pop_2007 * 1e9
tot.health.ZAF / ZAF_pop_2007 * 1e9

tot.education.BRA / BRA_pop_2007 * 1e9
tot.education.IND / IND_pop_2007 * 1e9
tot.education.ZAF / ZAF_pop_2007 * 1e9


################################
### Clothing (incl. shoes)
################################

# ICP 50, 51, 54: only for purchases and no repairs (repair expenditure observations are not consistent across countries)
# I will assume just e
idx.clothing.exio <- 56:57 #55:57  # No textile
idx.clothing.icp <- c(50, 51)  # 50:55
idx.footwear.icp <- 54  # 50:55

# Average intensity (MJ/USD)
weighted.mean(colMeans(BRA_intensity.use[, idx.clothing.icp]), weight=BRA_FD_ICP_usd2007[idx.clothing.icp, 1])
weighted.mean(colMeans(IND_intensity.use[, idx.clothing.icp]), weight=IND_FD_ICP_usd2007[idx.clothing.icp, 1])
weighted.mean(colMeans(ZAF_intensity.use[, idx.clothing.icp]), weight=ZAF_FD_ICP_usd2007[idx.clothing.icp, 1])

# Total energy/year (EJ/year) - Good to be based on household consumption (ICP)
tot.clothing.BRA <- rowSums(BRA_intensity.use[, idx.clothing.icp] %*% diag(BRA_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6
tot.clothing.IND <- rowSums(IND_intensity.use[, idx.clothing.icp] %*% diag(IND_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6
tot.clothing.ZAF <- rowSums(ZAF_intensity.use[, idx.clothing.icp] %*% diag(ZAF_FD_ICP_usd2007[idx.clothing.icp, 1])) / 1e6
tot.footwear.BRA <- (BRA_intensity.use[, idx.footwear.icp] * BRA_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6
tot.footwear.IND <- (IND_intensity.use[, idx.footwear.icp] * IND_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6
tot.footwear.ZAF <- (ZAF_intensity.use[, idx.footwear.icp] * ZAF_FD_ICP_usd2007[idx.footwear.icp, 1]) / 1e6


### Total energy/year (GJ/year) 

# Approach 1. Based on TPEI (either ICP or EXIO) and original expenditure from survey 
c(min(tot.clothing.BRA), max(tot.clothing.BRA)) #/ as.numeric(BRA_pop_2007) * 1e9
c(min(tot.clothing.IND), max(tot.clothing.IND)) #/ as.numeric(IND_pop_2007) * 1e9
c(min(tot.clothing.ZAF), max(tot.clothing.ZAF)) #/ as.numeric(ZAF_pop_2007) * 1e9

# Approach 2. Based on TPEI (.use) and individual hh expenditure (GJ/cap) - Better!
# Apply both FC_ICP: Original and adjusted
# Take the min and max from the two results for the DLE comparison
load("./Saved tables/BRA_intensities_val_BRA.use.Rda") #BRA_intensity.use
load("./Saved tables/BRA_FD_ICP_HH_adj_BR.Rda") #BRA_FD_ICP_HH_adj_BR
load("./Saved tables/BRA_FD_harmonized.Rda") #BRA_FD_ICP_AllHH
list[all_HH_c_BR, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity.use) # more reliable than IND_intensity
(SummarizeGJPerCap(all_HH_c_BR) * as.numeric(BRA_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)
list[all_HH_c_BR, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'BRA', BRA_FD_ICP_AllHH, BRA_intensity.use) # more reliable than IND_intensity
(SummarizeGJPerCap(all_HH_c_BR) * as.numeric(BRA_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)

load("./Saved tables/IND_intensities.use.Rda")
load("./Saved tables/IND_FD_ICP_HH_adj.Rda")
load("./Saved tables/IND_FD_harmonized.Rda") #IND_FD_ICP_AllHH
list[all_HH_c_IN, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'IND', IND_FD_ICP_HH_adj, IND_intensity.use) # more reliable than IND_intensity
(SummarizeGJPerCap(all_HH_c_IN) * as.numeric(IND_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)
list[all_HH_c_IN, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'IND', IND_FD_ICP_AllHH, IND_intensity.use) # more reliable than IND_intensity
(SummarizeGJPerCap(all_HH_c_IN) * as.numeric(IND_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)

load("./Saved tables/ZAF_intensities.use.Rda")
load("./Saved tables/ZAF_FD_ICP_HH_adj.Rda")
load("./Saved tables/ZAF_FD_harmonized.Rda") #ZAF_FD_ICP_AllHH
list[all_HH_c_ZA, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'ZAF', ZAF_FD_ICP_HH_adj, ZAF_intensity.use) # more reliable than IND_intensity
(SummarizeGJPerCap(all_HH_c_ZA) * as.numeric(ZAF_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)
list[all_HH_c_ZA, sd_hs] <- GetHHSectoralEnergyPerCap(idx.clothing.icp,'ZAF', ZAF_FD_ICP_AllHH, ZAF_intensity.use) # more reliable than IND_intensity
(SummarizeGJPerCap(all_HH_c_ZA) * as.numeric(ZAF_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd)

rm(all_HH_c_BR, all_HH_c_IN, all_HH_c_ZA)
gc()


################################
### Food
################################

idx.food.icp <- 1:45

# Averaging ICP sectors - Based on TPEI and original expenditure from survey
weighted.mean(colMeans(BRA_intensity.use[, idx.food.icp]), weight=BRA_FD_ICP_usd2007[idx.food.icp, 1])
weighted.mean(colMeans(IND_intensity.use[, idx.food.icp]), weight=IND_FD_ICP_usd2007[idx.food.icp, 1])
weighted.mean(colMeans(ZAF_intensity.use[, idx.food.icp]), weight=ZAF_FD_ICP_usd2007[idx.food.icp, 1])

# Total energy/year (EJ/year) - Good to be based on household consumption (ICP)
tot.food.BRA <- rowSums(BRA_intensity[, idx.food.icp] %*% diag(BRA_FD_ICP_usd2007[idx.food.icp, 1])) / 1e6
tot.food.IND <- rowSums(IND_intensity[, idx.food.icp] %*% diag(IND_FD_ICP_usd2007[idx.food.icp, 1])) / 1e6
tot.food.ZAF <- rowSums(ZAF_intensity[, idx.food.icp] %*% diag(ZAF_FD_ICP_usd2007[idx.food.icp, 1])) / 1e6

# Approach 1.  Total energy/capita/year (GJ/cap/year) 
c(min(tot.food.BRA), max(tot.food.BRA)) / as.numeric(BRA_pop_2007) * 1e9
c(min(tot.food.IND), max(tot.food.IND)) / as.numeric(IND_pop_2007) * 1e9
c(min(tot.food.ZAF), max(tot.food.ZAF)) / as.numeric(ZAF_pop_2007) * 1e9

# Approach 2. Based on TPEI (either ICP) and individual hh expenditure (GJ/cap) - Better!
list[all_HH_f_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity.use) # more reliable than IND_intensity
a <- (SummarizeGJPerCap(all_HH_f_BR) * (BRA_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ] 
tot.ENE.food.BRA <- a$u
list[all_HH_f_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'BRA', BRA_FD_ICP_AllHH, BRA_intensity.use) # more reliable than IND_intensity
(SummarizeGJPerCap(all_HH_f_BR) * (BRA_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]

list[all_HH_f_IN, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'IND', IND_FD_ICP_HH_adj, IND_intensity.use) # more reliable than IND_intensity
a <- (SummarizeGJPerCap(all_HH_f_IN) * (IND_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]
tot.ENE.food.IND <- a$u
list[all_HH_f_IN, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'IND', IND_FD_ICP_AllHH, IND_intensity.use) # more reliable than IND_intensity
(SummarizeGJPerCap(all_HH_f_IN) * (IND_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]

list[all_HH_f_ZA, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'ZAF', ZAF_FD_ICP_HH_adj, ZAF_intensity.use) # more reliable than IND_intensity
a <- (SummarizeGJPerCap(all_HH_f_ZA) * (ZAF_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]
tot.ENE.food.ZAF <- a$u
list[all_HH_f_ZA, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'ZAF', ZAF_FD_ICP_AllHH, ZAF_intensity.use) # more reliable than IND_intensity
(SummarizeGJPerCap(all_HH_f_ZA) * (ZAF_pop_2007)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]

rm(all_HH_f_BR, all_HH_f_IN, all_HH_f_ZA)
gc()

################################
### Total P vs Final Elec use ##
################################

idx.elec.exio <- 128:141  # or 128:139 # 

# This intensity seems reasonable as proper primary (use) extension, instead of extraction.

# This includes ONLY primary energy carrier rows from use block (all columns)
# This makes more sense now.
sum(tot.fdE.elec[,BRA_idx_fd]) / sum(eigenMapMatMult(indirect_pE_int.elec.prirow[,BRA_idx_ex[idx.elec.exio]], diag(BRA_fd_exio[idx.elec.exio])))
sum(tot.fdE.elec[,IND_idx_fd]) / sum(eigenMapMatMult(indirect_pE_int.elec.prirow[,IND_idx_ex[idx.elec.exio]], diag(IND_fd_exio[idx.elec.exio])))
sum(tot.fdE.elec[,ZAF_idx_fd]) / sum(eigenMapMatMult(indirect_pE_int.elec.prirow[,ZAF_idx_ex[idx.elec.exio]], diag(ZAF_fd_exio[idx.elec.exio])))

# It is still hard to carve out only final use. We need Arkaitz for that.






View(data.frame(name = t(EX_catnames[idx.elec.exio]),
                BRA_int = colSums(indirect_E_int[,BRA_idx_ex[idx.elec.exio]]),
                BRA_fd = BRA_fd_exio[idx.elec.exio], 
                IND_int = colSums(indirect_E_int[,IND_idx_ex[idx.elec.exio]]),
                IND_fd = IND_fd_exio[idx.elec.exio],
                ZAF_int = colSums(indirect_E_int[,ZAF_idx_ex[idx.elec.exio]]),
                ZAF_fd = ZAF_fd_exio[idx.elec.exio]))
     
# Note for later: I may need to have something like SPA (structural path analysis) to understand more about the intensity numbers by sector.