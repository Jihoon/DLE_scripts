# Intensity comparison of countries for DLE gap identification 
# Now in primary energy terms

# Survey expenditure
load(file="./Saved tables/IND_FD_ICP_io.yr.Rda")
load(file="./Saved tables/BRA_FD_ICP_io.yr.Rda")
load(file="./Saved tables/ZAF_FD_ICP_io.yr.Rda")

# ICP TFEIs
load(file="./Saved tables/BRA.tfei.icp.Rda")
load(file="./Saved tables/IND.tfei.icp.Rda")
load(file="./Saved tables/ZAF.tfei.icp.Rda")

# Assume tfei.exio is loaded.

################################
### Health & Education: Directly from EXIO sector
################################

idx.education.exio <- 174
idx.health.exio <- 175

# Intensity (MJ/USD 2007 MER)
sum(tfei.exio[,BRA_idx_ex[idx.health.exio]])
sum(tfei.exio[,IND_idx_ex[idx.health.exio]])
sum(tfei.exio[,ZAF_idx_ex[idx.health.exio]])

sum(tfei.exio[,BRA_idx_ex[idx.education.exio]])
sum(tfei.exio[,IND_idx_ex[idx.education.exio]])
sum(tfei.exio[,ZAF_idx_ex[idx.education.exio]])


# Total
tot.health.BRA <- sum(tfei.exio[,BRA_idx_ex[idx.health.exio]]) * BRA_fd_exio[idx.health.exio] / 1e6 #[EJ]
tot.health.IND <- sum(tfei.exio[,IND_idx_ex[idx.health.exio]]) * IND_fd_exio[idx.health.exio] / 1e6
tot.health.ZAF <- sum(tfei.exio[,ZAF_idx_ex[idx.health.exio]]) * ZAF_fd_exio[idx.health.exio] / 1e6
c(tot.health.BRA, tot.health.IND, tot.health.ZAF)

tot.education.BRA <- sum(tfei.exio[,BRA_idx_ex[idx.education.exio]]) * BRA_fd_exio[idx.education.exio] / 1e6
tot.education.IND <- sum(tfei.exio[,IND_idx_ex[idx.education.exio]]) * IND_fd_exio[idx.education.exio] / 1e6
tot.education.ZAF <- sum(tfei.exio[,ZAF_idx_ex[idx.education.exio]]) * ZAF_fd_exio[idx.education.exio] / 1e6
c(tot.education.BRA, tot.education.IND, tot.education.ZAF)

# Total per cap
tot.health.BRA / BRA_pop_io.yr * 1e9
tot.health.IND / IND_pop_io.yr * 1e9
tot.health.ZAF / ZAF_pop_io.yr * 1e9

tot.education.BRA / BRA_pop_io.yr * 1e9
tot.education.IND / IND_pop_io.yr * 1e9
tot.education.ZAF / ZAF_pop_io.yr * 1e9


################################
### Clothing (incl. shoes)
################################

# Moved to "DLE_integration_analysis_clothing.R"


################################
### Food
################################

          # We will use values from optimization (for India) or estimates from Hugo (for ZAF/BRA) for DLE food integration.

          # idx.food.icp <- 1:45
          # 
          # # Averaging ICP sectors - Based on TPEI and original expenditure from survey
          # weighted.mean(colMeans(BRA_intensity.use[, idx.food.icp]), weight=BRA_FD_ICP_io.yr[idx.food.icp, 1])
          # weighted.mean(colMeans(IND_intensity.use[, idx.food.icp]), weight=IND_FD_ICP_io.yr[idx.food.icp, 1])
          # weighted.mean(colMeans(ZAF_intensity.use[, idx.food.icp]), weight=ZAF_FD_ICP_io.yr[idx.food.icp, 1])
          # 
          # # Total energy/year (EJ/year) - Good to be based on household consumption (ICP)
          # tot.food.BRA <- rowSums(BRA_intensity[, idx.food.icp] %*% diag(BRA_FD_ICP_io.yr[idx.food.icp, 1])) / 1e6
          # tot.food.IND <- rowSums(IND_intensity[, idx.food.icp] %*% diag(IND_FD_ICP_io.yr[idx.food.icp, 1])) / 1e6
          # tot.food.ZAF <- rowSums(ZAF_intensity[, idx.food.icp] %*% diag(ZAF_FD_ICP_io.yr[idx.food.icp, 1])) / 1e6
          # 
          # # Approach 1.  Total energy/capita/year (GJ/cap/year) 
          # c(min(tot.food.BRA), max(tot.food.BRA)) / as.numeric(BRA_pop_io.yr) * 1e9
          # c(min(tot.food.IND), max(tot.food.IND)) / as.numeric(IND_pop_io.yr) * 1e9
          # c(min(tot.food.ZAF), max(tot.food.ZAF)) / as.numeric(ZAF_pop_io.yr) * 1e9
          # 
          # # Approach 2. Based on TPEI (either ICP) and individual hh expenditure (GJ/cap) - Better!
          # list[all_HH_f_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'BRA', BRA_FD_ICP_HH_adj_BR, BRA_intensity.use) # more reliable than IND_intensity
          # a <- (SummarizeGJPerCap(all_HH_f_BR) * (BRA_pop_io.yr)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ] 
          # tot.ENE.food.BRA <- a$u
          # list[all_HH_f_BR, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'BRA', BRA_FD_ICP_AllHH, BRA_intensity.use) # more reliable than IND_intensity
          # (SummarizeGJPerCap(all_HH_f_BR) * (BRA_pop_io.yr)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]
          # 
          # list[all_HH_f_IN, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'IND', IND_FD_ICP_HH_adj, IND_intensity.use) # more reliable than IND_intensity
          # a <- (SummarizeGJPerCap(all_HH_f_IN) * (IND_pop_io.yr)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]
          # tot.ENE.food.IND <- a$u
          # list[all_HH_f_IN, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'IND', IND_FD_ICP_AllHH, IND_intensity.use) # more reliable than IND_intensity
          # (SummarizeGJPerCap(all_HH_f_IN) * (IND_pop_io.yr)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]
          # 
          # list[all_HH_f_ZA, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'ZAF', ZAF_FD_ICP_HH_adj, ZAF_intensity.use) # more reliable than IND_intensity
          # a <- (SummarizeGJPerCap(all_HH_f_ZA) * (ZAF_pop_io.yr)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]
          # tot.ENE.food.ZAF <- a$u
          # list[all_HH_f_ZA, sd_hs] <- GetHHSectoralEnergyPerCap(ICP_food_idx,'ZAF', ZAF_FD_ICP_AllHH, ZAF_intensity.use) # more reliable than IND_intensity
          # (SummarizeGJPerCap(all_HH_f_ZA) * (ZAF_pop_io.yr)) %>% mutate(min=u-2*sd, max=u+2*sd) / 1e9 # [EJ]
          # 
          # rm(all_HH_f_BR, all_HH_f_IN, all_HH_f_ZA)
          # gc()

################################
### Total P vs Final Elec use ##
################################

idx.elec.exio <- 128:141  # or 128:139 # 

# This intensity seems reasonable as proper primary (use) extension, instead of extraction.

# This includes ONLY primary energy carrier rows from use block (all columns)
# This makes more sense now.

### !!! Need to read tot.fdE.elec from EXIO3 to get valid numbers from this
sum(tot.fdE.elec[,BRA_idx_fd]) / sum(eigenMapMatMult(tpei.USE[,BRA_idx_ex[idx.elec.exio]], diag(BRA_fd_exio[idx.elec.exio])))
sum(tot.fdE.elec[,IND_idx_fd]) / sum(eigenMapMatMult(tpei.USE[,IND_idx_ex[idx.elec.exio]], diag(IND_fd_exio[idx.elec.exio])))
sum(tot.fdE.elec[,ZAF_idx_fd]) / sum(eigenMapMatMult(tpei.USE[,ZAF_idx_ex[idx.elec.exio]], diag(ZAF_fd_exio[idx.elec.exio])))

# Note for later: I may need to have something like SPA (structural path analysis) to understand more about the intensity numbers by sector.