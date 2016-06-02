###########################################################
### Calculate average energy intensity by income decile ###
###########################################################

# Without direct energy
scaler_IND <- sum(IND_FD_ICP_usd2007[,1])/sum(IND_fd_exio)
scaler_FRA <- sum(FRA_FD_ICP_usd2007[,1])/sum(FRA_fd_exio)

totE_by_decile_IN_noVal <- IND_inten_RAS_combined_noVal %*% (IND_FD_ICP_usd2007 * EXR_EUR$r) / scaler_IND   # n_draw X n_decile (11)
# int_by_decile_IN_noVal <- sweep(totE_by_decile_IN_noVal, 2, colSums(IND_FD_ICP_usd2007* EXR_EUR$r, na.rm = TRUE), '/') * scaler_IND 
PPP_IND2007 <- WDI(country = "IN", indicator = "PA.NUS.PRVT.PP", start = 2007, end = 2007, extra = FALSE, cache = NULL)
int_by_decile_IN_noVal <- sweep(totE_by_decile_IN_noVal, 2, colSums(IND_FD_ICP_usd2007* EXR_IND$r / PPP_IND2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') * scaler_IND 
totE_per_cap_by_dec_IN <- 1000*cbind(totE_by_decile_IN_noVal[,1]/IND_pop_2007, totE_by_decile_IN_noVal[,2:11]/IND_pop_2007*10)
colnames(totE_per_cap_by_dec_IN) <- colnames(IND_FD_ICP_usd2007)

totE_by_decile_FR_noVal <- FRA_inten_RAS_combined_noVal %*% (FRA_FD_ICP_usd2007* EXR_EUR$r) / scaler_FRA   # n_draw X n_decile (11)
# int_by_decile_FR_noVal  <- sweep(totE_by_decile_FR_noVal, 2, colSums(FRA_FD_ICP_usd2007* EXR_EUR$r, na.rm = TRUE), '/') *scaler_FRA
PPP_FRA2007 <- WDI(country = "FR", indicator = "PA.NUS.PRVT.PP", start = 2007, end = 2007, extra = FALSE, cache = NULL)
int_by_decile_FR_noVal  <- sweep(totE_by_decile_FR_noVal, 2, colSums(FRA_FD_ICP_usd2007* EXR_EUR$r / PPP_FRA2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') *scaler_FRA
totE_per_cap_by_dec_FR <- 1000*cbind(totE_by_decile_FR_noVal[,1]/FRA_pop_2007, totE_by_decile_FR_noVal[,2:11]/FRA_pop_2007*10)
colnames(totE_per_cap_by_dec_FR) <- colnames(IND_FD_ICP_usd2007)

# Food only
foodE_by_decile_IN_noVal <- IND_inten_RAS_combined_noVal[,1:40] %*% (IND_FD_ICP_usd2007[1:40,] * EXR_EUR$r) / scaler_IND   # n_draw X n_decile (11) in 1000 GJ
# food_int_by_decile_IN_noVal <- sweep(foodE_by_decile_IN_noVal, 2, colSums(IND_FD_ICP_usd2007[1:40,]* EXR_EUR$r, na.rm = TRUE), '/') * scaler_IND
food_int_by_decile_IN_noVal <- sweep(foodE_by_decile_IN_noVal, 2, colSums(IND_FD_ICP_usd2007[1:40,]* EXR_IND$r / PPP_IND2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') * scaler_IND
foodE_per_cap_by_dec_IN <- 1000*cbind(foodE_by_decile_IN_noVal[,1]/IND_pop_2007, foodE_by_decile_IN_noVal[,2:11]/IND_pop_2007*10)

foodE_by_decile_FR_noVal <- FRA_inten_RAS_combined_noVal[,1:11] %*% (FRA_FD_ICP_usd2007[1:11,] * EXR_EUR$r) / scaler_FRA   # n_draw X n_decile (11) in 1000 GJ
# food_int_by_decile_FR_noVal <- sweep(foodE_by_decile_FR_noVal, 2, colSums(FRA_FD_ICP_usd2007[1:11,]* EXR_EUR$r, na.rm = TRUE), '/') *scaler_FRA
food_int_by_decile_FR_noVal <- sweep(foodE_by_decile_FR_noVal, 2, colSums(FRA_FD_ICP_usd2007[1:11,]* EXR_EUR$r / PPP_FRA2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') *scaler_FRA
foodE_per_cap_by_dec_FR <- 1000*cbind(foodE_by_decile_FR_noVal[,1]/FRA_pop_2007, foodE_by_decile_FR_noVal[,2:11]/FRA_pop_2007*10)



#### Plot - Figures for the IIOA paper
options(scipen=-2)

pdf(file = paste0(figure_path, "1.1 IND total embodied energy per unit expenditure by decile - without valuation uncertainty.pdf"), width = 18, height = 10)
boxplot(totE_by_decile_IN_noVal[,2:11], ylab ="IND - Total energy [TJ] w/o DE", range=0, axes = TRUE)
dev.off()

pdf(file = paste0(figure_path, "1.2 FRA total embodied energy per unit expenditure by decile - without valuation uncertainty.pdf"), width = 18, height = 10)
boxplot(totE_by_decile_FR_noVal[,2:11], ylab ="FRA - Total energy [TJ] w/o DE", range=0, axes = TRUE)
dev.off()

# boxplot(int_by_decile_IN, ylab ="IND - Avg energy intensity [MJ/EUR] w/o DE", range=0, axes = TRUE)
pdf(file = paste0(figure_path, "2.1 IND embodied energy intensity by decile - without valuation uncertainty.pdf"), width = 15, height = 8)
boxplot(int_by_decile_IN_noVal, ylab ="Average energy intensity [MJ/EUR]",range=0, axes = TRUE)
dev.off()

pdf(file = paste0(figure_path, "2.2 FRA embodied energy intensity by decile - without valuation uncertainty.pdf"), width = 15, height = 8)
boxplot(int_by_decile_FR_noVal, ylab ="Average energy intensity [MJ/EUR]", range=0, axes = TRUE)
dev.off()

# Per capita
pdf(file = paste0(figure_path, "1.1 IND embodied energy per cap by decile.pdf"), width = 15, height = 8)
boxplot(totE_per_cap_by_dec_IN, ylab ="Embodied energy per capita [GJ/cap]",range=0, axes = TRUE)
dev.off()

pdf(file = paste0(figure_path, "1.2 FRA embodied energy per cap by decile.pdf"), width = 15, height = 8)
boxplot(totE_per_cap_by_dec_FR, ylab ="Embodied energy per capita [GJ/cap]", range=0, axes = TRUE)
dev.off()

### Total food E by decile

pdf(file = paste0(figure_path, "3.1 IND total embodied energy in food per unit expenditure by decile.pdf"), width = 18, height = 10)
boxplot(foodE_by_decile_IN_noVal[,2:11], ylab ="Total embodied energy in food [TJ]", range=0, axes = TRUE)
title("Total embodied energy in food consumption - India")
dev.off()

pdf(file = paste0(figure_path, "3.2 FRA total embodied energy in food per unit expenditure by decile.pdf"), width = 18, height = 10)
boxplot(foodE_by_decile_FR_noVal[,2:11], ylab ="Total embodied energy in food [TJ]", range=0, axes = TRUE)
title("Total embodied energy in food consumption - France")
dev.off()

pdf(file = paste0(figure_path, "4.1 IND embodied energy intensity in food by decile.pdf"), width = 15, height = 8)
boxplot(food_int_by_decile_IN_noVal[,2:11], ylab ="IND Avg food energy intensity [MJ/EUR]", range=0, axes = TRUE)
dev.off()

pdf(file = paste0(figure_path, "4.2 FRA embodied energy intensity in food by decile.pdf"), width = 15, height = 8)
boxplot(food_int_by_decile_FR_noVal[,2:11], ylab ="FRA Avg food energy intensity [MJ/EUR]", range=0, axes = TRUE)
dev.off()

pdf(file = paste0(figure_path, "2.1 IND food embodied energy per cap by decile.pdf"), width = 15, height = 8)
boxplot(foodE_per_cap_by_dec_IN[,2:11], ylab ="Embodied energy per capita [GJ/cap]", range=0, axes = TRUE)
dev.off()

pdf(file = paste0(figure_path, "2.2 FRA food embodied energy per cap by decile.pdf"), width = 15, height = 8)
boxplot(foodE_per_cap_by_dec_FR[,2:11], ylab ="Embodied energy per capita [GJ/cap]", range=0, axes = TRUE)
dev.off()



##########################################
### Plot ICP Sectoral energy intensity ###
##########################################

## Main ##
# COICOP sectors with zero expenditure included here
# With RAS and with uncertainty in valuation mtx
# Without direct energy
divider <- c(2, 8, 14, 17, 22, 25, 28, 32, 37, 40, 47, 55, 65, 84, 95, 112, 116, 134, 135, 137)
idx_section_name <- c(divider)+1
section_name <- icp_ntnu$ICP_Heading[idx_section_name]
section_name <- gsub("UNBR ", "", section_name)
section_name[19] <- "Restaurants and hotels"

Plot_ICP_sectors <- function(intensity_mtx, noexp, icp=1, ymax, titlename) {
  if(icp==1) {
    divider <- c(2, 8, 14, 17, 22, 25, 28, 32, 37, 
                 40, 47, 55, 65, 84, 95, 112, 116, 134, 135, 137)
  }
  else {
    divider <- c(0, 11, 15, 21, 36, 48, 55, 69, 72, 93, 94, 97)
  }
  
  boxplot(intensity_mtx, range=0, ylim=c(0, ymax), axes = FALSE, add=FALSE)
  col_div <- c(par("usr")[1], divider+0.5, par("usr")[2])
  
  # Paint alternating colors
  for(i in 1:(length(col_div)-1)) { 
    color_bgn <- c("gray60", "gray15")[i %% 2]
    rect(col_div[i], par("usr")[3], col_div[i+1], par("usr")[4],col = color_bgn, border=FALSE)  
  }
  boxplot(intensity_mtx, ylab ="Embodied energy intensity [MJ/EUR]", 
          axes = FALSE, ylim=c(0, ymax), add=TRUE, cex.lab=1.3)
  # axis(side = 1, at = seq(1,n_sector_icp,10))
  title(xlab ="Consumption sectors", line=1, cex.lab=1.3) 
  axis(side = 2, at = seq(0,ymax,50), cex.axis=1.1)
  
  if(icp==0) { 
    section_name <- c("Food and non-alcoholic beverages", section_name[10:20])
  }
  idx_section <- c(divider)+1
  
  text(idx_section-1, y=50, section_name, pos=4, offset=0.7, cex = 0.9, srt = 90)
  if(icp==1) { 
    text(1:n_sector_icp, y=apply(intensity_mtx, 2, max)+5, 1:n_sector_icp, pos=4, offset=-.1, cex = 0.7, srt = 90)
  }
  else {
    text(1:n_sector_coicop, y=apply(intensity_mtx, 2, max)+5, 1:n_sector_coicop, pos=4, offset=-.1, cex = 0.7, srt = 90)
  }
  text(noexp, y=apply(intensity_mtx[,noexp], 2, max)+11, '*', pos=2, offset=-.1, cex = 1.2, srt = 90)
  title(titlename)
}

# figure_path <- "H:/MyDocuments/IO work/DLE_scripts/Plots/"
# pdf(file = paste0(figure_path, "IND intensity by ICP sector-val-noDE.pdf"), width = 18, height = 10)
# Plot_ICP_sectors(IND_inten_RAS, no_expense_IND, 500)
# title("India with valuation uncertainty")
# dev.off()

pdf(file = paste0(figure_path, "0.1 IND embodied energy intensity by ICP sector - no direct energy.pdf"), width = 16, height = 9)
Plot_ICP_sectors(IND_inten_RAS_noVal, no_expense_IND, icp=1, 180, 
                 "Embodied energy intensity by ICP consumption category: India")
dev.off()

pdf(file = paste0(figure_path, "0.2 FRA embodied energy intensity by ICP sector - no direct energy.pdf"), width = 16, height = 9)
Plot_ICP_sectors(FRA_inten_RAS_noVal, no_expense_FRA, icp=0, 180, 
                 "Embodied energy intensity by COICOP consumption category: France")
dev.off()

# Distributions for some example sectors
opar <- par() 
par(mfrow=c(2,2),oma = c(2, 0, 0, 0))
hist(IND_inten_RAS_noVal[,6], 200, main="Bread", xlab =NULL)
hist(IND_inten_RAS_noVal[,11], 200, main="Pork", xlab =NULL)
hist(IND_inten_RAS_noVal[,54], 200, main="Shoes and other footwear", xlab =NULL)
hist(IND_inten_RAS_noVal[,103], 200, main="Fuels and lubricants for personal transport", xlab =NULL)
mtext("Energy intensity for selected sectors [MJ/EUR]", outer = TRUE, side=1)
par(opar)