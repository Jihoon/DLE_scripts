## See how the uncertainty is transferred
# Mainly for IIOA conference Seoul

### For India for the example of "Butter and margarine"
IN_place <- which(exio_ctys=='IN')
IN_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
IN_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country

focus_sect_ICP <- 4 # Rice in ICP
EXIO_sect_mapped <- which(bridge_ICP_EXIO_q[focus_sect_ICP,-1]!=0)
EX_catnames[EXIO_sect_mapped]

hist(int_by_decile_IN_noVal[,2], 200, main="Decile 1 - India", xlab ="Intensity [MJ/EUR]")
hist(IND_inten_RAS_noVal[,focus_sect_ICP], 200, main="Butter and margarine (ICP) - India", xlab ="Intensity [MJ/EUR]")

map_result_ICP_sect <- do.call("rbind", lapply(final_alloc_list_IND_noVal, '[', focus_sect_ICP,))
map_result_ICP_sect_bp <- t(get_basic_price(t(map_result_ICP_sect), 'IN'))
opar <- par() 
pdf(file = paste0(figure_path, "6.1 IND rice breakdown.pdf"), width = 12, height = 7)
  par(mfrow=c(2,2), oma = c(2, 0, 0, 0))
  for (i in 1:4) {
    hist(map_result_ICP_sect[,EXIO_sect_mapped[i]], seq(0,1,0.005), 
         # main=paste0("Share of \"", EX_catnames[EXIO_sect_mapped[i]],"\" (EXIO) assigned to \"", ICP_catnames[focus_sect_ICP], "\" (ICP)"), 
         main=paste0(EX_catnames[EXIO_sect_mapped[i]]),
         xlim=c(0,1), xlab = "")
  }
  mtext(paste0("Shares of mapped EXIO commodities assigned to \"", ICP_catnames[focus_sect_ICP], "\" (ICP)"), outer = TRUE, side=1)
dev.off()
par(opar)

### For France for the example of "oil and fats"
FR_place <- which(exio_ctys=='FR')
FR_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
FR_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country

focus_sect_COICOP <- 5 
EXIO_sect_mapped_FRA <- which(bridge_COICOP_EXIO_q[focus_sect_COICOP,-1]!=0)
EX_catnames[EXIO_sect_mapped_FRA]

map_result_COICOP_sect <- do.call("rbind", lapply(final_alloc_list_FRA_noVal, '[', focus_sect_COICOP,))
map_result_COICOP_sect_bp <- t(get_basic_price(t(map_result_COICOP_sect), 'FR'))
opar <- par() 
pdf(file = paste0(figure_path, "6.2 FRA oil-fat breakdown.pdf"), width = 12, height = 7)
par(mfrow=c(2,2), oma = c(2, 0, 0, 0))
for (i in 3:6) {
  hist(map_result_COICOP_sect[,EXIO_sect_mapped_FRA[i]], seq(0,1,0.005), 
       main=paste0(EX_catnames[EXIO_sect_mapped_FRA[i]]), xlim=c(0,1), xlab = "")
}
mtext(paste0("Shares of mapped EXIO commodities assigned to \"", COICOP_catnames2[focus_sect_COICOP,1], "\" (COICOP)"), outer = TRUE, side=1)
dev.off()
par(opar)

# Energy use of these mapped sectors
PE_int_sector_direc <- energy_int[nature_input_idx, IN_idx[1]-1+EXIO_sect_mapped]  # The energy extension has not intensities but total consumptions.
PE_int_sector_indirec <- indirect_E_int[nature_input_idx, IN_idx[1]-1+EXIO_sect_mapped]  # The energy extension has not intensities but total consumptions.
colSums(PE_int_sector_indirec)

barplot(IND_FD_ICP_usd2007[,2], xlab="ICP sector", main="Sectoral expenditure by the lowest decile in India [2007 US$] ")
text(1:n_sector_icp*1.2, y=IND_FD_ICP_usd2007[,2], 1:n_sector_icp, pos=1, offset=0, cex = 0.7, srt = 90)

divider <- c(2, 8, 14, 17, 22, 25, 28, 32, 37, 40, 47, 55, 65, 84, 95, 112, 116, 134, 135, 137)
col_IND <- c(shadepalette(5, "white", "orange"), shadepalette(5, "darkgreen", "white"))
col_FRA <- c(shadepalette(5, "white", "blue"), shadepalette(5, "red", "white"))
barplot(t(IND_FD_ICP_usd2007)[2:11,1:40], beside=TRUE, ylab = "Expenditure [2007 US$]", las=2, col = col_IND, 
        names.arg = 1:40)
barplot(t(IND_FD_ICP_usd2007)[2:11,41:80], beside=TRUE, ylab = "Expenditure [2007 US$]", las=2, col = col_IND, 
        names.arg = 41:80)
barplot(t(IND_FD_ICP_usd2007)[2:11,81:120], beside=TRUE, ylab = "Expenditure [2007 US$]", las=2, col = col_IND, 
        names.arg = 81:120)
barplot(t(IND_FD_ICP_usd2007)[2:11,121:151], beside=TRUE, ylab = "Expenditure [2007 US$]", las=2, col = col_IND, 
        names.arg = 121:151)
barplot(t(IND_FD_ICP_usd2007)[2:11,1:151], beside=TRUE, ylab = "Expenditure [2007 US$]", las=2, col = col_IND, 
        names.arg = 1:151)

opar <- par() 
food_ICP <- 1:40
food_COICOP <- 1:11
a<-(sweep(IND_FD_ICP_usd2007[food_ICP,], 2, colSums(IND_FD_ICP_usd2007[food_ICP,], na.rm = TRUE), '/'))
remove_idx <- which(rowSums(a)==0)
a <- a[-remove_idx,]
pdf(file = paste0(figure_path, "5.1 IND food breakdown.pdf"), width = 15, height = 10)
  par(mar=c(29,4.1,2.1,2.1))
  barplot(t(a)[2:11,], beside=TRUE, ylab = "Fraction among food consumption", 
          las=2, col = col_IND, names.arg = ICP_catnames[food_ICP[-remove_idx]])
dev.off()

b<-(sweep(FRA_FD_ICP_usd2007[food_COICOP,], 2, colSums(FRA_FD_ICP_usd2007[food_COICOP,], na.rm = TRUE), '/'))
pdf(file = paste0(figure_path, "5.2 FRA food breakdown.pdf"), width = 15, height = 10)
  par(mar=c(22,4.1,2.1,2.1))
  barplot(t(b)[2:11,], beside=TRUE, ylab = "Fraction among food consumption", 
          las=2, col = col_FRA, names.arg = COICOP_catnames2[food_COICOP,1])
dev.off()
par(opar)

focus_sect_COICOP <- 42 # "Small electric household appliances" in COICOP
hist(int_by_decile_FR[,2], 200, main="Decile 1 - France", xlab ="Intensity [MJ/EUR]")
hist(FRA_inten_RAS[,focus_sect_COICOP], 200, main="Small electric household appliances (COICOP) - FRANCE", xlab ="Intensity [MJ/EUR]")
barplot(FRA_FD_ICP_usd2007[,2])
barplot(t(FRA_FD_ICP_usd2007)[2:11,1:50], beside=TRUE, ylab = "Expenditure [2007 US$]", las=2, col = col_IND, 
        names.arg = 1:50)
barplot(t(FRA_FD_ICP_usd2007)[2:11,51:109], beside=TRUE, ylab = "Expenditure [2007 US$]", las=2, col = col_IND, 
        names.arg = 51:109)
