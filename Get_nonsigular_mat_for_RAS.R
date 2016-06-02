#####################
# Main run of rIPFP #
#####################

# Expenditure in cells
n_draw <- 1000

# D_val_uncertainty <- 1
# list[result_IND, NC_IND] <- Run_rIPFP(bridge_ICP_EXIO_q[,-1], "IND")
D_val_uncertainty <- 0
list[result_IND_noVal, NC_IND_noVal] <- Run_rIPFP(bridge_ICP_EXIO_q[,-1], "IND")

D_val_uncertainty <- 1
# list[result_FRA, NC_FRA] <- Run_rIPFP(bridge_COICOP_EXIO_q[,-1], "FRA")
D_val_uncertainty <- 0
list[result_FRA_noVal, NC_FRA_noVal] <- Run_rIPFP(bridge_COICOP_EXIO_q[,-1], "FRA")

# Allocation ratio in cells - summing up to 1 each row
func1 <- function (x) {
  a <- diag(1/rowSums(x))
  a[is.infinite(a)] <- 0
  x <- a %*% x
}

# final_alloc_list_FRA <- lapply(result_FRA, func1)
final_alloc_list_FRA_noVal <- lapply(result_FRA_noVal, func1)
# final_alloc_list_IND <- lapply(result_IND, func1)
final_alloc_list_IND_noVal <- lapply(result_IND_noVal, func1)



###############################################
### Calculate ICP Sectoral energy intensity ###
###############################################

# in MJ/EUR
xlcFreeMemory()

# Including val mtx uncertainty
# India
D_val_uncertainty <- 1
IND_inten_RAS <- SetupSectorIntensities(final_alloc_list_IND, NC_IND, "IN")
alloc_nonRAS <- get_bridge_COICOP_EXIO(bridge_ICP_EXIO_q[,-1], n_draw)
IND_inten_nonRAS <- SetupSectorIntensities(alloc_nonRAS, NC_IND, "IN")

D_val_uncertainty <- 0
IND_inten_RAS_noVal <- SetupSectorIntensities(final_alloc_list_IND_noVal, NC_IND_noVal, "IN")
IND_inten_nonRAS_noVal <- SetupSectorIntensities(alloc_nonRAS, NC_IND_noVal, "IN")

# France
D_val_uncertainty <- 1
FRA_inten_RAS <- SetupSectorIntensities(final_alloc_list_FRA, NC_FRA, "FR")
alloc_nonRAS <- get_bridge_COICOP_EXIO(bridge_COICOP_EXIO_q[,-1], n_draw)
FRA_inten_nonRAS <- SetupSectorIntensities(alloc_nonRAS, NC_FRA, "FR")

D_val_uncertainty <- 0
FRA_inten_RAS_noVal <- SetupSectorIntensities(final_alloc_list_FRA_noVal, NC_FRA_noVal, "FR")
FRA_inten_nonRAS_noVal <- SetupSectorIntensities(alloc_nonRAS, NC_FRA_noVal, "FR")


# Replacing intensity values for ICP sectors with 0 expenditure
# Note: There are cases where ICP expenditure is zero for a sector from a country.
# In the RAS approach, we do not get intensity numbers because no expenditure is assigned and thus no emission.
# In this case, I copy intensities from the non-RAS estimation.
no_expense_IND <- which((rowSums(bridge_ICP_EXIO_q[,-1])!=0) & (IND_FD_ICP_usd2007[,1]==0))
no_expense_IND <- no_expense_IND[!(no_expense_IND %in% grep("UNBR", ICP_catnames))]   # Remove UNBR items
no_expense_FRA <- which((rowSums(bridge_COICOP_EXIO_q[,-1])!=0) & (FRA_FD_ICP_usd2007[,1]==0))
# IND_inten_RAS_combined <- IND_inten_RAS
IND_inten_RAS_combined_noVal <- IND_inten_RAS_noVal
# FRA_inten_RAS_combined <- FRA_inten_RAS
FRA_inten_RAS_combined_noVal <- FRA_inten_RAS_noVal
IND_inten_RAS_combined_noVal[,no_expense_IND] <- IND_inten_nonRAS_noVal[,no_expense_IND]
# IND_inten_RAS_combined[,no_expense_IND] <- IND_inten_nonRAS[,no_expense_IND]
FRA_inten_RAS_combined_noVal[,no_expense_FRA] <- FRA_inten_nonRAS_noVal[,no_expense_FRA]
# FRA_inten_RAS_combined[,no_expense_FRA] <- FRA_inten_nonRAS[,no_expense_FRA]


# Add in direct energy intensities for fuel/energy sectors
# idx_DE_ICP <- c(63:65,103)
# idx_DE_COICOP <- c(32:36,61)
# IND_inten_RAS_DE <- IND_inten_RAS_combined
# IND_inten_RAS_DE[, idx_DE_ICP] <- IND_inten_RAS_combined[, idx_DE_ICP] + 
#   do.call("rbind", replicate(dim(IND_inten_RAS_combined)[1], IND_DE_intst2007$de_tot, simplify = FALSE))



# # France
# COICOP_divider <- c(0, 11, 15, 21, 36, 48, 55, 69, 72, 93, 94, 97)
# COICOP1_name <- c("Food and non-alcoholic beverages", section_name[10:20])
# 
# pdf(file = paste0(figure_path, "0.2 FRA embodied energy intensity by ICP sector - no direct energy.pdf"), width = 18, height = 10)
# boxplot(FRA_inten_RAS, range=0, ylim=c(0, 150), axes = FALSE, add=FALSE)
# col_div <- c(par("usr")[1], COICOP_divider+0.5, par("usr")[2])
# 
# # Paint alternating colors
# for(i in 1:(length(col_div)-1)) { 
#   color_bgn <- c("gray60", "gray15")[i %% 2]
#   rect(col_div[i], par("usr")[3], col_div[i+1], par("usr")[4],col = color_bgn, border=FALSE)  
# }
# boxplot(FRA_inten_RAS, xlab ="ICP sectors", ylab ="Energy intensity by ICP sector [MJ/EUR] w/o DE", 
#         axes = FALSE, ylim=c(0, 150), add=TRUE)
# axis(side = 2, at = seq(0,150,50))
# 
# text(COICOP_divider+1, y=100, COICOP1_name, pos=4, offset=-.05, cex = 0.7, srt = 90)
# text(1:n_sector_coicop, y=apply(FRA_inten_RAS, 2, max)+5, 1:n_sector_coicop, pos=2, offset=-.1, cex = 0.7, srt = 90)
# text(no_expense_FRA, y=apply(FRA_inten_RAS[,no_expense_FRA], 2, max)+8, '*', pos=2, offset=-.1, cex = 1.2, srt = 90)
# title("France without valuation uncertainty")
# dev.off()



# 
# # With direct energy
# png(filename = paste(figure_path, "Energy intensity by ICP sector w/ DE.png", sep=""), width = 781, height = 553, units = "px")
# boxplot(inten_RAS, xlab ="ICP sectors", ylab ="Energy intensity by ICP sector [MJ/EUR] w/ DE", range=0, axes = FALSE)
# axis(side = 1, at = seq(1,n_sector_icp,10))
# axis(side = 2, at = seq(0,300,50))
# text(1:n_sector_icp, y=apply(inten_RAS, 2, max)+5, 1:n_sector_icp, pos=4, offset=-.1, cex = 0.7, srt = 90)
# text(no_expense, y=apply(inten_RAS[,no_expense], 2, max)+22, '*', pos=2, offset=-.1, cex = 1.2, srt = 90)
# dev.off()
# 

# 
# # png(filename = paste(figure_path, "Energy intensity by COICOP consumption category.png", sep=""), width = 781, height = 553, units = "px")
# # COICOP sectors with zero expenditure excluded here
# boxplot(ind_inten_RAS, xlab ="ICP sectors", ylab ="Energy intensity by ICP sector [MJ/EUR] w/ RAS", range=0, axes = FALSE)
# axis(side = 1, at = seq(1,n_sector_icp,10))
# axis(side = 2, at = seq(0,300,50))
# text(1:n_sector_icp, y=apply(ind_inten_RAS, 2, max)+5, 1:n_sector_icp, pos=4, offset=-.1, cex = 0.6, srt = 90)
# # dev.off()

# COICOP sectors with zero expenditure included here
# With RAS
# # Not include uncertainty in valuation mtx
# boxplot(ind_inten_RAS_no_val_combined, xlab ="ICP sectors", ylab ="Energy intensity by ICP sector [MJ/EUR] w/o RAS", range=0, axes = FALSE)
# axis(side = 1, at = seq(1,n_sector_icp,10))
# axis(side = 2, at = seq(0,300,50))
# text(1:n_sector_icp, y=apply(ind_inten_RAS_no_val_combined, 2, max)+5, 1:n_sector_icp, pos=4, offset=-.1, cex = 0.6, srt = 90)
# 
# # Just random draw on Q mapping
# # No RAS, but including val mtx uncertainty
# boxplot(ind_inten_nonRAS, xlab ="ICP sectors", ylab ="Energy intensity by ICP sector [MJ/EUR] w/o RAS", range=0, axes = FALSE)
# axis(side = 1, at = seq(1,n_sector_icp,10))
# axis(side = 2, at = seq(0,600,100))
# text(1:n_sector_icp, y=apply(ind_inten_nonRAS, 2, max)+15, 1:n_sector_icp, pos=4, offset=-.1, cex = 0.6, srt = 90)
# 
# # Just random draw on Q mapping
# # No RAS and no val mtx uncertainty
# boxplot(ind_inten_nonRAS_no_val, xlab ="ICP sectors", ylab ="Energy intensity by ICP sector [MJ/EUR] w/o RAS", range=0, axes = FALSE)
# axis(side = 1, at = seq(1,n_sector_icp,10))
# axis(side = 2, at = seq(0,600,100))
# text(1:n_sector_icp, y=apply(ind_inten_nonRAS_no_val, 2, max)+15, 1:n_sector_icp, pos=4, offset=-.1, cex = 0.6, srt = 90)


####################
### Sanity check ###
####################

# 1. Electricity allocation results
elec_alloc <- do.call("rbind", lapply(final_alloc_list, '[', 63,))
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

# Total primary energy demand 
# https://www.iea.org/publications/freepublications/publication/India_study_FINAL_WEB.pdf
# 650 Mtoe(2009)
2.72142E+19 / 1e9 / IND_pop_2007
# http://www.statista.com/statistics/265582/primary-energy-consumption-in-india/
# 420.3 Mtoe(2007)
1.75971204E+19 / 1e9 / IND_pop_2007
# http://www.statista.com/statistics/265582/primary-energy-consumption-in-india/
# 575 Mtoe
2.40741E+19 / 1e9 / IND_pop_2007

# France
FRA_pop_2007 <- 64e6
# https://www.google.at/search?q=primary+energy+consumption+france&espv=2&biw=1920&bih=911&tbm=isch&tbo=u&source=univ&sa=X&ved=0ahUKEwiXnpScuLHMAhXBIcAKHaWoAV0QsAQIIQ&dpr=1#imgrc=h_RwRGCAT_rZyM%3A
# 250 Mtoe (2007)
1.0467E+19 / 1e9 / FRA_pop_2007
# per-cap energy
a <- WDI(country = "FR", indicator = "NE.CON.PETC.CD", start = 2007, end = 2011, extra = FALSE, cache = NULL)
a$NE.CON.PETC.CD[5] * EXR_EUR$r * median(int_by_decile_FR[,1]) / 1e3 / FRA_pop_2007

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
# int_summary_combined <- round(int_summary_combined, digits = 2)
# int_summary_combined <- format(int_summary_combined, digits = 2, nsmall = 1, scientific=FALSE)

# int_summary <- stat.desc(inten_RAS) 
# int_summary <- t(int_summary[c(4,5,9,13),]) %>% round(digits=2) %>% format(digits = 2, nsmall = 1, scientific=FALSE)

write.csv(IND_int_summary_combined, "India - Intensity summary without direct E.csv")
write.csv(FRA_int_summary_combined, "France - Intensity summary without direct E.csv")
# write.csv(int_summary, "Intensity summary with direct E.csv")

intensities <- cbind(int_summary_combined, int_summary)
