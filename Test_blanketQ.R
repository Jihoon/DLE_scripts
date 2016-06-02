
n_draw <- 500

Q_all_ICP <- matrix(1, dim(bridge_ICP_EXIO_q[,-1])[1], dim(bridge_ICP_EXIO_q[,-1])[2])
Q_all_COICOP <- matrix(1, dim(bridge_COICOP_EXIO_q[,-1])[1], dim(bridge_COICOP_EXIO_q[,-1])[2])

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

totE_by_decile_IN_all <- IND_inten_RAS_all %*% (IND_FD_ICP_usd2007 * EXR_EUR$r) / scaler_IND   # n_draw X n_decile (11)
int_by_decile_IN_all <- sweep(totE_by_decile_IN_all, 2, colSums(IND_FD_ICP_usd2007* EXR_IND$r / PPP_IND2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') * scaler_IND 

totE_by_decile_FR_all <- FRA_inten_RAS_all %*% (FRA_FD_ICP_usd2007* EXR_EUR$r) / scaler_FRA   # n_draw X n_decile (11)
int_by_decile_FR_all  <- sweep(totE_by_decile_FR_all, 2, colSums(FRA_FD_ICP_usd2007* EXR_EUR$r / PPP_FRA2007$PA.NUS.PRVT.PP, na.rm = TRUE), '/') *scaler_FRA

Plot_ICP_sectors(FRA_inten_RAS_all, no_expense_FRA, icp=0, 180, 
                 "Embodied energy intensity by COICOP consumption category: France")
Plot_ICP_sectors(IND_inten_RAS_all, no_expense_IND, icp=1, 180, 
                 "Embodied energy intensity by COICOP consumption category: India")
