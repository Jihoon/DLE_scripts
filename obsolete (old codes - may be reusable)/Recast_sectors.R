n_draw <- 20
list[result_IND_noVal, NC_IND_noVal] <- Run_rIPFP(bridge_ICP_EXIO_q[,-1], "IND")
final_alloc_list_IND_noVal <- lapply(result_IND_noVal, func1)

IND.ICP.exenditure <- IND_FD_ICP[,1]
IND.ICP.exenditure[-ICP_svc_idx] <- 0

ICP_food_idx <- 1:45
ICP_hhold_idx <- c(56:84, 138:151)  # Household goods/services
ICP_svc_idx <- 85:137   # Health, Transport, Communication, Recreation
ICP_fuel_idx <-152:164
# For the breakdowns for health or education, which is easier with EXIO classification, I need


# Exercise for ICP food sectors c(4:8, 10:14)
# DeriveConsumptionEnergyShares() defined in Bridge_RAS.R
a <- DeriveConsumptionEnergyShares(final_alloc_list_IND_noVal, IND.ICP.exenditure, NC_IND_noVal, "IN")
# Food items
a <- sapply(c(4:8, 10:14), function(x) {
  DeriveConsumptionEnergyShares(final_alloc_list_IND_noVal, unit.vector(x,length(ICP_catnames)), NC_IND_noVal, "IN")})
af <- sapply(c(4:8, 10:14), function(x) {
  DeriveConsumptionEnergyShares(final_alloc_list_IND_noVal, unit.vector(x,length(ICP_catnames)), NC_IND_noVal, "IN", "final")})
view(af)       


af <- sapply(c(4:8), function(x) {
  DeriveEnergyCarrierSharesbySector(final_alloc_list_IND_noVal, unit.vector(x,length(ICP_catnames)), NC_IND_noVal, "IN", "final")})
names(af) <- c("solid", "liquid", "gas", "elec")
view(af)       
