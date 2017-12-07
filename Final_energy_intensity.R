# Indexation (on EXIO energy extension: 69 carriers) based on IEA balance builder (BalanceBuilderTemplate.xls)
idx_coalpeat <- c(3:10, 18:20, 57:60)
idx_crude <- c(11, 13:14, 41)
idx_oilprod <- c(21:38)
idx_natgas <- 12
idx_bio <- c(1:2, 16:17, 40, 42:44, 61, 63:69)
idx_nuclear <- c(15,39)
idx_elecheat <- c(45:56, 62)

# Indexation (on EXIO sectors) based on IEA balance builder (BalanceBuilderTemplate.xls)
# energy.carrier.idx.ex <- c(20:32, 64:85, 91:95, 128:146, 148, 176:182)    # Column index for energy sectors (excluding pulp and extraction sectors)
exio_coal <- c(20:27, 91)
exio_oil <- 28
exio_ng <- c(29:31)
exio_elec <- c(128:139)
exio_oilprod <- c(64:84, 92:95)  # Biofuel included
exio_energy <- c(exio_coal, exio_oil, exio_ng, exio_elec, exio_oilprod)

# final consumption industries

# India
cty_idx <- BRA_idx_ex # IND_idx_ex # 
cty_idx_fd <- BRA_idx_fd # IND_idx_fd # 
supply <- materials[energy_carrier_supply_idx, cty_idx]
use <- materials[energy_carrier_use_idx, cty_idx]
primary <- materials[nature_input_idx, cty_idx]
row.names(supply) <- material.name[energy_carrier_supply_idx]
names(supply) <- EX_catnames
b <- data.frame(e.supply = colSums(supply))
c <- data.frame(e.supply = rowSums(supply))

sum(primary)
sum(supply)
sum(use)


### To compare primary energy production with IEA India values (2007)
tot.priE <- data.frame(name = material.name[nature_input_idx], TJ=rowSums(materials[nature_input_idx,cty_idx]))
tot.priE.sect <- data.frame(material.name[nature_input_idx], (materials[nature_input_idx,cty_idx]))
names(tot.priE.sect) <- c("name", EX_catnames)
view(tot.priE.sect)

# Primary energy which come in to the country (imported + produced)
sum(materials[nature_input_idx,cty_idx])/1000+5206+1433+402 # =25907PJ : Add imported primary energy from IEA to match the total (coal/gas/oil w/o elec/oil prod)
# IEA has 26065 PJ

### To compare final energy
# FD has only uses.

# All energy suppl from all industries (incl. import + export)
tot.supplE <- data.frame(name = material.name[energy_carrier_supply_idx], TJ=rowSums(materials[energy_carrier_supply_idx,cty_idx]))
tot.supplE.sect <- data.frame(material.name[energy_carrier_supply_idx], (materials[energy_carrier_supply_idx,cty_idx]))
names(tot.supplE.sect) <- c("name", EX_catnames)
view(tot.supplE.sect)

sum(tot.supplE$TJ)/1000  # Doesn't mean much (double counting) - generation/extraction/refining

tot.elec.supplE <- data.frame(name = material.name[elec_supply_idx], TJ=rowSums(materials[elec_supply_idx,cty_idx]))


# All energy use from all industries (incl. export)
tot.useE <- data.frame(name = material.name[energy_carrier_use_idx], 
                       TJ=rowSums(materials[energy_carrier_use_idx,cty_idx]) + rowSums(fd_materials[energy_carrier_use_idx,cty_idx_fd]))
tot.useE.sect <- data.frame(material.name[energy_carrier_use_idx], 
                            materials[energy_carrier_use_idx,cty_idx], fd_materials[energy_carrier_use_idx,cty_idx_fd])
names(tot.useE.sect) <- c("name", EX_catnames, final_demand.name)
view(tot.useE.sect)


# All emission energy use from all industries 
tot.emissionE.sect <- data.frame(material.name[emission_energy_carrier_idx], 
                            (materials[emission_energy_carrier_idx,cty_idx]), (fd_materials[emission_energy_carrier_idx,cty_idx_fd]))
names(tot.emissionE.sect) <- c("name", EX_catnames, final_demand.name)
view(tot.emissionE.sect)


# Only for industries w/o households -> probably not a good idea
tot.useE.final <- data.frame(name = material.name[energy_carrier_use_idx], 
                             tot.useE.sect[,-c(1, 202:208)] - tot.supplE.sect[,-1], 
                             (fd_materials[energy_carrier_use_idx,IND_idx_fd]))
names(tot.useE.final) <- names(tot.useE.sect)
tot.useE.final[,-1][tot.useE.final[,-1] <1e-4] <- 0
view(tot.useE.final)



tot.elec.useE <- data.frame(name = material.name[elec_use_idx], 
                            TJ=rowSums(materials[elec_use_idx,cty_idx]) + rowSums(fd_materials[elec_use_idx,IND_idx_fd]))
tot.pri.useE <- data.frame(name = material.name[energy_pri_carrier_use_idx], 
                           TJ=rowSums(materials[energy_pri_carrier_use_idx,cty_idx]) + rowSums(fd_materials[energy_pri_carrier_use_idx,IND_idx_fd]))
tot.sec.useE <- data.frame(name = material.name[energy_sec_carrier_use_idx], 
                           TJ=rowSums(materials[energy_sec_carrier_use_idx,cty_idx]) + rowSums(fd_materials[energy_sec_carrier_use_idx,IND_idx_fd]))
tot.sec.useE.sect <- data.frame(material.name[energy_sec_carrier_use_idx], 
                           (materials[energy_sec_carrier_use_idx,cty_idx]), (fd_materials[energy_sec_carrier_use_idx,IND_idx_fd]))
names(tot.sec.useE.sect) <- c("name", EX_catnames, final_demand.name)
tot.sec.useE.sect[,-1][tot.sec.useE.sect[,-1] <1e-4] <- 0
view(tot.sec.useE.sect)

sum(tot.useE$TJ)/1000  # Doesn't mean much (double counting)
sum(tot.pri.useE$TJ)/1000  
sum(tot.sec.useE$TJ)/1000  
sum(tot.elec.supplE$TJ)/1000 # = 2217 without import IEA=2289=(2965-676)PJ after subtracting stat.diff.
sum(tot.elec.useE$TJ)/1000 # = 2236 PJ, IEA=2119+PJ (incl. own use)
sum(materials[energy_carrier_use_idx,cty_idx])/1000
sum(materials[energy_pri_carrier_use_idx,cty_idx])/1000
sum(materials[energy_sec_carrier_use_idx,cty_idx])/1000
name = material.name[energy_pri_carrier_use_idx]




#### Direct final energy directly from survey data
conv.ene <- read.xlsx("H:/MyDocuments/Analysis/Final energy/India-NSS energy conversion.xlsx", cols=1:3)
# View(IND_FUEL_Alldata[1:200,])

price_sum <- IND_FUEL_Alldata %>% mutate(price=val_tot/qty_tot) %>% select(id, fuel, price) %>% 
  group_by(fuel) %>% summarise(mean=mean(price, na.rm=TRUE), sd=sd(price, na.rm=TRUE), min=min(price, na.rm=TRUE), max=max(price, na.rm=TRUE))
fuel_price <- IND_FUEL_Alldata %>% group_by(fuel) %>% mutate(price=val_tot/qty_tot) %>% summarise(price_avg = mean(price, na.rm=TRUE))
IND_fuel_datasum <- IND_FUEL_Alldata %>% group_by(fuel) %>% mutate(price=val_tot/qty_tot) %>% 
  summarise(qty_sum=sum(qty_tot, na.rm=TRUE), val_sum=sum(val_tot, na.rm=TRUE), 
            price_avg = mean(price, na.rm=TRUE), cnt = n(), na.qty = sum(is.na(qty_tot)), na.val = sum(is.na(val_tot)))
IND_fuel_hh <- IND_FUEL_Alldata %>% group_by(id, fuel) %>% 
  summarise(qty_tot=sum(qty_tot, na.rm=TRUE), val_tot=sum(val_tot, na.rm=TRUE)) %>% arrange(id)

# Dung price estimation - India
# USD 1=60 rupee (2015) https://tradingeconomics.com/india/currency
# 59(=60) rupee per 1.5 kg (2015) https://www.thequint.com/technology/2015/12/30/get-cow-dung-cakes-online-via-amazon-for-your-puja-at-rs-59 
CPI_2015 <- as.numeric(CPI %>% filter(year==2015 & iso2c=='IN') %>% select(FP.CPI.TOTL) / CPI %>% filter(year==2007 & iso2c=='IN') %>% select(FP.CPI.TOTL))
prc_gobar <- 1/1.5/CPI_2015  # USD2007/kg

fuel_price <- fuel_price %>% mutate_cond(fuel=="Dung", price_avg = prc_gobar) %>%
  mutate_cond(fuel=="Diesel, transport", price_avg = fuel_price$price_avg[fuel_price$fuel=="Diesel, non-transport"]) %>% 
  mutate_cond(fuel=="Gasoline, transport", price_avg = fuel_price$price_avg[fuel_price$fuel=="Gasoline, non-transport"])

IND_fuel_hh <- IND_fuel_hh %>% mutate(price_hh=val_tot/qty_tot) %>%
  left_join(fuel_price) %>% mutate_cond(is.nan(price_hh), price_hh=price_avg) %>% arrange(id) %>% mutate(qty_impu = qty_tot) %>% 
  mutate_cond(qty_tot==0, qty_impu = val_tot/price_avg) %>% left_join(conv.ene) %>% mutate(MJ=MJ_per_unit*qty_impu)

IND_MJ_hh <- IND_fuel_hh %>% select(id, fuel, qty_impu, val_tot, MJ)


# Final direct energy intensity
IND_MJ_ALL <- readFinalEnergyfromDBAllHH() # in MJ / HH
IND.tot.direct.MJ <- colSums(IND_MJ_ALL[,c(-1, -2)]*IND_MJ_ALL$weight)  # in MJ
IND.tot.fuel.cost <- IND_FD_ICP_usd2011[152:164, 1]  # National total (from NSS) in M.USD 2007 (MER)
IND.E.direct.int <- IND.tot.direct.MJ/IND.tot.fuel.cost/1e6 # MJ/USD 2007 MER
IND.E.direct.int[is.nan(IND.E.direct.int)] <- 0


#### Aggregating some EXIO sectors
# to resolvethe issue of very high intensity for e.g. meat sectors and sand/clay (Ind)
Q <-bridge_ICP_EXIO_q[,-1]
mapped <- data.frame(which(Q==1, arr.ind=TRUE)) %>% arrange(row) %>% left_join(data.frame(col=1:length(Q), colsum=colSums(Q)))

IdentifyColumnsToCombine <- function(n.dim=2) {
  a <- mapped %>% filter(colsum==n.dim) %>% arrange(col) %>% group_by(col) %>% mutate(order=paste0("row",1:n.dim)) %>% spread(key=order, value=row) %>% 
    left_join(data.frame(col=1:200, EXIO=t(EX_catnames))) 
  a$id <- do.call(paste, a[names(a)[startsWith(names(a), "row")]])
  col_combine <- a %>% left_join(data.frame(table(a$id)) %>% rename(id = Var1)) %>% filter(Freq>1) 
  
  # When there is no more
  if(dim(col_combine)[1]==0) {
    stop(paste("exiting at", i))
  }
  
  col_combine <- col_combine %>% arrange(id) %>%
    group_by(id) %>% mutate(num=paste0("col", str_pad(row_number(), 2, pad="0"))) %>% select(-EXIO) %>%
    spread(key=num, value=col)
  return(col_combine)
}

# Figuring out what to merge
col_combine <- list()
for(i in 1:10) {
  col_combine[[i]] <- IdentifyColumnsToCombine(i)
}

# Specify new names for the merged columns
col_combine[[1]]$new.name <- c("Construction material & service", "Services n.e.c", "Coal", "Electricty", "Natural gas")
col_combine[[2]]$new.name <- c("Rice", "Sugar crop, plant fiber", "Waste management", "Metal", "Gasoline", "Kerosene", "See & inland transport",
                               "Finanancial services")
col_combine[[3]]$new.name <- c("xxx", "Pulp and secondary paper", "Stone and sand")
col_combine[[4]]$new.name <- c("Other petroleum products", "Fertiliser")


# Show the EXIO column names to merge
idx.combine <- list()
k <- 1
for(j in 1:length(col_combine)) {
  print(paste("colSum =", j+1))
  for(i in 1:dim(col_combine[[j]])[1]) {
    # print(paste(EX_catnames[col_combine[[j]]$col01[i]], EX_catnames[col_combine[[j]]$col02[i]]))
    ex.name.idx <- as.numeric((col_combine[[j]] %>% select(starts_with("col")))[i,-2:-1])
    ex.name.idx <- ex.name.idx[!is.na(ex.name.idx)]
    idx.combine[[k]] <- ex.name.idx
    names(idx.combine)[k] <- col_combine[[j]]$new.name[i]
    k <- k+1
    # print(EX_catnames[ex.name.idx])
  }
}

# Manually add/adjust some more columns to merge
# We can overwrite any automatically assigned aggr sectors above from here.
idx.combine$Pork <- c(44, 10)
idx.combine$Chicken <- c(45, 11)
idx.combine$Beef <- c(43, 9)
idx.combine$"Other meat" <- c(46, 12)
idx.combine$"Finanancial services" <- c(165:167)  #Overwrite above
idx.combine$"Other services" <- c(168:173, 196:200)
idx.combine$Metal <- idx.combine$Metal[c(-1, -2)]
idx.combine$"Services n.e.c" <- NULL   #Overwrite above
idx.combine$xxx <- NULL    #Overwrite above


# length(ex.name.idx)
# idx.combine <- sapply(ex.name.idx, function(x) {x+seq(0, 9400, 200)})
# indirect_E_int[,as.numeric(t(idx.combine))]
# weight.comb <- tot_demand[as.numeric(t(idx.combine))]


# Assume indirect_E_int and tot_demand exist
# a <- ShrinkExtension(ex.name.idx)

ShrinkExtension <- function(idx, mat = indirect_E_int) {
  mat.shrnk <- mat
  for (j in 1:length(idx)) {
    for (i in 1:48) {
      idx.cty <- idx[[j]] + 200*(i-1)
      mat.shrnk[,idx.cty[1]] <- mat.shrnk[,idx.cty] %*% tot_demand[idx.cty] / sum(tot_demand[idx.cty])
      # mat.shrnk <- mat.shrnk[, -idx.cty[-1]]
    }  
  }
  idx.col.remove <- sort(unlist(sapply(idx, function(x) x[-1])))
  idx.col.remove <- as.numeric(sapply(idx.col.remove, function(x) {x+seq(0, 9400, 200)}))
  # mat.shrnk <- mat.shrnk[,-idx.col.remove]
  mat.shrnk[,idx.col.remove] <- 0
  mat.shrnk[is.nan(mat.shrnk)] <- 0   # There are sectors with tot_demand==0
  return(mat.shrnk)
}

ShrinkQMapping <- function(idx, mat = bridge_ICP_EXIO_q[,-1], fd_vec = IND_fd_exio) {
  mat.shrnk <- mat
  fc_mat <- t(fd_vec)
  for (j in 1:length(idx)) {
    idx.cty <- idx[[j]] 
    mat.shrnk[,idx.cty[1]] <- as.numeric(rowSums(mat.shrnk[,idx.cty])!=0)
    fc_mat[idx.cty[1]] <- sum(fc_mat[idx.cty])
    # mat.shrnk <- mat.shrnk[, -idx.cty[-1]]
  }
  idx.col.remove <- sort(unlist(sapply(idx, function(x) x[-1])))
  mat.shrnk[,idx.col.remove] <- 0
  fc_mat[idx.col.remove] <- 0
  return(list(mat.shrnk, t(fc_mat)))
}

# Rename the merged columns
idx.final <- unlist(sapply(idx.combine, function(x) x[1]))
EX_catnames.new <- EX_catnames
EX_catnames.new[idx.final] <- names(idx.final)

# Intensity by EXIO sector
indirect_fE_int_shrnk <- ShrinkExtension(idx.combine, indirect_fE_int)
indirect_E_int_shrnk <- ShrinkExtension(idx.combine, indirect_E_int)
list[q, fd] <- ShrinkQMapping(idx.combine, bridge_ICP_EXIO_q[,-1], IND_fd_exio)

list[result_IND_FE, NC_IND_FE] <- Run_rIPFP_FE(q, "IND", fd)

final_alloc_list_IND_FE <- lapply(result_IND_FE, func1)
IND_inten_RAS_FE <- SetupSectorIntensities_FE(final_alloc_list_IND_FE, NC_IND_FE, "IN", "final", idx.combine)   
IND_inten_RAS_PE <- SetupSectorIntensities_FE(final_alloc_list_IND_FE, NC_IND_FE, "IN", "primary", idx.combine)  
# 'idx.combine' is used in SetupSectorIntensities_FE() to convert fd_bp

# Incorporating direct energy 
IND_inten_RAS_FE[,152:164] <- sweep(IND_inten_RAS_FE[,152:164], 2, IND.E.direct.int, '+')

indirect_int_EXIO_IND <- data.frame(EXIO=t(EX_catnames.new), 
                               primary=colSums(indirect_E_int_shrnk[, IND_idx_ex]), 
                               final=colSums(indirect_fE_int_shrnk[, IND_idx_ex])) %>% 
  mutate(ratio = primary/final)
indirect_int_ICP_IND <- data.frame(ICP=ICP_catnames, 
                                    primary=colMeans(IND_inten_RAS_PE), 
                                    final=colMeans(IND_inten_RAS_FE)) %>% 
  mutate(ratio = primary/final)

view(indirect_int_EXIO_IND)
view(indirect_int_ICP_IND)

indirect_int_EXIO_IND_carrier <- data.frame(EXIO=t(EX_catnames.new), 
                                    primary=t(indirect_E_int_shrnk[, IND_idx_ex]), 
                                    final=colSums(indirect_fE_int_shrnk[, IND_idx_ex])) %>% 
  mutate(ratio = primary/final)


### Try looking at shares (monetary and energy) for Indian sectors

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/iot.Rda")  # iot
iot.mat <- as.matrix(iot)

energy_int[,6601]    # f
indirect_E_int  # f*L
rownames(indirect_E_int) <- carrier.name.pr
rownames(energy_int) <- carrier.name.pr


# Look at monetary input shares for countries
country.idx <- which(exio_ctys=="IN")
country_ex <- seq(200*(country.idx-1)+1, 200*country.idx)   # 200 EXIO comodities
country_ex.i <- seq(163*(country.idx-1)+1, 163*country.idx)   # 163 EXIO industries

exidx <- 1 # Specify sector codes
exidx.i <- 1 

# compare monetary shares A vs (L-I)
# tot.demand.IND <- tot_demand[IND_idx_ex]
# tot.demand.IND[128:140]

direc.input.i <- matrix(tot_use[,country_ex.i[exidx.i]], ncol=48)
# sum(direc.input.i[32,])
# sum(direc.input.i[85,])
direc.input.i <- data.frame(name=t(EX_catnames), dom=direc.input.i[,country.idx], imp=rowSums(direc.input.i[,-country.idx])) %>% 
  mutate(sum=(dom+imp)/sum(dom+imp), dom=dom/sum(dom), imp=imp/sum(imp))

direc.input <- matrix(iot.mat[,country_ex[exidx]], ncol=48)
# sum(direc.input[32,])
# sum(direc.input[85,])
direc.input <- data.frame(name=t(EX_catnames), dom=direc.input[,country.idx], imp=rowSums(direc.input[,-country.idx])) %>% 
  mutate(sum=(dom+imp)/sum(dom+imp), dom=dom/sum(dom), imp=imp/sum(imp))
colSums(direc.input[,-1])

indirec.input <- matrix(L_inverse[,country_ex[exidx]], ncol=48)  # -diag(9600)
# indirec.input[exidx, country.idx] <- indirec.input[exidx, country.idx] - 1
indirec.input <- data.frame(name=t(EX_catnames), 
                            dom=indirec.input[,country.idx], # 
                            dir.f.int=colSums(f_energy_int[,country_ex]),
                            dir.p.int=colSums(p_energy_int[,country_ex]),
                            dir.p.int.nat=colSums(energy_int[,country_ex]),
                            dir.el.int=colSums(elec_int[,country_ex]),
                            imp=rowSums(indirec.input[,-country.idx])) %>% 
  mutate(sum=dom+imp, fenergy=dom*dir.f.int, penergy.nat=dom*dir.p.int.nat, penergy=dom*dir.p.int, elec=dom*dir.el.int)
View(indirec.input)

View(direc.input.i)
View(direc.input)

ShowExpenditureShare <- function(countrty="IN", EXIOsectnum) {
  country.idx <- which(exio_ctys==countrty)
  country_ex <- seq(200*(country.idx-1)+1, 200*country.idx)   # 200 EXIO comodities
  # country_ex.i <- seq(163*(country.idx-1)+1, 163*country.idx)   # 163 EXIO industries
  
  direc.input <- matrix(iot.mat[,country_ex[EXIOsectnum]], ncol=48)
  indirec.input <- matrix(L_inverse[,country_ex[EXIOsectnum]], ncol=48)  # -diag(9600)
  
  input.monetary <- data.frame(name=t(EX_catnames), 
                               dir.tot=direc.input[,country.idx] + rowSums(direc.input[,-country.idx]), # domestic + import
                               ind.tot=indirec.input[,country.idx] + rowSums(indirec.input[,-country.idx]))
  
  return(input.monetary)
}

view(ShowExpenditureShare("IN", 155))


fd <- rowSums(final_demand)
L_inv <- as.matrix(L_inverse)

# Look at ratios between embodied p/f intensities and correlation between electricity share
total_Elec_emb <- colSums(indirect_El_int) %*% diag(tot_demand)  
  # Total embodied electricity by sector/country. s*L*x. The sum of this will be larger than global electricty use. (double counted)
  # Each cell can give total embodied elec use from producing X commodity.
total_Elec_emb_fd <- colSums(indirect_El_int) %*% diag(fd)  
  # Total embodied electricity by sector/country. s*L*x. This will sum up to right global total.
total_Elec <- colSums(elec_int) %*% L_inv %*% diag(rowSums(final_demand))  # Global total electricity. s*L*y (Total direct elec use by sector)

total_Elec <- matrix(, nrow = 0, ncol = 9600)
# Each i-th row is electricity consumed in each of 9600 sectors to produce final demand of i-th product. (concept of embodied)
# Sum of Each i-th column is total electricity directly consumed in each of 9600 sectors to meet global demand.
for (i in 1:9600) {
	b <- colSums(elec_int) * L_inv[,i] * fd[i]  # Global total electricity. s*L*y
	total_Elec <- rbind(total_Elec, b)
}
	
total_final_ENE <- colSums(indirect_fE_int) %*% diag(tot_demand)  # indirect_fE_int is not including captive inputs and energy sector inputs.
# total_Elec_share <- total_Elec[IND_idx_ex] / total_final_ENE[IND_idx_ex]  # Share of electricity among total embodied final energy by Indian sector (200)
total_Elec_share <- colSums(indirect_El_int)[IND_idx_ex] / colSums(indirect_fE_int)[IND_idx_ex]  # Share of electricity among total embodied final energy by Indian sector (200)
P_F_ratio <- colSums(indirect_fE_int[,IND_idx_ex]) / colSums(indirect_pE_int[,IND_idx_ex])
# P_F_ratio <- colSums(indirect_fE_int[,IND_idx_ex]) / colSums(indirect_E_int[,IND_idx_ex])

# Plot share of embodied electricity in embodied final E vs ratio between Primary/Final embodied intensity 
ggplot(data.frame(num=1:200, total_Elec_share, P_F_ratio) %>% slice(-c(energy_sector_idx_ex, captive_sector_idx_ex)) %>% filter(P_F_ratio<1) , 
       aes(total_Elec_share, P_F_ratio)) +
  geom_point() + geom_text_repel(aes(label=num), size = 3)



total_Elec_share.dir <- colSums(tot.useE.elec[,IND_idx_ex]) / colSums(tot.finE.sect[,IND_idx_ex])
# plot(data.frame(total_Elec_share.dir, P_F_ratio) %>% filter(P_F_ratio<1))
ggplot(data.frame(num=1:200, total_Elec_share.dir, P_F_ratio) %>% filter(P_F_ratio<1) , 
       aes(total_Elec_share.dir, P_F_ratio)) +
  geom_point() + geom_text_repel(aes(label=num), size = 3)

View(data.frame(indirect_fE_int[,IND_idx_ex[exidx]], indirect_pE_int[,IND_idx_ex[exidx]]))
sort(P_F_ratio[P_F_ratio<1])



#### Aggregate industry sectors and energy carriers
# Values for EXIO sectors are not reliable and misleading. 
# 1. IEA 2007 data has no industrial breakdowns for electricity.
# 2. Inputs for captive generation for aluminum (from a seperate source) are much bigger than the total use from the sector.
# 3. No correlation between elec share and p/f ratio
# 4. India is a major Aluminum producer, but its GJ/kg Al is way too low. 
#   Al production: 1348 kt (2008) http://www.aluminium-india.org/Worldscenario.php
#   Exio gives 270TJ electricity => 0.2 MJ/kg Al
#   while avg 54 MJ elec per kg http://wordpress.mrreid.org/2011/07/15/electricity-consumption-in-the-production-of-aluminium/)

# Energy carrier aggregation
aggmap.ene <- read.xlsx("H:/MyDocuments/Analysis/Final energy/EXIO sectoral aggregation.xlsx", sheet=2, cols = 2:16)
aggmap.ene[is.na(aggmap.ene)] <- 0 
tot.useE.agg <- t(as.matrix(aggmap.ene)) %*% as.matrix(tot.useE.sect)
row.names(tot.useE.agg) <- names(aggmap.ene)
IND.tot.useE.agg <- data.frame(name=row.names(tot.useE.agg), tot.useE.agg[,IND_idx_ex])
names(IND.tot.useE.agg)[-1] <- EX_catnames
view(IND.tot.useE.agg)

# EXIO sector aggregation
aggmap.exio <- read.xlsx("H:/MyDocuments/Analysis/Final energy/EXIO sectoral aggregation.xlsx", sheet=1, cols = 3:26)
aggmap.exio[is.na(aggmap.exio)] <- 0 



### Some visualizations and checks of the derived intensities
# Compare intensities direct vs indirect, primary vs final
# in MJ/EUR
intensity.sum.IND <- data.frame(num=1:200, EXIO=t(EX_catnames), 
                                dir.el=colSums(elec_int[,IND_idx_ex]), 
                                dir.f=colSums(f_energy_int[,IND_idx_ex]), 
                                dir.p.nat=colSums(energy_int[,IND_idx_ex]), 
                                dir.p.use=colSums(p_energy_int[,IND_idx_ex]), 
                                tot.el=colSums(indirect_El_int[,IND_idx_ex]), 
                                tot.f=colSums(indirect_fE_int[,IND_idx_ex]), 
                                tot.p.nat=colSums(indirect_E_int[,IND_idx_ex]), 
                                # tot.p.nat.nobio=colSums(indirect_E_int.nobio[,IND_idx_ex]),
                                tot.p.use=colSums(indirect_pE_int[,IND_idx_ex]))
view(intensity.sum.IND)
tot.elec.gen.IND <- sum(tot.supplE.elec[,IND_idx_ex[128:139]]) # TJ
tot.elec.use.IND <- sum(tot.useE.elec[,IND_idx_ex]) # TJ
p <- sum(tot_demand[IND_idx_ex[128:139]])/tot.elec.gen.IND # price EUR/MJ = M.EUR/TJ


# BRA
intensity.sum.BRA <- data.frame(num=1:200, EXIO=t(EX_catnames), 
                                dir.el=colSums(elec_int[,BRA_idx_ex]), 
                                dir.f=colSums(f_energy_int[,BRA_idx_ex]), 
                                dir.p.nat=colSums(energy_int[,BRA_idx_ex]), 
                                dir.p.use=colSums(p_energy_int[,BRA_idx_ex]), 
                                tot.el=colSums(indirect_El_int[,BRA_idx_ex]), 
                                tot.f=colSums(indirect_fE_int[,BRA_idx_ex]), 
                                tot.p.nat=colSums(indirect_E_int[,BRA_idx_ex]), 
                                # tot.p.nat.nobio=colSums(indirect_E_int.nobio[,BRA_idx_ex]),
                                tot.p.use=colSums(indirect_pE_int[,BRA_idx_ex]))
view(intensity.sum.BRA)

# Derive ratio between total elec used and direct elec used
sum(intensity.sum.IND$tot.el * tot_demand[IND_idx_ex]) / sum(intensity.sum.IND$dir.el * tot_demand[IND_idx_ex]) 
sum(intensity.sum.IND$tot.el * rowSums(final_demand[,IND_idx_fd])) / sum(intensity.sum.IND$dir.el * rowSums(final_demand[,IND_idx_fd]))
a <- vector("numeric", 9600)
a[IND_idx_ex[128:139]] <- rowSums(final_demand[IND_idx_ex[128:139],IND_idx_fd])
sum(intensity.sum.IND$tot.el * a) / sum(intensity.sum.IND$dir.el * a) # sum(fd_materials[elec_use_idx,IND_idx_fd])
# sum(intensity.sum.IND$tot.el * tot_demand[IND_idx_ex]) / (sum(tot.useE.elec[,IND_idx_ex]) + sum(fd_materials[elec_use_idx,IND_idx_fd]))

sum(intensity.sum.BRA$tot.el * tot_demand[BRA_idx_ex]) / sum(intensity.sum.BRA$dir.el * tot_demand[BRA_idx_ex]) 
sum(intensity.sum.BRA$tot.el * rowSums(final_demand[,BRA_idx_fd])) / sum(intensity.sum.BRA$dir.el * rowSums(final_demand[,BRA_idx_fd]))
sum(intensity.sum.BRA$tot.el * rowSums(final_demand[,BRA_idx_fd])) / sum(fd_materials[elec_use_idx,BRA_idx_fd])
# sum(intensity.sum.BRA$tot.el * tot_demand[BRA_idx_ex]) / (sum(tot.useE.elec[,BRA_idx_ex]) + sum(fd_materials[elec_use_idx,BRA_idx_fd]))
view(data.frame(EXIO=t(EX_catnames), IND.tot.el=intensity.sum.IND$tot.el, BRA.tot.el=intensity.sum.BRA$tot.el))


# Derive T&D loss assumption
1-(sum(tot.useE.elec[4:5,IND_idx_ex])+sum(fd_materials[elec_use_idx[4:5],IND_idx_fd]))/sum(tot.priE.sect[,IND_idx_ex[131:132]]) # India 23%
1-(sum(tot.useE.elec[4:5,BRA_idx_ex])+sum(fd_materials[elec_use_idx[4:5],BRA_idx_fd]))/sum(tot.priE.sect[,BRA_idx_ex[131:132]]) # Brazil 16%


### India energy summing (for a sector) from final demand and from total demand to check whether they match.. but how?
# The monetary flow always match between
# 1. each row i of L*final_demand
# 2. total demand of i product (tot_demand)

library(Rcpp)
sourceCpp("matmult_test.cpp")

global.demand.sect <- eigenMapMatMult(as.matrix(L_inverse), diag(rowSums(final_demand)))
head(global.demand.sect[IND_idx_ex[108],])
tot_demand[IND_idx_ex[108]]



# Share of mobility/shelter/comfort/food (4svc)
tot_emb_FE_HH <- colSums(indirect_fE_int)*rowSums(final_demand[, -seq(7, 336, 7)])  # Global total indirect FE induced from HH consumption
tot_emb_PE_HH <- colSums(indirect_E_int)*rowSums(final_demand[, -seq(7, 336, 7)])  # Global total indirect FE induced from HH consumption
tot_emb_pE_HH <- colSums(indirect_pE_int)*rowSums(final_demand[, -seq(7, 336, 7)])  # Global total indirect FE induced from HH consumption
idx_4svc_exio <- c(1:14, 19:30, 43:57, 64:84, 93:95, 123,124, 128:153, 166, 168, 176:195)  # mobility/shelter/comfort/food
idx_4svc_exio <- as.vector(sapply(seq(0,9400,200), function(x) x+idx_4svc_exio, simplify = "array"))
(sum(tot_emb_FE_HH[idx_4svc_exio]) + sum(tot.fdE.sect))/(sum(tot_emb_FE_HH) + sum(tot.fdE.sect))
(sum(tot_emb_FE_HH[idx_4svc_exio]) + sum(tot.fdE.sect))/(sum(tot.finE.sect) + sum(tot.fdE.sect))
sum(tot_emb_PE_HH[idx_4svc_exio])/sum(tot_emb_PE_HH)
sum(tot_emb_pE_HH[idx_4svc_exio])/sum(tot_emb_pE_HH)




### Heuristic 1. Use nature input block and scale it with some ratio
### Deriving scaling ratio : 

# 1. (Embodied Elec)/(Embodied primary (nat.input))
indir.elec.nat <- colSums(indirect_El_int)/colSums(indirect_E_int)
indir.elec.nat[is.nan(indir.elec.nat)] <- 0
# indir.elec.share <- colSums(indirect_El_int)/colSums(indirect_use_int)  # looks good but renewable electricity shouldn't not be included

# 2. (Embodied fossil Elec)/(all Embodied carrier use)
# Denominator is something not physically meaningful
indir.elec.share <- colSums(indirect_El_int[c(1,2,6),])/colSums(indirect_use_int)  # only fossil electricity
indir.elec.share[is.nan(indir.elec.share)] <- 0

# 3. (Embodied expenditure on fossil Elec)/(all Embodied energy expenditure)  # Replace the one above
  # Read in energy price (USD2005/GJ)
fuel_price.industry <- read_xlsx("H:/MyDocuments/Analysis/Final energy/Fuel price/unlinked_countries_2017_06.xlsx", sheet=2, skip=2) %>%
  select(-c(1, 3:6))
names(fuel_price.industry) <- c("country", "ng", "oil", "coal", "elec", "oilprod")
fuel_price.industry <- fuel_price.industry %>% filter(country=="IND" | country=="BRA")


# An attempt to derive fossil elec share for all 9600 exio sectors
# But this is false because the expenditure ratios I am using here is not a good representation of final energy shares. 
# And allocating energy carriers purely based on those shares is not really trustworthy.
# So not recommended for use.
# Then, for now (Nov 16, 2017) better to just carve indirect electricity intensity based on the carrier use block.
CarrierSharefromIOT <- function(L_vec) {
  indirec.input <- rowSums(matrix(L_vec, ncol=48))
  total <- (sum(indirec.input[exio_coal]) / fuel_price.industry$coal[1] +
              sum(indirec.input[exio_ng]) / fuel_price.industry$ng[1] +
              sum(indirec.input[exio_oil]) / fuel_price.industry$oil[1] +
              sum(indirec.input[exio_elec]) / fuel_price.industry$elec[1] +
              sum(indirec.input[exio_oilprod]) / fuel_price.industry$oilprod[1])
  share <- data.frame(fossil.elec=sum(indirec.input[fossil.elec.idx.ex]) / fuel_price.industry$elec[1] / total,
                      coal=sum(indirec.input[exio_coal]) / fuel_price.industry$coal[1] / total,
                      ng=sum(indirec.input[exio_ng]) / fuel_price.industry$ng[1] / total,
                      elec=sum(indirec.input[exio_elec]) / fuel_price.industry$elec[1] / total,
                      oil=sum(indirec.input[exio_oil]) / fuel_price.industry$oil[1] / total,
                      oilprod=sum(indirec.input[exio_oilprod]) / fuel_price.industry$oilprod[1] / total)
  
      # Nuclear expenditure is anyway not included in L inv.
  # share <- sum(indirec.input[exio_elec]) / sum(indirec.input[exio_energy]) # Monetary share
  
  return(share$fossil.elec)
}

indir.elec.share <- apply(L_inverse, 2, CarrierSharefromIOT)  # only fossil electricity
indir.elec.share[is.nan(indir.elec.share)] <- 0

# dir.elec.share[is.nan(dir.elec.share)] <- 0

IEA.final.tot.2007 <- 3.51e8 # GJ without own use  # 3.84e8 (with own use)
IEA.final.IND.2007 <- 16.8e6 # GJ without own use  
IEA.final.BRA.2007 <- 7.86e6 # GJ without own use  

# Primary intensity scaled by (1-indir.elec.share), which is a simple scaler without physical meaning
# I assume a linear relationship between elec share and primary-final ratio.
# This is still 19x9600
a <- t((1-indir.elec.share) * t(indirect_E_int))   

# Scale a matrix to normalize it to IEA total final energy in 2007
# It can be 19x9600 which doesn't mean much. 
# So I can either take colSums to make it a vector (of 9600), or leave it and take colsums after a SetupSectorIntensities run
indir.fin.eng.int.derived <- IEA.final.tot.2007 / sum(a %*% rowSums(final_demand)) * a    # ind.fe.int * final.demand = total final energy (global)

view(data.frame(EXIO=t(EX_catnames), #pri.final.ratio.dir=pri.final.ratio.dir[IND_idx_ex], 
                indir.elec.share=indir.elec.share[IND_idx_ex],
                int.pri.nat=intensity.sum.IND$tot.p.nat,
                indir.fin.eng.int=colSums(indirect_fE_int[,IND_idx_ex]), 
                indir.fin.eng.int.derived=colSums(indir.fin.eng.int.derived)[IND_idx_ex]))
view(data.frame(EXIO=t(EX_catnames), #pri.final.ratio.dir=pri.final.ratio.dir[IND_idx_ex], 
                indir.elec.share=indir.elec.share[BRA_idx_ex],
                int.pri.nat=colSums(indirect_E_int[,BRA_idx_ex]),
                indir.fin.eng.int=colSums(indirect_fE_int[,BRA_idx_ex]), 
                indir.fin.eng.int.derived=colSums(indir.fin.eng.int.derived)[BRA_idx_ex]))

# Compare country total final energy values
sum(indir.fin.eng.int.derived[, IND_idx_ex] %*% rowSums(final_demand[IND_idx_ex,-seq(7, 336, 7)]))
sum(indir.fin.eng.int.derived[, BRA_idx_ex] %*% rowSums(final_demand[BRA_idx_ex,-seq(7, 336, 7)]))

indir.fin.eng.int <- colSums(indir.fin.eng.int.derived)
indir.elec.int <- colSums(indirect_El_int)
indir.gasol.int <- colSums(indirect_gasol_int)
indir.other.int <- indir.fin.eng.int - indir.elec.int - indir.gasol.int

indir.fin.eng.int <- data.frame(indir.fin.eng.int, indir.elec.int, indir.gasol.int, indir.other.int)
view(indir.fin.eng.int)

test <- DeriveConsumptionEnergyShares(final_alloc_list_IND_all, unit.vector(3, length(ICP_catnames)), NC_IND_all, "IN", "primary")
list[IND_f.intensity, IND_f.alloc, NC_f.IND, IND_f.FD_adj] <- DeriveIntensities('IND', 'final', colSums(indir.fin.eng.int.derived))
list[IND_el.intensity, IND_f.alloc, NC_f.IND, IND_f.FD_adj] <- DeriveIntensities('IND', 'final', indir.elec.int)
list[IND_gsol.intensity, IND_f.alloc, NC_f.IND, IND_f.FD_adj] <- DeriveIntensities('IND', 'final', indir.gasol.int)

test <- data.frame(tot.fin=colMeans(IND_f.intensity), tot.el=colMeans(IND_el.intensity), 
           tot.gs=colMeans(IND_gsol.intensity), tot.oth=colMeans(IND_f.intensity)-colMeans(IND_el.intensity)-colMeans(IND_gsol.intensity))
View(test)

### Heuristic 2. Starting with external data ("IEA extended energy balance") only for (direct) final energy
### Adopt aggregate final consumption total for IEA sectors.(25 or so sectors incl. non-energy use)
### First we use IEA 2008 data because in 2007 India does not have by-carrier breakdown of industrial final energy.
### Non-energy use is not considered for now. Only final energy consumption
### No consideration on residence and territorial difference
final.IEA.raw <- read.xlsx("H:/MyDocuments/Analysis/Final energy/Energy extension IEA/a.xlsx", sheet="2008", startRow=4) 
# final.IEA.raw <- read_xls("H:/MyDocuments/Analysis/Final energy/Energy extension IEA/a.xls", sheet="2008") # xls format doesn't work (unknown reason)
IEA.carriers <- unique(as.character(read.xlsx("H:/MyDocuments/Analysis/Final energy/Energy extension IEA/a.xlsx", sheet="2008", colNames=FALSE, rows=3))[-1])
IEA.flows <- read.xlsx("H:/MyDocuments/Analysis/Final energy/Energy extension IEA/a.xlsx", sheet="2008", rowNames=FALSE, cols=1)[-c(1:2),]


