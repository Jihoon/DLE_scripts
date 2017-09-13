# Categorization based on IEA balance builder (BalanceBuilderTemplate.xls)
idx_coalpeat <- c(3:10, 18:20, 57:60)
idx_crude <- c(11, 13:14, 41)
idx_oilprod <- c(21:38)
idx_natgas <- 12
idx_bio <- c(1:2, 16:17, 40, 42:44, 61, 63:69)
idx_nuclear <- c(15,39)
idx_elecheat <- c(45:56, 62)

# final consumption industries


# India
cty_idx <- IND_idx_ex # BRA_idx_ex # 
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
                       TJ=rowSums(materials[energy_carrier_use_idx,cty_idx]) + rowSums(fd_materials[energy_carrier_use_idx,IND_idx_fd]))
tot.useE.sect <- data.frame(material.name[energy_carrier_use_idx], 
                            (materials[energy_carrier_use_idx,cty_idx]), (fd_materials[energy_carrier_use_idx,IND_idx_fd]))
names(tot.useE.sect) <- c("name", EX_catnames, final_demand.name)
view(tot.useE.sect)


# All emission energy use from all industries 
tot.emissionE.sect <- data.frame(material.name[emission_energy_carrier_idx], 
                            (materials[emission_energy_carrier_idx,cty_idx]), (fd_materials[emission_energy_carrier_idx,IND_idx_fd]))
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
IND_inten_RAS_FE[,152:164] <- sweep(IND_inten_RAS_FE[,152:164], 2, 
                                    , '+')

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

iot <- read.table(paste(path_iot, "mrIot_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", skip=2)
iot <- iot[,c(-1,-2,-3)]
iot.mat <- as.matrix(iot)

energy_int[,6601]    # f
indirect_E_int  # f*L
rownames(indirect_E_int) <- carrier.name.pr
rownames(energy_int) <- carrier.name.pr


# Look at monetary input shares for countries
country.idx <- which(exio_ctys=="IN")
country_ex <- seq(200*(country.idx-1)+1, 200*country.idx)   # 200 EXIO comodities
country_ex.i <- seq(163*(country.idx-1)+1, 163*country.idx)   # 163 EXIO industries

exidx <- 130 # Specify sector codes
exidx.i <- 98 

# compare monetary shares A vs (L-I)
# tot.demand.IND <- tot_demand[IND_idx_ex]
# tot.demand.IND[128:140]

direc.input.i <- matrix(tot_use[,country_ex.i[exidx.i]], ncol=48)
sum(direc.input.i[32,])
sum(direc.input.i[85,])
direc.input.i <- data.frame(name=t(EX_catnames), dom=direc.input.i[,country.idx], imp=rowSums(direc.input.i[,-country.idx])) %>% 
  mutate(sum=(dom+imp)/sum(dom+imp), dom=dom/sum(dom), imp=imp/sum(imp))

direc.input <- matrix(iot.mat[,country_ex[exidx]], ncol=48)
sum(direc.input[32,])
sum(direc.input[85,])
direc.input <- data.frame(name=t(EX_catnames), dom=direc.input[,country.idx], imp=rowSums(direc.input[,-country.idx])) %>% mutate(sum=(dom+imp)/sum(dom+imp))
colSums(direc.input[,-1])

indirec.input <- matrix((L_inverse)[,country_ex[exidx]], ncol=48)  # -diag(9600)
indirec.input[exidx, country.idx] <- indirec.input[exidx, country.idx] - 1
indirec.input <- data.frame(name=t(EX_catnames), dom=indirec.input[,country.idx], imp=rowSums(indirec.input[,-country.idx])) %>% mutate(sum=dom+imp)

View(direc.input.i)
View(direc.input)
View(indirec.input)
