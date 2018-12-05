###
### Deriving FE distribution from NSS samples
### Based on the new EXIO3 intensities (Also try to compare it with the older one (.bug suffix))

list[IND.tfei.icp, IND_alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND', 'final', final.intensity.mat=tfei.exio)
# list[IND.tpei.icp, IND_alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND', 'primary', pri.intensity.mat=tnei.exio)
chng_pct_IND <- (IND_FD_adj - init_FD_IND) / init_FD_IND
chng_pct_IND[is.nan(chng_pct_IND)] <- 0

IND_FD_ICP_HH_adj <- IND_FD_ICP_AllHH * (chng_pct_IND + 1)
idx_inf <- which(is.infinite(chng_pct_IND))  # Identify rows with Inf adjustments
r_HH <- colSums(IND_FD_ICP_AllHH)/sum(IND_FD_ICP_AllHH)  # ratio of hh total to (unweighted) total
IND_FD_ICP_HH_adj[idx_inf,] <- t(sapply(IND_FD_adj[idx_inf] * 1e6, # M.USD to USD
                                        function(x) x * r_HH / sum(r_HH * IND_HH$weight))) * scaler_IND   
rm(r_HH)
gc()

save(IND_alloc, file="./Saved tables/IND_alloc.Rda")
save(IND_FD_adj, file="./Saved tables/IND_FD_adj.Rda")
save(IND_FD_ICP_HH_adj, file="./Saved tables/IND_FD_ICP_HH_adj.Rda")

load(file="./Saved tables/IND.tfei.icp.Rda")
load(file="./Saved tables/IND_alloc.Rda")
load(file="./Saved tables/IND_FD_adj.Rda")
load(file="./Saved tables/IND_FD_ICP_HH_adj.Rda")

# Or just do this if saved alright
# load(file="./Saved tables/IND.tfei.icp.Rda")
# load(file="./Saved tables/IND.tnei.icp.Rda")

IND.tfei.icp.avg <- colMeans(IND.tfei.icp)
IND.tpei.icp.avg <- colMeans(IND.tpei.icp)

# a <- data.frame(ICP = ICP_catnames, tfei.new =IND.tfei.icp.avg, tpei.new =IND.tpei.icp.avg, tfei.bug =IND.tfei.icp.bug.avg, tpei.bug =IND.tpei.icp.bug.avg) 
# view(a)

list[eHH_IND, eHH_sd] <-  GetHHSectoralEnergyPerCap(ICP_all_idx, 'IND', IND_FD_ICP_HH_adj, IND.tfei.icp)
b <- SummarizeGJPerCapByDecile(eHH_IND)

# Without transportation (based on NTRA extension)
# list[IND.tfei.icp.NTRA, IND_alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND', 'final', final.intensity.mat=as.matrix(tfei.sub$NTRA))
# list[eHH_IND.NTRA, eHH_sd.NTRA] <-  GetHHSectoralEnergyPerCap(ICP_all_idx, 'IND', IND_FD_ICP_HH_adj, IND.tfei.icp.NTRA)
# a1 <- SummarizeGJPerCapByDecile(eHH_IND.NTRA)

# Without transportation (based on NTRA extension)
# list[IND.tfei.icp.NTRA, IND_alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND', 'final', final.intensity.mat=as.matrix(tfei.sub$NTRA))
ICP_transport_idx <- c(96:112, 154, 160) # Both Public transportation service & Private fuel
IND_FD_ICP_HH_adj.noTRP <- IND_FD_ICP_HH_adj
IND_FD_ICP_HH_adj.noTRP[ICP_transport_idx,] <- 0 # Zero out transportation expenditure.
list[eHH_IND.noTRP, eHH_sd.noTRP] <-  GetHHSectoralEnergyPerCap(ICP_all_idx, 'IND', IND_FD_ICP_HH_adj.noTRP, IND.tfei.icp)
a1 <- SummarizeGJPerCapByDecile(eHH_IND.noTRP)


IND_FUEL_Alldata <-selectDBdata(tables='IND1_FUEL')
IND_MJ_ALL <- readFinalEnergyfromDBAllHH() # in MJ per HH (wide format)

# Direct energy for the hh (not per cap)
IND_dir.FE_hh <- IND_MJ_ALL %>% 
  mutate(tot.GJ.dir = rowSums(select(.,-hhid, -weight), na.rm=TRUE)/1000) %>%  
  select(hhid, weight, tot.GJ.dir) 
IND_dir.FE_hh.eq <- IND_MJ_ALL %>% 
  mutate(LPG.eq = Firewood.and.other.fuels/4) %>% # Approx 4 times.
  select(-Firewood.and.other.fuels) %>% # To replace biomass with LPG equiv.
  mutate(tot.GJ.dir = rowSums(select(.,-hhid, -weight), na.rm=TRUE)/1000) %>%  
  select(hhid, weight, tot.GJ.dir) 
IND_dir.FE_hh.eq.noTRP <- IND_MJ_ALL %>% 
  mutate(LPG.eq = Firewood.and.other.fuels/4) %>% # Approx 4 times.
  select(-Firewood.and.other.fuels, -Diesel, -Gasoline) %>% # To replace biomass with LPG equiv.
  mutate(tot.GJ.dir = rowSums(select(.,-hhid, -weight), na.rm=TRUE)/1000) %>%  
  select(hhid, weight, tot.GJ.dir)

IND_emb.FE_hh <- eHH_IND %>% mutate(tot.GJ.emb.pcap = rowMeans(select(., starts_with("V")))) %>% select(-starts_with("V"))
IND_emb.FE_hh.noTRP <- eHH_IND.noTRP %>% mutate(tot.GJ.emb.pcap = rowMeans(select(., starts_with("V")))) %>% select(-starts_with("V"))

# Convert to per-cap
IND_tot.FE_hh.noTRP <- IND_emb.FE_hh.noTRP %>% left_join(data.table(IND_dir.FE_hh.eq.noTRP, key="hhid")) %>%
  mutate(tot.GJ.dir.pcap = tot.GJ.dir / hh_size)
IND_tot.FE_hh.All <- IND_emb.FE_hh %>% left_join(data.table(IND_dir.FE_hh.eq, key="hhid")) %>%
  mutate(tot.GJ.dir.pcap = tot.GJ.dir / hh_size)
IND_tot.FE_hh.Biom <- IND_emb.FE_hh %>% left_join(data.table(IND_dir.FE_hh, key="hhid")) %>%
  mutate(tot.GJ.dir.pcap = tot.GJ.dir / hh_size)
# IND_tot.FE_hh <- IND_emb.FE_hh %>% left_join(data.table(IND_dir.FE_hh, key="hhid")) %>%
#   mutate(tot.GJ.dir.pcap = tot.GJ.dir / hh_size)



# Scale to match the IEA stat
IND_tot.FE_hh.noTRP <- data.frame(IND_tot.FE_hh.noTRP) %>% mutate_at(vars(starts_with("tot.GJ")), funs(./scaler_IND)) %>% 
  mutate(tot.GJ.pcap = tot.GJ.dir.pcap + tot.GJ.emb.pcap, income.pcap=income/hh_size) 
IND_tot.FE_hh.All <- data.frame(IND_tot.FE_hh.All) %>% mutate_at(vars(starts_with("tot.GJ")), funs(./scaler_IND)) %>% 
  mutate(tot.GJ.pcap = tot.GJ.dir.pcap + tot.GJ.emb.pcap, income.pcap=income/hh_size) 
IND_tot.FE_hh.Biom <- data.frame(IND_tot.FE_hh.Biom) %>% mutate_at(vars(starts_with("tot.GJ")), funs(./scaler_IND)) %>% 
  mutate(tot.GJ.pcap = tot.GJ.dir.pcap + tot.GJ.emb.pcap, income.pcap=income/hh_size) 

avg.FE.hh <- IND_tot.FE_hh.All %>% summarise_at(vars(starts_with("tot.GJ")), funs(weighted.mean(., w=weight)))
avg.FE.hh.noTRP <- IND_tot.FE_hh.noTRP %>% summarise_at(vars(starts_with("tot.GJ")), funs(weighted.mean(., w=weight)))

decile_h.noTRP <- IND_tot.FE_hh.noTRP %>% filter(decile=="decile10")
decile_l.noTRP <- IND_tot.FE_hh.noTRP %>% filter(decile=="decile1")



library(reldist)
gini(IND_tot.FE_hh.All$tot.GJ.dir.pcap, weights=IND_tot.FE_hh.All$weight)
gini(IND_tot.FE_hh.Biom$tot.GJ.dir.pcap, weights=IND_tot.FE_hh.Biom$weight)
gini(IND_tot.FE_hh.noTRP$tot.GJ.dir.pcap, weights=IND_tot.FE_hh.noTRP$weight)

gini(IND_tot.FE_hh.All$tot.GJ.emb.pcap, weights=IND_tot.FE_hh.All$weight)
gini(IND_tot.FE_hh.Biom$tot.GJ.emb.pcap, weights=IND_tot.FE_hh.Biom$weight)
gini(IND_tot.FE_hh.noTRP$tot.GJ.emb.pcap, weights=IND_tot.FE_hh.noTRP$weight)

gini(IND_tot.FE_hh.All$tot.GJ.pcap, weights=IND_tot.FE_hh.All$weight)
gini(IND_tot.FE_hh.Biom$tot.GJ.pcap, weights=IND_tot.FE_hh.Biom$weight)
gini(IND_tot.FE_hh.noTRP$tot.GJ.pcap, weights=IND_tot.FE_hh.noTRP$weight)

gini(IND_tot.FE_hh.noTRP$tot.GJ.emb.pcap, weights=IND_tot.FE_hh.noTRP$weight)
gini(IND_tot.FE_hh.All$tot.GJ.emb.pcap, weights=IND_tot.FE_hh.All$weight)

gini(IND_tot.FE_hh.All$tot.GJ.pcap, weights=IND_tot.FE_hh$weight)
gini(IND_tot.FE_hh.All$tot.GJ.pcap*IND_tot.FE_hh$hh_size, weights=IND_tot.FE_hh$weight*IND_tot.FE_hh$hh_size)

gini(IND_tot.FE_hh.noTRP$tot.GJ.pcap, weights=IND_tot.FE_hh.noTRP$weight)
gini(IND_tot.FE_hh.noTRP$tot.GJ.pcap*IND_tot.FE_hh.noTRP$hh_size, weights=IND_tot.FE_hh.noTRP$weight*IND_tot.FE_hh.noTRP$hh_size)

# gini(IND_tot.FE_hh.All$income/IND_tot.FE_hh$hh_size, weights=IND_tot.FE_hh$weight)
# gini(IND_tot.FE_hh.All$income, weights=IND_tot.FE_hh$weight*IND_tot.FE_hh$hh_size)


weighted.hist(IND_tot.FE_hh.Biom$tot.GJ.emb.pcap, w=IND_tot.FE_hh.Biom$weight, breaks=seq(0,50, 0.25), xlim=c(0, 25), ylim=c(0, 1e7))
weighted.hist(IND_tot.FE_hh.noTRP$tot.GJ.emb.pcap, w=IND_tot.FE_hh.noTRP$weight, breaks=seq(0,25, 0.25), xlim=c(0, 25), ylim=c(0, 1e7))
weighted.hist(IND_tot.FE_hh.All$tot.GJ.emb.pcap, w=IND_tot.FE_hh.All$weight, breaks=seq(0,25, 0.25), xlim=c(0, 25), ylim=c(0, 1e7))

weighted.hist(IND_tot.FE_hh.Biom$tot.GJ.dir.pcap, w=IND_tot.FE_hh.Biom$weight, breaks=seq(0,50, 0.25), xlim=c(0, 25), ylim=c(0, 1e7))
weighted.hist(IND_tot.FE_hh.noTRP$tot.GJ.dir.pcap, w=IND_tot.FE_hh.noTRP$weight, breaks=seq(0,50, 0.25), xlim=c(0, 25), ylim=c(0, 1e7))
weighted.hist(IND_tot.FE_hh.All$tot.GJ.dir.pcap, w=IND_tot.FE_hh.All$weight, breaks=seq(0,50, 0.25), xlim=c(0, 25), ylim=c(0, 1e7))

weighted.hist(IND_tot.FE_hh.eq$tot.GJ.dir.pcap, w=IND_tot.FE_hh.eq$weight, breaks=seq(0,50, 0.25), xlim=c(0, 50))
weighted.hist(IND_tot.FE_hh$tot.GJ.pcap, w=IND_tot.FE_hh$weight, breaks=seq(0,70, 0.25), xlim=c(0, 70))
weighted.hist(IND_tot.FE_hh$income/IND_tot.FE_hh$hh_size, w=IND_tot.FE_hh$weight, breaks=seq(0,7000, 10), xlim=c(0, 7000))

weighted.hist(decile_h$tot.GJ.dir.pcap, w=decile_h$weight, breaks=seq(0,50, 0.25), xlim=c(0, 30))
weighted.hist(decile_l$tot.GJ.dir.pcap, w=decile_l$weight, breaks=seq(0,50, 0.25), xlim=c(0, 30))


## Check: Probably we also need to remove 


xmax = 70
d <- density(IND_tot.FE_hh$tot.GJ.pcap, weights = IND_tot.FE_hh$weight)
plot(d, axes=FALSE, xlim=c(0, xmax), main=' ')
axis(side = 1, at = seq(0,xmax,2))
axis(side = 2)

abline(v=avg.FE.hh$tot.GJ.pcap, col="lightgray", lty="dotted")

ggplot(IND_tot.FE_hh.All %>% filter(income.pcap <20000), aes(x=income.pcap, y=tot.GJ.pcap)) +
  geom_point(size=0.01) + coord_trans(y="log10", x="log10")
