# Run this once Init.R is run.
setwd("H:/MyDocuments/IO work/DLE_scripts")  # Change if run from OneDrive "/IIASA/DLE - Documents/WS2 - Documents/Analysis/IO/DLE_scripts/"
org_wd <- getwd()
EX_industry <- t(read.table(paste(path_sut, "mrUse_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", nrows=1, skip=1)[3+1:163])
EX_industry <- data.frame(ID=1:length(EX_industry), IND=EX_industry)

datapath <- 'C:/Users/min/IIASA/DLE - Documents/Air Pollution/'
setwd(datapath)

case <- 'policy' # 'base' # 
if (case=='base') {
  PM_ext_i <- readr::read_csv('PM_by_IO_Industry_v2019-06-17_16-27-39.csv') %>% rename("IND" = "Item") # 'PPM_by_IO_Industry_v2019-06-06_11-35-10'
} else if (case=='policy') {
  PM_ext_i <- readr::read_csv('PM_MFR_by_IO_Industry_v2019-12-04_09-23-33.csv') %>% rename("IND" = "Item") # 'PPM_by_IO_Industry_v2019-06-06_11-35-10'
}
  

# For the 1st round result, there are only 127 industries assigned with emissions values. (Household emissions yet to come)
PM_ext_i <- EX_industry %>% full_join(PM_ext_i)
PM_ext_i$PM_2_5[is.na(PM_ext_i$PM_2_5)] <- 0

i2p_EXIO <- read_xlsx('EXIOBASE20p_EXIOBASE20i.xlsx') %>% slice(1:200)
p_names <-i2p_EXIO %>% select(1) 
i2p_EXIO <- i2p_EXIO %>% select(-1)
i_names <- names(i2p_EXIO)

# Mapping mtx indicating which industries will be broken down.
ind2map <- which(colSums(i2p_EXIO)>1) # only indexes
ind2map_mtx <- as.matrix(i2p_EXIO %>% select(ind2map))

# India domestic primary energy total (direct)
totPE <- colSums(dne.exio[,IND_idx_ex])

a <- diag(totPE) %*% ind2map_mtx
ind2map_share <- a %*% diag(1/colSums(a))
i2p_EXIO[,ind2map] <- ind2map_share

# Convert i to p with 200 entries (i-to-p conversion)
PM_ext_p <- rowSums(as.matrix(i2p_EXIO) %*% diag(PM_ext_i$PM_2_5))
PM_ext_p <- data.frame(p_names, PM_2_5=PM_ext_p)

### 2. put it in IND's position in 1x9800 vector. (direct emission intensity extension) all other cells are zero. (No import emissions)
s_PM <- rep(0, 9800) # stressor
s_PM[IND_idx_ex] <- PM_ext_p$PM_2_5

# Make the direct intensity values in ug/m3 to ug/m3/M.EUR
y <- 1/tot_demand # tot_demand in M.EUR
y[is.infinite(y)] <- 0 
PM_int <- s_PM * y   # These vectors are one dimensional.

### 3. multiply with L_inverse to get indirect intensities (1x9800) : s*L
st_PM <- eigenMapMatMult(t(matrix(PM_int)), as.matrix(L_inverse)) # indirect stressor (need to only care for India domestic consumption)
# View(data.frame(EXIO=EX_catnames , ind.int=rowSums(matrix(st_PM, nrow=200))))
# View(data.frame(EXIO=EX_catnames , dir.int=rowSums(matrix(PM_int, nrow=200))))

### 4. go through DeriveIntensities to have ind.intensity by coicop/icp (this gives domestic indirect int by icp) in ug/m3/M.USD2010

IND_FD_ICP_io.yr[which(ICP_catnames=="Firewood and other fuels"),] = 0 # Used in rIPFP - This will also make the intensity to zero.

# 4.0 Refine IND_FD_ICP_io.yr to zero out fuelwood consumption (because it is too informal to be included in IO analysis.)
# This IND.tPM.icp is returned as MJ/USD (MER @IO.year)
list[IND.tPM.icp, IND.alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND', 'final', final.intensity.mat=st_PM)
saveRDS(IND.tPM.icp, file=paste0('IND.tPM.icp.', case,'.Rds'))
saveRDS(IND_FD_adj, file=paste0('IND_FD_adj.', case,'.Rds'))
# 4.1 multiply it with fd (by any percentile appended with updated cooking fuel consumption, 164xn_percentile) or each percentile can be a diag mtx to
#   be able to see the effect of each consumption product.

scaler_IND.nofw <- sum(IND_FD_ICP_io.yr[,1]) / sum(get_purch_price(IND_fd_exio, "IN")) # New scaler without fuelwood

# 4.1.1. For nationwide total FD
totPM.IND.sum <- eigenMapMatMult(t(as.matrix(colMeans(IND.tPM.icp))), IND_FD_ICP_io.yr) / scaler_IND.nofw
totPM.IND.icp <- data.frame(eigenMapMatMult(diag(colMeans(IND.tPM.icp)), IND_FD_ICP_io.yr) / scaler_IND.nofw)
totPM.IND.icp.diag <- data.frame(eigenMapMatMult(diag(colMeans(IND.tPM.icp)), diag(IND_FD_ICP_io.yr[,1])) / scaler_IND.nofw)

names(totPM.IND.icp) <- c('Total', paste0('D', 1:10)) 
totPM.IND.icp <- totPM.IND.icp %>% mutate(ineq = D10/D1, ICP_catnames) %>% filter(!is.nan(ineq)) 

# 4.1.2. For urban/rural FD separately
list[IND_FD.UrbRur, IND_pop.UrbRur] <- readFinalDemandfromDBbyDecile.UrbRur('IND1')
IND_pop.UrbRur <- IND_pop.UrbRur %>% ungroup() %>% mutate(decile=factor(decile, labels=paste0('D', 1:10)))
IND_FD.Urb <- IND_FD.UrbRur %>% filter(urban==1)
IND_FD.Rur <- IND_FD.UrbRur %>% filter(urban==0)
IND_pop.Urb <- IND_pop.UrbRur %>% filter(urban==1)
IND_pop.Rur <- IND_pop.UrbRur %>% filter(urban==0)

  # Urban ICP
  IND_FD.Urb_code <- IND_FD.Urb[-grep("taxes", IND_FD.Urb$item, ignore.case = TRUE), ]
  IND_FD.Urb_code <- merge(IND_FD.Urb_code[1:(dim(IND_FD.Urb_code)[1]-n_CES_fuel),], IND_map %>% select(CODE, item=ITEM_DLE), 
                       by="item", all.x = TRUE) %>% arrange(CODE) %>%
    rbind(IND_FD.Urb[-(1:(dim(IND_FD.Urb)[1]-n_CES_fuel)),]%>% mutate(CODE=999))    # Fuels are temprarily assigned to 999. (not used)
  IND_FD.Urb_code[is.na(IND_FD.Urb_code)] <- 0
  
  IND_FD.Urb_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD.Urb_code[1:(dim(IND_FD.Urb_code)[1]-n_CES_fuel), 3:13]) %>%
    rbind(IND_FD.Urb_code[-(1:(dim(IND_FD.Urb_code)[1]-n_CES_fuel)), 3:13]) # for all deciles and total
  IND_FD.Urb_ICP <- as.matrix(IND_FD.Urb_ICP)
  
  IND_FD_ICP_svy.yr.U <- IND_FD.Urb_ICP * PPP_IND / CPI_ratio_IND / EXR_IND / 1e6 # to M.USD at IO.year (MER)  - consumption quantity at the survey year (IND1:2011)
  IND_FD_ICP_io.yr.U <- IND_FD_ICP_svy.yr.U / IND_con_grwth # M.USD 2007 (MER)  - consumption quantity at the common IO year (EXIO2:2007)

  # Rural ICP
  IND_FD.Rur_code <- IND_FD.Rur[-grep("taxes", IND_FD.Rur$item, ignore.case = TRUE), ]
  IND_FD.Rur_code <- merge(IND_FD.Rur_code[1:(dim(IND_FD.Rur_code)[1]-n_CES_fuel),], IND_map %>% select(CODE, item=ITEM_DLE), 
                           by="item", all.x = TRUE) %>% arrange(CODE) %>%
    rbind(IND_FD.Rur[-(1:(dim(IND_FD.Rur)[1]-n_CES_fuel)),]%>% mutate(CODE=999))    # Fuels are temprarily assigned to 999. (not used)
  IND_FD.Rur_code[is.na(IND_FD.Rur_code)] <- 0
  
  IND_FD.Rur_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD.Rur_code[1:(dim(IND_FD.Rur_code)[1]-n_CES_fuel), 3:13]) %>%
    rbind(IND_FD.Rur_code[-(1:(dim(IND_FD.Rur_code)[1]-n_CES_fuel)), 3:13]) # for all deciles and total
  IND_FD.Rur_ICP <- as.matrix(IND_FD.Rur_ICP)
  
  IND_FD_ICP_svy.yr.R <- IND_FD.Rur_ICP * PPP_IND / CPI_ratio_IND / EXR_IND / 1e6 # to M.USD at IO.year (MER)  - consumption quantity at the survey year (IND1:2011)
  IND_FD_ICP_io.yr.R <- IND_FD_ICP_svy.yr.R / IND_con_grwth # M.USD 2007 (MER)  - consumption quantity at the common IO year (EXIO2:2007)
  
  totPM.IND.sum.U <- eigenMapMatMult(t(as.matrix(colMeans(IND.tPM.icp))), IND_FD_ICP_io.yr.U) / scaler_IND.nofw
  totPM.IND.icp.U <- data.frame(eigenMapMatMult(diag(colMeans(IND.tPM.icp)), IND_FD_ICP_io.yr.U) / scaler_IND.nofw)
  totPM.IND.icp.diag.U <- data.frame(eigenMapMatMult(diag(colMeans(IND.tPM.icp)), diag(IND_FD_ICP_io.yr.U[,1])) / scaler_IND.nofw)
  
  totPM.IND.sum.R <- eigenMapMatMult(t(as.matrix(colMeans(IND.tPM.icp))), IND_FD_ICP_io.yr.R) / scaler_IND.nofw
  totPM.IND.icp.R <- data.frame(eigenMapMatMult(diag(colMeans(IND.tPM.icp)), IND_FD_ICP_io.yr.R) / scaler_IND.nofw)
  totPM.IND.icp.diag.R <- data.frame(eigenMapMatMult(diag(colMeans(IND.tPM.icp)), diag(IND_FD_ICP_io.yr.R[,1])) / scaler_IND.nofw)
  
  names(totPM.IND.icp.U) <- c('Total', paste0('D', 1:10)) 
  totPM.IND.icp.U <- totPM.IND.icp.U %>% mutate(ineq = D10/D1, ICP_catnames) %>% filter(!is.nan(ineq)) 
  
  names(totPM.IND.icp.R) <- c('Total', paste0('D', 1:10)) 
  totPM.IND.icp.R <- totPM.IND.icp.R %>% mutate(ineq = D10/D1, ICP_catnames) %>% filter(!is.nan(ineq)) 
  

totPM.highlight.ineq <- totPM.IND.icp %>% arrange(-ineq) %>% slice(c(1:5)) %>% 
  gather(key="decile", value="PM2.5", D1:D10) %>% arrange(ICP_catnames) %>% select(-ineq, -Total) %>% 
  mutate(decile=factor(decile, levels=paste0('D', 1:10))) %>% filter(PM2.5!=0)

totPM.highlight.D10 <- totPM.IND.icp %>% arrange(-D10) %>% slice(c(1:5)) %>% 
  gather(key="decile", value="PM2.5", D1:D10) %>% arrange(ICP_catnames) %>% select(-ineq, -Total) %>% 
  mutate(decile=factor(decile, levels=paste0('D', 1:10)))

totPM.highlight.D1 <- totPM.IND.icp %>% arrange(-D1) %>% slice(c(1:5)) %>% 
  gather(key="decile", value="PM2.5", D1:D10) %>% arrange(ICP_catnames) %>% select(-ineq, -Total) %>% 
  mutate(decile=factor(decile, levels=paste0('D', 1:10)))

totPM.highlight.tot <- totPM.IND.icp %>% arrange(-Total) %>% slice(c(1:8)) %>% 
  gather(key="decile", value="PM2.5", D1:D10) %>% arrange(ICP_catnames) %>% select(-ineq, -Total) %>% 
  mutate(decile=factor(decile, levels=paste0('D', 1:10))) %>% left_join(IND_pop.UrbRur %>% group_by(decile) %>% summarise(Pop=sum(Pop))) %>% mutate(PM.pcap = PM2.5/Pop)

  totPM.highlight.tot.U <- totPM.IND.icp.U %>% arrange(-Total) %>% slice(c(1:8)) %>% 
    gather(key="decile", value="PM2.5", D1:D10) %>% arrange(ICP_catnames) %>% select(-ineq, -Total) %>% 
    mutate(decile=factor(decile, levels=paste0('D', 1:10))) %>% left_join(IND_pop.Urb) %>% mutate(PM.pcap = PM2.5/Pop) 
  
  totPM.highlight.tot.R <- totPM.IND.icp.R %>% arrange(-Total) %>% slice(c(1:8)) %>% 
    gather(key="decile", value="PM2.5", D1:D10) %>% arrange(ICP_catnames) %>% select(-ineq, -Total) %>% 
    mutate(decile=factor(decile, levels=paste0('D', 1:10))) %>% left_join(IND_pop.Rur) %>% mutate(PM.pcap = PM2.5/Pop)

library(directlabels)

dlist <- list(ineq=totPM.highlight.ineq, D10=totPM.highlight.D10, D1=totPM.highlight.D1, Tot=totPM.highlight.tot, 
              Tot.R=totPM.highlight.tot.R, Tot.U=totPM.highlight.tot.U)

for (d in 1:6) {
  pdf(file = paste0("./Draft results/Indirect/PM2.5_by_decile_", names(dlist)[d], " ",case, ".pdf"), width = 7, height = 7)
  p <- ggplot(data=dlist[[d]], aes(x=decile, y=PM2.5)) +
    geom_line(aes(group=ICP_catnames, color=ICP_catnames), size=1) +
    scale_color_manual(values=brewer.pal(10, "Paired"), guide = 'none') +
    # geom_dl(aes(label = ICP_catnames), method = list("last.points", hjust = 0.9)) +
    geom_text(data=dlist[[d]]%>%filter(decile=="D6"), aes(x=decile, y=PM2.5, label = ICP_catnames), hjust=0, vjust=1) +
    scale_y_log10() + labs(y='log PM2.5')
  print(p)
  dev.off()
}
# Per capita - Urban-Rural separated
for (d in 4:6) {
  pdf(file = paste0("./Draft results/Indirect/PM2.5_by_decile_pcap_", names(dlist)[d], " ", case, ".pdf"), width = 7, height = 7)
  p <- ggplot(data=dlist[[d]], aes(x=decile, y=PM.pcap)) +
    geom_line(aes(group=ICP_catnames, color=ICP_catnames), size=1) +
    scale_color_manual(values=brewer.pal(10, "Paired"), guide = 'none') +
    # geom_dl(aes(label = ICP_catnames), method = list("last.points", hjust = 0.9)) +
    geom_text(data=dlist[[d]]%>%filter(decile=="D6"), aes(x=decile, y=PM.pcap, label = ICP_catnames), hjust=0, vjust=1) +
    labs(y=paste('log PM2.5 per cap:', names(dlist)[d])) +
    scale_y_log10(limits = c(5e-11, 1e-8))
  print(p)
  dev.off()
}
# 
# ggplot(data=totPM.highlight.tot.R, aes(x=decile, y=PM.pcap)) +
#   geom_line(aes(group=ICP_catnames, color=ICP_catnames), size=1) + 
#   scale_color_manual(values=brewer.pal(10, "Paired"), guide = 'none') +
#   # geom_dl(aes(label = ICP_catnames), method = list("last.points", hjust = 0.9)) + 
#   geom_text(data=totPM.highlight.tot.R%>%filter(decile=="D6"), aes(x=decile, y=PM.pcap, label = ICP_catnames), hjust=0, vjust=1) +
#   scale_y_log10(limits = c(5e-11, 1e-8)) + labs(y='log PM2.5 per cap: Rural') 
# 
# ggplot(data=totPM.highlight.tot.U, aes(x=decile, y=PM.pcap)) +
#   geom_line(aes(group=ICP_catnames, color=ICP_catnames), size=1) + 
#   scale_color_manual(values=brewer.pal(10, "Paired"), guide = 'none') +
#   # geom_dl(aes(label = ICP_catnames), method = list("last.points", hjust = 0.9)) + 
#   geom_text(data=totPM.highlight.tot.U%>%filter(decile=="D6"), aes(x=decile, y=PM.pcap, label = ICP_catnames), hjust=0, vjust=1) +
#   scale_y_log10(limits = c(5e-11, 1e-8)) + labs(y='log PM2.5 per cap: Urban') 


write.xlsx(totPM.IND.icp, paste0("./Draft results/Indirect/PM2.5 by decile - Indirect.no.fuelwood - ", case, ".xlsx"))
write.xlsx(rbind(totPM.IND.icp.U %>% mutate(urban=1), totPM.IND.icp.R %>% mutate(urban=0)), 
           paste0("./Draft results/Indirect/PM2.5 by decile - Indirect.no.fuelwood.U_R - ", case, ".xlsx"))

a <- data.frame(p_names, dir=PM_int[IND_idx_ex], indir=st_PM[IND_idx_ex]) %>% mutate(diff=indir-dir, r=diff/indir)

setwd(org_wd)



### Save consumption data by decile - M.USD in IO.year (2010 for now) at MER
IND.con.by.decile <- IND_FD_ICP_io.yr / scaler_IND.nofw
BRA.con.by.decile <- BRA_FD_ICP_io.yr / scaler_BRA
ZAF.con.by.decile <- ZAF_FD_ICP_io.yr / scaler_ZAF

xlsx::write.xlsx(data.frame(item=ICP_catnames, IND.con.by.decile), paste0(datapath, "Consumption by decile.xlsx"), sheetName="IND")
xlsx::write.xlsx(data.frame(item=ICP_catnames, BRA.con.by.decile), paste0(datapath, "Consumption by decile.xlsx"), sheetName="BRA", append=TRUE)
xlsx::write.xlsx(data.frame(item=ICP_catnames, ZAF.con.by.decile), paste0(datapath, "Consumption by decile.xlsx"), sheetName="ZAF", append=TRUE)



### Look into some details per COICOP item
# This shows which IO industry is emitting a lot for the given ICP item (idx_icp).
bridge <- read.xlsx(paste0("C:/Users/min/IIASA/DLE - Documents/Air Pollution/IND.bridge.EXIO-COICOP.xlsx"))
tot.FD.ex.IND <- t(bridge[,-1]) %*% diag(IND_FD_ICP_io.yr[,1])  / scaler_IND.nofw#200x164: total FD in exio products (no deciles)
tot.FD.ex.IND.adj <- t(bridge[,-1]) %*% diag(IND_FD_adj)  # IND_FD_adj is already scaled.
# idx_icp <- which(ICP_catnames == "Firewood and other fuels")
idx_icp <- which(ICP_catnames == "Furniture and furnishings")
tot.FD.ex.sector <- rep(0, 9800) # stressor
tot.FD.ex.sector[IND_idx_ex] <- tot.FD.ex.IND[, idx_icp]
tot.production.ex.sector <-  eigenMapMatMult(as.matrix(L_inverse), matrix(tot.FD.ex.sector, ncol=1)) # embodied economic output from FD of idx_icp
tot.PM.ex.sector <- (tot.production.ex.sector * PM_int)[IND_idx_ex] # indirect emission from the consumption of the icp sector #idx_icp
View(data.frame(exio=EX_catnames, tot.x=tot.production.ex.sector[IND_idx_ex], pm.int=PM_int[IND_idx_ex], tot.PM.ex.sector))

# For all ICP items
a <- matrix(0, nrow=length(EX_catnames), ncol=length(ICP_catnames))
for (i in 1:length(ICP_catnames)) {
  tot.FD.ex.sector <- rep(0, 9800) # stressor
  tot.FD.ex.sector[IND_idx_ex] <- tot.FD.ex.IND.adj[, i]
  tot.production.ex.sector <-  eigenMapMatMult(as.matrix(L_inverse), matrix(tot.FD.ex.sector, ncol=1)) # embodied economic output from FD of idx_icp
  tot.PM.ex.sector <- (tot.production.ex.sector * PM_int)[IND_idx_ex] # indirect emission from the consumption of the icp sector #idx_icp
  a[,i] = tot.PM.ex.sector
}
a <- data.frame(a) %>% mutate(EXIO=EX_catnames) %>% select(EXIO, everything())
names(a)[-1] = ICP_catnames
view(a)

# Check out the breakdown among FD sectors
PM_t = st_PM %*% as.matrix(final_demand[,IND_idx_fd])
PM_t / sum(PM_t) # 53:47 for 2010 (hh consumption vs other)
