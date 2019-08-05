# Run this once Init.R is run.

org_wd <- getwd()
EX_industry <- t(read.table(paste(path_sut, "mrUse_version2.2.2.txt", sep=""), header=FALSE, sep="\t", dec=".", nrows=1, skip=1)[3+1:163])
EX_industry <- data.frame(ID=1:length(EX_industry), IND=EX_industry)

datapath <- 'C:/Users/min/IIASA/DLE - Documents/Air Pollution/'
setwd(datapath)
PM_ext_i <- readr::read_csv('PM_by_IO_Industry_v2019-06-06_11-35-10.csv') %>% rename("IND" = "Item") # 'PM_by_IO_Industry_v20190318.csv'

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

# 2. put it in IND's position in 1x9800 vector. (direct emission intensity extension) all other cells are zero. (No import emissions)
s_PM <- rep(0, 9800) # stressor
s_PM[IND_idx_ex] <- PM_ext_p$PM_2_5

# Make the direct intensity values in ug/m3 to ug/m3/M.EUR
y <- 1/tot_demand # tot_demand in M.EUR
y[is.infinite(y)] <- 0 
PM_int <- s_PM * y   # These vectors are one dimensional.

# 3. multiply with L_inverse to get indirect intensities (1x9800) : s*L
st_PM <- eigenMapMatMult(t(matrix(PM_int)), as.matrix(L_inverse)) # indirect stressor (need to only care for India domestic consumption)
# View(data.frame(EXIO=EX_catnames , ind.int=rowSums(matrix(st_PM, nrow=200))))
# View(data.frame(EXIO=EX_catnames , dir.int=rowSums(matrix(PM_int, nrow=200))))

# 4. go through DeriveIntensities to have ind.intensity by coicop/icp (this gives domestic indirect int by icp) in ug/m3/M.USD2010

# 4.0 Refine IND_FD_ICP_io.yr to zero out fuelwood consumption (because it is too informal to be included in IO analysis.)
IND_FD_ICP_io.yr[which(ICP_catnames=="Firewood and other fuels"),] = 0 # Used in rIPFP

list[IND.tPM.icp, IND.alloc, NC_IND, IND_FD_adj] <- DeriveIntensities('IND', 'final', final.intensity.mat=st_PM)

# 4. multiply it with fd (by any percentile appended with updated cooking fuel consumption, 164xn_percentile) or each percentile can be a diag mtx to
#   be able to see the effect of each consumption product.
scaler_IND.nofw <- sum(IND_FD_ICP_io.yr[,1]) / sum(get_purch_price(IND_fd_exio, "IN")) # New scaler without fuelwood
totPM.IND.sum <- eigenMapMatMult(t(as.matrix(colMeans(IND.tPM.icp))), IND_FD_ICP_io.yr) / scaler_IND.nofw
totPM.IND.icp <- data.frame(eigenMapMatMult(diag(colMeans(IND.tPM.icp)), IND_FD_ICP_io.yr) / scaler_IND.nofw)
totPM.IND.icp.diag <- data.frame(eigenMapMatMult(diag(colMeans(IND.tPM.icp)), diag(IND_FD_ICP_io.yr[,1])) / scaler_IND.nofw)

names(totPM.IND.icp) <- c('Total', paste0('D', 1:10)) 
totPM.IND.icp <- totPM.IND.icp %>% mutate(ineq = D10/D1, ICP_catnames) %>% filter(!is.nan(ineq)) 

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
  mutate(decile=factor(decile, levels=paste0('D', 1:10)))

library(directlabels)

dlist <- list(ineq=totPM.highlight.ineq, D10=totPM.highlight.D10, D1=totPM.highlight.D1, Tot=totPM.highlight.tot)
for (d in 1:4) {
  
  pdf(file = paste0("PM2.5_by_decile_", names(dlist)[d], ".pdf"), width = 7, height = 7)
  ggplot(data=dlist[[d]], aes(x=decile, y=PM2.5)) +
    geom_line(aes(group=ICP_catnames, color=ICP_catnames), size=1) + 
    scale_color_manual(values=brewer.pal(10, "Paired"), guide = 'none') +
    # geom_dl(aes(label = ICP_catnames), method = list("last.points", hjust = 0.9)) + 
    geom_text(data=dlist[[d]]%>%filter(decile=="D6"), aes(x=decile, y=PM2.5, label = ICP_catnames), hjust=0, vjust=1) +
    scale_y_log10() + labs(y='log PM2.5')
  dev.off()
  
}

write.xlsx(totPM.IND.icp, "PM2.5 by decile - Indirect.nofw.xlsx")

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


