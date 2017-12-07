###########################################
## Functions for intensity analysis ##

# Intensity under ICP classification
# In the end, I will replace the codes above with this function.
DeriveIntensities <- function(country='IND', type='final', final.intensity.mat=indirect_fE_int) {
  icp_fd_cty_usd <- eval(parse(text=paste0(country, "_FD_ICP_usd2007")))
  
  list[result_all, NC_all, FD_adj] <- Run_rIPFP(bridge_ICP_EXIO_q[,-1], country)
  final_alloc_list_all <- lapply(result_all, func1)
  
  alloc_nonRAS <- get_bridge_COICOP_EXIO(bridge_ICP_EXIO_q[,-1], n_draw)
  inten_RAS_all <- SetupSectorIntensities(final_alloc_list_all, NC_all, countrycode(country,"iso3c", "iso2c"), type, final.intensity.mat)
  nonRAS_all <- SetupSectorIntensities(alloc_nonRAS, NC_all, countrycode(country,"iso3c", "iso2c"), type, final.intensity.mat)
  
  no_expense <- which((rowSums(bridge_ICP_EXIO_q[,-1])!=0) & (icp_fd_cty_usd[,1]==0))
  no_expense <- no_expense[!(no_expense %in% grep("UNBR", ICP_catnames))]   # Remove UNBR items
  # inten_comb_all <- inten_RAS_all
  # inten_comb_all[,no_expense] <- nonRAS_all[,no_expense]
  
  return(list(inten_RAS_all, final_alloc_list_all, NC_all, FD_adj))
}


# Allocation ratio in cells - summing up to 1 each row
func1 <- function (x) {
  a <- diag(1/rowSums(x))
  a[is.infinite(a)] <- 0
  x <- a %*% x
}

GetHHSectoralEnergyPerCap <- function(idx, country='IND', fd_HH, int_sect) {
  xlcFreeMemory()
  scaler <- eval(parse(text=paste0("scaler_",country)))
  eHH_sect <- t(int_sect[,idx, drop=FALSE] %*% fd_HH[idx, , drop=FALSE] / 1000) / scaler # MJ to GJ / HH
  
  xlcFreeMemory()
  eHH_sect <- data.table(hhid = colnames(fd_HH), eHH_sect)
  idx_end <- dim(eHH_sect)[2]
  setkey(eHH_sect, hhid)
  
  xlcFreeMemory()
  cty_HH <- eval(parse(text=paste0(country, "_HH")))
  eHH_sect <- merge(eHH_sect, cty_HH, by="hhid") 
  setkey(eHH_sect, hhid, consumption)
  
  # eHH.per.cap.by.decile <- eHH_sect %>% group_by(decile) %>% mutate(pop=weight*hh_size) %>% mutate_at(vars(V1:V500), funs(.*weight)) %>% 
  #   summarise_at(vars(V1:V500,pop), sum) %>% mutate_at(vars(V1:V500), funs(./pop))
  
  xlcFreeMemory()
  # Data.table doesn't look reliable. The column division doesn't work!!! (WTF)
  eHH_cap_sect <- eHH_sect
  eHH_cap_sect <- eHH_cap_sect[, 2:idx_end := eHH_cap_sect[,2:idx_end, with=FALSE] / hh_size]  # idx_end instead of (n_draw+1) because of no-converge runs
  eHH_cap_sd <- data.table(hhid = colnames(fd_HH),
                           sd = apply(eHH_cap_sect[,2:idx_end, with=FALSE], 1, sd),
                           eHH_cap_sect[,(idx_end+1):dim(eHH_sect)[2], with=FALSE])
  
  # eHH_cap_sect <- data.frame(eHH_sect)
  # eHH_cap_sect[, 2:idx_end] <- eHH_cap_sect[, 2:idx_end] / eHH_cap_sect$hh_size # idx_end instead of (n_draw+1) because of no-converge runs
  # eHH_cap_sd <- data.table(hhid = colnames(fd_HH),
  #                      sd = apply(eHH_cap_sect[,2:idx_end], 1, sd),
  #                      eHH_cap_sect[,(idx_end+1):dim(eHH_sect)[2]])
  
  return(list(eHH_cap_sect, eHH_cap_sd))
}



IndirecIntensitiesByICPSect <- function(sect_idx, cty) {
  
  # sect_idx <- grep(ICP_sect_name, ICP_catnames, ignore.case = TRUE)
  print(ICP_catnames[sect_idx])
  map_idx <- which(bridge_ICP_EXIO_q[sect_idx,-1]==1)
  cty_alloc <- eval(parse(text=paste0(countrycode(cty,"iso2c", "iso3c"), "_alloc")))
  sect_alloc <- do.call("rbind", lapply(cty_alloc, '[', sect_idx,))
  
  cty_place <- which(exio_ctys==cty)
  # cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  # cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  
  all_bp_idx <- c(map_idx, trd_idx, trp_idx)
  own_ex_idx <- (cty_place-1)*200 + all_bp_idx
  alloc <- apply(sect_alloc[,all_bp_idx], 2, mean)
  
  indirec.int <- colSums(indirect_E_int[,own_ex_idx]) * EXR_EUR$r  # MJ/USD2007
  direc.int <- colSums(energy_int[,own_ex_idx]) * EXR_EUR$r  # MJ/USD2007
  
  comp <- rbind(as.integer(all_bp_idx), indirec.int, direc.int, alloc)
  
  print(map_idx)
  print(comp)  # MJ/Eur
  
  return(comp)
}


# Get by-decile summary (mean,sd) of any eHH
SummarizeGJPerCapByDecile <- function(eHH) {
  nColTot <- dim(eHH)[2]
  nColAdd <- dim(BRA_HH)[2]-1   # we do this because # of drawn columns are not equal to n_draw because of no convergences in rIPFP
  
  eHH_sum <- data.table(hhid = eHH[,1, with=FALSE],
                        avg = apply(eHH[,2:(nColTot-nColAdd), with=FALSE], 1, mean), # , with=FALSE all removed (data.table issue)
                        sd = apply(eHH[,2:(nColTot-nColAdd), with=FALSE], 1, sd),
                        eHH[,(nColTot-nColAdd+1):nColTot, with=FALSE])
  
  # eHH_sum <- eHH_sum %>% group_by(decile) %>% summarise(u=weighted.mean(avg, weight), sd=weighted.mean(sd, weight))
  eHH_sum <- eHH_sum %>% mutate(tothhE = avg*hh_size*weight, sdE = sd*hh_size*weight) %>% group_by(decile) %>% 
    summarise(u = sum(tothhE)/sum(weight*hh_size), sd=sum(sdE)/sum(weight*hh_size)) #, pop=sum(weight*hh_size)
  return(eHH_sum[,-1])
}

SummarizeGJPerCap <- function(eHH) {
  nColTot <- dim(eHH)[2]
  nColAdd <- dim(BRA_HH)[2]-1   # we do this because # of drawn columns are not equal to n_draw because of no convergences in rIPFP
  
  eHH_sum <- data.table(hhid = eHH[,1, with=FALSE],
                        avg = apply(eHH[,2:(nColTot-nColAdd), with=FALSE], 1, mean), # , with=FALSE all removed (data.table issue)
                        sd = apply(eHH[,2:(nColTot-nColAdd), with=FALSE], 1, sd),
                        eHH[,(nColTot-nColAdd+1):nColTot, with=FALSE])
  
  # eHH_sum <- eHH_sum %>% group_by(decile) %>% summarise(u=weighted.mean(avg, weight), sd=weighted.mean(sd, weight))
  eHH_sum <- eHH_sum %>% mutate(tothhE = avg*hh_size*weight, sdE = sd*hh_size*weight) %>%
    summarise(u = sum(tothhE)/sum(weight*hh_size), sd=sum(sdE)/sum(weight*hh_size)) #, pop=sum(weight*hh_size)
  return(eHH_sum)
}

cbind.dt.simple = function(...) {
  x = c(...)
  setattr(x, "class", c("data.table", "data.frame"))
  ans = .Call(data.table:::Calloccolwrapper, x, max(100L, ncol(x) + 64L), FALSE)
  .Call(data.table:::Csetnamed, ans, 0L)
}



###########################
### Plotting functions  ###
###########################
PlotIntensityHist <- function (intens_HH, name="V", xmax, bin_size=0.1, drawline=TRUE, linedata, ticksize=10) {
  xlcFreeMemory()
  
  opar <- par() 
  
  par(mfrow=c(10,1), oma = c(0, 0, 0, 0), mar= c(2, 0, 0, 0))
  
  for (i in 1:10) {
    a <- as.matrix(intens_HH %>% filter(decile==paste0("decile",i)) %>% select(starts_with(name)))
    w0 <- intens_HH$weight[intens_HH$decile==paste0("decile",i)]
    w1 <- w0[rep(1:length(w0), each=dim(a)[2])]   # dim(a)[2] instead of n_draw because of non-converge runs
    # weighted.hist(a, w1, seq(0, max(a)+bin_size, bin_size), xlim=c(0,xmax), main=NULL, xaxis = FALSE)
    d<- density(a, weights = w1)
    plot(d, axes=FALSE, xlim=c(1,xmax), main=' ')
    axis(side = 1, at = seq(0,xmax,ticksize))
    if (drawline) {
      abline(v=linedata[i,1], col="red")    
    }
  }
  par(opar)
}


PlotIncomeByCountry <- function (f_HH, xmax, bin_size=0.1, tick_size = 1) {  # bin/tick size in kilo USD
  xlcFreeMemory()
  
  opar <- par() 
  
  par(mfrow=c(10,1), oma = c(0, 0, 0, 0), mar= c(2, 0, 0, 0))
  
  for (i in 1:10) {
    a <- as.matrix(f_HH %>% filter(decile==paste0("decile",i)) %>% select(consumption)) / 1000 # 1000 USD/yr
    w0 <- f_HH$weight[f_HH$decile==paste0("decile",i)]
    w1 <- w0[rep(1:length(w0), each=dim(a)[2])]   # dim(a)[2] instead of n_draw because of non-converge runs
    weighted.hist(a, w1, seq(0, max(a)+bin_size, bin_size), xlim=c(0,xmax), main=NULL, xaxis = FALSE)
    axis(side = 1, at = seq(0, xmax, tick_size), cex.axis=1.5)
  }
  
  par(opar)
}



PlotMainHistBRA <- function (intens_HH, name="V", xmax, bin_size=0.1, linedata, ticksize=10) {
  xlcFreeMemory()
  
  # opar <- par() 
  layout(matrix(1:11, ncol=1))
  
  par(oma = c(0, 0, 2, 0), mar= c(2, 0, 0, 0))
  
  for (i in 1:10) {
    a <- as.matrix(intens_HH %>% filter(decile==paste0("decile",i)) %>% select(starts_with(name)))
    w0 <- intens_HH$weight[intens_HH$decile==paste0("decile",i)]
    w1 <- w0[rep(1:length(w0), each=dim(a)[2])]   # dim(a)[2] instead of n_draw because of non-converge runs
    # weighted.hist(a, w1, seq(0, max(a)+bin_size, bin_size), xlim=c(0,xmax), freq=FALSE, main=NULL, xaxis = FALSE)
    d<- density(a, weights = w1); d$y <- d$y/sum(d$y) 
    plot(d, axes=FALSE, xlim=c(1,xmax), main=' ')
    axis(side = 1, at = seq(0,xmax,ticksize))
    axis(side = 2)
    
    abline(v=(seq(0,250,10)), col="lightgray", lty="dotted")
    
    m1 <- linedata[i,8]; m2 <- linedata[i,6]; m3 <- linedata[i,4];m4 <- linedata[i,2];
    sd1 <- linedata[i,9]; sd2 <- linedata[i,7]; sd3 <- linedata[i,5]; sd4 <- linedata[i,3];
    
    # Uncertainty bands
    polygon(x=c(m1-2*sd1, m1+2*sd1, m1+2*sd1, m1-2*sd1), y=c(0,0,1,1), col=rgb(1,0,0,0.3), border=NA)
    polygon(x=c(m2-2*sd2, m2+2*sd2, m2+2*sd2, m2-2*sd2), y=c(0,0,1,1), col=rgb(0,0,1,0.3), border=NA)
    polygon(x=c(m3-2*sd3, m3+2*sd3, m3+2*sd3, m3-2*sd3), y=c(0,0,1,1), col=rgb(0.3,0.8,0.3,0.3), border=NA)
    polygon(x=c(m4-2*sd4, m4+2*sd4, m4+2*sd4, m4-2*sd4), y=c(0,0,1,1), col=rgb(1,0,1,0.3), border=NA)
    
    # Mean lines
    abline(v=m1,lwd=1, col="red")    
    abline(v=m2,lwd=1, col="blue")    
    abline(v=m3,lwd=1, col=rgb(0.3,0.8,0.3,1))    
    abline(v=m4,lwd=1, col="purple")    
  }
  
  plot.new()
  legend(x="center", ncol=4, c("ownVal-adjFD", "defVal-adjFD", "ownVal-orgFD", "defVal-orgFD"), 
         lty = 1, lwd=2, col=c("red", "blue", rgb(0.3,0.8,0.3,1), "purple"), 
         y.intersp=0.4, cex = 1, bty = "n", 
         pt.bg = c(rgb(1,0,0,0.3),rgb(0,0,1,0.3),rgb(0.3,0.8,0.3,0.3),rgb(1,0,1,0.3)))
  
  # par(opar)
}


PlotMainHistIND <- function (intens_HH, name="V", xmax, bin_size=0.1, linedata, ticksize=10) {
  xlcFreeMemory()
  
  layout(matrix(1:11, ncol=1))
  par(oma = c(0, 0, 2, 0), mar= c(2, 0, 0, 0))
  
  for (i in 1:10) {
    a <- as.matrix(intens_HH %>% filter(decile==paste0("decile",i)) %>% select(starts_with(name)))
    w0 <- intens_HH$weight[intens_HH$decile==paste0("decile",i)]
    w1 <- w0[rep(1:length(w0), each=dim(a)[2])]   # dim(a)[2] instead of n_draw because of non-converge runs
    # weighted.hist(a, w1, seq(0, max(a)+bin_size, bin_size), xlim=c(0,xmax), freq=FALSE, main=NULL, xaxis = FALSE)
    d<- density(a, weights = w1); d$y <- d$y/sum(d$y) 
    plot(d, axes=FALSE, xlim=c(1,xmax), main=' ')
    axis(side = 1, at = seq(0,xmax,ticksize))
    axis(side = 2)
    
    abline(v=(seq(0,100,10)), col="lightgray", lty="dotted")
    
    m1 <- linedata[i,12]; m2 <- linedata[i,10];
    sd1 <- linedata[i,13]; sd2 <- linedata[i,11]; 
    
    # Uncertainty bands
    polygon(x=c(m1-2*sd1, m1+2*sd1, m1+2*sd1, m1-2*sd1), y=c(0,0,1,1), col=rgb(1,0,0,0.3), border=NA)
    polygon(x=c(m2-2*sd2, m2+2*sd2, m2+2*sd2, m2-2*sd2), y=c(0,0,1,1), col=rgb(0,0,1,0.3), border=NA)
    
    # Mean lines
    abline(v=m1,lwd=1, col="red")    
    abline(v=m2,lwd=1, col="blue")    
  }
  plot.new()
  legend(x="center", ncol=2, c("adjFD", "orgFD"), 
         lty = 1, lwd=2, col=c("red", "blue"), 
         y.intersp=0.4, cex = 1, bty = "n", 
         pt.bg = c(rgb(1,0,0,0.3),rgb(0,0,1,0.3)))
}




PlotNonfuelIntensity <- function(intensity_mtx, noexp, ymax, titlename) {
  
  divider <- c(2, 8, 14, 17, 22, 25, 28, 32, 37, 
               40, 47, 55, 65, 84, 95, 112, 116, 134, 135, 137)
  idx_section_name <- c(divider)+1
  section_name <- icp_ntnu$ICP_Heading[idx_section_name]
  section_name <- gsub("UNBR ", "", section_name)
  section_name[19] <- "Restaurants and hotels"
  
  nonFuelIntensity <- intensity_mtx[,1:151]
  
  n_sector_nonfuel <- 151
  noexp_nonfuel <- noexp[noexp<=151]
  
  # Non-fuel
  boxplot(nonFuelIntensity, range=0, ylim=c(0, ymax), axes = FALSE, add=FALSE)
  col_div <- c(par("usr")[1], divider+0.5, par("usr")[2])
  
  # Paint alternating colors
  for(i in 1:(length(col_div)-1)) { 
    color_bgn <- c("gray60", "gray15")[i %% 2]
    rect(col_div[i], par("usr")[3], col_div[i+1], par("usr")[4],col = color_bgn, border=FALSE)  
  }
  boxplot(nonFuelIntensity, ylab ="Primary energy intensity [MJ/2007USD]", 
          axes = FALSE, ylim=c(0, ymax), add=TRUE, cex.lab=1.3, range=0)
  # axis(side = 1, at = seq(1,n_sector_icp,10))
  title(xlab ="Consumption items", line=1, cex.lab=1.3) 
  axis(side = 2, at = seq(0,ymax,20), cex.axis=1.1)
  
  idx_section <- c(divider)+1
  
  text(idx_section-1, y=40, section_name, pos=4, offset=0.8, cex = 1, srt = 90)
  text(1:n_sector_nonfuel+0.5, y=apply(nonFuelIntensity, 2, max), 1:n_sector_nonfuel, pos=3, offset=1, cex = 0.7, srt = 90)
  text(noexp_nonfuel+0.9, y=apply(nonFuelIntensity[,noexp_nonfuel], 2, max), '+', pos=3, offset=2, cex = 1.2, srt = 90)
  title(titlename)
  
  pp <- recordPlot()
  
  return(pp)
}

PlotFuelIntensity <- function(intensity_mtx, noexp, ymax) {
  # Fuel
  opar <- par()
  
  par(mar=c(10,4,1,1))
  FuelIntensity <- intensity_mtx[,152:164]
  noexp_fuel <- noexp[noexp>151]-151
  
  FuelLabel <- DLE_fuelnames_std$item
  FuelLabel[2] <- "Charcoal/coal/brisquette"
  FuelLabel[6] <- "Fuelwood"
  FuelLabel[8] <- "Fuel oil"
  
  boxplot(FuelIntensity, axes = FALSE, ylim=c(0, ymax), add=FALSE, cex.lab=1.3, range=0, ylab ="Primary energy intensity [MJ/2007USD]")
  
  axis(side = 2, at = seq(0,ymax,50), cex.axis=1.1)
  text(noexp_fuel, y=apply(FuelIntensity[,noexp_fuel], 2, max), '+', pos=3, offset=1, cex = 1.2, srt = 90)
  axis(side = 1, at = 1:dim(DLE_fuelnames_std)[1], labels=FuelLabel, las=2, srt = 45)
  # axis(side = 1, at = 1:dim(DLE_fuelnames_std)[1], labels=FALSE)
  # text(x=1:dim(DLE_fuelnames_std)[1], y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), cex = 0.8, labels=FuelLabel, srt = 45, xpd=TRUE, adj=1)
  
  par(opar)
  pp <- recordPlot()
  
  return(pp)
}
