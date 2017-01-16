# Read in PPP rates (using openxlsx library) because of an error for XLConnect loadWorkbook function
# Probably need to go for either library, not both
wb <- openxlsx::loadWorkbook("P:/ene.general/DecentLivingEnergy/Surveys/International Comparison Program 2011/ICP2011 Data for Researchers_June22-2015 [IIASA].xlsx")
a <- openxlsx::readWorkbook(wb, "PPPs (AGG)", colNames = FALSE, rowNames = FALSE, startRow = 7)
ctry_code <- t(a[1,c(3:dim(a)[2])])
PPP_GDP <- t(as.numeric(a[2,c(3:dim(a)[2])]))
PPP_health <- t(as.numeric(a[9,c(3:dim(a)[2])]))
PPP_health_scaler <- as.numeric(PPP_health/PPP_GDP)
PPP_health_scaler[is.na(PPP_health_scaler)] <- 0
options(stringsAsFactors = FALSE)
PPP_scaler <- data.frame(ctry_code, PPP_health_scaler)
names(PPP_scaler) <- c("code", "PPP_scaler")

# WHO health outcome 
fpath <- "H:/MyDocuments/Health/Data/"
cm <- read.csv(paste(fpath,"WHO - Child mortality.csv", sep=""), header=TRUE)
le <- read.csv(paste(fpath,"WHO - Life expectancy.csv", sep=""), header=TRUE)
av <- read.csv(paste(fpath,"WHO - Median availability of selected generic medicines.csv", sep=""), header=TRUE)
ex <- read.csv(paste(fpath,"WHo - Health expenditure per capita, all countries, selected years.csv", sep=""), header=TRUE)
wd <- read.csv(paste(fpath,"WHO - Health workforce Density per 1000.csv", sep=""), header=TRUE)
infra <- read.csv(paste(fpath,"WHO - Health infrastructure.csv", sep=""), header=TRUE)
me <- read.csv(paste(fpath,"WHO - Medical equipment.csv", sep=""), header=TRUE)
hb <- read.csv(paste(fpath,"UN - Hospital beds.csv", sep=""), header=TRUE, nrows=326)
hb_wb <- read.csv(paste(fpath,"WB - Hospital beds.csv", sep=""), header=TRUE, nrows=269, skip=4) %>% select(2,X2008:X2012)

cm <- cm[cm$Year=="2012",1:3]
names(cm)[2:3] <- c("YearCM", "InfMort")
a <- str_split(cm$InfMort, " ")
cm$InfMort <- as.numeric(unlist(a)[seq(1,2*length(a),2)])

le <- le[-1,1:3]
names(le) <- c("Country", "YearLE", "LifeExp")
le[,2] <- as.numeric(le[,2])
le[,3] <- as.numeric(le[,3])
# le$YearLE <- as.numeric(levels(le$YearLE))[le$YearLE]
# le <- aggregate(.~Country, le, head, 1)
# le$LifeExp <- as.numeric(levels(le$LifeExp))[le$LifeExp]
le <- le[le$YearLE==2012,]

cm$cmIdx <- (cm$InfMort-105)/(1-105)
le$leIdx <- (le$LifeExp - 45)/(85-45)

# Cleasing, reformatting
av <- av[-1,]
names(av) <- c("Country", "MedicineAvailPrv", "MedicineAvailPub")

ex <- cbind(ex[-1,1], ex[-1,ex[1,]==2012])
ex <- ex[,c(1,3,5)] # Only PPP
names(ex) <- c("Country", "TotExpPerCapita", "GovExpPerCapita")

a <- aggregate(.~Country, wd[,1:3], head, 1)
b <- aggregate(.~Country, wd[,c(1,2,4)], head, 1)
c <- aggregate(.~Country, wd[,c(1,2,5)], head, 1)
d <- aggregate(.~Country, wd[,c(1,2,6)], head, 1)
wd <- Reduce(function(...) merge(..., all=T, by="Country"), list(a, b, c, d))
# wd <- merge(a, b, by="Country", all=T)
names(wd) <- c("Country", "YearPhy", "PhysicianPer1000", "YearNurs", "NursePer1000", 
               "YearDent", "DentistPer1000", "YearPharm", "PharmPer1000")
wd$MedPer1000 <- rowSums(wd[,c("PhysicianPer1000","NursePer1000","DentistPer1000","PharmPer1000")])

infra <- infra[infra$Year=="2013",]
# infra <- aggregate(.~Country, infra[,1:3], head, 1)
names(infra) <- c("Country", "YearHosp", "HospitalPer1M" ,"HealthPosts", "HealthCentres", "DistRuralHosp", 
                  "ProvHosp" ,"SpecHosp")

hb <- hb %>% select(-4) %>% mutate(Value=Value/10)   # Originally beds per 10,000
names(hb) <- c("Country", "Year", "BedPer1000")
# hb <- hb %>% filter(Year==2009)   # Most countries in 2009

names(hb_wb)[1] <- "code"
# hb_wb <- hb_wb %>% rowwise() %>% mutate(Avg = mean(c(X2010, X2011, X2012), rm.na=TRUE))
hb_wb <- hb_wb %>% mutate(BedPer1000 = rowMeans(.[,-1], na.rm=TRUE)) %>% filter(!is.nan(BedPer1000)) %>%
  select(-starts_with("X")) #%>% filter(!grepl("income",Country))

me <- me[,1:4]
names(me)[2:4] <- c("YearEquip", "MRIPer1M", "CTPer1M")
me <- aggregate(.~Country, me, head, 1) 

CtyPerf <- Reduce(function(...) merge(..., all=T, by="Country"), 
       list(le, cm, av, ex, wd, infra, me))
CtyPerf$code <- countrycode(CtyPerf$Country, "country.name", "iso3c")
CtyPerf <- CtyPerf %>% left_join(hb_wb)
CtyPerf <- merge(CtyPerf, PPP_scaler, by="code")

# Scale health expenditures (from WHO) by health PPP rates
CtyPerf$TotExpPerCapita <- CtyPerf$TotExpPerCapita * CtyPerf$PPP_scaler
CtyPerf$GovExpPerCapita <- CtyPerf$GovExpPerCapita * CtyPerf$PPP_scaler

# Import population data
a <- read.fwf(paste(fpath,"population_rawdata_2119.txt", sep=""), widths=c(-7,45,-6,13), 
              header=FALSE, colClasses="character")
a$code <- countrycode(a$V1, "country.name", "iso3c")
names(a)[1:2] <- c("Country", "Pop2015")
a$Pop2015 <- as.numeric(gsub(",","", a$Pop2015))
CtyPerf <- merge(CtyPerf, a[,2:3], by="code")

# Import GDP data
a <- read.csv(paste(fpath,"GDP_per_capita_2012.csv", sep=""), header=TRUE, skip = 4)
a <- a[,c("Country.Name", "Country.Code", "X2012")]
names(a) <- c("Country", "code", "GDP2012")
CtyPerf <- merge(CtyPerf, a[,2:3], by="code")

CtyPerf$PerfIdx <- (CtyPerf$cmIdx + CtyPerf$leIdx)/2
CtyPerf$FacilitiesPer1M <- CtyPerf$HospitalPer1M + CtyPerf$HealthCentres + CtyPerf$HealthPosts
CtyPerf$GovSupport <- CtyPerf$GovExpPerCapita / CtyPerf$TotExpPerCapita
CtyPerf$HospitalPer1M[CtyPerf$HospitalPer1M > 40] <- NA   # Wrong entry for Guinea-Bissau? (HospitalPer1M > 50)
CtyPerf$HealthExpRatio <- CtyPerf$TotExpPerCapita/CtyPerf$GDP2012

# Set country code
CtyPerf$ColIdx <- ceiling(CtyPerf$PerfIdx*1000) 
Col <- femmecol(1000)

CtyPerf <- CtyPerf[CtyPerf$Pop2015 > 1000000,]  # Remove small countries

attach(CtyPerf)

# Plots

opar <- par() 
par(mar=c(5.1,4.1,4.1,2.1))
figure_path = "H:/MyDocuments/Health/Plot/"

pdf(file = paste(figure_path, "totexp_hsi.pdf", sep=""), width=7, height=5)
plot(TotExpPerCapita, PerfIdx, xlab="Total health exp/capita [PPP 2012$]", ylab="Health status index", pch=14)
dev.off()

pdf(file = paste(figure_path, "phy_hsi.pdf", sep=""), width=7, height=5)
plot(PhysicianPer1000, PerfIdx, xlab="Physicians per 1000", ylab="Health status index", pch=14)
dev.off()

png(file = paste(figure_path, "totexp_le.png", sep=""), width = 581, height = 453, units = "px")
plot(TotExpPerCapita, LifeExp, xlab="Total health exp/capita [PPP 2012$]", ylab="Life Expectancy", pch=14)
dev.off()

png(file = paste(figure_path, "totexp_im.png", sep=""), width = 581, height = 453, units = "px")
plot(TotExpPerCapita, InfMort, xlab="Total health exp/capita [PPP 2012$]", ylab="Child Mortality (<5 yr)", pch=14)
dev.off()

png(file = paste(figure_path, "phy_le.png", sep=""), width = 581, height = 453, units = "px")
plot(PhysicianPer1000, LifeExp, xlab="Physicians per 1000 population", ylab="Life Expectancy", pch=14)
dev.off()

png(file = paste(figure_path, "phy_im.png", sep=""), width = 581, height = 453, units = "px")
plot(PhysicianPer1000, InfMort, xlab="Physicians per 1000 population", ylab="Infant Mortality", pch=14)
dev.off()


plot(CtyPerf$BedPer1000, CtyPerf$PerfIdx, xlab="Number of hospital beds per 1000", ylab="Health status index", pch=14)

median(CtyPerf$PerfIdx[CtyPerf$BedPer1000>1], na.rm = T)

#   plot(GovExpPerCapita, PerfIdx, ylab="Performance index", pch=14)
#   plot(MRIPer1M, PerfIdx, ylab="Performance index", pch=14)
#   plot(CTPer1M, PerfIdx, ylab="Performance index", pch=14)
#   
#   plot(Bed, PerfIdx, ylab="Performance index", pch=14)
#   plot(Physician, PerfIdx, ylab="Performance index", pch=14)
#   plot(GradMed, PerfIdx, ylab="Performance index", pch=14)
#   plot(ExpPerCapita, PerfIdx, ylab="Performance index", pch=14)
#   plot(OopExpPerCapita, PerfIdx, ylab="Performance index", pch=14)
#   
#   plot(NursePer1000, PerfIdx, ylab="Performance index", pch=14)
#   plot(DentistPer1000, PerfIdx, ylab="Performance index", pch=14)
#   plot(PharmPer1000, PerfIdx, ylab="Performance index", pch=14)
#   plot(MedPer1000, PerfIdx, ylab="Performance index", pch=14)
#   plot(MedicineAvailPrv, PerfIdx, ylab="Performance index", pch=14)
#   plot(HospitalPer1M, PerfIdx, ylab="Performance index", pch=14)
#   plot(GovSupport, PerfIdx, ylab="Performance index", pch=14)
#   plot(HealthPosts, PerfIdx, ylab="Performance index", pch=14)
#   
#   plot(PhysicianPer1000, TotExpPerCapita, pch=14)
#   plot(PhysicianPer1000, GovExpPerCapita, pch=14)
#   plot(MRIPer1M, TotExpPerCapita, pch=14)
#   plot(CTPer1M, TotExpPerCapita, pch=14)
#   plot(CTPer1M, GovExpPerCapita, pch=14)
  
plot(TotExpPerCapita, -log(2/(PerfIdx+1) - 1), ylab="Performance index", pch=14)
plot(log(TotExpPerCapita), -log(2/(PerfIdx+1) - 1), ylab="Performance index", pch=14)
plot(log(PhysicianPer1000), -log(2/(PerfIdx+1) - 1), ylab="Performance index", pch=14)
plot(log(TotExpPerCapita), exp(PerfIdx*2), ylab="Performance index", pch=14)
plot((TotExpPerCapita), exp(PerfIdx*9), ylab="Performance index", pch=14)
plot(PhysicianPer1000, exp(PerfIdx*9), ylab="Performance index", pch=14)
plot(TotExpPerCapita, log(1/PerfIdx - 1), ylab="Performance index", pch=14)
plot(TotExpPerCapita, 1/PerfIdx, ylab="Performance index", pch=14)
plot(TotExpPerCapita, PerfIdx, ylab="Performance index", pch=14)

# LE vs. IM vs. TotExp

plot(LifeExp, InfMort, col=Col[CtyPerf$ColIdx], xlab="Life Expectancy", ylab="Infant Mortality (<5 yr)", pch=19)
text(LifeExp, InfMort, CtyPerf$code, cex=0.6, pos=4)

plot(LifeExp, InfMort, col=Col[CtyPerf$ColIdx], xlab="Life Expectancy", ylab="Infant Mortality (<5 yr)", 
     xlim=c(70, 85), ylim=c(0, 40), pch=19)
text(LifeExp, InfMort, CtyPerf$code, cex=0.6, pos=4)

radius <- sqrt(TotExpPerCapita/pi)

pdf(file = paste(figure_path, "le_im_bubble_all.pdf", sep=""), width = 10, height = 7)
# png(file = paste(figure_path, "le_im_bubble_all.png", sep=""), width = 581, height = 453, units = "px")
symbols(LifeExp, InfMort, circles=radius, 
        inches=0.26, fg="white", bg=Col[CtyPerf$ColIdx], xlab="Life Expectancy", ylab="Child Mortality (<5 yr)")
text(LifeExp, InfMort, CtyPerf$code, cex=0.6, pos=4)
rect(64,-1,86,40, lty = 3)
colorbar.plot(52, -7, 1:1000, strip.width = 0.03, strip.length = 0.5, 
              zrange = NULL, adj.x = 0.5, adj.y = 0.5, col = femmecol(1000), 
              horizontal = TRUE)
text(44, -3, "0", cex=0.6, pos=4)
text(58.5, -3, "1", cex=0.6, pos=4)
text(48, -2.5, "Health status index", cex=0.8, pos=4)
legend("topright", title="Total health expenditure per capita (PPP 2012$)",
  legend=c("1000", "5000", "10000"), 
  pch = 16,
  bty = "n",
  col = "red",
  y.intersp = 2.5,    
  x.intersp = 2,         
  pt.cex = sqrt(c(1000, 5000, 10000)/pi)/sqrt(8845/pi)/0.15,
  cex = 0.8
)
dev.off()

pdf(file = paste(figure_path, "le_im_bubble_zoomed.pdf", sep=""), width = 10, height = 7)
symbols(LifeExp, InfMort, circles=radius, 
        inches=0.26, fg="white", bg=Col[CtyPerf$ColIdx], 
        xlab="Life Expectancy", ylab="Child Mortality (<5 yr)",
        xlim=c(65, 85), ylim=c(0, 40))
text(LifeExp, InfMort, CtyPerf$code, cex=0.6, pos=4)
rect(64.8,-2,86,25, lty = 3)
rect(73.8,-2,86,15.3, lty = 3)
text(81, 24, "minimum-performance", cex=0.8, pos=4, font=4)
text(81, 14.3, "decent-performance", cex=0.8, pos=4, font=4)
colorbar.plot(69, 0, 501:1000, strip.width = 0.03, strip.length = 0.5, 
              zrange = NULL, adj.x = 0.5, adj.y = 0.5, col = femmecol(1000)[501:1000], 
              horizontal = TRUE)
text(65.5, 1.2, "0.5", cex=0.6, pos=4)
text(71.9, 1.2, "1", cex=0.6, pos=4)
text(67.3, 1.5, "Health status index", cex=0.8, pos=4)
legend(
  "topright", title="Total health expenditure per capita (PPP 2012$)",
  legend=c("1000", "5000", "10000"), 
  pch = 16,
  bty = "n",
  col = "red",
  yjust = 0,
  y.intersp = 2.5,    
  x.intersp = 2,         
  pt.cex = sqrt(c(1000, 5000, 10000)/pi)/sqrt(8845/pi)/0.15,
  cex = 0.8
)
dev.off()




# 3D plots

# a<-scatterplot3d(TotExpPerCapita, PhysicianPer1000, log(1/PerfIdx - 1), pch=14, highlight.3d=TRUE,
#               type="h", main="3D Scatterplot")
# a<-scatterplot3d(TotExpPerCapita, PhysicianPer1000, PerfIdx, pch=14, highlight.3d=TRUE,
#                  type="h", main="3D Scatterplot")
# a$plane3d(fit_who)

plot3d(TotExpPerCapita, PhysicianPer1000, PerfIdx, col=Col[CtyPerf$ColIdx], size=4)
plot3d(TotExpPerCapita, PhysicianPer1000, LifeExp, col=Col[CtyPerf$ColIdx], size=4)
plot3d(TotExpPerCapita, PhysicianPer1000, InfMort, col=Col[CtyPerf$ColIdx], size=4)


# Summarize groups

a <- TotExpPerCapita[LifeExp >= 65 & InfMort <= 25]
b <- sort(a)[1:(length(a)/2)]
c<-stat.desc(a)[c(1,4,5,8,9,13)]
c<-rbind(c, stat.desc(b)[c(1,4,5,8,9,13)])

a <- TotExpPerCapita[LifeExp >= 74 & InfMort <= 15]
b <- sort(a)[1:(length(a)/2)]
c<-rbind(c, stat.desc(a)[c(1,4,5,8,9,13)])
c<-rbind(c, stat.desc(b)[c(1,4,5,8,9,13)])

a <- PhysicianPer1000[LifeExp >= 65 & InfMort <= 25]
b <- sort(a)[1:(length(a)/2)]
d<-stat.desc(a)[c(1,4,5,8,9,13)]
d<-rbind(d, stat.desc(b)[c(1,4,5,8,9,13)])

a <- PhysicianPer1000[LifeExp >= 74 & InfMort <= 15]
b <- sort(a)[1:(length(a)/2)]
d<-rbind(d, stat.desc(a)[c(1,4,5,8,9,13)])
d<-rbind(d, stat.desc(b)[c(1,4,5,8,9,13)])

write.csv(rbind(c,d), file=paste(fpath,"summary_performance.csv", sep=""))

a <- CtyPerf[CtyPerf$LifeExp >= 74 & CtyPerf$InfMort <= 15,]
a <- a[!is.na(a$code),]
a$ColIdx <- a$ColIdx - min(a$ColIdx) + 1
ColPal <- femmecol(max(a$ColIdx))

summary(a$TotExpPerCapita)
summary(a$PhysicianPer1000)
pdf(file = paste(figure_path, "totexp_phy_decent.pdf", sep=""), width = 10, height = 7)
plot(a$TotExpPerCapita, a$PhysicianPer1000, col=ColPal[a$ColIdx], 
     xlab="Total health exp per capita (PPP 2012$)", ylab="Physicians per 1000 population", pch=19)
colorbar.plot(7000, 0.5, 1:1000, strip.width = 0.03, strip.length = 0.6, 
              zrange = NULL, adj.x = 0.5, adj.y = 0.5, col = femmecol(1000), 
              horizontal = TRUE)
text(5050, 0.7, "Low", cex=0.6, pos=4)
text(8550, 0.7, "High", cex=0.6, pos=4)
text(5650, 0.8, "Relative performance within the group", cex=0.8, pos=4)
text(a$TotExpPerCapita, a$PhysicianPer1000, a$code, cex=0.6, pos=4)
title(main=list("decent-performance (LE >= 74 & CM <= 15)", font=4))
dev.off()

plot(TotExpPerCapita, GDP2012, xlab="Total health exp/capita", ylab="GDP/capita")
text(TotExpPerCapita, GDP2012, code, cex=0.6, pos=4)
title("All countries")
plot(a$TotExpPerCapita, a$GDP2012, xlab="Total health exp/capita [PPP 2012$]", ylab="GDP/capita [PPP 2012$]")
text(a$TotExpPerCapita, a$GDP2012, a$code, cex=0.6, pos=4)
title("Countries with LE >= 75 & IM <= 15")

a <- CtyPerf[CtyPerf$LifeExp >= 65 & CtyPerf$InfMort <= 25,]
a <- a[!is.na(a$code),]
a$ColIdx <- a$ColIdx - min(a$ColIdx) + 1
ColPal <- femmecol(max(a$ColIdx))

summary(a$TotExpPerCapita)
summary(a$PhysicianPer1000)
pdf(file = paste(figure_path, "totexp_phy_minimum.pdf", sep=""), width = 10, height = 7)
plot(a$TotExpPerCapita, a$PhysicianPer1000, col=ColPal[a$ColIdx], 
     xlab="Total health exp per capita (PPP 2012$)", ylab="Physicians per 1000 population", pch=19)
text(a$TotExpPerCapita, a$PhysicianPer1000, a$code, cex=0.6, pos=4)
colorbar.plot(7000, 0.5, 1:1000, strip.width = 0.03, strip.length = 0.6, 
              zrange = NULL, adj.x = 0.5, adj.y = 0.5, col = femmecol(1000), 
              horizontal = TRUE)
text(5050, 0.7, "Low", cex=0.6, pos=4)
text(8550, 0.7, "High", cex=0.6, pos=4)
text(5650, 0.8, "Relative performance within the group", cex=0.8, pos=4)
title(main=list("minimum-performance (LE >= 65 & CM <= 25)", font=4))
dev.off()

# Model fitting

# Simple models
# Assuming a sigmoid functional form
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000 
              + NursePer1000 + MRIPer1M 
              + HospitalPer1M + HealthPosts, CtyPerf)
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000 + HealthPosts, CtyPerf)
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000 + HospitalPer1M, CtyPerf)
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000 + FacilitiesPer1M, CtyPerf)
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000 + HealthPosts + GovSupport, CtyPerf)
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000 + GovSupport, CtyPerf)
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000, CtyPerf)
fit_who <- lm(log(2/(PerfIdx+1) - 1) ~ TotExpPerCapita + PhysicianPer1000, CtyPerf)
fit_who <- lm(exp(PerfIdx*9) ~ TotExpPerCapita + PhysicianPer1000 + HealthPosts  + GovSupport, CtyPerf)
fit_who <- lm(exp(PerfIdx*9) ~ TotExpPerCapita + PhysicianPer1000, CtyPerf)
fit_who <- lm(exp(PerfIdx*2) ~ log(TotExpPerCapita) + log(PhysicianPer1000), CtyPerf)
fit_who <- lm(log(2/(PerfIdx+1) - 1) ~ log(TotExpPerCapita) + log(PhysicianPer1000), CtyPerf)  # This is good
fit_who <- lm(log(2/(PerfIdx+1) - 1) ~ log(TotExpPerCapita) + log(PhysicianPer1000) + log(HospitalPer1M), CtyPerf)  # This is good

summary(fit_who)
vif(fit_who)
qqPlot(fit_who, main="QQ Plot")
ncvTest(fit_who)
spreadLevelPlot(fit_who)

prediction <- 1/ (exp(predict(fit_who, CtyPerf))+1)
CtyPerf$prediction <- log(predict(fit_who, CtyPerf))/9
CtyPerf[,c("Country", "PerfIdx", "prediction")]
plot(PerfIdx, prediction)


par(opar) 

detach(CtyPerf)



### Health energy intensity (indirect) for EXIO countries
idx_health_sectors <- seq(175, 9600, 200)
idx_edu_sectors <- seq(174, 9600, 200)
idx_rice_sectors <- seq(1, 9600, 200)
health_int <- data.frame(iso2c=exio_ctys, int = colSums(indirect_E_int[,idx_health_sectors])*EXR_EUR$r) %>% 
  left_join(WDI(country = exio_ctys, indicator = c("NY.GDP.PCAP.PP.CD", "NY.GDP.PCAP.CD"), 
                start = 2007, end = 2007, extra = FALSE, cache = NULL)) %>%
  rename(PPP=NY.GDP.PCAP.PP.CD, MER=NY.GDP.PCAP.CD) %>%
  filter(!is.na(country))
edu_int <- data.frame(iso2c=exio_ctys, int = colSums(indirect_E_int[,idx_edu_sectors])*EXR_EUR$r) %>% 
  left_join(WDI(country = exio_ctys, indicator = c("NY.GDP.PCAP.PP.CD", "NY.GDP.PCAP.CD"), 
                start = 2007, end = 2007, extra = FALSE, cache = NULL)) %>%
  rename(PPP=NY.GDP.PCAP.PP.CD, MER=NY.GDP.PCAP.CD) %>%
  filter(!is.na(country))
ggplot(data=edu_int, aes(x=PPP, y=int))+
  geom_point(size=3, aes(color="blue"))+
  theme(legend.position="none") + 
  labs(x='GDP per capita (PPP USD) in 2007',y="Energy intensity: Health sector (MJ/USD)")+
  # geom_text(data=health_int, aes(label=iso2c), hjust=0, vjust=0.5, offset=1, size=3)
  geom_text_repel(aes(label=countrycode(iso2c, "iso2c", "iso3c")), size = 3)












# Visualize how the averages change depending on different PerfIdx thresholds.
# Better not to use this because it can set too high goals (e.g. 78 yr LE?)

stat.desc(CtyPerf[PerfIdx > 0.8,]) # Gives avg 2245 PPP$/capita and 2.66 Phy/1000
stat.desc(CtyPerf[PerfIdx > 0.85,]) # Gives avg 2967 PPP$/capita and 3.10 Phy/1000
stat.desc(CtyPerf[PerfIdx > 0.9,]) # Gives avg 3753 PPP$/capita and 3.48 Phy/1000

a <- vector()
b <- vector()

for (r in seq(70, 80, 1)) {
  n <- sum(LifeExp > r)
  a <- rbind(a, mean(PhysicianPer1000[LifeExp > r], na.rm = TRUE))
  b <- rbind(b, mean(TotExpPerCapita[LifeExp > r], na.rm = TRUE))
}
# plot(seq(0.8, 0.95, 0.01), a)
# plot(seq(0.8, 0.95, 0.01), b) # Looks that 0.9 is a nice threshold
plot(seq(70, 80, 1), a)
plot(seq(70, 80, 1), b) # Looks that 0.9 is a nice threshold








# OECD health data
# Not used for now

Mapping <- system.file("OECDHealthData2012.xls", package = "XLConnect")
wb <- loadWorkbook("C:/Users/min/Dropbox/Literature/Health/OECDHealthData2012.xls")
Country  <- readWorksheet(wb, "Pharma exp., per capita US$ PPP", header=TRUE, 
                          startRow=4, endRow=38, startCol=1, endCol=1, forceConversion=T)
names(Country) <- "Country"

ExpPerCapita  <- readWorksheet(wb, "Total exp., per capita US$ PPP", header=TRUE, startRow=4, endRow=38, startCol=2, 
                               colTypes=XLC$DATA_TYPE.NUMERIC, forceConversion=FALSE)
ExpPerCapita <- ExpPerCapita[,dim(ExpPerCapita)[2]]

PubExpPerCapita  <- readWorksheet(wb, "Public exp., per capita US$ PPP", header=TRUE, startRow=4, endRow=38, startCol=2, 
                                  colTypes=XLC$DATA_TYPE.NUMERIC, forceConversion=FALSE)
PubExpPerCapita <- PubExpPerCapita[,dim(PubExpPerCapita)[2]]

OopExpPerCapita  <- readWorksheet(wb, "OOP payments, per capita US$PPP", header=TRUE, startRow=4, endRow=38, startCol=2, 
                                  colTypes=XLC$DATA_TYPE.NUMERIC, forceConversion=FALSE)
OopExpPerCapita <- OopExpPerCapita[,dim(OopExpPerCapita)[2]]

PharExpPerCapita  <- readWorksheet(wb, "Pharma exp., per capita US$ PPP", header=TRUE, startRow=4, endRow=38, startCol=2, 
                                   colTypes=XLC$DATA_TYPE.NUMERIC, forceConversion=FALSE)
PharExpPerCapita <- PharExpPerCapita[,dim(PharExpPerCapita)[2]]

Physician  <- readWorksheet(wb, "Physicians", header=TRUE, startRow=4, endRow=38, startCol=2, 
                            colTypes=XLC$DATA_TYPE.NUMERIC, forceConversion=FALSE)
Physician <- Physician[,dim(Physician)[2]]

Nurse  <- readWorksheet(wb, "Nurses", header=TRUE, startRow=4, endRow=38, startCol=2, 
                        colTypes=XLC$DATA_TYPE.NUMERIC, forceConversion=FALSE)
Nurse <- Nurse[,dim(Nurse)[2]]

GradMed  <- readWorksheet(wb, "Medical graduates", header=TRUE, startRow=4, endRow=38, startCol=2, 
                          colTypes=XLC$DATA_TYPE.NUMERIC, forceConversion=FALSE)
GradMed <- GradMed[,dim(GradMed)[2]]

GradNurse  <- readWorksheet(wb, "Nursing graduates", header=TRUE, startRow=4, endRow=38, startCol=2, 
                            colTypes=XLC$DATA_TYPE.NUMERIC, forceConversion=FALSE)
GradNurse <- GradNurse[,dim(GradNurse)[2]]

Bed  <- readWorksheet(wb, "Hospital beds", header=TRUE, startRow=4, endRow=38, startCol=2, 
                      colTypes=XLC$DATA_TYPE.NUMERIC, forceConversion=FALSE)
Bed <- Bed[,dim(Bed)[2]]

MRI  <- readWorksheet(wb, "MRI", header=TRUE, startRow=4, endRow=38, startCol=2, 
                      colTypes=XLC$DATA_TYPE.NUMERIC, forceConversion=FALSE)
MRI <- MRI[,dim(MRI)[2]]

CT  <- readWorksheet(wb, "CT scanners", header=TRUE, startRow=4, endRow=38, startCol=2,  
                     colTypes=XLC$DATA_TYPE.NUMERIC, forceConversion=FALSE)
CT <- CT[,dim(CT)[2]]

OECD <- cbind(Country, ExpPerCapita, PubExpPerCapita, OopExpPerCapita, PharExpPerCapita, Physician, Nurse,
              GradMed, GradNurse, Bed, MRI, CT)
# OECD <- Reduce(function(...) merge(..., all=T, by="Col1"), 
#        list(ExpPerCapita, PubExpPerCapita, OopExpPerCapita, PharExpPerCapita, Physician, Nurse,
#             GradMed, GradNurse, Bed, MRI, CT))
# names(OECD) <- c("Country", "ExpPerCapita", "PubExpPerCapita", "OopExpPerCapita", "PharExpPerCapita", 
#                  "Physician", "Nurse", "GradMed", "GradNurse", "Bed", "MRI", "CT")

# CtyPerf <- merge(CtyPerf, OECD, all=TRUE, by="Country")



# Plotting OECD data
fit_oecd <- lm(log(1/PerfIdx - 1) ~ ExpPerCapita + Physician + Nurse + GradMed + Bed + MRI, CtyPerf)
summary(fit_oecd)
vif(fit_oecd)