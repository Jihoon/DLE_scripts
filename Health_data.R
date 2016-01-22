# WHO health outcome 
fpath <- "H:/MyDocuments/Health/Data/"
cm <- read.csv(paste(fpath,"WHO - Child mortality.csv", sep=""), header=TRUE)
le <- read.csv(paste(fpath,"WHO - Life expectancy.csv", sep=""), header=TRUE)
av <- read.csv(paste(fpath,"WHO - Median availability of selected generic medicines.csv", sep=""), header=TRUE)
ex <- read.csv(paste(fpath,"WHo - Health expenditure per capita, all countries, selected years.csv", sep=""), header=TRUE)
wd <- read.csv(paste(fpath,"WHO - Health workforce Density per 1000.csv", sep=""), header=TRUE)
infra <- read.csv(paste(fpath,"WHO - Health infrastructure.csv", sep=""), header=TRUE)
me <- read.csv(paste(fpath,"WHO - Medical equipment.csv", sep=""), header=TRUE)

cm <- cm[cm$Year=="2012",1:3]
names(cm)[2:3] <- c("YearCM", "InfMort")
a <- str_split(cm$InfMort, " ")
cm$InfMort <- as.numeric(unlist(a)[seq(1,2*length(a),2)])

le <- le[-1,1:3]
names(le) <- c("Country", "YearLE", "LifeExp")
le$YearLE <- as.numeric(levels(le$YearLE))[le$YearLE]
le$LifeExp <- as.numeric(levels(le$LifeExp))[le$LifeExp]
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
  
me <- me[,1:4]
names(me)[2:4] <- c("YearEquip", "MRIPer1M", "CTPer1M")
me <- aggregate(.~Country, me, head, 1) 

CtyPerf <- Reduce(function(...) merge(..., all=T, by="Country"), 
       list(le, cm, av, ex, wd, infra, me))
CtyPerf$PerfIdx <- (CtyPerf$cmIdx + CtyPerf$leIdx)/2


# OECD health data
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

CtyPerf <- merge(CtyPerf, OECD, all=TRUE, by="Country")

plot(CtyPerf$PhysicianPer1000, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$TotExpPerCapita, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$GovExpPerCapita, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$MRIPer1M, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$CTPer1M, CtyPerf$PerfIdx, ylab="Performance index", pch=14)

plot(CtyPerf$Bed, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$Physician, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$GradMed, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$ExpPerCapita, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$OopExpPerCapita, CtyPerf$PerfIdx, ylab="Performance index", pch=14)

plot(CtyPerf$NursePer1000, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$DentistPer1000, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$PharmPer1000, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$MedPer1000, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$MedicineAvailPrv, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$HospitalPer1M, CtyPerf$PerfIdx, ylab="Performance index", pch=14)
plot(CtyPerf$HealthPosts, CtyPerf$PerfIdx, ylab="Performance index", pch=14)

plot(CtyPerf$PhysicianPer1000, CtyPerf$TotExpPerCapita, pch=14)
plot(CtyPerf$PhysicianPer1000, CtyPerf$GovExpPerCapita, pch=14)
plot(CtyPerf$MRIPer1M, CtyPerf$TotExpPerCapita, pch=14)
plot(CtyPerf$CTPer1M, CtyPerf$TotExpPerCapita, pch=14)
plot(CtyPerf$CTPer1M, CtyPerf$GovExpPerCapita, pch=14)

# Simple models
# Assuming a sigmoid functional form
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000 
              + NursePer1000 + MRIPer1M 
              + HospitalPer1M + HealthPosts, CtyPerf)
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000 
              + HealthPosts, CtyPerf)
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000, CtyPerf)
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000 + DentistPer1000, CtyPerf)
fit_who <- lm(log(1/PerfIdx - 1) ~ TotExpPerCapita + PhysicianPer1000, CtyPerf)
summary(fit_who)
crPlots(fit_who)
vif(fit_who)
CtyPerf$prediction <- 1/ (exp(predict(fit_who, CtyPerf))+1)
CtyPerf[,c("Country", "PerfIdx", "prediction")]
plot(CtyPerf$PerfIdx, CtyPerf$prediction)
attach(CtyPerf)
a<-scatterplot3d(TotExpPerCapita, PhysicianPer1000, log(1/PerfIdx - 1), pch=14, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")
a$plane3d(fit_who)
stat.desc(CtyPerf[CtyPerf$PerfIdx > 0.8,]) # Gives avg 2245 PPP$/capita and 2.66 Phy/1000

fit_oecd <- lm(log(1/PerfIdx - 1) ~ ExpPerCapita + Physician + Nurse + GradMed + Bed + MRI, CtyPerf)
summary(fit_oecd)
vif(fit_oecd)
crPlots(fit_oecd)
