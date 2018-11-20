### Read DB for India
IND_HH_Alldata <-selectDBdata(tables='IND1_HH')
IND_FOOD_Alldata <-selectDBdata(tables='IND1_FOOD')
IND_FUEL_Alldata <-selectDBdata(tables='IND1_FUEL')
IND_OTH_Alldata <-selectDBdata(tables='IND1_OTHCON')

IND_FD <- readFinalDemandfromDBbyDecile('IND1')
IND2_FD <- readFinalDemandfromDBbyDecile('IND2')

list[IND_FD_ALL, IND_HH] <- readFinalDemandfromDBAllHH('IND1')
IND_FD_ALL <- data.table(IND_FD_ALL)
IND_HH <- data.table(IND_HH, key="hhid")
setorder(IND_HH, hhid)

list[IND2_FD_ALL, IND2_HH] <- readFinalDemandfromDBAllHH('IND2')
IND2_FD_ALL <- data.table(IND2_FD_ALL)
IND2_HH <- data.table(IND2_HH, key="hhid")
setorder(IND2_HH, hhid)

# Saving the raw data in R format (to save time)
save(IND_FD, file="./Saved tables/IND_FD.Rda")
save(IND_FD_ALL, file="./Saved tables/IND_AllHHConsump.Rda")
# save(IND_FD_ALL, file="./Saved tables/IND_AllHHConsump_prcadj.Rda")
save(IND2_FD_ALL, file="./Saved tables/IND2_AllHHConsump.Rda")

save(IND_HH, file="./Saved tables/IND_HH.Rda")
save(IND2_HH, file="./Saved tables/IND2_HH.Rda")

save(IND_HH_Alldata, file="./Saved tables/IND1_HH_All.Rda")
save(IND_FOOD_Alldata, file="./Saved tables/IND1_Food_All.Rda")
save(IND_FUEL_Alldata, file="./Saved tables/IND1_FUEL_Alldata.Rda")
save(IND_OTH_Alldata, file="./Saved tables/IND1_OTH_Alldata.Rda")




### Read DB for Brazil
BRA_FD <- readFinalDemandfromDBbyDecile('BRA0')
BRA1_FD <- readFinalDemandfromDBbyDecile('BRA1')

list[BRA_FD_ALL, BRA_HH] <- readFinalDemandfromDBAllHH('BRA0')
BRA_FD_ALL <- data.table(BRA_FD_ALL)
BRA_HH <- data.table(BRA_HH, key="hhid")
setorder(BRA_HH, hhid)

list[BRA2_FD_ALL, BRA2_HH] <- readFinalDemandfromDBAllHH('BRA2')
BRA2_FD_ALL <- data.table(BRA2_FD_ALL)
BRA2_HH <- data.table(BRA2_HH, key="hhid")
setorder(BRA2_HH, hhid)

# Saving the raw data in R format (to save time)
save(BRA_FD_ALL, file="./Saved tables/BRA_AllHHConsump.Rda")
save(BRA_HH, file="./Saved tables/BRA_HH.Rda")






### Read DB for South Africa
ZAF_FD <- readFinalDemandfromDBbyDecile('ZAF1')

list[ZAF_FD_ALL, ZAF_HH] <- readFinalDemandfromDBAllHH('ZAF1')
ZAF_FD_ALL <- data.table(ZAF_FD_ALL)
ZAF_HH <- data.table(ZAF_HH, key="hhid")
setorder(ZAF_HH, hhid)

ZAF_FOOD.raw <- selectDBdata(tables='ZAF1_FOOD') #%>% select(code, item) %>% distinct() %>% arrange(code)
ZAF_OTH.raw <- selectDBdata(tables='ZAF1_OTHCON')# %>% select(code, item) %>% distinct() %>% arrange(code)

# Saving the raw data in R format (to save time)
save(ZAF_FD, file="./Saved tables/ZAF_FD.Rda")
save(ZAF_FD_ALL, file="./Saved tables/ZAF_AllHHConsump.Rda")
save(ZAF_HH, file="./Saved tables/ZAF_HH.Rda")

save(ZAF_FOOD.raw, file="./Saved tables/ZAF_FOOD_All.Rda")
save(ZAF_OTH.raw, file="./Saved tables/ZAF_OTH_All.Rda")
