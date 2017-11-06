
# India
IND_HH_Alldata <-selectDBdata(tables='IND1_HH')
IND_FOOD_Alldata <-selectDBdata(tables='IND1_FOOD')
IND_FUEL_Alldata <-selectDBdata(tables='IND1_FUEL')
save(IND_HH_Alldata, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_HH_All.Rda")
save(IND_FOOD_Alldata, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_Food_All.Rda")
save(IND_FUEL_Alldata, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_FUEL_Alldata.Rda")

IND_FD <- readFinalDemandfromDBbyDecile('IND1')
IND2_FD <- readFinalDemandfromDBbyDecile('IND2')
BRA_FD <- readFinalDemandfromDBbyDecile('BRA0')
BRA1_FD <- readFinalDemandfromDBbyDecile('BRA1')

list[IND_FD_ALL, IND_HH] <- readFinalDemandfromDBAllHH('IND1')
IND_FD_ALL <- data.table(IND_FD_ALL)
IND_HH <- data.table(IND_HH, key="hhid")
setorder(IND_HH, hhid)

list[IND2_FD_ALL, IND2_HH] <- readFinalDemandfromDBAllHH('IND2')
IND2_FD_ALL <- data.table(IND2_FD_ALL)
IND2_HH <- data.table(IND2_HH, key="hhid")
setorder(IND2_HH, hhid)

list[BRA_FD_ALL, BRA_HH] <- readFinalDemandfromDBAllHH('BRA0')
BRA_FD_ALL <- data.table(BRA_FD_ALL)
BRA_HH <- data.table(BRA_HH, key="hhid")
setorder(BRA_HH, hhid)

list[BRA2_FD_ALL, BRA2_HH] <- readFinalDemandfromDBAllHH('BRA2')
BRA2_FD_ALL <- data.table(BRA2_FD_ALL)
BRA2_HH <- data.table(BRA2_HH, key="hhid")
setorder(BRA2_HH, hhid)

save(IND_FD, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_FD.Rda")
save(IND_FD_ALL, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_AllHHConsump.Rda")
# save(IND_FD_ALL, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_AllHHConsump_prcadj.Rda")
save(IND2_FD_ALL, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_AllHHConsump.Rda")
save(BRA_FD_ALL, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_AllHHConsump.Rda")
save(IND_HH, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_HH.Rda")
save(IND2_HH, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_HH.Rda")
save(BRA_HH, file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_HH.Rda")