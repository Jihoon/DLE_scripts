
### Arkaitz/Richard Question on EXIO

# exio.industry <- 86:96 # 104:Steel, 108:Alu, 101: Cement, 100: Brick, 86:96: Chemicals, 97:103: non-matallic mineral, 106:115: non-ferrous metal
# exio.industry <- 97:103 # 104:Steel, 108:Alu, 101: Cement, 100: Brick, 86:96: Chemicals, 97:103: non-matallic mineral, 106:115: non-ferrous metal
exio.industry <- 106:115 # 104:Steel, 108:Alu, 101: Cement, 100: Brick, 86:96: Chemicals, 97:103: non-matallic mineral, 106:115: non-ferrous metal

# compare.ei <- data.frame(carrier.name.fin, 
#                          IND.DFE=dfe.exio[,IND_idx_ex[exio.industry]], 
#                          IND.DNE=dne.exio[,IND_idx_ex[exio.industry]], 
#                          IND.DFEI=dfei.exio[,IND_idx_ex[exio.industry]], 
#                          IND.TFEI=tfei.exio[,IND_idx_ex[exio.industry]], 
#                          IND.TPEI=tpei.nature[,IND_idx_ex[exio.industry]], 
#                          IND.TPEI.USE=tpei.USE[,IND_idx_ex[exio.industry]],
#                          IND.TPEI.net=tnei.exio[,IND_idx_ex[exio.industry]], 
#                          
#                          BRA.DFE=dfe.exio[,BRA_idx_ex[exio.industry]], 
#                          BRA.DNE=dne.exio[,BRA_idx_ex[exio.industry]], 
#                          BRA.DFEI=dfei.exio[,BRA_idx_ex[exio.industry]], 
#                          BRA.TFEI=tfei.exio[,BRA_idx_ex[exio.industry]], 
#                          BRA.TPEI=tpei.nature[,BRA_idx_ex[exio.industry]], 
#                          BRA.TPEI.USE=tpei.USE[,BRA_idx_ex[exio.industry]], 
#                          BRA.TPEI.net=tnei.exio[,BRA_idx_ex[exio.industry]], 
#                          
#                          ZAF.DFE=dfe.exio[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.DNE=dne.exio[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.DFEI=dfei.exio[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.TFEI=tfei.exio[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.TPEI=tpei.nature[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.TPEI.USE=tpei.USE[,ZAF_idx_ex[exio.industry]], 
#                          ZAF.TPEI.net=tnei.exio[,ZAF_idx_ex[exio.industry]])
compare.ei <- data.frame(carrier.name.fin, 
                         IND.DFE=dfe.exio[,IND_idx_ex[exio.industry]], 
                         BRA.DFE=dfe.exio[,BRA_idx_ex[exio.industry]], 
                         ZAF.DFE=dfe.exio[,ZAF_idx_ex[exio.industry]])
compare.ei <- compare.ei %>% rbind(data.frame(carrier.name.fin="Elec.total", t(colSums(compare.ei[idx_elec,-1])))) %>%
  rbind(data.frame(carrier.name.fin="FE.total", t(colSums(compare.ei[,-1]))))
names(compare.ei) <- c("Carrier", paste("IND", EX_catnames[exio.industry]), paste("BRA", EX_catnames[exio.industry]), paste("ZAF", EX_catnames[exio.industry]))

# view(compare.ei)
if (exio.industry[1]==86) {
  write.csv(compare.ei, "H:/MyDocuments/Analysis/Final energy/Arkaitz/For Debug/IND-BRA-ZAF Chemical and petrochemical.csv")
} else if (exio.industry[1]==97) {
  write.csv(compare.ei, "H:/MyDocuments/Analysis/Final energy/Arkaitz/For Debug/IND-BRA-ZAF Non-matallic minerals.csv")
} else {
  write.csv(compare.ei, "H:/MyDocuments/Analysis/Final energy/Arkaitz/For Debug/IND-BRA-ZAF Non-ferrous metals.csv")
}
