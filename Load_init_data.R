#########################
### IND
#########################
# IND_FD_ALL
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_AllHHConsump.Rda")
# IND_HH
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_HH.Rda")
# IND_FD_ICP_AllHH
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_FD_harmonized.Rda")
#IND_FD_ICP_usd2007
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_FD_ICP_usd2007.Rda")

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND_FD_ICP_HH_adj.Rda")

# # IND2_HH
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_HH.Rda")
# # IND2_FD_ALL
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND2_AllHHConsump.Rda")

IND_fd_exio_pp <- get_purch_price(IND_fd_exio, "IN")
scaler_IND <- sum(IND_FD_ICP_usd2007[,1]) / sum(IND_fd_exio_pp)
init_FD_IND <- IND_FD_ICP_usd2007[,1] / scaler_IND



#########################
### BRA
#########################

# BRA_FD_ALL
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_AllHHConsump.Rda")
# BRA_HH
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_HH.Rda")
# BRA_FD_ICP_AllHH
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_FD_harmonized.Rda")
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_FD_ICP_usd2007.Rda")

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/BRA_FD_ICP_HH_adj_BR.Rda")

# All in M.USD 2007
BRA_fd_exio_pp_EX <- get_purch_price(BRA_fd_exio, "BR")
scaler_BRA <- sum(BRA_FD_ICP_usd2007[,1]) / sum(BRA_fd_exio_pp_EX)
init_FD_BRA <- BRA_FD_ICP_usd2007[,1] / scaler_BRA



#########################
### ZAF1 - ZAF_HH
#########################

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/ZAF_HH.Rda")
# ZAF1 - ZAF_FD
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/ZAF_FD.Rda")
# ZAF1 - ZAF_FD_ALL
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/ZAF_AllHHConsump.Rda")
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/ZAF_FD_ICP_usd2007.Rda")

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/ZAF_FD_ICP_HH_adj.Rda")

ZAF_fd_exio_pp <- get_purch_price(ZAF_fd_exio, "ZA")
scaler_ZAF <- sum(ZAF_FD_ICP_usd2007[,1]) / sum(ZAF_fd_exio_pp)
init_FD_ZAF <- ZAF_FD_ICP_usd2007[,1] / scaler_ZAF
