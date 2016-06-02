# Get IND final demand in ICP 

# Remove tax observations from DB
IND_FD_code <- IND_FD[-grep("taxes", IND_FD$item, ignore.case = TRUE), ]
IND_FD_code <- merge(IND_FD_code, IND_map[,c("CODE", "COICOP1", "COICOP2", "ITEM_DLE")], by.x="item", by.y="ITEM_DLE", all.x = TRUE)
IND_FD_code <- IND_FD_code[order(IND_FD_code$CODE),]
IND_FD_code[is.na(IND_FD_code)] <- 0

# For IND2
# Note: need to add COICOP1 info to get annual hh consumption increase rates.
# Some item names are different from IND1.
IND2_FD_code <- IND2_FD[-grep("taxes", IND2_FD$item, ignore.case = TRUE), ]
IND2_FD_code <- merge(IND2_FD_code, IND_map[,c("CODE", "COICOP1", "COICOP2", "ITEM_DLE")], by.x="item", by.y="ITEM_DLE", all.x = TRUE)

missing <- IND2_FD_code %>% filter(CODE==0)
write.table(missing, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
# So I assigned COICOP numbers to items only appearing in IND2.
wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/For IND2-IND1 matching.xlsx")
missing_cat  <- XLConnect::readWorksheet(wb, "Sheet1", header=TRUE, forceConversion=T) 

IND2_FD_code <- merge(IND2_FD_code, missing_cat[,c("CODE", "COICOP1", "COICOP2", "item")], by="item", all.x = TRUE)
IND2_FD_code[is.na(IND2_FD_code$CODE.x), ] <- IND2_FD_code %>% filter(is.na(CODE.x)) %>% mutate(CODE.x = CODE.y, COICOP1.x = COICOP1.y, COICOP2.x = COICOP2.y)
IND2_FD_code <- select(IND2_FD_code,-CODE.y, -COICOP1.y, -COICOP2.y) %>% rename(CODE = CODE.x, COICOP1 = COICOP1.x, COICOP2 = COICOP2.x)

IND2_FD_code <- IND2_FD_code[order(IND2_FD_code$CODE),]
IND2_FD_code[is.na(IND2_FD_code)] <- 0

# CES_ICP_IDN, CES_ICP_IND rows are sorted by Survey Code.
# IND_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD_code[,2])   # only for total
IND_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD_code[,2:12])  # for all deciles and total


# Get IDN final demand from EXIO [M.EUR]
# ID_place <- which(exio_ctys=="ID")
# ID_idx <- seq(200*(ID_place-1)+1, 200*ID_place)  # 200 EXIO commodities per country
# ID_idx_fd <- seq(7*(ID_place-1)+1, 7*ID_place)   # 7 final demand columns per country
# ID_fd_exio <- rowSums(final_demand[ID_idx, seq(1, dim_fd[2], 7)]) # Sum all HH FD across countries
# 
# # Get IDN final demand in ICP 
# 
# # Remove tax observations from DB
# IDN_FD_code <- IDN_FD[-grep("taxes", IDN_FD$ITEM, ignore.case = TRUE), ]
# IDN_FD_code <- merge(IDN_FD_code, IDN_map[,c("CODE", "COICOP2", "ITEM_DLE")], by.x="ITEM", by.y="ITEM_DLE")
# IDN_FD_code <- IDN_FD_code[order(IDN_FD_code$CODE),]
# IDN_FD_code[is.na(IDN_FD_code)] <- 0
# 
# # CES_ICP_IDN, CES_ICP_IDN rows are sorted by Survey Code.
# IDN_FD_ICP <- t(CES_ICP_IDN) %*% as.matrix(IDN_FD_code[,2])


##########################################
### Adjust FD based on the indicators  ###
##########################################

#### 1.1 India - Real consumption growth

# IND1 is for 2010-2011
IND_FD_code$COICOP1 <- as.numeric(IND_FD_code$COICOP1)
IND2_FD_code$COICOP1 <- as.numeric(IND2_FD_code$COICOP1)
a <- aggregate(. ~ COICOP1, data=IND_FD_code[,2:14], sum)
b <- aggregate(. ~ COICOP1, data=IND2_FD_code[,2:14], sum)

consumption_growth <- (a[,2:12]/b[,2:12])^(1/7) # general consumption growth rate in 7 years by decile
consumption_growth_DE <- (IND_DE[,2:12]/IND2_DE[,2:12])^(1/7) # direct energy consumption growth rate in 7 years by decile
consumption_growth_FD_DE <- (IND_FD_DE[,2:12]/IND2_FD_DE[,2:12])^(1/7) # consumption growth rate in 7 years by decile

#### 1.2 India - Inflation & exchange rate

IND_FD_ICP_usd2007 <- IND_FD_ICP * PPP_IND / CPI_ratio_IND / EXR_IND$r / 1e6 # M.Rs to M.USD 2007
IND_FD_ICP_usd2007 <- IND_FD_ICP_usd2007/((consumption_growth^4)[as.numeric(icp_ntnu$COICOP1)[1:151],])
# IN_fd_exio_usd2007 <- IN_fd_exio / EXR_EUR$r
IND_DE_intst2007 <- (IND_DE[,2:12] / (consumption_growth_DE^4)) / (IND_FD_DE[,2:12] / (consumption_growth_FD_DE^4))

# sum(IND_FD_ICP_usd2007) # CES FD in 2007$ (MER) based on 2011-2012 NSS -> Use this for RAS
# sum(IN_fd_exio_usd2007) # EXIO FD in 2007$ (MER?) based on 2004 IoT -> Use this for RAS

# From Indian I-O 2007 - total PFCE (Private Final Consumption Expenditure)
IND_FD_IO2007_usd <- 278289600 * 1e5 / EXR_IND$r / 1e6 # Convert Rs. Lakhs to Million $ 2007


#### 2.1 France - Inflation 

# This is in 109 NTNU COICOP classifications.
a <- WDI(country = "FR", indicator = "NE.CON.PETC.KD", start = 2007, end = 2011, extra = FALSE, cache = NULL)
consumption_growth_FR <- a$NE.CON.PETC.KD[1]/a$NE.CON.PETC.KD[5]
FRA_FD_ICP_usd2007 <- as.matrix(fd_decile_FRA / CPI_ratio_FRA / EXR_EUR$r) / consumption_growth_FR # M.EUR to M.USD 2007 [length: 109]

GetTotalEmbeddedEnergy <- function(country='IN') {

  if(country=='ALL') {
    cty_idx_fd <- 1:dim(final_demand)[2]
  }
  else {
    cty_place <- which(exio_ctys==country)
    cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
    cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  }
  
  emb_energy <- indirect_E_int %*% as.matrix(final_demand[,cty_idx_fd])
  emb_energy <- cbind(emb_energy, rowSums(emb_energy))
  
  # country <- countrycode(country, "iso2c", "iso3c")
  # pop2007 <- eval(parse(text=paste0(country, "_pop_2007")))
  
  # return(colSums(emb_energy)/pop2007*1000)  # in GJ/capita
  return(colSums(emb_energy))  # in TJ
}

global_total <- 0
for (i in exio_ctys) {
  country_total <- GetTotalEmbeddedEnergy(i)[8]
  global_total <- global_total + country_total
  print(paste0(i, ' ', country_total))
}
