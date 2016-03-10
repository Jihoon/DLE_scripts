# Load a table with code number and item names used for our DB
# Issue: BRA - Need to consolidate survey 'code' 
#            - There are non-food items in food DB (e.g. Coal).
#        ZAF - Need to consolidate survey 'code' 

# Read code-item mapping for IDN
wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/IDN NSES July 2008 CE Codes.xlsx")
code_item_IDN  <- XLConnect::readWorksheet(wb, "Codes", header=TRUE, forceConversion=T)
names(code_item_IDN) <- c("CODE", "ITEM_DLE", "UNIT")
code_item_IDN$UNIT <- NULL
# code_item_IDN <- code_item_IDN[match(unique(code_item_IDN$ITEM_DLE), code_item_IDN$ITEM_DLE),]    # There are duplicate ITEM_DLEs because they are input in two units (e.g. weight & expenditure) -> Not necessary because merge will anyhow remove these.

# WB removed all ceremonial spendings from the consumption. I put those back at '151: other services' under ICP.
IDN_WB$ICP_SEQ[c(338, 340, 342, 343)] <- 151

# Read code-item name in DB mapping for IND
# Theoretically ITEM_DLE could be identical to Surv_Heading, but ITEM_DLE is somehow modified.
wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/IND NSS 68 2011-2012 CE Codes.xlsx")
code_item_IND  <- XLConnect::readWorksheet(wb, "NSS68", header=TRUE, forceConversion=T)
names(code_item_IND) <- c("CODE", "ITEM_DLE", "UNIT")
code_item_IND$UNIT <- NULL
# code_item_IND <- code_item_IND[match(unique(code_item_IND$ITEM_DLE), code_item_IND$ITEM_DLE),]    # There are duplicate ITEM_DLEs because they are input in two units (e.g. weight & expenditure)

# WB assigned "CES 291: Biscuits, chocolates, etc" to "ICP 2: UNBR Food".
# Sending it to "ICP 36:Confectionery, chocolate and ice cream"
IND_WB$ICP_SEQ[IND_WB$CODE==291] <- 36
# WB assigned "CES 274-276" to "ICP 39: Coffee, tea and cocoa".
# They should be "ICP 40: Mineral waters, soft drinks, fruit and vegetable juices"
# 274 "mineral water (litre)"
# 275 "cold beverages: bottled/canned (litre)"
# 276 "fruit juice and shake (litre)"
IND_WB$ICP_SEQ[IND_WB$CODE>=274 & IND_WB$CODE<=276] <- 40



# Linking NTNU COICOP with WB ICP
# XXX_WB has original survey code, ICP seq number, and ICP heading (except for BRA where 'heading' is missing).
# icp_ntnu is for ICP seq number, NTNU-COICOP seq number, and ICP heading.
IND_map <- merge(IND_WB, icp_ntnu, by="ICP_SEQ")
IND_map <- IND_map[c("CODE", "Surv_Heading", "ICP_SEQ", "Subcategory", "ICP_Heading", "NTNU_109")]
# IND_map$ORD <- 1:dim(IND_map)[1]  # Is this necessary?
IND_map <- merge(IND_map, code_item_IND, by="CODE")
IND_map <- IND_map[order(IND_map$CODE),]

IDN_map <- merge(IDN_WB, icp_ntnu, by="ICP_SEQ")
IDN_map <- IDN_map[c("CODE", "Surv_Heading", "ICP_SEQ", "Subcategory", "ICP_Heading", "NTNU_109")]
# IDN_map$ORD <- 1:dim(IDN_map)[1]
IDN_map <- merge(IDN_map, code_item_IDN, by="CODE")
IDN_map <- IDN_map[order(IDN_map$CODE),]


# Create CES to ICP bridge matrices
CES_ICP_IDN <- matrix(0, length(IDN_map$CODE), max(IDN_map$ICP_SEQ))
CES_ICP_IND <- matrix(0, length(IND_map$CODE), max(IND_map$ICP_SEQ))
CES_ICP_IDN[cbind(1:length(IDN_map$CODE), IDN_map$ICP_SEQ)] <- 1
row.names(CES_ICP_IDN) <- IDN_map$CODE
CES_ICP_IND[cbind(1:length(IND_map$CODE), IND_map$ICP_SEQ)] <- 1
row.names(CES_ICP_IND) <- IND_map$CODE

# Detect which CES items are classifed to UNBR.
a <- IDN_map[grep("UNBR", IDN_map$ICP_Heading),]
a <- IND_map[grep("UNBR", IND_map$ICP_Heading),]