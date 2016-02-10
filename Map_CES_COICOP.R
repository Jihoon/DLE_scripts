# Load a table with code number and item names used for our DB
# Issue: BRA - Need to consolidate survey 'code' 
#            - There are non-food items in food DB (e.g. Coal).
#        ZAF - Need to consolidate survey 'code' 

# Read code-item mapping for IDN
wb <- loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/IDN NSES July 2008 CE Codes.xlsx")
code_item_IDN  <- readWorksheet(wb, "Codes", header=TRUE, forceConversion=T)
names(code_item_IDN) <- c("CODE", "ITEM_DLE", "UNIT")
code_item_IDN$UNIT <- NULL
# code_item_IDN <- code_item_IDN[match(unique(code_item_IDN$ITEM_DLE), code_item_IDN$ITEM_DLE),]    # There are duplicate ITEM_DLEs because they are input in two units (e.g. weight & expenditure) -> Not necessary because merge will anyhow remove these.

# WB removed all ceremonial spendings from the consumption. I put those back at '151: other services' under ICP.
IDN_WB$ICP_SEQ[c(338, 340, 342, 343)] <- 151

# Read code-item name in DB mapping for IND
# Theoretically ITEM_DLE could be identical to Surv_Heading, but ITEM_DLE is somehow modified.
wb <- loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/IND NSS 68 2011-2012 CE Codes.xlsx")
code_item_IND  <- readWorksheet(wb, "NSS68", header=TRUE, forceConversion=T)
names(code_item_IND) <- c("CODE", "ITEM_DLE", "UNIT")
code_item_IND$UNIT <- NULL
# code_item_IND <- code_item_IND[match(unique(code_item_IND$ITEM_DLE), code_item_IND$ITEM_DLE),]    # There are duplicate ITEM_DLEs because they are input in two units (e.g. weight & expenditure)

# Linking NTNU COICOP with WB ICP
# XXX_WB has original survey code, ICP seq number, and ICP heading (except for BRA where 'heading' is missing).
# icp_ntnu is for ICP seq number, NTNU-COICOP seq number, and ICP heading.
IND_map <- merge(IND_WB, icp_ntnu, by="ICP_SEQ")
IND_map <- IND_map[c("CODE", "Surv_Heading", "ICP_SEQ", "ICP_Heading", "NTNU_109")]
# IND_map$ORD <- 1:dim(IND_map)[1]  # Is this necessary?
IND_map <- merge(IND_map, code_item_IND, by="CODE")
IND_map <- IND_map[order(IND_map$CODE),c("CODE", "Surv_Heading", "ITEM_DLE", "ICP_SEQ", "ICP_Heading", "NTNU_109")]

IDN_map <- merge(IDN_WB, icp_ntnu, by="ICP_SEQ")
IDN_map <- IDN_map[c("CODE", "Surv_Heading", "ICP_SEQ", "ICP_Heading", "NTNU_109")]
# IDN_map$ORD <- 1:dim(IDN_map)[1]
IDN_map <- merge(IDN_map, code_item_IDN, by="CODE")
IDN_map <- IDN_map[order(IDN_map$CODE),c("CODE", "Surv_Heading", "ITEM_DLE", "ICP_SEQ", "ICP_Heading", "NTNU_109")]

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

# Make a new qual mapping between ICP (instead of NTNU109) and EXIO
# temp <- icp_ntnu[!is.na(icp_ntnu$NTNU_109),]  # icp_ntnu$NTNU_109 has indices matching each ICP item to NTNU109 items.
temp <- str_split(icp_ntnu$NTNU_109, '/')         # Some ICP item match to multiple NTNU109 items.
temp <- lapply(temp, as.numeric)              # temp will hold those numerical indices.

# Build a list with COICOP-EXIO mapping rows
bridge_icp_exio_list <- lapply(temp, function(ind) {
      a <- bridge_COICOP_EXIO_q[ind,]         # Get the mapping row (or rows if multiple) from the qual mapping
      b <- as.numeric(apply(a, 2, function(x) { Reduce("|", x) }))  # Logical OR of multiple rows
      return(b)
})
# I first copy the same qual mapping row from COICOP-EXIO bridge (e.g. copy meat row into all meat types in ICP), as a placeholder.
# Then, I need to tweak manually based on my assumption (e.g. )
n_row <- dim(icp_ntnu)[1]  # Use all ICP seq numbers 
# n_row <- length(icp_ntnu$ICP_Heading[!is.na(icp_ntnu$NTNU_109)])    # Do we need all rows or only those with matching categories. all rows because of compliance with other mtx
bridge_icp_exio <- matrix(unlist(bridge_icp_exio_list), nrow=n_row, byrow=T)  # Re-format the resulting list as a matrix
bridge_icp_exio[is.na(bridge_icp_exio)] <- 0
len <- dim(bridge_icp_exio)[1]
bridge_icp_exio <- bridge_icp_exio[-c(len-1, len),]  # We do not need the last two lines: "in the rest of the world" & "non-residential households"

bridge_icp_exio <- data.frame(bridge_icp_exio)
names(bridge_icp_exio) <- EX_catnames  
row.names(bridge_icp_exio) <- icp_ntnu$ICP_Heading[-c(len-1, len)]
# row.names(bridge_icp_exio) <- icp_ntnu$ICP_Heading[!is.na(icp_ntnu$NTNU_109)]
write.csv(bridge_icp_exio, "H:/MyDocuments/IO work/Bridging/CES-COICOP/ICP_EXIO_Qual.csv")
# This matrix is modified externally manually to fine-allocate mostly for food-subsectors.
# The result is in H:\MyDocuments\IO work\Bridging\CES-COICOP\ICP_EXIO_Qual_Edited.xlsx
# Manually changed cells are colored in green in the xlsx file.

wb <- loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/ICP_EXIO_Qual_Edited.xlsx")
bridge_icp_exio_q  <- readWorksheet(wb, "ICP_EXIO_Qual", header=TRUE, forceConversion=T)
