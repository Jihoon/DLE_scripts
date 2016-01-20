# Load a table with code number and item names used for our DB

# IDN
wb <- loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/IDN NSES July 2008 CE Codes.xlsx")
code_item_IDN  <- readWorksheet(wb, "Codes", header=TRUE, forceConversion=T)
names(code_item_IDN) <- c("CODE", "ITEM_DLE", "UNIT")

# IND
wb <- loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/IND NSS 68 2011-2012 CE Codes.xlsx")
code_item_IND  <- readWorksheet(wb, "NSS68", header=TRUE, forceConversion=T)
names(code_item_IND) <- c("CODE", "ITEM_DLE", "UNIT")


IND_map <- merge(IND, icp_ntnu, by="ICP_SEQ")
IND_map <- IND_map[c("CODE", "DESC_ORG", "ICP_SEQ", "ICP_Heading", "NTNU_109")]
IND_map$ORD <- 1:dim(IND_map)[1]
IND_map <- merge(IND_map, code_item_IND, by="CODE")
IND_map <- IND_map[order(IND_map$CODE),c("ORD", "CODE", "DESC_ORG", "ITEM_DLE", "ICP_SEQ", "ICP_Heading", "NTNU_109")]

IDN_map <- merge(IDN, icp_ntnu, by="ICP_SEQ")
IDN_map <- IDN_map[c("CODE", "DESC_ORG", "ICP_SEQ", "ICP_Heading", "NTNU_109")]
IDN_map$ORD <- 1:dim(IDN_map)[1]
IDN_map <- merge(IDN_map, code_item_IDN, by="CODE")
IDN_map <- IDN_map[order(IDN_map$CODE),c("ORD", "CODE", "DESC_ORG", "ITEM_DLE", "ICP_SEQ", "ICP_Heading", "NTNU_109")]

# Make a new qual mapping between ICP (instead of NTNU109) and EXIO
temp <- icp_ntnu[!is.na(icp_ntnu$NTNU_109),]  # icp_ntnu$NTNU_109 has indices matching each ICP item to NTNU109 items.
temp <- str_split(temp$NTNU_109, '/')         # Some ICP item match to multiple NTNU109 items.
temp <- lapply(temp, as.numeric)              # temp will hold those numerical indices.

# Build a list with COICOP-EXIO mapping rows
bridge_icp_exio_list <- lapply(temp, function(ind) {
      a <- bridge_COICOP_EXIO_q[ind,]         # Get the mapping row (or rows if multiple) from the qual mapping
      b <- as.numeric(apply(a, 2, function(x) { Reduce("|", x) }))  # Logical OR of multiple rows
      return(b)
})
bridge_icp_exio <- matrix(unlist(bridge_icp_exio_list), nrow=105, byrow=T)  # Re-format the resulting list as a matrix

  