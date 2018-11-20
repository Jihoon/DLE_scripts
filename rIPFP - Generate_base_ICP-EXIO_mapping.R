# Read in Q_UN_EXIO_e edited version
Q_UN_EXIO_e <- read_excel("../Bridging/CES-COICOP/COICOIP_EXIO_Qual_UN_Edited.xlsx")
Q_UN_EXIO_e <- as.matrix(Q_UN_EXIO_e[-dim(Q_UN_EXIO_e)[1],-c(1,dim(Q_UN_EXIO_e)[2])])

# Make a new qual mapping between ICP (instead of NTNU109) and EXIO
# temp <- icp_ntnu[!is.na(icp_ntnu$NTNU_109),]  # icp_ntnu$NTNU_109 has indices matching each ICP item to NTNU109 items.

temp <- str_split(icp_ntnu$NTNU_109, '/')         # Some ICP item match to multiple NTNU109 items.
temp <- lapply(temp, as.numeric)              # temp will hold those numerical indices.


# Build a list with COICOP-EXIO mapping rows
bridge_icp_exio_list <- lapply(temp, function(ind) {
  # a <- bridge_COICOP_EXIO_q[ind,-1,drop=FALSE]         # Get the mapping row (or rows if multiple) from the qual mapping
  a <- Q_UN_EXIO_e[ind, ,drop=FALSE]         # UN mapping
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

bridge_icp_exio[62:65,] <- 0 # Reset original ICP fuel rows mapping
bridge_icp_exio <- bridge_icp_exio %>% rbind(bridge_fuel_EXIO_q)
row.names(bridge_icp_exio)[63] <- "Electricity.COICOP"   # Duplicate row names not allowed
row.names(bridge_icp_exio)[155] <- "Electricity"

Q_UN_ICP_EXIO <- bridge_icp_exio
write.csv(bridge_icp_exio, "../Bridging/CES-COICOP/ICP_EXIO_Qual_UN.csv")
# write.csv(bridge_icp_exio, "../Bridging/CES-COICOP/ICP_EXIO_Qual.csv")

# This matrix (ICP_EXIO_Qual.csv) is modified externally manually to fine-allocate mostly for food-subsectors.
# The result is in H:\MyDocuments\IO work\Bridging\CES-COICOP\ICP_EXIO_Qual_Edited.xlsx
# Manually changed cells are colored in green in the xlsx file.
# This final mapping is read in from Init.R.
