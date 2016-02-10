# Remove tax observations from DB
IND_FD <- IND_FD[-grep("taxes", IND_FD$ITEM, ignore.case = TRUE), ]
IDN_FD <- IDN_FD[-grep("tax", IDN_FD$ITEM, ignore.case = TRUE), ] 
IDN_map$ITEM_DLE[IDN_map$CODE==208] <- "Fish (fried, roasted, presto, pindang, pepes, etc)"
IDN_FD$ITEM[grep("Fish ", IDN_FD$ITEM)] <- "Fish (fried, roasted, presto, pindang, pepes, etc)"

IND_FD_code <- merge(IND_FD, IND_map[,c("CODE", "ITEM_DLE")], by.x="ITEM", by.y="ITEM_DLE")
IDN_FD_code <- merge(IDN_FD, IDN_map[,c("CODE", "ITEM_DLE")], by.x="ITEM", by.y="ITEM_DLE")

IND_FD_code <- IND_FD_code[order(IND_FD_code$CODE),]
IDN_FD_code <- IDN_FD_code[order(IDN_FD_code$CODE),]

# CES_ICP_IDN, CES_ICP_IND rows are sorted by Survey Code.
IDN_FD_ICP <- t(CES_ICP_IDN) %*% IDN_FD_code$FD_TOT
IND_FD_ICP <- t(CES_ICP_IND) %*% IND_FD_code$FD_TOT

# IDN_FD_ICP <- cbind(icp_ntnu[1:151,c(1,3)], IDN_FD_ICP)
# IND_FD_ICP <- cbind(icp_ntnu[1:151,c(1,3)], IND_FD_ICP)

FD_ICP <- cbind(icp_ntnu[1:151,c(1,3)], IDN_FD_ICP, IND_FD_ICP)

# Plot food and non-alco bev only
Food_num <- 1:40
plotdata <- data.matrix(FD_ICP[Food_num,3:4])
plotratio <- plotdata %*% diag(1/colSums(plotdata))

barplot(t(plotratio),beside=TRUE)
# axis(1, at=1:47, labels=FD_ICP$ICP_Heading[1:47])
# axis(1, at=21, labels="test")
