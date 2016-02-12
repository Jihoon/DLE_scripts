# Remove tax observations from DB
IND_FD_code <- IND_FD[-grep("taxes", IND_FD$ITEM, ignore.case = TRUE), ]

IDN_FD_code <- IDN_FD[-grep("tax", IDN_FD$ITEM, ignore.case = TRUE), ] 

IDN_map$ITEM_DLE[IDN_map$CODE==208] <- "Fish (fried, roasted, presto, pindang, pepes, etc)" # Correct DB input errors
IDN_FD_code$ITEM[grep("Fish ", IDN_FD_code$ITEM)] <- "Fish (fried, roasted, presto, pindang, pepes, etc)"

IND_FD_code <- merge(IND_FD_code, IND_map[,c("CODE", "Subcategory", "ITEM_DLE")], by.x="ITEM", by.y="ITEM_DLE")
IDN_FD_code <- merge(IDN_FD_code, IDN_map[,c("CODE", "Subcategory", "ITEM_DLE")], by.x="ITEM", by.y="ITEM_DLE")

IND_FD_code <- IND_FD_code[order(IND_FD_code$CODE),]
IDN_FD_code <- IDN_FD_code[order(IDN_FD_code$CODE),]

IND_FD_code[is.na(IND_FD_code)] <- 0
IDN_FD_code[is.na(IDN_FD_code)] <- 0

# CES_ICP_IDN, CES_ICP_IND rows are sorted by Survey Code.
IDN_FD_ICP <- t(CES_ICP_IDN) %*% as.matrix(IDN_FD_code[,2:12])
IND_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD_code[,2:12])

# IDN_FD_ICP <- cbind(icp_ntnu[1:151,c(1,3)], IDN_FD_ICP)
# IND_FD_ICP <- cbind(icp_ntnu[1:151,c(1,3)], IND_FD_ICP)

FD_ICP <- cbind(icp_ntnu[1:151,c(1:2)], IDN_FD_ICP, IND_FD_ICP)

# Plot food and non-alco bev only
Food_category <- c("Bread/cereals", "Meat", "Fish/seafood", "Milk/cheese/eggs", "Oils/fats", "Fruits", 
  "Vegetables", "Sugar/jam/honey/chocolate/etc", "Food products n.e.c", "Non-alcoholic beverages")

# Index for food subcategories - 3:8, 9:14, 15:17, 18:22, 23:25, 26:28, 29:32, 33:37, 38:40
Food_num <- 1:40
a <- aggregate(. ~ Subcategory, data = FD_ICP[Food_num,2:dim(FD_ICP)[2]], sum)
plotdata <- data.matrix(a[3:dim(a)[1],2:dim(a)[2]])
plotratio <- plotdata %*% diag(1/colSums(plotdata))

col_IDN <- shadepalette(10, "red", "white")
col_IND <- shadepalette(10, "blue", "white")

opar <- par() 
par(mar=c(12.5,4.1,2.1,2.1))
barplot(t(plotratio)[c(1,12),], beside=TRUE,
        ylab = "Share", las=2, col = c("red", "blue"),
        names.arg = Food_category, legend = c("IDN", "IND"))
barplot(t(plotratio)[-c(1,12),], beside=TRUE,
        ylab = "Share", las=2, col = c(col_IDN, col_IND),
        names.arg = Food_category)
legend("topright", legend=c("IDN", "IND"), fill=c("red", "blue"))
barplot(t(plotratio)[c(2,6,11,13,17,22),], beside=TRUE,
        ylab = "Share", las=2, col = c(shadepalette(3, "red", "white"), shadepalette(3, "blue", "white")),
        names.arg = Food_category)
legend("topright", legend=c("IDN", "IND"), fill=c("red", "blue"))
par(opar)

# axis(1, at=1:47, labels=FD_ICP$ICP_Heading[1:47])
# axis(1, at=21, labels="test")
