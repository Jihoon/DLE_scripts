FD_ICP <- cbind(icp_ntnu[1:n_sector_icp,c(1:2)], IDN_FD_ICP, IND_FD_ICP)

# Plot food and non-alco bev only
Food_category <- c("Bread/cereals", "Meat", "Fish/seafood", "Milk/cheese/eggs", "Oils/fats", "Fruits", 
  "Vegetables", "Sugar/jam/honey/chocolate/etc", "Food products n.e.c", "Non-alcoholic beverages")

# Index for food subcategories - 3:8, 9:14, 15:17, 18:22, 23:25, 26:28, 29:32, 33:37, 38:40
Food_num <- 1:40
a <- aggregate(. ~ COICOP2, data = FD_ICP[Food_num,2:dim(FD_ICP)[2]], sum)
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
