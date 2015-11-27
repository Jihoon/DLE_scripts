# GBR Example
# Let's validate their output in the FIDELIO Example file.

setwd("H:/MyDocuments/IO work/Fidelio/")

require(XLConnect)
Fidelio <- system.file("Bridge_EU27_FIDELIO.xlsx", package = "XLConnect")
wb <- loadWorkbook("Bridge_EU27_FIDELIO.xlsx")

# Read in the xlsx file
bridge_GBR_init <- as.matrix(readWorksheet(wb, "bridge", 1540, 3, header=F))
SUT_GBR <- as.matrix(readWorksheet(wb, "COICOP", 297, 26, 297, 84, header=F))
COICOP_GBR <- as.matrix(readWorksheet(wb, "COICOP", 297, 4, 297, 20, header=F))
scale <- sum(SUT_GBR)/sum(COICOP_GBR)
COICOP_GBR_True <- COICOP_GBR*scale

alloc <- t(apply(bridge_GBR_init, 1, function(x) as.matrix(x*COICOP_GBR_True)))
sum_row <- apply(alloc, 1, sum)
ratio_row <- sum_row / SUT_GBR
ratio_row[is.nan(ratio_row)] <- 1

pr <- 10

while(abs(pr-1)>0.0000001) {

alloc2 <- apply(alloc, 2, function(x) as.matrix(x/ratio_row))
sum_col <- apply(alloc2, 2, sum)
ratio_col <- sum_col / COICOP_GBR_True

alloc <- t(apply(alloc2, 1, function(x) as.matrix(x/ratio_col)))
sum_row <- apply(alloc, 1, sum)
ratio_row <- sum_row / SUT_GBR
ratio_row[is.nan(ratio_row)] <- 1

pr <- prod(ratio_row)
}

alloc
