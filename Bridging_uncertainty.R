setwd("H:/MyDocuments/IO work/Uncertainty/")

require(XLConnect)
require(Surrogate)
Mapping <- system.file("COICOP3_EXIO_bridge.xlsx", package = "XLConnect")
wb <- loadWorkbook("COICOP3_EXIO_bridge.xlsx")

bridge <- readWorksheet(wb, sheet="Qual", header=F, startRow=3, startCol=3, forceConversion=T)

CP_catnames <- readWorksheet(wb, sheet="Qual", header=F, startRow=3, startCol=2, endCol=2)
EX_catnames <- readWorksheet(wb, sheet="Qual", header=F, startRow=2, endRow=2, startCol=3)

num_sectors <- rowSums(bridge)

cnt <- table(num_sectors)
assign_num <- rbind(as.numeric(names(cnt)), cnt)

bridge_r <- as.matrix(bridge)
bridge_t <- t(bridge_r) # Columns of this mtx will be replaced

for (i in 2:dim(cnt)) {
  vec <- RandVec(0, 1, 1, assign_num[1,i], assign_num[2,i])    # Uniform draw
  assign_ratio <- as.matrix(vec$RandVecOutput)
  v_ind <- (num_sectors == assign_num[1,i]) # Vertical indices of rows with assign_num[1,i] ones.
  bridge_t[,v_ind][bridge_t[,v_ind]==1] <- assign_ratio
}

bridge_r <- t(bridge_t) # Transpose to get back to original dimension
rowSums(bridge_r)
