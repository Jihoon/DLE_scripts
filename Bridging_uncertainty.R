get_bridge_COICOP_EXIO <- function(){
  
# Number of mapped categories in EXIO per each in COICOP
num_sectors <- rowSums(bridge_COICOP_EXIO_q)

cnt <- table(num_sectors)
assign_num <- rbind(as.numeric(names(cnt)), cnt)

bridge_COICOP_EXIO_q <- as.matrix(bridge_COICOP_EXIO_q)
bridge_t <- t(bridge_COICOP_EXIO_q) # Columns of this mtx will be replaced

for (i in 2:dim(cnt)) {
  vec <- RandVec(0, 1, 1, assign_num[1,i], assign_num[2,i])    # Uniform draw
  assign_ratio <- as.matrix(vec$RandVecOutput)
  v_ind <- (num_sectors == assign_num[1,i]) # Vertical indices of rows with assign_num[1,i] ones.
  bridge_t[,v_ind][bridge_t[,v_ind]==1] <- assign_ratio
}

bridge_COICOP_EXIO <- t(bridge_t) # Transpose to get back to original dimension
# rowSums(bridge_COICOP_EXIO)

return(bridge_COICOP_EXIO)
}