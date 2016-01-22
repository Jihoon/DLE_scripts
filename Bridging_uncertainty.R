# Draw a randomized mapping based on the qualitative mapping from COICOP to EXIO

get_bridge_COICOP_EXIO <- function(qual_map, num_draw, country){
  
# Number of mapped categories in EXIO per each in COICOP
num_sectors <- rowSums(qual_map)

cnt <- table(num_sectors)
assign_num <- rbind(as.numeric(names(cnt)), cnt)

qual_map <- t(as.matrix(qual_map)) # Columns of this mtx will be replaced
bridge <- list()
r_vec <- list()

for (i in 2:dim(cnt)) {

  r_vec[i-1] <- RandVec(0, 1, 1, assign_num[1,i], assign_num[2,i]*num_draw)    # Uniform draw

}

for (j in 1:num_draw) {
  temp_b <- qual_map
  for (i in 2:dim(cnt)) {
    idx <- ((j-1)*assign_num[2,i]+1):(j*assign_num[2,i])
    assign_ratio <- as.matrix(r_vec[[i-1]][,idx])
    v_ind <- (num_sectors == assign_num[1,i]) # Vertical indices of rows with assign_num[1,i] ones.
    temp_b[,v_ind][temp_b[,v_ind]==1] <- assign_ratio
  }
  bridge[[j]] <- temp_b
  
#   if(country=="FRA") {
#     bridge[[j]][,which(COICOP_catnames2 == "Electricity")] <- 
#   }
}

testColSums(bridge, num_draw)

# bridge_COICOP_EXIO <- t(bridge_t) # Transpose to get back to original dimension

return(bridge)
}

testColSums <- function(x, n){
  for (j in 1:n) {
    good <- prod(colSums(x[[j]]))
    if(!good) {
      print("Doomed!")
    }
  }
  return(good)
}