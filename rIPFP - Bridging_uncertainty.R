# Draw a randomized mapping based on the qualitative mapping from COICOP to EXIO

get_bridge_COICOP_EXIO <- function(qual_map, num_draw, country='IN'){
  
  # Number of mapped categories in EXIO per each in COICOP
  num_sectors <- rowSums(qual_map)
  
  cnt <- table(num_sectors)
  cnt <- cnt[as.numeric(names(cnt))>1] # remove rows with only one cell with 1
  
  assign_num <- rbind(as.numeric(names(cnt)), cnt)
  
  qual_map <- t(as.matrix(qual_map)) # Columns of this mtx will be replaced
  bridge <- list()
  r_vec <- list()
  
  # assign_num may start from 0 or 1. We need to start from 2.
  # start_index <- which(assign_num[1,]==2)
  start_index <- 1
  for (i in start_index:dim(assign_num)[2]) {
  
    r_vec[i] <- RandVec(0, 1, 1, assign_num[1,i], assign_num[2,i]*num_draw)    # Uniform draw
  
  }
  
  for (j in 1:num_draw) {
    temp_b <- qual_map
    for (i in start_index:dim(assign_num)[2]) {
      idx <- ((j-1)*assign_num[2,i]+1):(j*assign_num[2,i])
      assign_ratio <- as.matrix(r_vec[[i]][,idx])
      v_ind <- (num_sectors == assign_num[1,i]) # Vertical indices of rows with assign_num[1,i] ones.
      temp_b[,v_ind][temp_b[,v_ind]==1] <- assign_ratio
    }
    bridge[[j]] <- t(temp_b) # Transpose to get back to original dimension
  }
  
  testColSums(bridge, num_draw)
  
  return(t(bridge))
}

testColSums <- function(x, n){
  for (j in 1:n) {
    # good <- prod(colSums(x[[j]]))
    good <- sum(x[[j]])
    
    if(good > dim(x[[j]])[2]) {
      print("Doomed!")
    }
  }
  return(good)
}