##############################
###     Clean version      ###
##############################

# This library needed to do multiple returns from functions
library(devtools)  
source_url("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

# Run RAS and construct final matrix in original dimension
library(mipfp)

RemoveConflictsInConstraints <- function() {
  
  # Convert to pp without tax
  colConst_init <- get_purch_price(IN_fd_exio_usd2007) 
  scaler_IND <- sum(IND_FD_ICP_usd2007)/sum(colConst_init)
  rowConst_init <- as.vector(IND_FD_ICP_usd2007) / scaler_IND
  
  qual_map <- qual_map_init
  
  rowConst <- rowConst_init
  colConst <- colConst_init

  # Do this until no conflicts remain.
  # At the end, we get consistent and RASable rowCon and colCon.
  repeat {
    # This will return a list of coordinates.
    # [[1]] fx_coord_rowCon - Coord for cells fixed by row constraint
    # [[2]] fx_coord_colCon
    # [[3]] lone_cell
    # [[4]] beyond_const
    list[fx_coord_rowCon, fx_coord_colCon, lone_cell, beyond_const_r, beyond_const_c] <- IdentifyConflicts(qual_map, colConst, rowConst) 
    conflicts <- rbind(lone_cell, beyond_const_r, beyond_const_c)
    
    # print(paste("Constrained by row con = ", fx_coord_rowCon[,1]))
    # print(paste("Constrained by col con = ", fx_coord_colCon[,2]))
    
    finish_cond <- dim(fx_coord_rowCon)[1] == 0 & dim(fx_coord_colCon)[1] == 0
    # finish_cond <- dim(conflicts)[1] == 0
    
    if (finish_cond) {
      break
    }
    
    # This is just for conflict row/cols!
    while (dim(conflicts)[1]) {
      # Get updated row constraint (corrected, scaled)
      # col constraint does not change.
      # [[1]] by row
      # [[2]] by col
      list[rowConst, colConst, idx_rowConflic, idx_colConflic] <- UpdateConstsForConflicts(qual_map, fx_coord_rowCon, fx_coord_colCon, lone_cell, beyond_const_r, 
                                                                                                     beyond_const_c, colConst, rowConst)
      
      # Need to remove from q_map the whole singular cells in the same row/col with conflicts
      a <- rbind(idx_rowConflic, lone_cell)
      b <- rbind(idx_colConflic, lone_cell)
      
      # Fill the map with NAs for singular row/cols
      # Fill constraint vectors with NAs for singular row/cols
      list[qual_map, colConst, rowConst] <- UpdateQualMap_Const(qual_map, a, b, colConst, rowConst)
      
      # New conflicts can occur as a result of updated constraints.
      list[fx_coord_rowCon, fx_coord_colCon, lone_cell, beyond_const_r, beyond_const_c] <- IdentifyConflicts(qual_map, colConst, rowConst) 
      conflicts <- rbind(lone_cell, beyond_const_r, beyond_const_c)
    }
    
    # Fill in all other fixed cells in result_RAS_fixed 
    # Singular cells in the same row/col with conflict cells are already fixed above.
    # So this is for only the non-conflict singular cells not in the same row/col with conflicts.
    list[non_conflicts_r, non_conflicts_c]  <- FillNonConflictCells(qual_map, fx_coord_rowCon, fx_coord_colCon, conflicts, colConst, rowConst)
    # list[rowConst, colConst] <- UpdateConstsForNonConflicts(non_conflicts_r, non_conflicts_c, colConst, rowConst)
    
    # Fill the map with NAs for singular row/cols
    # Fill constraint vectors with NAs for singular row/cols
    list[qual_map, colConst, rowConst] <- UpdateQualMap_Const(qual_map, non_conflicts_r, non_conflicts_c, colConst, rowConst)
    
    print('Sum total =')
    print(sum(rowConst, na.rm = T) + sum(result_RAS_fixed))
    print(sum(rowConst, na.rm = T))
    print(sum(colConst, na.rm = T))
  }
  
  
  # Keep the record of NA'ed coords
  idx_row_removed <- which(is.na(rowConst))
  idx_col_removed <- which(is.na(colConst))
  
  # Remove NAs and collapse to RASable matrix and constraints
  # Keep the coord of NAs somewhere
  list[qmap_RAS, rowCon_RAS, colCon_RAS] <- CollapseQualMap(qual_map, colConst, rowConst)
  
  # Remove dangling small numbers due to scaling
  rowCon_RAS <- round(rowCon_RAS, 7)
  colCon_RAS <- round(colCon_RAS, 7)
  gap <- sum(rowCon_RAS) - sum(colCon_RAS)
  
  # There can appear nearly zeros in the constraints because of scalings. So remove.
  idx_zero <- mapply(function(x, y) {isTRUE(all.equal(x, y))}, colCon_RAS, 0)
  # Temporary fix to match the constraint sums
  colCon_RAS[colCon_RAS>100][1] <- colCon_RAS[colCon_RAS>100][1] + sum(colCon_RAS[idx_zero]) + gap
  colCon_RAS[idx_zero] <- 0
  idx_zero <- mapply(function(x, y) {isTRUE(all.equal(x, y))}, rowCon_RAS, 0)
  # rowCon_RAS[rowCon_RAS>100][1] <- rowCon_RAS[rowCon_RAS>100][1] + sum(rowCon_RAS[idx_zero])
  rowCon_RAS[idx_zero] <- 0
  
  return(list(rowCon_RAS, colCon_RAS, idx_row_removed, idx_col_removed, result_RAS_fixed, qmap_RAS))
}


# Generate all the n_draw RAS results
D_val_uncertainty <- 1  # or 1 : Whether to include uncertainty analysis for valuation mtx - margins and tax rates

colCon <- NULL
rowCon <- NULL
result <- list()

qual_map_init <- bridge_icp_exio_q[,-1]

draw_count <- 1

for (i in 1:n_draw) {
  result_RAS_fixed <- matrix(0, dim(qual_map_init)[1],dim(qual_map_init)[2])
  
  draw_count <- i   # Indicate that we are on i-th draw. Used in get_basic_price and get_purch_price
  
  if (D_val_uncertainty == 0 & i==1) {
    list[rCon_RAS, cCon_RAS, idx_r_removed, idx_c_removed, result_fixed, q_RAS] <- RemoveConflictsInConstraints()  
    # Get draw
    bridge_ICP_EXIO <- get_bridge_COICOP_EXIO(q_RAS, n_draw)
  }
  else if (D_val_uncertainty == 1)  {
    list[rCon_RAS, cCon_RAS, idx_r_removed, idx_c_removed, result_fixed, q_RAS] <- RemoveConflictsInConstraints()  
    # Get draw
    bridge_ICP_EXIO <- get_bridge_COICOP_EXIO(q_RAS, 1)
  }
  
  if (D_val_uncertainty == 0)  {bridge <- bridge_ICP_EXIO[[i]]}
  else {bridge <- bridge_ICP_EXIO[[1]]}
    
  seed <- diag(rCon_RAS) %*% bridge   # RAS init mtx
  result_RAS <- Ipfp(seed, list(1,2), list(rCon_RAS, cCon_RAS), iter=10000)
  if(result_RAS$conv == FALSE) {print(paste("Didn't converge at", draw_count))}
  
  colCon <- cbind(colCon, colSums(result_RAS$x.hat))
  rowCon <- cbind(rowCon, rowSums(result_RAS$x.hat))
  
  final_RAS <- matrix(0, length(IND_FD_ICP_usd2007), length(IN_fd_exio_usd2007))
  final_RAS[-idx_r_removed, -idx_c_removed] <- result_RAS$x.hat
  final_RAS <- final_RAS + result_fixed
  
  result[[i]] <- final_RAS
  
  print(i)
  # print(norm(result_RAS$x.hat, type="F"))  #"O", "I", "F", "M", "2"
}

# Combine result_RAS and resut_RAS_fixed to form a whole allocation matrix
# final_RAS_list <- lapply(result, function (x) {
#   final_RAS <- matrix(0, dim(result_RAS_fixed)[1], dim(result_RAS_fixed)[2])
#   final_RAS[-idx_row_removed, -idx_col_removed] <- x
#   final_RAS <- final_RAS + result_RAS_fixed
#   })

final_alloc_list <- lapply(result, function (x) {
  a <- diag(1/rowSums(x))
  a[is.infinite(a)] <- 0
  x <- a %*% x
})

# final_rowCon <- rowSums(final_RAS_list[[1]])

# Estimate sectoral energy intensities with/without the RAS process
# Case 1: not including val mtx uncertainty
xlcFreeMemory()
ind_inten_RAS_no_val <- SetupSectorIntensities(final_alloc_list)
alloc_nonRAS_no_val <- get_bridge_COICOP_EXIO(qual_map_init, n_draw)
ind_inten_nonRAS_no_val <- SetupSectorIntensities(alloc_nonRAS)

# Case 2: including val mtx uncertainty
ind_inten_RAS <- SetupSectorIntensities(final_alloc_list)
alloc_nonRAS <- get_bridge_COICOP_EXIO(qual_map_init, n_draw)
ind_inten_nonRAS <- SetupSectorIntensities(alloc_nonRAS)

# There are cases where ICP expenditure is zero for a sector. 
# In the RAS approach, we do not get intensity numbers because no expenditure is assigned and thus no emission.
# In this case, I copy intensities from the non-RAS estimation.
no_expense <- which((rowSums(qual_map_init)!=0) & (IND_FD_ICP_usd2007==0))
ind_inten_RAS[,no_expense] <- ind_inten_nonRAS[,no_expense]
ind_inten_RAS_no_val[,no_expense] <- ind_inten_nonRAS_no_val[,no_expense]

# Plot intensities
png(filename = paste(figure_path, "Energy intensity by COICOP consumption category.png", sep=""), width = 781, height = 553, units = "px")
boxplot(ind_inten_RAS, xlab ="ICP sectors", ylab ="Energy intensity by ICP sector [MJ/EUR] w/ RAS", range=0, axes = FALSE)
axis(side = 1, at = seq(1,151,10))
axis(side = 2, at = seq(0,300,50))
# text(1:151, y=apply(ind_inten_RAS, 2, max)+50, bridge_icp_exio_q[,1], pos=4, offset=-.1, cex = 0.5, srt = 90)
text(1:151, y=apply(ind_inten_RAS, 2, max)+5, 1:151, pos=4, offset=-.1, cex = 0.6, srt = 90)
dev.off()

boxplot(ind_inten_nonRAS, xlab ="ICP sectors", ylab ="Energy intensity by ICP sector [MJ/EUR] w/o RAS", range=0, axes = FALSE)
axis(side = 1, at = seq(1,151,10))
axis(side = 2, at = seq(0,600,100))
text(1:151, y=apply(ind_inten_nonRAS, 2, max)+15, 1:151, pos=4, offset=-.1, cex = 0.6, srt = 90)



# Compare rowSum sizes between CES and RASed one
a <- cbind(bridge_icp_exio_q[,1], rowSums(final_RAS_list[[1]]), IND_FD_ICP_usd2007, IND_FD_ICP_usd2007 > rowSums(final_RAS_list[[1]]))

idx_anomaly <- IND_FD_ICP_usd2007 > rowSums(final_RAS_list[[1]])
anomaly <- data.frame(bridge_icp_exio_q[idx_anomaly,1], IND_FD_ICP_usd2007[idx_anomaly], rowSums(final_RAS_list[[1]])[idx_anomaly])
names(anomaly) <- c("SV_sector", "SV_exp", "RAS_exp")
write.table(anomaly, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

b <- cbind(qual_map_init, final_RAS_list[[1]])
exio_anomal <- apply(b[idx_anomaly,], 1, function(x) {cbind(names(which(x==1)), x[which(x==1)+200])})
# exio_anomal <- apply(qual_map_init[idx_anomaly,], 1, function(x) {cbind(names(which(x==1)), colConst_init[which(x==1)])})
a <- do.call("rbind", lapply(exio_anomal, '[', ,))

write.table(a, "clipboard", sep="\t", row.names = FALSE, col.names = FALSE)

# Let's see the 'Gas' item from CES (IND)
do.call("rbind", lapply(final_RAS_list, '[', 64,))
a<- cbind(names(bridge_icp_exio_q)[2:201], final_RAS_list[[1]][63,], final_RAS_list[[1]][63,]/sum(final_RAS_list[[1]][63,]))
