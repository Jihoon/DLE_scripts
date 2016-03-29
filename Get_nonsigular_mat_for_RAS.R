##############################
###     Clean version      ###
##############################

colConst_init <- get_purch_price(IN_fd_exio_usd2007) # Convert to pp without tax
scaler_IND <- sum(IND_FD_ICP_usd2007)/sum(colConst_init)
rowConst_init <- as.vector(IND_FD_ICP_usd2007) / scaler_IND

qual_map_init <- bridge_icp_exio_q[,-1]
qual_map <- qual_map_init

rowConst <- rowConst_init
colConst <- colConst_init

result_RAS_fixed <- matrix(0, dim(qual_map_init)[1],dim(qual_map_init)[2])


# Needed to do multiple returns from functions
library(devtools)  
source_url("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

repeat {
  # This will return a list of coordinates.
  # [[1]] fx_coord_rowCon - Coord for cells fixed by row constraint
  # [[2]] fx_coord_colCon
  # [[3]] lone_cell
  # [[4]] beyond_const
  list[fx_coord_rowCon, fx_coord_colCon, lone_cell, beyond_const_r, beyond_const_c] <- IdentifyConflicts(qual_map, colConst, rowConst) 
  conflicts <- rbind(lone_cell, beyond_const_r, beyond_const_c)
  
  print(paste("Constrained by row con = ", fx_coord_rowCon[,1]))
  print(paste("Constrained by col con = ", fx_coord_colCon[,2]))
  
  finish_cond <- dim(fx_coord_rowCon)[1] == 0 & dim(fx_coord_colCon)[1] == 0
  # finish_cond <- dim(conflicts)[1] == 0
  
  if (finish_cond) {
    break
  }
  
  # This is just for conflict row/cols!
  if (dim(conflicts)[1]) {
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
  }
  
  # Fill in all other fixed cells in result_RAS_fixed 
  # Singular cells in the same row/col with conflict cells are already fixed above.
  # So this is for only the non-conflict singular cells not in the same row/col with conflicts.
  list[non_conflicts_r, non_conflicts_c]  <- FillNonConflictCells(qual_map, fx_coord_rowCon, fx_coord_colCon, conflicts, colConst, rowConst)
  # list[rowConst, colConst] <- UpdateConstsForNonConflicts(non_conflicts_r, non_conflicts_c, colConst, rowConst)
  
  # Fill the map with NAs for singular row/cols
  # Fill constraint vectors with NAs for singular row/cols
  list[qual_map, colConst, rowConst] <- UpdateQualMap_Const(qual_map, non_conflicts_r, non_conflicts_c, colConst, rowConst)
  
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

# Get draw
bridge_ICP_EXIO <- get_bridge_COICOP_EXIO(qmap_RAS, n_draw)

# Run RAS and construct final matrix in original dimension
library(mipfp)

# There can appear nearly zeros in the constraints because of scalings. So remove.
idx_zero <- mapply(function(x, y) {isTRUE(all.equal(x, y))}, colCon_RAS, 0)
# Temporary fix to match the constraint sums
colCon_RAS[colCon_RAS>100][1] <- colCon_RAS[colCon_RAS>100][1] + sum(colCon_RAS[idx_zero]) + gap
colCon_RAS[idx_zero] <- 0
idx_zero <- mapply(function(x, y) {isTRUE(all.equal(x, y))}, rowCon_RAS, 0)
# rowCon_RAS[rowCon_RAS>100][1] <- rowCon_RAS[rowCon_RAS>100][1] + sum(rowCon_RAS[idx_zero])
rowCon_RAS[idx_zero] <- 0

colCon <- colCon_RAS
rowCon <- rowCon_RAS
result <- list()

for (i in 1:n_draw) {
  seed <- diag(rowCon_RAS) %*% bridge_ICP_EXIO[[i]]
  result_RAS <- Ipfp(seed, list(1,2), list(rowCon_RAS, colCon_RAS), iter=5000)
  
  colCon <- cbind(colCon, colSums(result_RAS$x.hat))
  rowCon <- cbind(rowCon, rowSums(result_RAS$x.hat))
  
  result[[i]] <- result_RAS$x.hat
  
  print(i)
  # print(norm(result_RAS$x.hat, type="F"))  #"O", "I", "F", "M", "2"
}

# # Test sums
# colSums(result_RAS$x.hat)
# sum(colCon_RAS)
# sum(rowCon_RAS)
# sum(result_RAS$x.hat)
# print.mat(cbind(colCon_RAS, colSums(result_RAS$x.hat)))

# Try to plot some rows with three non-zero elements
# idx_visual <- which(rowSums(qmap_RAS)==3)
# rowSum_RAS <- rowSums(result[[1]])
# scale_row <- diag(1/rowSum_RAS)
# scale_row[is.infinite(scale_row)] <- 0
# 
# alloc_mat <- scale_row %*% result[[1]]
# alloc_mat <- lapply(result, function (x) {scale_row %*% x})
# sum(alloc_mat[idx_visual[1],]!=0)
# 
# plot_i <- 16
# idx_plot <- which(qmap_RAS[idx_visual[plot_i],]==1)
# pt_init <- do.call("rbind", lapply(bridge_ICP_EXIO, `[`, , idx_visual[plot_i]))
# pt_conv <- do.call("rbind", lapply(alloc_mat, `[`, idx_visual[plot_i], ))
# pt_res <- do.call("rbind", lapply(result, `[`, idx_visual[plot_i], ))
# pt_conv[,idx_plot]
# 
# plot3d(pt_init[,idx_plot])
# plot3d(pt_conv[,idx_plot], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1))


# Combine result_RAS and resut_RAS_fixed to form a whole allocation matrix
final_RAS_list <- lapply(result, function (x) {
  final_RAS <- matrix(0, dim(result_RAS_fixed)[1], dim(result_RAS_fixed)[2])
  final_RAS[-idx_row_removed, -idx_col_removed] <- x
  final_RAS <- final_RAS + result_RAS_fixed
  })

final_alloc_list <- lapply(final_RAS_list, function (x) {
  a <- diag(1/rowSums(x))
  a[is.infinite(a)] <- 0
  x <- a %*% x
})

final_rowCon <- rowSums(final_RAS_list[[1]])


# Estimate sectoral energy intensities with/without the RAS process
xlcFreeMemory()
ind_inten_RAS <- SetupSectorIntensities(final_alloc_list)
alloc_nonRAS <- get_bridge_COICOP_EXIO(qual_map_init, n_draw)
ind_inten_nonRAS <- SetupSectorIntensities(alloc_nonRAS)

# Plot intensities
png(filename = paste(figure_path, "Energy intensity by COICOP consumption category.png", sep=""), width = 781, height = 553, units = "px")
boxplot(ind_inten_RAS, xlab ="ICP sectors", ylab ="Energy intensity by consumption sector [MJ/EUR]", range=0, axes = FALSE)
axis(side = 1, at = seq(1,151,10))
axis(side = 2, at = seq(0,300,50))
# text(1:151, y=apply(ind_inten_RAS, 2, max)+50, bridge_icp_exio_q[,1], pos=4, offset=-.1, cex = 0.5, srt = 90)
text(1:151, y=apply(ind_inten_RAS, 2, max)+5, 1:151, pos=4, offset=-.1, cex = 0.6, srt = 90)
dev.off()

boxplot(ind_inten_nonRAS, xlab ="ICP sectors", ylab ="Energy intensity by consumption sector [MJ/EUR]", range=0, axes = FALSE)
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


