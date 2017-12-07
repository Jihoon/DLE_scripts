########################
# Function definitions #
########################

# IdentifyConflicts
#
# Goal: For each iteration, this identifies cells that cannot satisfy two (row/col) constraints at the same time.
# 
# There are two main cases of conflicts:
# 1. Lone one: Singluar both by row and column
# 2. Beyond contrtaint: Singular by one side, but the constrained (fixed) cell value is larger than the marginal sum on the other side.
#
# Note: Other than these conflicts, there are still many singular cells which can still satisfy two constraints.
#       These are listed in fixed_cells_by_row and fixed_cells_by_col.

IdentifyConflicts <- function(qmap, colCon, rowCon) {
  idx_fixed_row <- which(rowSums(qmap, na.rm = TRUE)==1)
  idx_fixed_col <- which(colSums(qmap, na.rm = TRUE)==1)
  
  # There can be cases where these matrices have length or width = 1. I don't want this become a vector.
  fixed_row <- as.matrix(qmap[idx_fixed_row, , drop=FALSE])
  fixed_col <- as.matrix(qmap[, idx_fixed_col, drop=FALSE])
  fixed_cells_by_row <- cbind(idx_fixed_row, apply(fixed_row, 1, function (x) {which(x==1)}))
  fixed_cells_by_col <- cbind(apply(fixed_col, 2, function (x) {which(x==1)}), idx_fixed_col)
  
  # Singular by both row and col const
  a <- rbind(fixed_cells_by_row, fixed_cells_by_col)
  lone <- a[duplicated(a),,drop=FALSE]
  lone <- lone[colCon[lone[,2]]!=rowCon[lone[,1]],] # Lone cell Only when two constraints are different
  
  # Sum of all values in a row or column, which has only singular cells, is different from the constraint on the other side.
  # These can be treated as (group) lone cells
  group_lone_by_r <- which(colSums(fixed_row) == colSums(qmap) & colSums(fixed_row) >1)
  group_lone_by_c <- which(rowSums(fixed_col) == rowSums(qmap) & rowSums(fixed_col) >1)
  lone <- rbind(lone, 
                fixed_cells_by_col[fixed_cells_by_col[,1] %in% group_lone_by_c,],
                fixed_cells_by_row[fixed_cells_by_row[,2] %in% group_lone_by_r,])
  
  mat <- matrix(0, nrow = length(rowCon), ncol = length(colCon))
  mat[fixed_cells_by_row] <- 1
  if (length(idx_fixed_row) > 0) {
    # browser()
    mat[idx_fixed_row,] <- sweep(mat[idx_fixed_row, ,drop=FALSE], 1, rowCon[idx_fixed_row], '*')
  }
  # Singular by row const, which is larger than col const
  # idx_beyond_const <- rowCon[fixed_cells_by_row[,1]] > colCon[fixed_cells_by_row[,2]]
  idx_col <- sort(unique(fixed_cells_by_row[,2]))
  idx_beyond_const <- idx_col[colSums(mat[,idx_col,drop=FALSE], na.rm = TRUE) > colCon[idx_col]]
  beyond_const_r <- fixed_cells_by_row[fixed_cells_by_row[,2] %in% idx_beyond_const, ,drop=FALSE]
  
  mat[,] <- 0
  mat[fixed_cells_by_col] <- 1
  if (length(idx_fixed_col) > 0) {
    # browser()
    mat[,idx_fixed_col] <- sweep(mat[,idx_fixed_col,drop=FALSE], 2, colCon[idx_fixed_col], '*')
  }
  # Singular by col const, which is larger than row const
  # idx_beyond_const <- colCon[fixed_cells_by_col[,2]] > rowCon[fixed_cells_by_col[,1]]
  idx_row <- sort(unique(fixed_cells_by_col[,1]))
  idx_beyond_const <- idx_row[rowSums(mat[idx_row,,drop=FALSE], na.rm = TRUE) > rowCon[idx_row]]
  beyond_const_c <- fixed_cells_by_col[fixed_cells_by_col[,1] %in% idx_beyond_const, ,drop=FALSE]
  
  # beyond_const_r and beyond_const_c will include lone cells.
  # remove them.
  b <- rbind(lone, beyond_const_r)
  dupl <- b[duplicated(b),,drop=FALSE]
  beyond_const_r <- beyond_const_r[!(beyond_const_r[,2] %in% dupl[,2]),,drop=FALSE]

  b <- rbind(lone, beyond_const_c)
  dupl <- b[duplicated(b),,drop=FALSE]
  beyond_const_c <- beyond_const_c[!(beyond_const_c[,1] %in% dupl[,1]),,drop=FALSE]

  return(list(fixed_cells_by_row, fixed_cells_by_col, lone, beyond_const_r, beyond_const_c))
}


# UpdateConstsForConflicts
#
# Goal: 1. Scale up/down the conflict cell values in the row sum const
#       2. Fill in the fixed values for those conflict cells in result_RAS_fixed mtx.
# Type: 1. Lone cells where two const vals are different
#       2. Multiple singular cells in a row but their sum is larger than row sum const.
#       3. Multiple singular cells in a col but their sum is larger than col sum const.
# Remedy: 1. Replace the row const value with col const val.
#         2. Replace the row const value with the sum of all col const values for the singular cells.
#         3. Replace the row const values with the sum of all col const values for the singular cells.
# And, copy those fixed cells to result_RAS_fixed
#
# Test: 1. sum(result_RAS_fixed) == sum of copied cells in mat
#       2. sum(result_RAS_fixed) == rowCon[idx_fixed]

# Just correct for conflicts and at the end contstraints are all met.
# I don't subtract values from constraints here.
UpdateConstsForConflicts <- function(qmap, fx_rowCon, fx_colCon, lone, beyond_con_r, beyond_con_c, colCon, rowCon) {
  # Remove lone cells from beyond_con_r, if it is in beyond_con_r
  # a <- rbind(beyond_con_r, lone)
  # beyond_con_r <- a[!duplicated(a, fromLast = FALSE) & !duplicated(a, fromLast = TRUE), , drop = FALSE] 
  # a <- rbind(beyond_con_c, lone)
  # beyond_con_c <- a[!duplicated(a, fromLast = FALSE) & !duplicated(a, fromLast = TRUE), , drop = FALSE]
  
  lone_r <- lone[lone[,2] %in% lone[duplicated(lone[,2]),2],]
  lone_other <- lone[!lone[,2] %in% lone[duplicated(lone[,2]),2],]
  cols_with_conflic <- sort(unique(beyond_con_r[,2]))  # Need to treat this differently.
  rows_with_conflic <- sort(unique(beyond_con_c[,1])) 
  
  # Get coord for cells that are in the same row/col with conflicts, but not conflicts themselves
  cell_rowCon <- fx_rowCon[fx_rowCon[,2] %in% beyond_con_r[,2], , drop = FALSE]
  #   a <- rbind(cell_rowCon, beyond_con_r)
  #   rest_rowCon <- a[!duplicated(a, fromLast = FALSE) & !duplicated(a, fromLast = TRUE), , drop = FALSE] 
  
  cell_colCon <- fx_colCon[fx_colCon[,1] %in% beyond_con_c[,1], , drop = FALSE]
  #   a <- rbind(cell_colCon, beyond_con_c)
  #   rest_colCon <- a[!duplicated(a, fromLast = FALSE) & !duplicated(a, fromLast = TRUE), , drop = FALSE] 
  
  # Set a dummy mtx
  mat <- matrix(0, length(rowCon), length(colCon))
  
  # There can be NAs in qmap. Set it to 0 for the use in this function.
  qmap <- as.matrix(qmap)
  qmap[is.na(qmap)] <- 0
  
  # mat keeps constraints for conflict row/cols
  mat[lone] <- 1
  mat[cell_rowCon] <- qmap[cell_rowCon] # Need to take care of the whole row/cols containing singular cells. 
  mat[cell_colCon] <- qmap[cell_colCon]
  
  # Constrained by diff constraints
  # temp <- mat[,-cols_with_conflic] %*% diag(colCon[-cols_with_conflic])
  temp <- unique(c(rows_with_conflic))
  if(length(cols_with_conflic)) {
    mat[temp,-cols_with_conflic] <- mat[temp,-cols_with_conflic] %*% diag(colCon[-cols_with_conflic]) # Keep only the conflict rows  
    mat[,cols_with_conflic] <- diag(rowCon) %*% mat[,cols_with_conflic]
  }
  else {
    mat[temp,] <- mat[temp,] %*% diag(colCon[1:200]) # Keep only the conflict rows  
    # I don't know why, but diag(colCon[1:200]) and diag(colCon) are different
  }
  mat[lone_other] <- colCon[lone_other[,2]]  # lone cells should always be updated by col Const.
  
  # Scaling the group lone cells aligned vertically
  for (i in unique(lone_r[,2])) {
    sclr <- colCon[i] / sum(mat[,i]*rowCon, na.rm = TRUE)
    mat[,i] <- mat[,i] * rowCon * sclr
  }
  
  # Fill in the result matrix with the fixed values
  # This is ok for loners and beyond_con_c, but not beyond_con_r, because they violate col constraints, which need to be prioritized.
  mat[is.na(mat)] <- 0
  result_RAS_fixed[lone] <<- mat[lone]
  result_RAS_fixed[cell_colCon] <<- mat[cell_colCon]
  
  # Scale down row constraints when there are multiple singular cells in one column, whose sum is greater than col const of the col.
  idx_col <- cols_with_conflic
  scaler_col <- colCon[idx_col] / colSums(mat, na.rm = TRUE)[idx_col]
  scale_idx <- idx_col[which(scaler_col < 1)]
  scaler_col <- scaler_col[which(scaler_col < 1)]
  mat[,scale_idx] <- mat[,scale_idx] %*% diag(scaler_col, length(scaler_col), length(scaler_col)) # The length can be 1.
  result_RAS_fixed[cell_rowCon] <<- mat[cell_rowCon]
  
  # Rows that are not to be scaled to match col sum total
  # idx_fixed <- unique(rbind(rest_rowCon, rest_colCon, beyond_con_c, lone, beyond_con_r)[,1])
  idx_fixed <- unique(rbind(cell_colCon, cell_rowCon, lone)[,1])
  
  rowCon[idx_fixed] <- rowSums(mat, na.rm = TRUE)[idx_fixed]
  rowCon[-idx_fixed] <- rowCon[-idx_fixed] * 
    (sum(colCon, na.rm = TRUE) - sum(rowCon[idx_fixed], na.rm = TRUE)) / 
    (sum(rowCon, na.rm = TRUE) - sum(rowCon[idx_fixed], na.rm = TRUE))
  
  # list[rowCon, colCon] <- UpdateConstsForNonConflicts(rest_rowCon, rest_colCon, colCon, rowCon)
  
  return(list(rowCon, colCon, cell_rowCon, cell_colCon))
}

# UpdateConstsForNonConflicts
#
# Goal: 1. Adjust (i.e. subtract) fixed cell values from the two consts. (Singular but non-conflict cells)

UpdateConstsForNonConflicts <- function(non_conf_r, non_conf_c, colCon, rowCon) {
  row_r <- unique(non_conf_r[,1])
  col_r <- unique(non_conf_r[,2])
  
  row_c <- unique(non_conf_c[,1])
  col_c <- unique(non_conf_c[,2])
  
  mtx_for_sum <- matrix(0, dim(result_RAS_fixed)[1],dim(result_RAS_fixed)[2])
  mtx_for_sum[non_conf_r] <- result_RAS_fixed[non_conf_r]
  
  rowCon[row_r] <- rowCon[row_r] - rowSums(mtx_for_sum[row_r, , drop=FALSE], na.rm = TRUE)
  colCon[col_r] <- colCon[col_r] - colSums(mtx_for_sum[, col_r, drop=FALSE], na.rm = TRUE)
  
  if(sum(colCon <0)) {
    # browser()
  }
  
  mtx_for_sum[,] <- 0
  mtx_for_sum[non_conf_c] <- result_RAS_fixed[non_conf_c]
  
  rowCon[row_c] <- rowCon[row_c] - rowSums(mtx_for_sum[row_c, , drop=FALSE], na.rm = TRUE)
  colCon[col_c] <- colCon[col_c] - colSums(mtx_for_sum[, col_c, drop=FALSE], na.rm = TRUE)
  
  if(sum(colCon <0)) {
    # browser()
  }
  
  return(list(rowCon, colCon))
}

# FillNonConflictCells
#
# Goal: 1. Copy fixed cell values to result_RAS_fixed mtx (Singular but non-conflict cells)
# Note: Conflict cells are already copied at UpdateConstsForConflicts.

FillNonConflictCells <- function(qual_map, fx_rowCon, fx_colCon, conflic, colCon, rowCon) {
  
  # Pick only non-conflict singular cells
  #   a <- rbind(fx_rowCon, conflic)
  #   conf_row <- a[duplicated(a), , drop = FALSE]
  #   a <- rbind(fx_rowCon, conf_row)
  #   nonconf_row <- a[!duplicated(a, fromLast = FALSE) & !duplicated(a, fromLast = TRUE), , drop = FALSE] 
  # 
  #   a <- rbind(fx_colCon, conflic)
  #   conf_col <- a[duplicated(a), , drop = FALSE]
  #   a <- rbind(fx_colCon, conf_col)
  #   nonconf_col <- a[!duplicated(a, fromLast = FALSE) & !duplicated(a, fromLast = TRUE), , drop = FALSE]
  
  # Pick only non-conflict singular cells not in the same row/col with conflicts
  idx_conf_row <- unique(conflic[,1])
  idx_conf_col <- unique(conflic[,2])
  
  is.noncon_row <- !(fx_rowCon[,1] %in% idx_conf_row) & !(fx_rowCon[,2] %in% idx_conf_col)
  nonconf_row <- fx_rowCon[is.noncon_row, , drop=FALSE]
  
  is.noncon_col <- !(fx_colCon[,1] %in% idx_conf_row) & !(fx_colCon[,2] %in% idx_conf_col)
  nonconf_col <- fx_colCon[is.noncon_col, , drop=FALSE]
  
  result_RAS_fixed[nonconf_row] <<- rowCon[nonconf_row[,1]]
  result_RAS_fixed[nonconf_col] <<- colCon[nonconf_col[,2]]
  
  return(list(nonconf_row, nonconf_col))
}

# UpdateQualMap_Const
# Goal: Remove row/col that already satisfy consts from qual_map and const vectors 

UpdateQualMap_Const <- function(qmap, to_remove_r, to_remove_c, colCon, rowCon) {
  idx_row_r <- unique(to_remove_r[,1])
  idx_row_c <- unique(to_remove_r[,2])
  idx_col_r <- unique(to_remove_c[,1])
  idx_col_c <- unique(to_remove_c[,2])
  
  qmap[idx_row_r, ] <- NA
  qmap[, idx_col_c] <- NA
  
  mtx_for_sum <- matrix(0, dim(result_RAS_fixed)[1],dim(result_RAS_fixed)[2])
  mtx_for_sum[to_remove_r] <- result_RAS_fixed[to_remove_r]
  
  if(sum(colCon[idx_row_c] < colSums(mtx_for_sum[, idx_row_c, drop=FALSE], na.rm = TRUE), na.rm = TRUE) > 0) {
    # browser()
  }
  
  rowCon[idx_row_r] <- NA
  colCon[idx_row_c] <- colCon[idx_row_c] - colSums(mtx_for_sum[, idx_row_c, drop=FALSE], na.rm = TRUE)
  # Temporary fix to match the constraint sums 
  # There are cases where some elements of colCon are near-zero (but non-zero) because of scaling.
  idx_zero <- mapply(function(x, y) {isTRUE(all.equal(x, y))}, colCon, 0)
  colCon[which(colCon>100)[1]] <- colCon[which(colCon>100)[1]] + sum(colCon[idx_zero], na.rm = TRUE) 
  colCon[idx_zero] <- 0
  
  mtx_for_sum[,] <- 0
  mtx_for_sum[to_remove_c] <- result_RAS_fixed[to_remove_c]
  
  if(sum(rowCon[idx_col_r] < rowSums(mtx_for_sum[idx_col_r, , drop=FALSE], na.rm = TRUE), na.rm = TRUE) > 0) {
    # browser()
  }
  
  rowCon[idx_col_r] <- rowCon[idx_col_r] - rowSums(mtx_for_sum[idx_col_r, , drop=FALSE], na.rm = TRUE)
  colCon[idx_col_c] <- NA  
  # Temporary fix to match the constraint sums 
  # There are cases where some elements of rowCon are near-zero (but non-zero around 1e-12) because of scaling.
  idx_zero <- mapply(function(x, y) {isTRUE(all.equal(x, y))}, rowCon, 0)
  rowCon[which(rowCon>100)[1]] <- rowCon[which(rowCon>100)[1]] + sum(rowCon[idx_zero], na.rm = TRUE) 
  rowCon[idx_zero] <- 0
  
  return(list(qmap, colCon, rowCon))
}


CollapseQualMap <- function(qmap, colCon, rowCon) {
  colRAS <- !is.na(colCon)
  rowRAS <- !is.na(rowCon)
  colCon <- colCon[colRAS]
  rowCon <- rowCon[rowRAS]
  qmap <- qmap[rowRAS, colRAS]
  
  return(list(qmap, rowCon, colCon))
}


# Calculate ICP sectoral intensities from given allocation ratio matrix based on random draws (either RASed or non-RASed)
SetupSectorIntensities <- function (mapping_list, not_conv_idx , country = "IN", type='final', final.intensity.mat=indirect_fE_int) {
  ind_intensity <- vector()
  n_sector <- ifelse(country=="FR", n_sector_coicop, n_sector_icp_fuel)
  
  null_demand_int <- matrix(0, 9600, n_sector)
  SectoralE_per_hh <- vector()
  
  cty_place <- which(exio_ctys==country)
  cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  
  cty_fd <- matrix(final_demand[, cty_idx_fd[1]], nrow=200)  # The country's hh fd column to a matrix (200x48) in bp
  a <- diag(1/rowSums(cty_fd))
  a[is.infinite(a)] <- 0
  cty_fd_ratio <- a %*% cty_fd  # fd exio-sectoral ratio in bp across countries
  cty_fd_ratio <- matrix(cty_fd_ratio, ncol=1) # 9600x1
  
  for (i in 1:length(mapping_list)) {  # length(mapping_list) instead of n_draws, because of potential no-convergence runs
    draw_count <<- i  # Used in get_basic_price
    
    # Identity mtx representing 1 2007USD spending in each ICP sector, now mapped to 200 EXIO sectors
    unit_exio <- diag(n_sector) %*% mapping_list[[i]]  # 164x200 
    
    # To run without valuation, toggle comment on this line.
    fd_bp_cty <- get_basic_price(t(unit_exio), country)  # Convert to bp (200x164) - each col represents bp fd in each exio sector (for 1 USD in ICP sector)
    # fd_bp <- t(unit_exio)  # Without valuation
    
    a <- do.call(rbind, replicate(48, fd_bp_cty, simplify = FALSE))   # 48 regions in EXIO
    fd_bp <- apply(a, 2, function(x) {x * cty_fd_ratio})  # 9600X164
    
    # fd_exio <- mapping_list[[i]] %*% diag(fd_decile[,2])  # For Ensemble
    
    # cty_fd <- null_demand_int
    # cty_fd[cty_idx,] <- get_basic_price(fd_exio, country)
    
    if(type=='final') {
      # a <- matrix(0, nrow=9600, ncol=164)
      # a[cty_idx,] <- fd_bp_cty
      # energy_int <- (indirect_fE_int %*% fd_bp + int.hh %*% a) * EXR_EUR$r  # indirect energy use from the supply chains (MJ/USD2007) 69x164
      # int.e <- indirect_fE_int  # We still need to add direct final energy intensity after this.
      # int.e <- indir.fin.eng.int.derived  # We still need to add direct final energy intensity after this.
      int.e <- final.intensity.mat # Testing carving out electricity/gasoline. this function will be run separately for these carriers.
    }
    else if(type=='primary') {
      int.e <- indirect_E_int
    }
    
    energy_int <- int.e %*% fd_bp * EXR_EUR$r  # indirect energy use from the supply chains (MJ/USD2007)
    ind_intensity <- rbind(ind_intensity, colSums(energy_int)) # Total indirect energy/cap by decile
  }
  
  # not_conv_idx has 1 where the RAS did not converge.
  ind_intensity <- ind_intensity[not_conv_idx!=1,]
  
  return(ind_intensity)
}


# Emission intensity in ICP classification
SetupEmissionIntensities <- function (mapping_list, not_conv_idx , country = "IN") {
  ind_intensity <- vector()
  n_sector <- ifelse(country=="FR", n_sector_coicop, n_sector_icp_fuel)
  
  null_demand_int <- matrix(0, 9600, n_sector)
  SectoralE_per_hh <- vector()
  
  cty_place <- which(exio_ctys==country)
  cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  
  cty_fd <- matrix(final_demand[, cty_idx_fd[1]], nrow=200)  # The country's hh fd column to a matrix (200x48) in bp
  a <- diag(1/rowSums(cty_fd))
  a[is.infinite(a)] <- 0
  cty_fd_ratio <- a %*% cty_fd  # fd exio-sectoral ratio in bp across countries
  cty_fd_ratio <- matrix(cty_fd_ratio, ncol=1) # 9600x1
  
  for (i in 1:length(mapping_list)) {  # length(mapping_list) instead of n_draws, because of potential no-convergence runs
    draw_count <<- i  # Used in get_basic_price
    
    # Identity mtx representing 1 2007USD spending in each ICP sector, now mapped to 200 EXIO sectors
    unit_exio <- diag(n_sector) %*% mapping_list[[i]]  # 164x200 
    
    # To run without valuation, toggle comment on this line.
    # fd_bp <- get_basic_price(t(unit_exio), country)  # Convert to bp (200x164) - each col represents bp fd in each exio sector (for 1 USD in ICP sector)
    fd_bp <- t(unit_exio)
    
    a <- do.call(rbind, replicate(48, fd_bp, simplify = FALSE))   # 48 regions in EXIO
    fd_bp <- apply(a, 2, function(x) {x * cty_fd_ratio})  # 9600X164
    
    energy_int <- indirect_em_int %*% fd_bp * EXR_EUR$r  # indirect energy use from the supply chains (MJ/USD2007)

    ind_intensity <- rbind(ind_intensity, colSums(energy_int)) # Total indirect energy/cap by decile
  }
  
  # not_conv_idx has 1 where the RAS did not converge.
  ind_intensity <- ind_intensity[not_conv_idx!=1,]
  
  return(ind_intensity)
}


RemoveConflictsInConstraints <- function(qmap_i, country="IND") {
  
  # Convert to pp without tax
  exio_fd_cty_usd <- eval(parse(text=paste0(country, "_fd_exio")))              # Mil.USD2007
  icp_fd_cty_usd <- eval(parse(text=paste0(country, "_FD_ICP_usd2007")))        # Mil.USD2007
  
  colConst_init <- get_purch_price(exio_fd_cty_usd, countrycode(country,"iso3c", "iso2c")) 
  scaler_cty <- sum(icp_fd_cty_usd[,1])/sum(colConst_init)
  rowConst_init <- as.vector(icp_fd_cty_usd[,1]) / scaler_cty
  
  qual_map <- qmap_i
  
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


Run_rIPFP <- function(qual_map_init, country = "IND") {
  
  colCon <- NULL
  rowCon <- NULL
  result <- list()
  non_converge <- rep(0, n_draw)
  
  draw_count <<- 1
  
  for (i in 1:n_draw) {
    result_RAS_fixed <<- matrix(0, dim(qual_map_init)[1],dim(qual_map_init)[2])
    
    draw_count <<- i   # Indicate that we are on i-th draw. Used in get_basic_price and get_purch_price
    
    if (D_val_uncertainty == 0 & i==1) {
      list[rCon_RAS, cCon_RAS, idx_r_removed, idx_c_removed, result_fixed, q_RAS] <- 
        RemoveConflictsInConstraints(qual_map_init, country)  
      # Get draw
      bridge_draw <- get_bridge_COICOP_EXIO(q_RAS, n_draw)
    }
    else if (D_val_uncertainty == 1)  {
      list[rCon_RAS, cCon_RAS, idx_r_removed, idx_c_removed, result_fixed, q_RAS] <- 
        RemoveConflictsInConstraints(qual_map_init, country)  
      # Get draw
      bridge_draw <- get_bridge_COICOP_EXIO(q_RAS, 1)
    }
    
    if (D_val_uncertainty == 0)  {bridge <- bridge_draw[[i]]}
    else {bridge <- bridge_draw[[1]]}
    
    seed <- diag(rCon_RAS) %*% bridge   # RAS init mtx
    # print(head(seed[2,]))
    result_RAS <- Ipfp(seed, list(1,2), list(rCon_RAS, cCon_RAS), iter=10000)
    if(result_RAS$conv == FALSE) {
      print(paste("Didn't converge at", draw_count))
      non_converge[i] <- 1 }
    
    colCon <- cbind(colCon, colSums(result_RAS$x.hat))
    rowCon <- cbind(rowCon, rowSums(result_RAS$x.hat))
    
    final_RAS <- matrix(0, dim(qual_map_init)[1],dim(qual_map_init)[2])
    # Need to differentiate for cases without any conflicts
    if (length(idx_r_removed)==0 & length(idx_c_removed)==0) {
      final_RAS <- result_RAS$x.hat
    }
    else if (length(idx_r_removed)==0 & length(idx_c_removed)>0) {
      final_RAS[, -idx_c_removed] <- result_RAS$x.hat
    }
    else if (length(idx_r_removed)>0 & length(idx_c_removed)==0) {
      final_RAS[-idx_r_removed, ] <- result_RAS$x.hat
    }
    else {
      final_RAS[-idx_r_removed, -idx_c_removed] <- result_RAS$x.hat
    }
    final_RAS <- final_RAS + result_fixed
    
    result[[i]] <- final_RAS
    
    if (i %% 10 == 0) {print(i)}
    rCon_new <- rowSums(result_fixed) 
    rCon_new[-idx_r_removed] <- rCon_new[-idx_r_removed] + rCon_RAS
    # print(paste("sum of new rCon", draw_count))
    # print(result[[i]][40,])
  }
  return(list(result, non_converge, rCon_new))
}


# Deriving energy shares between Industry vs Transportation for certain consumption baskets (in ICP classification).
DeriveConsumptionEnergyShares <- function (mapping_list, consumption_vec, not_conv_idx, country = "IN", type='primary') {
  # length(consumption_vec) = n_sector_icp_fuel (row matrix)
  industry_trp_energy <- vector()
  n_sector <- ifelse(country=="FR", n_sector_coicop, n_sector_icp_fuel) # n_sector_icp_fuel=164
  idx_trnsprt_EXIO <- 157:163
  
  null_demand_int <- matrix(0, 9600, n_sector)
  SectoralE_per_hh <- vector()
  
  cty_place <- which(exio_ctys==country)
  cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  
  cty_fd <- matrix(final_demand[, cty_idx_fd[1]], nrow=200)  # The country's hh fd column to a matrix (200x48) in bp
  a <- diag(1/rowSums(cty_fd))
  a[is.infinite(a)] <- 0
  cty_fd_ratio <- a %*% cty_fd  # fd exio-sectoral ratio in bp across countries
  cty_fd_ratio <- matrix(cty_fd_ratio, ncol=1) # 9600x1
  
  for (i in 1:length(mapping_list)) {  # length(mapping_list) instead of n_draws, because of potential no-convergence runs
    draw_count <<- i  # Used in get_basic_price
    
    # Converting the given cvec (ICP) into cvec (EXIO)
    cv_exio <- consumption_vec %*% mapping_list[[i]]  # 1x200 
    
    # To run without valuation, toggle comment on this line.
    fd_bp_cty <- get_basic_price(t(cv_exio), country)  # Convert to bp (200x164) - each col represents bp fd in each exio sector (for 1 USD in ICP sector)
    # fd_bp <- t(unit_exio)  # Without valuation
    
    a <- do.call(rbind, replicate(48, fd_bp_cty, simplify = FALSE))   # 48 regions in EXIO
    fd_bp <- apply(a, 2, function(x) {x * cty_fd_ratio})  # 9600X1
    
    if(type=='final') {
      int.e <- indir.fin.eng.int.derived  # We still need to add direct final energy intensity after this.
    }
    else if(type=='primary') {
      int.e <- indirect_E_int
    }
    
    energy <- eigenMapMatMult(int.e, diag(as.numeric(fd_bp))) * EXR_EUR$r  # n_carrierx9600  indirect energy use from the supply chains (MJ/USD2007)
    # trp_energy <- sum(energy[, as.numeric(sapply(idx_trnsprt_EXIO, function(x) {x+seq(0, 9400, 200)}))])
    energy.i <- function(i) {
      return(sum(energy[, i+seq(0, 9400, 200)]))
    }
    trp_energy <- sapply(idx_trnsprt_EXIO, energy.i)
    ind_energy <- sum(energy[, -as.numeric(sapply(idx_trnsprt_EXIO, function(x) {x+seq(0, 9400, 200)}))])
    industry_trp_energy <- rbind(industry_trp_energy, c(ind_energy, trp_energy)) # Total indirect energy/cap by decile
  }
  
  # not_conv_idx has 1 where the RAS did not converge.
  industry_trp_energy <- industry_trp_energy[not_conv_idx!=1,]
  
  return(colMeans(industry_trp_energy))
}


# Deriving energy carrier shares for each consumption sector (in ICP classification).
DeriveEnergyCarrierSharesbySector <- function (mapping_list, consumption_vec, not_conv_idx, country = "IN", type='primary') {
  # length(consumption_vec) = n_sector_icp_fuel (row matrix)
  energy_by_carrier <- vector()
  n_sector <- ifelse(country=="FR", n_sector_coicop, n_sector_icp_fuel) # n_sector_icp_fuel=164
  
  # EXIO carrier index:   
  exio_solid <- c(exio_coal, 64:65)
  exio_gas <- c(exio_ng, 75, 142:146)
  # exio_elec # already exists
  exio_liquid <- setdiff(c(exio_oil, exio_oilprod, 30:31), c(64:65, 75))
  
  null_demand_int <- matrix(0, 9600, n_sector)
  SectoralE_per_hh <- vector()
  
  cty_place <- which(exio_ctys==country)
  cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  
  cty_fd <- matrix(final_demand[, cty_idx_fd[1]], nrow=200)  # The country's hh fd column to a matrix (200x48) in bp
  a <- diag(1/rowSums(cty_fd))
  a[is.infinite(a)] <- 0
  cty_fd_ratio <- a %*% cty_fd  # fd exio-sectoral ratio in bp across countries
  cty_fd_ratio <- matrix(cty_fd_ratio, ncol=1) # 9600x1
  
  for (i in 1:length(mapping_list)) {  # length(mapping_list) instead of n_draws, because of potential no-convergence runs
    draw_count <<- i  # Used in get_basic_price
    
    # Converting the given cvec (ICP) into cvec (EXIO)
    cv_exio <- consumption_vec %*% mapping_list[[i]]  # 1x200 
    
    # To run without valuation, toggle comment on this line.
    fd_bp_cty <- get_basic_price(t(cv_exio), country)  # Convert to bp (200x164) - each col represents bp fd in each exio sector (for 1 USD in ICP sector)
    # fd_bp <- t(unit_exio)  # Without valuation
    
    a <- do.call(rbind, replicate(48, fd_bp_cty, simplify = FALSE))   # 48 regions in EXIO
    fd_bp <- apply(a, 2, function(x) {x * cty_fd_ratio})  # 9600X1
    
    if(type=='final') {
      int.e <- indir.fin.eng.int.derived  # We still need to add direct final energy intensity after this.
    }
    else if(type=='primary') {
      int.e <- indirect_E_int
    }
    
    energy <- eigenMapMatMult(int.e, diag(as.numeric(fd_bp))) * EXR_EUR$r  # n_carrierx9600  indirect energy use from the supply chains (MJ/USD2007)
    # trp_energy <- sum(energy[, as.numeric(sapply(idx_trnsprt_EXIO, function(x) {x+seq(0, 9400, 200)}))])
    energy.i <- function(i) {
      return(sum(energy[, i+seq(0, 9400, 200)]))
    }
    energy_solid <- sapply(exio_solid, energy.i)
    energy_liquid <- sapply(exio_liquid, energy.i)
    energy_elec <- sapply(exio_elec, energy.i)
    energy_gas <- sapply(exio_gas, energy.i)
    energy_by_carrier <- rbind(energy_by_carrier, c(energy_solid, energy_liquid, energy_gas, energy_elec)) # Total indirect energy/cap by decile
  }
  
  # not_conv_idx has 1 where the RAS did not converge.
  energy_by_carrier <- energy_by_carrier[not_conv_idx!=1,]
  
  return(colMeans(energy_by_carrier))
}


