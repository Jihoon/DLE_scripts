#### Important note ####

# Functions here are revised versions of original Bridge_RAS.R.
# The purpose is to incorporate the schrunken Q and fd_exio matrices in the rIPFP process, 
# so in the end to be able to estimate final energy intensity by ICP item.



RemoveConflictsInConstraints_FE <- function(qmap_i, country="IND", fd_ex) {
  
  # Convert to pp without tax
  exio_fd_cty_usd <- fd_ex #eval(parse(text=paste0(country, "_fd_exio")))              # Mil.USD2007
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


Run_rIPFP_FE <- function(qual_map_init, country = "IND", fd_ex) {
  
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
        RemoveConflictsInConstraints_FE(qual_map_init, country, fd_ex)  
      # Get draw
      bridge_draw <- get_bridge_COICOP_EXIO(q_RAS, n_draw)
    }
    else if (D_val_uncertainty == 1)  {
      list[rCon_RAS, cCon_RAS, idx_r_removed, idx_c_removed, result_fixed, q_RAS] <- 
        RemoveConflictsInConstraints_FE(qual_map_init, country, fd_ex)  
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


SetupSectorIntensities_FE <- function (mapping_list, not_conv_idx , country = "IN", type='final', idx.comb) {
  ind_intensity <- vector()
  n_sector <- ifelse(country=="FR", n_sector_coicop, n_sector_icp_fuel)
  
  null_demand_int <- matrix(0, exio.len, n_sector)
  SectoralE_per_hh <- vector()
  
  cty_place <- which(exio_ctys==country)
  cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  
  cty_fd <- matrix(final_demand[, cty_idx_fd[1]], nrow=200)  # The country's hh fd column to a matrix (200x49) in bp
  a <- diag(1/rowSums(cty_fd))
  a[is.infinite(a)] <- 0
  cty_fd_ratio <- a %*% cty_fd  # fd exio-sectoral ratio in bp across countries
  cty_fd_ratio <- matrix(cty_fd_ratio, ncol=1) # 9800x1
  
  for (i in 1:length(mapping_list)) {  # length(mapping_list) instead of n_draws, because of potential no-convergence runs
    draw_count <<- i  # Used in get_basic_price
    
    # Identity mtx representing 1 2007USD spending in each ICP sector, now mapped to 200 EXIO sectors
    unit_exio <- diag(n_sector) %*% mapping_list[[i]]  # 164x200 
    
    # To run without valuation, toggle comment on this line.
    fd_bp_cty <- get_basic_price(t(unit_exio), country)  # Convert to bp (200x164) - each col represents bp fd in each exio sector (for 1 USD in ICP sector)
    list[q, fd_bp_cty] <- ShrinkQMapping(idx.comb, , fd_bp_cty)
    
    a <- do.call(rbind, replicate(num.cty, fd_bp_cty, simplify = FALSE))   # 49 regions in EXIO
    fd_bp <- apply(a, 2, function(x) {x * cty_fd_ratio})  # 9800X164
    
    if(type=='final') {
      int.e <- indirect_fE_int_shrnk  # We still need to add direct final energy intensity after this.
    }
    else if(type=='primary') {
      int.e <- indirect_E_int_shrnk
    }
    
    energy_int <- int.e %*% fd_bp * EXR_EUR$r  # indirect energy use from the supply chains (MJ/USD2007)
    ind_intensity <- rbind(ind_intensity, colSums(energy_int)) # Total indirect energy/cap by decile
  }
  
  # not_conv_idx has 1 where the RAS did not converge.
  ind_intensity <- ind_intensity[not_conv_idx!=1,]
  
  return(ind_intensity)
}

