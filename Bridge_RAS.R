# Test RAS for IND

# Get IND final demand from EXIO [M.EUR]
IN_place <- which(exio_ctys=="IN")
IN_idx_fd <- seq(7*(IN_place-1)+1, 7*IN_place)   # 7 final demand columns per country
IN_fd <- matrix(final_demand[,IN_idx_fd[1]], nrow=200)
# dim_fd <- dim(final_demand)
IN_fd_exio <- rowSums(IN_fd) # Sum all HH FD across countries
IN_fd_exio_imp <- rowSums(IN_fd[,-IN_place]) # Sum all HH FD across countries

# Get IND final demand in ICP 

# Remove tax observations from DB
IND_FD_code <- IND_FD[-grep("taxes", IND_FD$item, ignore.case = TRUE), ]
IND_FD_code <- merge(IND_FD_code, IND_map[,c("CODE", "COICOP1", "COICOP2", "ITEM_DLE")], by.x="item", by.y="ITEM_DLE", all.x = TRUE)
IND_FD_code <- IND_FD_code[order(IND_FD_code$CODE),]
IND_FD_code[is.na(IND_FD_code)] <- 0

# For IND2
# Note: need to add COICOP1 info to get annual hh consumption increase rates.
# Some item names are different from IND1.
IND2_FD_code <- IND2_FD[-grep("taxes", IND2_FD$item, ignore.case = TRUE), ]
IND2_FD_code <- merge(IND2_FD_code, IND_map[,c("CODE", "COICOP1", "COICOP2", "ITEM_DLE")], by.x="item", by.y="ITEM_DLE", all.x = TRUE)

missing <- IND2_FD_code %>% filter(CODE==0)
write.table(missing, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
# So I assigned COICOP numbers to items only appearing in IND2.
wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Bridging/CES-COICOP/For IND2-IND1 matching.xlsx")
missing_cat  <- XLConnect::readWorksheet(wb, "Sheet1", header=TRUE, forceConversion=T) 

IND2_FD_code <- merge(IND2_FD_code, missing_cat[,c("CODE", "COICOP1", "COICOP2", "item")], by="item", all.x = TRUE)
IND2_FD_code[is.na(IND2_FD_code$CODE.x), ] <- IND2_FD_code %>% filter(is.na(CODE.x)) %>% mutate(CODE.x = CODE.y, COICOP1.x = COICOP1.y, COICOP2.x = COICOP2.y)
IND2_FD_code <- select(IND2_FD_code,-CODE.y, -COICOP1.y, -COICOP2.y) %>% rename(CODE = CODE.x, COICOP1 = COICOP1.x, COICOP2 = COICOP2.x)

IND2_FD_code <- IND2_FD_code[order(IND2_FD_code$CODE),]
IND2_FD_code[is.na(IND2_FD_code)] <- 0

# 
# a <- strsplit(missing$item, split="[[:punct:] ]")
# a <- lapply(a, function(x) {x[x != ""]})
# b <- strsplit(IND_FD_code$item, split="[[:punct:] ]")
# b <- lapply(b, function(x) {x[x != ""]})



# CES_ICP_IDN, CES_ICP_IND rows are sorted by Survey Code.
# IND_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD_code[,2])   # only for total
IND_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD_code[,2:12])  # for all deciles and total


# Get IDN final demand from EXIO [M.EUR]
# ID_place <- which(exio_ctys=="ID")
# ID_idx <- seq(200*(ID_place-1)+1, 200*ID_place)  # 200 EXIO commodities per country
# ID_idx_fd <- seq(7*(ID_place-1)+1, 7*ID_place)   # 7 final demand columns per country
# ID_fd_exio <- rowSums(final_demand[ID_idx, seq(1, dim_fd[2], 7)]) # Sum all HH FD across countries
# 
# # Get IDN final demand in ICP 
# 
# # Remove tax observations from DB
# IDN_FD_code <- IDN_FD[-grep("taxes", IDN_FD$ITEM, ignore.case = TRUE), ]
# IDN_FD_code <- merge(IDN_FD_code, IDN_map[,c("CODE", "COICOP2", "ITEM_DLE")], by.x="ITEM", by.y="ITEM_DLE")
# IDN_FD_code <- IDN_FD_code[order(IDN_FD_code$CODE),]
# IDN_FD_code[is.na(IDN_FD_code)] <- 0
# 
# # CES_ICP_IDN, CES_ICP_IDN rows are sorted by Survey Code.
# IDN_FD_ICP <- t(CES_ICP_IDN) %*% as.matrix(IDN_FD_code[,2])



##########################################
### Check FD totals from EXIO and CES  ###
##########################################


library(WDI)

# DLE DB in PPP 2010$ (PPP in terms of private consumption)
# EXIO in MER 2007
# Need this PPP rate to go back to local currency in 2010
# [LCU/$]
PPP_IND = WDI(country = "IN", indicator = c("PA.NUS.PPP", "PA.NUS.PRVT.PP"), start = 2010, end = 2010, extra = FALSE, cache = NULL)
PPP_IND <- PPP_IND$PA.NUS.PRVT.PP

# Inflation
CPI_IND <- WDI(country = "IN", indicator = "FP.CPI.TOTL.ZG", start = 2007, end = 2010, extra = FALSE, cache = NULL)
CPI_IND <- CPI_IND %>% rename(cpi=FP.CPI.TOTL.ZG)
CPI_IND$cpi <- CPI_IND$cpi/100 + 1
CPI_ratio <- prod(CPI_IND$cpi)

# Exchange rate (MER) [LCU/$]
EXR_EUR <- WDI(country = "XC", indicator = "PA.NUS.FCRF", start = 2007, end = 2007, extra = FALSE, cache = NULL)
EXR_EUR <- EXR_EUR %>% rename(r=PA.NUS.FCRF)
EXR_IND <- WDI(country = "IN", indicator = "PA.NUS.FCRF", start = 2007, end = 2007, extra = FALSE, cache = NULL)
EXR_IND <- EXR_IND %>% rename(r=PA.NUS.FCRF)

# HH Consumption in India 2007 [US$]
HH_CON <- WDI(country = "IN", indicator = c("NE.CON.PETC.CD", "NE.CON.PRVT.CD", "NE.CON.PETC.CN", "NE.CON.PRVT.CN"), start = 2007, end = 2011, extra = FALSE, cache = NULL)
HH_CON <- HH_CON %>% rename(hhcon=NE.CON.PRVT.CD)
HH_CON[,4:7] <- HH_CON[,4:7]/1e6 #[million US$]


# Adjust for real consumption increase
# consumption_growth <- (sum(IND_FD[,2]) / sum(IND2_FD[,2]))^(1/7)  # 2004-2005 to 2011-2012
# consumption_growth <- (HH_CON$hhcon[5]/HH_CON$hhcon[1])^(1/4)
# (sum(IND_FD_ICP_usd2007) / consumption_growth^4) / (sum(IN_fd_exio_usd2007) ) # Assuming EXIO already incorporated real consumption increase..

# by decile
IND_FD_code$COICOP1 <- as.numeric(IND_FD_code$COICOP1)
IND2_FD_code$COICOP1 <- as.numeric(IND2_FD_code$COICOP1)
a <- aggregate(. ~ COICOP1, data=IND_FD_code[,2:14], sum)
b <- aggregate(. ~ COICOP1, data=IND2_FD_code[,2:14], sum)

consumption_growth <- (a[,2:12]/b[,2:12])^(1/7) # consumption growth rate in 7 years by decile
consumption_growth_DE <- (IND_DE[,2:12]/IND2_DE[,2:12])^(1/7) # consumption growth rate in 7 years by decile
consumption_growth_FD_DE <- (IND_FD_DE[,2:12]/IND2_FD_DE[,2:12])^(1/7) # consumption growth rate in 7 years by decile

# IND1 is for 2010-2011
# in M.EUR
IND_FD_ICP_usd2007 <- IND_FD_ICP * PPP_IND / CPI_ratio / EXR_IND$r / 1e6
IND_FD_ICP_usd2007 <- IND_FD_ICP_usd2007/((consumption_growth^4)[as.numeric(icp_ntnu$COICOP1)[1:151],])
IN_fd_exio_usd2007 <- IN_fd_exio / EXR_EUR$r

IND_DE_intst2007 <- (IND_DE[,2:12] / (consumption_growth_DE^4)) / (IND_FD_DE[,2:12] / (consumption_growth_FD_DE^4))

# sum(IND_FD_ICP_usd2007) # CES FD in 2007$ (MER) based on 2011-2012 NSS -> Use this for RAS
# sum(IN_fd_exio_usd2007) # EXIO FD in 2007$ (MER?) based on 2004 IoT -> Use this for RAS

# From Indian I-O 2007 - total PFCE (Private Final Consumption Expenditure)
IND_FD_IO2007_usd <- 278289600 * 1e5 / EXR_IND$r / 1e6 # Convert Rs. Lakhs to Million $ 2007




# Functions

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
  fixed_row <- as.matrix(qmap[idx_fixed_row,])
  fixed_col <- as.matrix(qmap[,idx_fixed_col])
  fixed_cells_by_row <- cbind(idx_fixed_row, apply(fixed_row, 1, function (x) {which(x==1)}))
  fixed_cells_by_col <- cbind(apply(fixed_col, 2, function (x) {which(x==1)}), idx_fixed_col)
  
  # Singular by both row and col const
  a <- rbind(fixed_cells_by_row, fixed_cells_by_col)
  lone <- a[duplicated(a),,drop=FALSE]
  
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
  
  # beyond_const_r and beyond_const_c will include lone celss.
  
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
  a <- rbind(beyond_con_r, lone)
  beyond_con_r <- a[!duplicated(a, fromLast = FALSE) & !duplicated(a, fromLast = TRUE), , drop = FALSE] 
  
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
  temp <- c(rows_with_conflic, lone[,1])
  if(length(cols_with_conflic)) {
    mat[temp,-cols_with_conflic] <- mat[temp,-cols_with_conflic] %*% diag(colCon[-cols_with_conflic]) # Keep only the conflict rows  
    mat[,cols_with_conflic] <- diag(rowCon) %*% mat[,cols_with_conflic]
  }
  else {
    mat[temp,] <- mat[temp,] %*% diag(colCon[1:200]) # Keep only the conflict rows  
    # I don't know why, but diag(colCon[1:200]) and diag(colCon) are different
  }
  
  # Fill in the result matrix with the fixed values
  # This is ok for loners and beyond_con_c, but not beyond_con_r, because they violate col constraints, which need to be prioritized.
  mat[is.na(mat)] <- 0
  result_RAS_fixed[lone] <<- mat[lone]
  result_RAS_fixed[cell_colCon] <<- mat[cell_colCon]
  
  # Scale down row constraints when there are multiple singular cells in one column, whose sum is greater than col const of the col.
  idx_col <- cols_with_conflic
  scaler_col <- colCon[idx_col] / colSums(mat, na.rm = TRUE)[idx_col]
  mat[,idx_col] <- mat[,idx_col] %*% diag(scaler_col, length(scaler_col), length(scaler_col)) # The length can be 1.
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
SetupSectorIntensities <- function (mapping_list, not_conv_idx , country = "IN") {
  ind_intensity <- vector()
  null_demand_int <- matrix(0, 9600, n_sector_icp)
  SectoralE_per_hh <- vector()
  
  cty_place <- which(exio_ctys==country)
  cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  
  cty_fd <- matrix(final_demand[, cty_idx_fd[1]], nrow=200)  # The country's hh fd column to a matrix (200x48) in bp
  a <- diag(1/rowSums(cty_fd))
  a[is.infinite(a)] <- 0
  cty_fd_ratio <- a %*% cty_fd  # fd exio-sectoral ratio in bp across countries
  cty_fd_ratio <- matrix(cty_fd_ratio, ncol=1) # 9600x1
  
  for (i in 1:n_draw) {
    draw_count <<- i  # Used in get_basic_price
    
    # Identity mtx representing 1 EUR spending in each ICP sector, now mapped to 200 EXIO sectors
    unit_exio <- diag(n_sector_icp) %*% mapping_list[[i]]  # 151x200 
    fd_bp <- get_basic_price(t(unit_exio), country)  # Convert to bp (200x151) - each col represents bp fd in each exio sector (for 1 EUR in ICP sector)

    a <- do.call(rbind, replicate(48, fd_bp, simplify = FALSE))   # 48 regions in EXIO
    fd_bp <- apply(a, 2, function(x) {x * cty_fd_ratio})  # 9600X151
    
    # fd_exio <- mapping_list[[i]] %*% diag(fd_decile[,2])  # For Ensemble
    
    # cty_fd <- null_demand_int
    # cty_fd[cty_idx,] <- get_basic_price(fd_exio, country)
    
    energy_int <- indirect_E_int %*% fd_bp   # indirect energy use from the supply chains
    # energy_tot <- indirect_E_int %*% cty_fd
    
    ind_intensity <- rbind(ind_intensity, colSums(energy_int)) # Total indirect energy/hh by decile
    
    # print(colSums(energy_int)[75])
  }
  
  # not_conv_idx has 1 where the RAS did not converge.
  ind_intensity <- ind_intensity[not_conv_idx!=1,]
  return(ind_intensity)
}


