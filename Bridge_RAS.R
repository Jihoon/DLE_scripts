# Test RAS for IND

# Get IND final demand from EXIO [M.EUR]
IN_place <- which(exio_ctys=="IN")
IN_idx <- seq(200*(IN_place-1)+1, 200*IN_place)  # 200 EXIO commodities per country
IN_idx_fd <- seq(7*(IN_place-1)+1, 7*IN_place)   # 7 final demand columns per country
dim_fd <- dim(final_demand)
IN_fd_exio <- rowSums(final_demand[IN_idx, seq(1, dim_fd[2], 7)]) # Sum all HH FD across countries

# Get IND final demand in ICP 

# Remove tax observations from DB
IND_FD_code <- IND_FD[-grep("taxes", IND_FD$ITEM, ignore.case = TRUE), ]
IND_FD_code <- merge(IND_FD_code, IND_map[,c("CODE", "Subcategory", "ITEM_DLE")], by.x="ITEM", by.y="ITEM_DLE")
IND_FD_code <- IND_FD_code[order(IND_FD_code$CODE),]
IND_FD_code[is.na(IND_FD_code)] <- 0

# CES_ICP_IDN, CES_ICP_IND rows are sorted by Survey Code.
IND_FD_ICP <- t(CES_ICP_IND) %*% as.matrix(IND_FD_code[,2])


# Get IDN final demand from EXIO [M.EUR]
ID_place <- which(exio_ctys=="ID")
ID_idx <- seq(200*(ID_place-1)+1, 200*ID_place)  # 200 EXIO commodities per country
ID_idx_fd <- seq(7*(ID_place-1)+1, 7*ID_place)   # 7 final demand columns per country
ID_fd_exio <- rowSums(final_demand[ID_idx, seq(1, dim_fd[2], 7)]) # Sum all HH FD across countries

# Get IDN final demand in ICP 

# Remove tax observations from DB
IDN_FD_code <- IDN_FD[-grep("taxes", IDN_FD$ITEM, ignore.case = TRUE), ]
IDN_FD_code <- merge(IDN_FD_code, IDN_map[,c("CODE", "Subcategory", "ITEM_DLE")], by.x="ITEM", by.y="ITEM_DLE")
IDN_FD_code <- IDN_FD_code[order(IDN_FD_code$CODE),]
IDN_FD_code[is.na(IDN_FD_code)] <- 0

# CES_ICP_IDN, CES_ICP_IDN rows are sorted by Survey Code.
IDN_FD_ICP <- t(CES_ICP_IDN) %*% as.matrix(IDN_FD_code[,2])



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

CPI_IND <- WDI(country = "IN", indicator = "FP.CPI.TOTL.ZG", start = 2007, end = 2010, extra = FALSE, cache = NULL)
CPI_IND <- CPI_IND %>% rename(cpi=FP.CPI.TOTL.ZG)
CPI_IND$cpi <- CPI_IND$cpi/100 + 1
CPI_ratio <- prod(CPI_IND$cpi)

# [LCU/$]
EXR_EUR <- WDI(country = "XC", indicator = "PA.NUS.FCRF", start = 2007, end = 2007, extra = FALSE, cache = NULL)
EXR_EUR <- EXR_EUR %>% rename(r=PA.NUS.FCRF)
EXR_IND <- WDI(country = "IN", indicator = "PA.NUS.FCRF", start = 2007, end = 2007, extra = FALSE, cache = NULL)
EXR_IND <- EXR_IND %>% rename(r=PA.NUS.FCRF)

IND_FD_ICP_usd2007 <- IND_FD_ICP * PPP_IND / CPI_ratio / EXR_IND$r / 1e6
# IND_FD_ICP_usd2007 <- IND_FD_ICP / CPI_ratio / 1e6
IND_FD_CES_usd2007 <- IND_FD$FD_TOT * PPP_IND / CPI_ratio / EXR_IND$r / 1e6
IN_fd_exio_usd2007 <- IN_fd_exio / EXR_EUR$r

sum(IND_FD_ICP_usd2007) # CES FD in 2007$ (MER) based on 2011-2012 NSS -> Use this for RAS
# sum(IND_FD_CES_usd2007) # CES FD in 2007$ (MER) Includes tax
sum(IN_fd_exio_usd2007) # EXIO FD in 2007$ (MER?) based on 2004 IoT -> Use this for RAS
# sum(IND_FD_ICP / CPI_ratio / 1e6) # CES FD in 2007$ (PPP)




#######################
###     Run RAS     ###
#######################

library(mipfp)

n_draw <- 2000

qual_map <- bridge_icp_exio_q[,-1]
bridge_ICP_EXIO <- get_bridge_COICOP_EXIO(qual_map, n_draw)
scaler_IND <- sum(IND_FD_ICP_usd2007)/sum(IN_fd_exio_usd2007)

rowConst <- as.vector(IND_FD_ICP_usd2007) / scaler_IND
colConst <- IN_fd_exio_usd2007 

idx_fixed_row <- which(rowSums(qual_map)==1)
idx_fixed_col <- which(colSums(qual_map)==1)

init <- list()
result <- list()

for (i in 1:n_draw) {
  seed <- diag(rowConst) %*% t(bridge_ICP_EXIO[[i]]) 
  init[[i]] <- seed
  
  # a <- t(bridge_ICP_EXIO[[1]]) * as.vector(IND_FD_ICP_usd2007)
  balanced_map <- Ipfp(seed, list(1,2), list(rowConst, colConst), tol = 1e-12, iter = 10000, print=FALSE)
  result[[i]] <- balanced_map$x.hat
  print(i)
  print(paste("Constraint total =", sum(init[[i]]), ", Result total =", sum(result[[i]]) ))
}

hist(rapply(init, function(x) {x[4,1]}))
hist(rapply(result, function(x) {x[4,1]}))

# Check out fixed row/column w
idx_fixed <- colSums(qual_map[,as.matrix(colSums(qual_map[idx_fixed_row,]))!=0])==1
fixed_cells_by_row <- cbind(idx_fixed_row, apply(qual_map[idx_fixed_row,], 1, function (x) {which(x==1)}))
fixed_cells_by_col <- cbind(apply(qual_map[, idx_fixed_col], 2, function (x) {which(x==1)}), idx_fixed_col)

rowSums(b[idx_fixed_row,]) # These should be values for fixed cells for rowSums
colSums(b[idx_fixed_col,]) # These should be values for fixed cells for colSums


balanced_bridge_ICP_EXIO <- rapply(result, function(x) {diag(1/rowConst) * x})


# Let's find out the ultimate matrix for RAS (after removing all fixed cells)

qual_map_reduced <- qual_map
final <- matrix(0, dim(qual_map)[1], dim(qual_map)[2])
colConst_reduced <- colConst
rowConst_reduced <- rowConst

repeat {
  idx_fixed_row <- which(rowSums(qual_map_reduced)==1)
  idx_fixed_col <- which(colSums(qual_map_reduced)==1)
  fixed_cells_by_row <- cbind(idx_fixed_row, apply(qual_map[idx_fixed_row,], 1, function (x) {which(x==1)}))
  fixed_cells_by_col <- cbind(apply(qual_map[, idx_fixed_col], 2, function (x) {which(x==1)}), idx_fixed_col)
  
  rowConst_reduced <- CheckCells(fixed_cells_by_row, fixed_cells_by_col, colConst_reduced, rowConst_reduced)
  
  print(idx_fixed_row)
  print(idx_fixed_col)
  
  if (length(idx_fixed_row)!=0 & length(idx_fixed_col)!=0) {
    qual_map_reduced[idx_fixed_row,idx_fixed_col] <- NA
    final[fixed_cells_by_row] <- rowConst_reduced[idx_fixed_row]
    colConst_reduced <- colConst_reduced - colSums(final)
    final[fixed_cells_by_col] <- colConst_reduced[idx_fixed_col]
    rowConst_reduced <- rowConst_reduced - rowSums(final)  
    
    rowConst_reduced <- rowConst_reduced[-idx_fixed_row]
    colConst_reduced <- colConst_reduced[-idx_fixed_col]
  }
  else if (length(idx_fixed_row)!=0 & length(idx_fixed_col)==0) {
    qual_map_reduced <- qual_map_reduced[-idx_fixed_row,]
    final[fixed_cells_by_row] <- rowConst_reduced[idx_fixed_row]
    colConst_reduced <- colConst_reduced - colSums(final)
    
    rowConst_reduced <- rowConst_reduced[-idx_fixed_row]
  }
  else if (length(idx_fixed_row)==0 & length(idx_fixed_col)!=0) {
    qual_map_reduced <- qual_map_reduced[,-idx_fixed_col]
    final[fixed_cells_by_col] <- colConst_reduced[idx_fixed_col]
    rowConst_reduced <- rowConst_reduced - rowSums(final)  
    
    colConst_reduced <- colConst_reduced[-idx_fixed_col]
  }
  else {
    break 
  }
}



CheckCells <- function(fixed_cells_by_row, fixed_cells_by_col, colCon, rowCon) {
  a <- rbind(fixed_cells_by_row, fixed_cells_by_col)
  conflict_cell <- a[duplicated(a)]
  if (colCon[conflict_cell[1]]==rowCon[conflict_cell[2]]) {return(0)}
  
  rowCon[conflict_cell[2]] <- colCon[conflict_cell[1]]
  
}


























# Kevin's test

result <- list()
initial <- list()
zz <- sample.int(50, 25)

for (i in 1:500) {
  # Commodity sums
  com_sums = round(100*runif(10))
  
  # Sector sums (total forced to match total of com_sums)
  sec_sums = round(100*runif(5))
  sec_sums = sec_sums * sum(com_sums)/sum(sec_sums)
  
  # Random matrix (set 50% of values to zero)
  x = matrix(runif(10*5), nrow=10)
  x[zz] = 0
  
  # Force row sums to equal 1
  x = x/rowSums(x)
  
  # Convert commodity shares to nominal values
  x = x * com_sums
  initial[i] <- x
  
  # Obviously, column sums do not match sec_sums
  # plot(sec_sums, colSums(x))
  
  # Apply RAS
  test = Ipfp(x, list(1,2), list(com_sums, sec_sums), tol=1e-11)
  result[i] <- test$x.hat
#   colSums(test$x.hat) - sec_sums
#   rowSums(test$x.hat) - com_sums
}
