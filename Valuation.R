### Read in valuation mtx for each country (AT for now for FRA)
### And define a function for quick conversion between pp and bp
### Issue: The format will be different for other countries where we need to collect data
### Note: Even a 1 EUR expenditure (PP) on one sector is converted to multiple sector expenditures in BP, because it gets transfered to trans/trade sectors.

xlcFreeMemory()

num_EXIO_sector <- 200
trade_margin_rate <- data.frame(AT = numeric(num_EXIO_sector),US = numeric(num_EXIO_sector), IN = numeric(num_EXIO_sector))
trans_margin_rate <- trade_margin_rate
tax_rate <- trade_margin_rate

trans_margin_breakdown <- data.frame(AT = numeric(7),US = numeric(7), IN = numeric(7))
trade_margin_breakdown <- data.frame(AT = numeric(4),US = numeric(4), IN = numeric(4))

trd_idx <- 152:155
trp_idx <- 157:163

get_valuation_mtx <- function(country, mc=0){   # Two-letter country code (mc: do Monte Carlo (1) or not (0) for val mtx?)
  
  # AT valuation layer was used for FR.
  if (country=='FR') country <- 'AT'
  
  cty_place <- which(exio_ctys==country)
  # cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  
  y_bp <- matrix(final_demand[, cty_idx_fd[1]], nrow=200)  # The country's hh fd column to a matrix (200x48) in bp
  # y_bp <- as.data.frame(rowSums(y_bp))
  y_bp <- rowSums(y_bp)
  
  if (mc==0) {
    
    # Mapping <- system.file(paste("H:/MyDocuments/IO work/Valuation/", country, "_output.xls", sep=""), package = "XLConnect")
    wb <- XLConnect::loadWorkbook(paste("H:/MyDocuments/IO work/Valuation/", country, "_output.xls", sep=""))
    
    # Index for xx_output.xls files
    f_hous_idx <- 169  # Column for Final hh demand
    row_start <- 15  # Starting row number "paddy rice"
    row_end <- 214  # Ending row number "Extra-territorial organizations and bodies"
  
    # "Taxes less subsidies on products purchased: Total"
    # y_bp        <- readWorksheet(wb, "usebptot", header=FALSE, startRow=row_start, endRow=row_end,
    #                       startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=TRUE)
    # y_pp        <- readWorksheet(wb, "usepptot", header=FALSE, startRow=row_start, endRow=row_end,
    #                       startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=TRUE)
    trd_margin  <- as.matrix(readWorksheet(wb, "trade_margins", header=FALSE, startRow=row_start, endRow=row_end, 
                          startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=TRUE))
    trp_margin  <- as.matrix(readWorksheet(wb, "transport_margins", header=FALSE, startRow=row_start, endRow=row_end, 
                          startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=TRUE))
    prod_tax    <- as.matrix(readWorksheet(wb, "product_taxes", header=FALSE, startRow=row_start, endRow=row_end, 
                          startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=TRUE))
    
    defect <- which(trd_margin[-trd_idx] < 0) # Why negative margin for non trd/trp sectors in NTNU val mtx???
    trd_margin[-trd_idx][defect] <- 0
    defect <- which(trp_margin[-trp_idx] < 0) # Why negative margin for non trd/trp sectors???
    trp_margin[-trp_idx][defect] <- 0
    
    D <- construct_val_mtx(y_bp, trd_margin, trp_margin, prod_tax)
    
    # Sort of redundant.. This is also in construct_val_mtx()
    trp_ratio <- trp_margin[trp_idx]/sum(trp_margin[trp_idx])
    trd_ratio <- trd_margin[trd_idx]/sum(trd_margin[trd_idx])
    
    trade_margin_breakdown[,country] <<- trd_ratio
    trans_margin_breakdown[,country] <<- trp_ratio
    trade_margin_rate[,country] <<- trd_margin/y_bp
    trans_margin_rate[,country] <<- trp_margin/y_bp
    tax_rate[,country] <<- prod_tax/y_bp
  }
  else {
    trd_margin <- diag(y_bp) %*% trd_rate_draws
    trp_margin <- diag(y_bp) %*% trp_rate_draws
    prod_tax <- diag(y_bp) %*% tax_rate_draws
    
    total_trd_margin <- colSums(trd_margin[-trd_idx,])
    total_trp_margin <- colSums(trp_margin[-trp_idx,])
    trd_margin[trd_idx,] <- -t(total_trd_margin * trade_brkdn_draws)
    trp_margin[trp_idx,] <- -t(total_trp_margin * trans_brkdn_draws)
    
    D <- list()
    for (i in 1:n_draw) {
      d_out <- construct_val_mtx(y_bp, as.matrix(trd_margin[,i]), as.matrix(trp_margin[,i]), as.matrix(prod_tax[,i]))
      D[[i]] <- d_out
    }
  }
  # 
  # F <- diag(y_bp[1:200,1])
  # F[-trp_idx, trp_idx] <- trp_margin_disag[-trp_idx,]
  # F[-trd_idx, trd_idx] <- trd_margin_disag[-trd_idx,]
  # 
  # F[trp_idx, trp_idx] <- diag(y_bp[trp_idx,]-colSums(trp_margin_disag[-trp_idx,]))
  # F[trd_idx, trd_idx] <- diag(y_bp[trd_idx,]-colSums(trd_margin_disag[-trd_idx,]))
  # F <- cbind(F,prod_tax)
  # 
  # # cbind(y_pp, rowSums(F))
  # 
  # y <- 1/y_pp[,1]
  # y[is.infinite(y)] <- 0 
  # D <- diag(y) %*% as.matrix(F)
  
  return(D)
}

construct_val_mtx <- function(ybp, trd, trp, tax) {
  # Estimate trade/transportation sectoral breakdowns based on the margin columns for FD
  trp_ratio <- trp[trp_idx]/sum(trp[trp_idx])
  trd_ratio <- trd[trd_idx]/sum(trd[trd_idx])

  # Disaggregating total margins for non-trd/trp sectors in FD margin columns
  # trp_margin_disag <- replicate(length(trp_idx), trp) %*% diag(trp_ratio)
  # trd_margin_disag <- replicate(length(trd_idx), trd) %*% diag(trd_ratio)
  trp_margin_disag <- trp[,rep(1,length(trp_idx))] %*% diag(trp_ratio)
  trd_margin_disag <- trd[,rep(1,length(trd_idx))] %*% diag(trd_ratio)
  
    # There are cases where the trd/trp sectors for ypp become negative as a result of random draws.
  # For now, I truncate those cases. Not sure whether this is realistic.
  # trd[ybp+trd<100] <- -ybp[ybp+trd<100]+100
  # trp[ybp+trp<100] <- -ybp[ybp+trp<100]+100
  trd[ybp+trd<0] <- -ybp[ybp+trd<0]
  trp[ybp+trp<0] <- -ybp[ybp+trp<0]
  
  # Calculate Ypp based on the other vectors
  ypp <- ybp + trd + trp + tax 
  
  # F <- diag(ybp[1:200,1])
  F <- diag(ybp)
  F[-trp_idx, trp_idx] <- trp_margin_disag[-trp_idx,]
  F[-trd_idx, trd_idx] <- trd_margin_disag[-trd_idx,]
  
  # F[trp_idx, trp_idx] <- diag(ybp[trp_idx]-colSums(trp_margin_disag[-trp_idx,]))
  # F[trd_idx, trd_idx] <- diag(ybp[trd_idx]-colSums(trd_margin_disag[-trd_idx,]))
  F[trp_idx, trp_idx] <- diag(ypp[trp_idx]-tax[trp_idx])
  F[trd_idx, trd_idx] <- diag(ypp[trd_idx]-tax[trd_idx])
  F <- cbind(F,tax)
  
  # cbind(y_pp, rowSums(F))
  
  y <- 1/ypp[,1]
  y[is.infinite(y)] <- 0 
  D <- diag(y) %*% as.matrix(F)
  
  return(D)
}





#################################################################################
#  Read in valuation files from NTNU and build basis (ranges) for random draws  #
#################################################################################

# Including all countries gives too wide range for margin/tax rates.
# For now, use three countries

val_IN <- get_valuation_mtx('IN', 0)
# val_ID <- get_valuation_mtx('ID', 0)
# val_BR <- get_valuation_mtx('BR', 0)
# val_CN <- get_valuation_mtx('CN', 0)
# val_ZA <- get_valuation_mtx('ZA', 0)
val_AT <- get_valuation_mtx('AT', 0)
val_US <- get_valuation_mtx('US', 0)
# val_mtx <- list(val_IN, val_ID, val_BR, val_CN, val_ZA, val_AT, val_US)
# names(val_mtx) <- c('IN', 'ID', 'BR', 'CN', 'ZA', 'AT', 'US')
val_mtx <- list(val_IN, val_AT, val_US)
names(val_mtx) <- c('IN', 'AT', 'US')


rownames(trade_margin_breakdown) <- EX_catnames[trd_idx]
rownames(trans_margin_breakdown) <- EX_catnames[trp_idx]
rownames(trade_margin_rate) <- EX_catnames
rownames(trans_margin_rate) <- EX_catnames
rownames(tax_rate) <- EX_catnames

trade_margin_rate[is.na(trade_margin_rate)] <- 0
trans_margin_rate[is.na(trans_margin_rate)] <- 0
tax_rate[is.na(tax_rate)] <- 0

# Set up a range for draws
trd_brkdn_range <- apply(trade_margin_breakdown, 1, function(x) {c(min(x), max(x))})
trp_brkdn_range <- apply(trans_margin_breakdown, 1, function(x) {c(min(x), max(x))})
trd_margin_range <- apply(trade_margin_rate, 1, function(x) {c(min(x), max(x))})
trp_margin_range <- apply(trans_margin_rate, 1, function(x) {c(min(x), max(x))})
tax_range <- apply(tax_rate, 1, function(x) {c(min(x), max(x))})

# Draw a value within the rate ranges (for all 200 EXIO sectors)
foo <- function(vec) {apply(vec, 2, function(x) {runif(1, x[1], x[2])})}
trd_rate_draws <- replicate(n_draw, foo(trd_margin_range))
trp_rate_draws <- replicate(n_draw, foo(trp_margin_range))
tax_rate_draws <- replicate(n_draw, foo(tax_range))

# Draw for share breakdowns between trade/transportation sectors
trans_brkdn_draws <- get_draws(100, dim(trans_margin_breakdown)[1], min=trp_brkdn_range[1,], max=trp_brkdn_range[2,])
trade_brkdn_draws <- get_draws(100, dim(trade_margin_breakdown)[1], min=trd_brkdn_range[1,], max=trd_brkdn_range[2,])
trans_brkdn_draws <- get_draws(100, dim(trans_margin_breakdown)[1], trans_margin_breakdown)
trade_brkdn_draws <- get_draws(100, dim(trade_margin_breakdown)[1], trade_margin_breakdown)



########################################
#  Generate n_draw valuation matrices  #
########################################

val_IN_rand <- get_valuation_mtx('IN', 1)
# val_ID_rand <- get_valuation_mtx('ID', 1)
# val_BR_rand <- get_valuation_mtx('BR', 1)
# val_CN_rand <- get_valuation_mtx('CN', 1)
# val_ZA_rand <- get_valuation_mtx('ZA', 1)
# val_AT_rand <- get_valuation_mtx('AT', 1)
# val_US_rand <- get_valuation_mtx('US', 1)




##############################################
### Function get_basic_price from PP to BP ### (only for FRA for now)
##############################################

# Any y_pp can be converted to y_bp by

get_basic_price <- function(v_pp, country = 'IN'){
  if (D_val_uncertainty == 0) {
    D <- val_mtx[[which(country==names(val_mtx))]]  
  }
  else {
    D <- val_IN_rand[[draw_count]]
  }
  v_bp <- t(D)[1:200,] %*% v_pp  # Remove tax row
  return(v_bp)
}


get_purch_price <- function(v_bp, country = 'IN'){
  if (D_val_uncertainty == 0) {
    D <- val_mtx[[which(country==names(val_mtx))]]
  }
  else {
    D <- val_IN_rand[[draw_count]]
  }
  Dinv <- get_inv_valmtx(D)
  v_pp <- t(Dinv) %*% v_bp
  return(v_pp)
}

get_inv_valmtx <- function(val_mat) {
  mat <- val_mat[,1:200]
  mat_inv <- matrix(0, dim(mat)[1], dim(mat)[2])
  
  idx_zero <- which(diag(mat)==0)
  D <- mat[-idx_zero, -idx_zero]
  Dinv <- solve(D)
  mat_inv[-idx_zero, -idx_zero] <- Dinv
  
  return(mat_inv)
}
