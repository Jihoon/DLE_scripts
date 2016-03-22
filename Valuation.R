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

get_valuation_mtx <- function(country){   # Two-letter country code
  
  # AT valuation layer was used for FR.
  if (country=='FR') country <- 'AT'
  
  cty_place <- which(exio_ctys==country)
  # cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  
  y_bp <- matrix(final_demand[, cty_idx_fd[1]], nrow=200)  # The country's hh fd column to a matrix (200x48) in bp
  y_bp <- as.data.frame(rowSums(y_bp))
  
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
  trd_margin  <- readWorksheet(wb, "trade_margins", header=FALSE, startRow=row_start, endRow=row_end, 
                        startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=TRUE)
  trp_margin  <- readWorksheet(wb, "transport_margins", header=FALSE, startRow=row_start, endRow=row_end, 
                        startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=TRUE)
  prod_tax    <- readWorksheet(wb, "product_taxes", header=FALSE, startRow=row_start, endRow=row_end, 
                        startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=TRUE)
  
  y_pp <- y_bp + trd_margin + trd_margin + prod_tax
  
  trp_ratio <- trp_margin[trp_idx,]/sum(trp_margin[trp_idx,])
  trd_ratio <- trd_margin[trd_idx,]/sum(trd_margin[trd_idx,])
  
  trade_margin_breakdown[,country] <<- trd_ratio
  trans_margin_breakdown[,country] <<- trp_ratio
  trade_margin_rate[,country] <<- trd_margin/y_bp
  trans_margin_rate[,country] <<- trp_margin/y_bp
  tax_rate[,country] <<- prod_tax/y_bp
  
  trp_margin_disag <- as.matrix(trp_margin[,rep(1,length(trp_idx))]) %*% diag(trp_ratio)
  trd_margin_disag <- as.matrix(trd_margin[,rep(1,length(trd_idx))]) %*% diag(trd_ratio)
  
  # colSums(trp_margin_disag[-trp_idx,])
  # colSums(trd_margin_disag[-trd_idx,])
  
  F <- diag(y_bp[1:200,1])
  F[-trp_idx, trp_idx] <- trp_margin_disag[-trp_idx,]
  F[-trd_idx, trd_idx] <- trd_margin_disag[-trd_idx,]
  
  F[trp_idx, trp_idx] <- diag(y_bp[trp_idx,]-colSums(trp_margin_disag[-trp_idx,]))
  F[trd_idx, trd_idx] <- diag(y_bp[trd_idx,]-colSums(trd_margin_disag[-trd_idx,]))
  F <- cbind(F,prod_tax)
  
  # cbind(y_pp, rowSums(F))
  
  y <- 1/y_pp[,1]
  y[is.infinite(y)] <- 0 
  D <- diag(y) %*% as.matrix(F)
  
  return(D)
}

val_IN <- get_valuation_mtx('IN')
val_ID <- get_valuation_mtx('ID')
val_BR <- get_valuation_mtx('BR')
val_CN <- get_valuation_mtx('CN')
val_ZA <- get_valuation_mtx('ZA')
val_AT <- get_valuation_mtx('AT')
val_US <- get_valuation_mtx('US')
val_mtx <- list(val_IN, val_ID, val_BR, val_CN, val_ZA, val_AT, val_US)
names(val_mtx) <- c('IN', 'ID', 'BR', 'CN', 'ZA', 'AT', 'US')

# 
# rownames(trade_margin_breakdown) <- EX_catnames[trd_idx]
# rownames(trans_margin_breakdown) <- EX_catnames[trp_idx]
# rownames(trade_margin_rate) <- EX_catnames
# rownames(trans_margin_rate) <- EX_catnames
# rownames(tax_rate) <- EX_catnames
#   
# trade_margin_rate[is.na(trade_margin_rate)] <- 0
# trans_margin_rate[is.na(trans_margin_rate)] <- 0
# tax_rate[is.na(tax_rate)] <- 0
# 
# # Set up a range for draws
# trd_brkdn_range <- apply(trade_margin_breakdown, 1, function(x) {c(min(x), max(x))})
# trp_brkdn_range <- apply(trans_margin_breakdown, 1, function(x) {c(min(x), max(x))})
# trd_margin_range <- apply(trade_margin_rate, 1, function(x) {c(min(x), max(x))})
# trp_margin_range <- apply(trans_margin_rate, 1, function(x) {c(min(x), max(x))})
# tax_range <- apply(tax_rate, 1, function(x) {c(min(x), max(x))})
# 
# # Draw a value within the ranges
# trd_brkdn_draw <- apply(trd_brkdn_range, 2, function(x) {runif(2, x[1], x[2])})
# trp_brkdn_draw <- apply(trp_brkdn_range, 2, function(x) {runif(1, x[1], x[2])})
# trd_rate_draw <- apply(trd_margin_range, 2, function(x) {runif(1, x[1], x[2])})
# trp_rate_draw <- apply(trp_margin_range, 2, function(x) {runif(1, x[1], x[2])})
# tax_rate_draw <- apply(tax_range, 2, function(x) {runif(1, x[1], x[2])})
# 
# a <- cbind(tax_rate, tax_rate_draw)
# 
# trans_margin_draws <- get_draws(100, dim(trans_margin_breakdown)[1], min=trp_brkdn_range[1,], max=trp_brkdn_range[2,])
# trade_margin_draws <- get_draws(100, dim(trade_margin_breakdown)[1], min=trd_brkdn_range[1,], max=trd_brkdn_range[2,])
# plot3d(trans_margin_draws[,1], trans_margin_draws[,2], b[,3], size=5)


##############################################
### Function get_basic_price from PP to BP ### (only for FRA for now)
##############################################

# Any y_pp can be converted to y_bp by

get_basic_price <- function(v_pp, country = 'IN'){
  D <- val_mtx[[which(country==names(val_mtx))]]
  v_bp <- t(D)[1:200,] %*% v_pp  # Remove tax row
  return(v_bp)
}


get_purch_price <- function(v_bp, country = 'IN'){
  D <- val_mtx[[which(country==names(val_mtx))]]
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