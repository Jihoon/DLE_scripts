### Read in valuation mtx for each country (AT for now for FRA)
### And define a function for quick conversion between pp and bp
### Issue: The format will be different for other countries where we need to collect data
### Note: Even a 1 EUR expenditure (PP) on one sector is converted to multiple sector expenditures in BP, because it gets transfered to trans/trade sectors.



# Just do Dirichlet with three points in n-dim space
get_draws = function(n_draw, n_dim=3, coords) {
  # alpha = 1
  if(missing(coords)) {coords <- diag(n_dim)} # Coords can be n_dim X k (any k)
  
  draw_standard <- rdirichlet(n_draw, rep(1, dim(coords)[2]))  # n_draw X k
  draw_projected <- draw_standard %*% t(as.matrix(coords))  # n_draw X n_dim
  
  return(draw_projected)
  
  # plot3d(draw_projected[,1], draw_projected[,2], draw_projected[,3], size=4)
}


xlcFreeMemory()

num_EXIO_sector <- 200
trade_margin_rate <- data.frame(FR = numeric(num_EXIO_sector),US = numeric(num_EXIO_sector), IN = numeric(num_EXIO_sector))
trans_margin_rate <- trade_margin_rate
tax_rate <- trade_margin_rate

trans_margin_breakdown <- data.frame(FR = numeric(7), US = numeric(7), IN = numeric(7))
trade_margin_breakdown <- data.frame(FR = numeric(4), US = numeric(4), IN = numeric(4))

trd_idx <- 152:155
trp_idx <- 157:163

get_valuation_mtx <- function(country, mc=0){   # Two-letter country code (mc: do Monte Carlo (1) or not (0) for val mtx?)
  
  cty_place <- which(exio_ctys==country)
  # cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  
  y_bp <- matrix(final_demand[, cty_idx_fd[1]], nrow=200)  # The country's hh fd column to a matrix (200x49) in bp
  # y_bp <- as.data.frame(rowSums(y_bp))
  y_bp <- rowSums(y_bp)
  
  if (mc==0) {
    wb <- XLConnect::loadWorkbook(paste("../Valuation/", country, "_output.xls", sep=""))
    
    # Index for xx_output.xls files
    f_hous_idx <- 169  # Column for Final hh demand
    row_start <- 15  # Starting row number "paddy rice"
    row_end <- 214  # Ending row number "Extra-territorial organizations and bodies"
  
    # "Taxes less subsidies on products purchased: Total"
    if (country == "BR") {
      y_bp        <- as.matrix(readWorksheet(wb, "usebptot", header=FALSE, startRow=row_start, endRow=row_end,
                                   startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=TRUE))
    }
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
    upper <- paste0(country, "upper")
    lower <- paste0(country, "lower")
    
    trd_range <- t(trd_margin_range[,c(lower, upper)])
    trp_range <- t(trp_margin_range[,c(lower, upper)])
    tx_range <- t(trp_margin_range[,c(lower, upper)])
    
    trd_r_draws <- replicate(n_draw, foo(trd_range)) %>% replace(is.na(.), 0)
    trp_r_draws <- replicate(n_draw, foo(trp_range)) %>% replace(is.na(.), 0)
    tax_r_draws <- replicate(n_draw, foo(tx_range)) %>% replace(is.na(.), 0)
    
    trd_margin <- diag(y_bp) %*% trd_r_draws
    trp_margin <- diag(y_bp) %*% trp_r_draws
    prod_tax <- diag(y_bp) %*% tax_r_draws
    
    total_trd_margin <- colSums(trd_margin[-trd_idx,])
    total_trp_margin <- colSums(trp_margin[-trp_idx,])
    total_trd_margin*t(replicate(n_draw, trade_margin_breakdown[,c(country)]))
    trd_margin[trd_idx,] <- -t(total_trd_margin * t(replicate(n_draw, trade_margin_breakdown[,c(country)])))
    trp_margin[trp_idx,] <- -t(total_trp_margin * t(replicate(n_draw, trans_margin_breakdown[,c(country)])))
    # trd_margin[trd_idx,] <- -t(total_trd_margin * trade_brkdn_draws)
    # trp_margin[trp_idx,] <- -t(total_trp_margin * trans_brkdn_draws)
    
    D <- list()
    for (i in 1:n_draw) {
      d_out <- construct_val_mtx(y_bp, as.matrix(trd_margin[,i]), as.matrix(trp_margin[,i]), as.matrix(prod_tax[,i]))
      D[[i]] <- d_out
    }
  }
  
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
  Fin <- diag(as.numeric(ybp))
  Fin[-trp_idx, trp_idx] <- trp_margin_disag[-trp_idx,]
  Fin[-trd_idx, trd_idx] <- trd_margin_disag[-trd_idx,]
  
  # F[trp_idx, trp_idx] <- diag(ybp[trp_idx]-colSums(trp_margin_disag[-trp_idx,]))
  # F[trd_idx, trd_idx] <- diag(ybp[trd_idx]-colSums(trd_margin_disag[-trd_idx,]))
  Fin[trp_idx, trp_idx] <- diag(ypp[trp_idx]-tax[trp_idx])
  Fin[trd_idx, trd_idx] <- diag(ypp[trd_idx]-tax[trd_idx])
  Fin <- cbind(Fin,tax)
  
  # cbind(y_pp, rowSums(Fin))
  
  y <- 1/ypp[,1]
  y[is.infinite(y)] <- 0 
  D <- diag(y) %*% as.matrix(Fin)
  
  return(D)
}



#################################################################################
#  Read in valuation files from NTNU and build basis (ranges) for random draws  #
#################################################################################

# Including all countries gives too wide range for margin/tax rates.
# For now, use three countries

val_IN <- get_valuation_mtx('IN', 0)
val_BR_EX <- get_valuation_mtx('BR', 0)
val_FR <- get_valuation_mtx('FR', 0)
val_US <- get_valuation_mtx('US', 0)
val_ZA <- get_valuation_mtx('ZA', 0)
val_mtx <- list(val_FR, val_BR_EX, val_US, val_IN, val_ZA)
names(val_mtx) <- c('FR', 'BR', 'US', 'IN', 'ZA')


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

# Temporarily for 10% uncertainty assumption for ranges
uncertainty_range <- 0.1 # +-10%
trd_margin_range <- cbind(trade_margin_rate*(1-uncertainty_range), trade_margin_rate*(1+uncertainty_range))
trp_margin_range <- cbind(trans_margin_rate*(1-uncertainty_range), trans_margin_rate*(1+uncertainty_range))
tax_range <- cbind(tax_rate*(1-uncertainty_range), tax_rate*(1+uncertainty_range))
trd_margin_range <- trd_margin_range[,c(1,4,2,5,3,6)]
trp_margin_range <- trp_margin_range[,c(1,4,2,5,3,6)]
tax_range <- tax_range[,c(1,4,2,5,3,6)]
names(trd_margin_range) <- c("FRlower", "FRupper", "USlower", "USupper", "INlower", "INupper")
names(trp_margin_range) <- c("FRlower", "FRupper", "USlower", "USupper", "INlower", "INupper")
names(tax_range) <- c("FRlower", "FRupper", "USlower", "USupper", "INlower", "INupper")

# Draw a value within the rate ranges (for all 200 EXIO sectors)
foo <- function(vec) {apply(vec, 2, function(x) {runif(1, x[1], x[2])})}
trd_rate_draws <- replicate(n_draw, foo(trd_margin_range))
trp_rate_draws <- replicate(n_draw, foo(trp_margin_range))
tax_rate_draws <- replicate(n_draw, foo(tax_range))

# Draw for share breakdowns between trade/transportation sectors
# trans_brkdn_draws <- get_draws(n_draw, dim(trans_margin_breakdown)[1], min=trp_brkdn_range[1,], max=trp_brkdn_range[2,])
# trade_brkdn_draws <- get_draws(n_draw, dim(trade_margin_breakdown)[1], min=trd_brkdn_range[1,], max=trd_brkdn_range[2,])
trans_brkdn_draws <- get_draws(n_draw, dim(trans_margin_breakdown)[1], trans_margin_breakdown)
trade_brkdn_draws <- get_draws(n_draw, dim(trade_margin_breakdown)[1], trade_margin_breakdown)



########################################
#  Generate n_draw valuation matrices  #
########################################

# val_IN_rand <- get_valuation_mtx('IN', 1)
# val_ID_rand <- get_valuation_mtx('ID', 1)
# val_BR_rand <- get_valuation_mtx('BR', 1)
# val_CN_rand <- get_valuation_mtx('CN', 1)
# val_ZA_rand <- get_valuation_mtx('ZA', 1)
# val_AT_rand <- get_valuation_mtx('AT', 1)
# val_FR_rand <- get_valuation_mtx('FR', 1)
# val_US_rand <- get_valuation_mtx('US', 1)



##############################################
### Function get_basic_price from PP to BP ### (only for FRA for now)
##############################################

# Any y_pp can be converted to y_bp by

get_basic_price <- function(v_pp, country = 'IN'){
  # if(country=='FR') {country <- 'AT'}
  if (D_val_uncertainty == 0) {
    # D <- val_AT
    D <- val_mtx[[which(country==names(val_mtx))]]
  }
  else {
    # if(country=='FR' | country=='AT') {D <- val_AT}
    # else {D <- eval(parse(text=paste0("val_", country, "_rand")))[[draw_count]] }
    D <- eval(parse(text=paste0("val_", country, "_rand")))[[draw_count]]
  }
  v_bp <- t(D)[1:200,] %*% v_pp  # Remove tax row
  return(v_bp)
}

get_purch_price <- function(v_bp, country = 'IN'){
  # if(country=='FR') {country <- 'AT'}
  if (D_val_uncertainty == 0) {
    D <- val_mtx[[which(country==names(val_mtx))]]
  }
  else {
    # if(country=='FR' | country=='AT') {D <- val_AT}
    # else {D <- eval(parse(text=paste0("val_", country, "_rand")))[[draw_count]] }
    D <- eval(parse(text=paste0("val_", country, "_rand")))[[draw_count]] 
  }
  Dinv <- get_inv_valmtx(D)
  v_pp <- t(Dinv) %*% v_bp
  
  taxR <- D[,201]
  
  # When using an IO year other than 2007 (e.g.2010), there can be a mismatch between HHFD of exio2010 and tax information from 2007,
  # which leads to 100% tax rate with FD=0. Then I set the tax rate to zero for these sectors.
  # Ultimately, we need to use the valuation data from the same IO year (once it's available).
  tax.div <- 1-taxR
  taxR[tax.div>=0 & tax.div<1e-5] <- 0 
  
  v_pp <- v_pp / (1-taxR)
  
  # print(which(v_pp<0))
  # For France, v_pp for trade sectors become negative..
  # Need to figure out a better way later.
  v_pp[v_pp<0 | is.nan(v_pp)] <- 0
  return(v_pp)
}

get_inv_valmtx <- function(val_mat) {
  mat <- val_mat[,1:200]
  mat_inv <- matrix(0, dim(mat)[1], dim(mat)[2])
  
  idx_zero <- which(diag(mat)==0)
  if (length(idx_zero)!=0) {
    D <- mat[-idx_zero, -idx_zero]
    Dinv <- solve(D)
    mat_inv[-idx_zero, -idx_zero] <- Dinv
  }
  else {
    mat_inv <- solve(mat)
  }
  
  return(mat_inv)
}



###################################################################
#  Read in Brazilian valuation file (from Guilioto) and summarize #
###################################################################

a <- c("Code", "Descr", "SupPP", "TrdMrg", "TrpMrg", "ImpTax", "IPI", "ICMS", "OthTaxSub", "TotTaxSub", "SupBP")
BRA_val_org <- read_excel("../Valuation/Brazil/56_tab1_2007_eng.xlsx", skip=5, col_names=a)
BRA_val_org <- BRA_val_org %>% select(-Descr, -ImpTax, -IPI, -ICMS, -OthTaxSub) %>% filter(!is.na(Code)) %>% filter(Code!="Total")
BRA_val <- BRA_val_org
BRA_val$Code <- floor(as.numeric(BRA_val$Code) / 1000)
BRA_val <- BRA_val %>% group_by(Code) %>% summarise_each(funs(sum))
BRA_exio_map <- read_excel("../Valuation/Brazil/56_tab1_2007_eng.xlsx", skip=0, col_names=TRUE, sheet=4) %>%
  mutate(Code=as.numeric(Code), CodeBRA=as.numeric(CodeBRA))

BRA_val <- BRA_val[match(BRA_exio_map$CodeBRA, BRA_val$Code),]

# We need to assume certain shares for each trd/trp subsectors.
# The only groud is from the EXIO FD...
BRA_val[trd_idx,-1] <- BRA_val[trd_idx,-1] * BRA_fd_exio[trd_idx] / sum(BRA_fd_exio[trd_idx])
BRA_val[trp_idx,-1] <- BRA_val[trp_idx,-1] * BRA_fd_exio[trp_idx] / sum(BRA_fd_exio[trp_idx])

Exceptions <- !is.na(BRA_exio_map$Exception)
BRA_val[Exceptions,-1] <- BRA_val_org[match(BRA_exio_map$Exception[Exceptions], BRA_val_org$Code),-1]

# Sum of each margin column needs to be zero.
valscale <- sum(BRA_val$TrpMrg[-trp_idx]) / sum(BRA_val$TrpMrg[trp_idx])
BRA_val$TrpMrg[-trp_idx] <- BRA_val$TrpMrg[-trp_idx] / abs(valscale)
valscale <- sum(BRA_val$TrdMrg[-trd_idx]) / sum(BRA_val$TrdMrg[trd_idx])
BRA_val$TrdMrg[-trd_idx] <- BRA_val$TrdMrg[-trd_idx] / abs(valscale)

attach(BRA_val)
val_BR_BR <- construct_val_mtx(as.matrix(SupBP), as.matrix(TrdMrg), as.matrix(TrpMrg), as.matrix(TotTaxSub))
detach(BRA_val)

val_mtx <- list(val_FR, val_BR_BR, val_US, val_IN, val_ZA)
names(val_mtx) <- c('FR', 'BR', 'US', 'IN', 'ZA')

# User can select which val_mtx to use with val_BR_BR or val_BR_EX. By default, val_BR_BR is preferred.