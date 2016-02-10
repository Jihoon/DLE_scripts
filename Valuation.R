### Read in valuation mtx for each country (AT for now for FRA)
### And define a function for quick conversion between pp and bp
### Issue: The format will be different for other countries where we need to collect data
### Note: Even a 1 EUR expenditure (PP) on one sector is converted to multiple sector expenditures in BP, because it gets transfered to trans/trade sectors.


# AT valuation layer was used for FR.
Mapping <- system.file("AT_valuation.xlsx", package = "XLConnect")
wb <- loadWorkbook("H:/MyDocuments/IO work/Valuation/AT_valuation.xls")

f_hous_idx <- 169  # Column for Final hh demand
row_start <- 15  # Starting row number "paddy rice"
row_end <- 214  # Ending row number "Extra-territorial organizations and bodies"

trd_idx <- c(152:155, 166)
trp_idx <- 157:163

# "Taxes less subsidies on products purchased: Total"
y_bp        <- readWorksheet(wb, "usebptot", header=FALSE, startRow=row_start, endRow=row_end,
                      startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=T)
y_pp        <- readWorksheet(wb, "usepptot", header=FALSE, startRow=row_start, endRow=row_end,
                      startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=T)
trd_margin  <- readWorksheet(wb, "trade_margins", header=FALSE, startRow=row_start, endRow=row_end, 
                      startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=T)
trp_margin  <- readWorksheet(wb, "transport_margins", header=FALSE, startRow=row_start, endRow=row_end, 
                      startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=T)
prod_tax    <- readWorksheet(wb, "product_taxes", header=FALSE, startRow=row_start, endRow=row_end, 
                      startCol=f_hous_idx, endCol=f_hous_idx, forceConversion=T)

trp_ratio <- trp_margin[trp_idx,]/sum(trp_margin[trp_idx,])
trd_ratio <- trd_margin[trd_idx,]/sum(trd_margin[trd_idx,])

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



##############################################
### Function get_basic_price from PP to BP ### (only for FRA for now)
##############################################

# Any y_pp can be converted to y_bp by

get_basic_price <- function(v_pp, country){
  v_bp <- (t(D) %*% v_pp)[1:200,]  # Remove tax row
  return(v_bp)
}