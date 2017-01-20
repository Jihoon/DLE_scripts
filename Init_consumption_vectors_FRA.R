CPI_ratio_FRA <- as.numeric(CPI %>% filter(year==2010 & iso2c=='FR') %>% select(FP.CPI.TOTL) / CPI %>% 
                              filter(year==2007 & iso2c=='FR') %>% select(FP.CPI.TOTL))



############################################################
### Read final demand vector from each country's CES DB  ###
############################################################

# France - No DB, only summary from Lucas

wb <- XLConnect::loadWorkbook("H:/MyDocuments/IO work/Uncertainty/2011_FRHHBS_per_decile.xlsx")

n_sector_coicop <- 109  # Num of COICOP sectors
n_col <- 11  # Num of columns (10 deciles + avg)

# Final demand per HH [Euro/HH]
fd_decile_FRA <- XLConnect::readWorksheet(wb, sheet="ValueDecile", header=T, forceConversion=T) %>% select(2:(n_col+1))
n_hh_FRA <- 26058600 # interpolated from https://www.ined.fr/en/everything_about_population/data/france/couples-households-families/households/

fd_decile_FRA <- fd_decile_FRA * n_hh_FRA / 1e6 # [M.EUR]
fd_decile_FRA[,2:11] <- fd_decile_FRA[,2:11]/10  / EXR_EUR$r # 1/10 for each decile & to M.USD



##########################################
### Adjust FD based on the indicators  ###
##########################################

#### 3.1 France - Inflation 

# This is in 109 NTNU COICOP classifications.
a <- WDI(country = "FR", indicator = "NE.CON.PETC.KD", start = 2007, end = 2011, extra = FALSE, cache = NULL)
consumption_growth_FR <- a$NE.CON.PETC.KD[1]/a$NE.CON.PETC.KD[5]
FRA_FD_ICP_usd2007 <- as.matrix(fd_decile_FRA / CPI_ratio_FRA) / consumption_growth_FR # M.EUR to M.USD 2007 [length: 109]

GetTotalEmbeddedEnergy <- function(country='IN') {
  
  if(country=='ALL') {
    cty_idx_fd <- 1:dim(final_demand)[2]
  }
  else {
    cty_place <- which(exio_ctys==country)
    cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
    cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  }
  
  emb_energy <- indirect_E_int %*% as.matrix(final_demand[,cty_idx_fd])
  emb_energy <- cbind(emb_energy, rowSums(emb_energy))
  
  # country <- countrycode(country, "iso2c", "iso3c")
  # pop2007 <- eval(parse(text=paste0(country, "_pop_2007")))
  
  # return(colSums(emb_energy)/pop2007*1000)  # in GJ/capita
  return(colSums(emb_energy))  # in TJ
}

global_total <- 0
for (i in exio_ctys) {
  country_total <- GetTotalEmbeddedEnergy(i)[8]
  global_total <- global_total + country_total
  print(paste0(i, ' ', country_total))
}



#########################################
### Get EXIO FD vectors for countries ###
#########################################

# Get FRA final demand from EXIO [M.EUR]
FRA_place <- which(exio_ctys=="FR")
FRA_idx_fd <- seq(7*(FRA_place-1)+1, 7*FRA_place)   # 7 final demand columns per country
FRA_fd <- matrix(final_demand[,FRA_idx_fd[1]], nrow=200) / EXR_EUR$r  # to M.USD
FRA_fd_exio <- rowSums(FRA_fd) # Sum all HH FD across countries
FRA_fd_exio_imp <- rowSums(FRA_fd[,-FRA_place]) # Sum all HH FD across countries
