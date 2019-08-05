# We have seven extensions.
# TMAR: Marine transport
# TAVI: Air transport
# TROA: Road transport
# TRAI: Rail transport
# TOTH: Other transport
# NTRA: Non-transport (industry + other)
# NENE: Non-energy use


### Note: Intensity import is not done as the following way. 
### The multiplications are directly done in Matlab, and the resulting TFEI/DFEIs get read in "import_EXIO_FE_extension.R".


####### Derive energy intensities #######
# All indirect intensity matrices are in MJ/EUR

# 1.1 embodied primary energy intensity (nature input)
# y <- 1/tot_demand
# y[is.infinite(y)] <- 0 
# DFEI.TMAR <- as.matrix(tot.TMAR) %*% diag(y)   # Derive energy intensities by dividing by total demand per sector TJ/M.EUR = MJ/EUR
# DFEI.TAVI <- as.matrix(tot.TAVI) %*% diag(y)   
# DFEI.TROA <- as.matrix(tot.TROA) %*% diag(y)   
# DFEI.TRAI <- as.matrix(tot.TRAI) %*% diag(y)   
# DFEI.TOTH <- as.matrix(tot.TOTH) %*% diag(y)   
# DFEI.INDR <- as.matrix(tot.NTRA) %*% diag(y)   
# DFEI.NENE <- as.matrix(tot.NENE) %*% diag(y)   
# # DFEI.EXIO <- lapply(tot.finalE, function(x) {as.matrix(x) %*% diag(y)})
# 
# # TFEI EXIO (69x9800): Not used
# TFEI.TMAR <- eigenMapMatMult(DFEI.TMAR, as.matrix(L_inverse)) 
# TFEI.TAVI <- eigenMapMatMult(DFEI.TAVI, as.matrix(L_inverse)) 
# TFEI.TROA <- eigenMapMatMult(DFEI.TROA, as.matrix(L_inverse)) 
# TFEI.TRAI <- eigenMapMatMult(DFEI.TRAI, as.matrix(L_inverse)) 
# TFEI.TOTH <- eigenMapMatMult(DFEI.TOTH, as.matrix(L_inverse)) 
# TFEI.INDR <- eigenMapMatMult(DFEI.INDR, as.matrix(L_inverse)) 
# TFEI.NENE <- eigenMapMatMult(DFEI.NENE, as.matrix(L_inverse)) 
# # TFEI.EXIO <- lapply(DFEI.EXIO, function(x) {eigenMapMatMult(x, as.matrix(L_inverse))})
# 
# DFEI.INDR.elec <- DFEI.INDR[elec_rows,]
# DFEI.INDR.nonelec <- DFEI.INDR[-elec_rows,]
# TFEI.INDR.elec <- TFEI.INDR[elec_rows,]
# TFEI.INDR.nonelec <- TFEI.INDR[-elec_rows,]


# Assumption: The efficiency gains are applied equally globally.
TFEI.ApplyKeyTechImprovement <- function(country='IND', dfei, coeff=c(steel=0.27, alu=0.45, cement=0.8, brick=0.8)) {
	exio.index <- c(104, 108, 100, 101) # Same order as coeff
	exio.index.global <- as.vector(sapply(seq(0,exio.len-200,200), function(x) x+exio.index, simplify = "array"))
	n_years <- Year.end - Year.base
	coeff.ann <- coeff^(1/n_years)
	coeff.obs <- lapply(Year.obs, function(x) coeff.ann^(x-Year.base)) # Coefficients for observation years
	names(coeff.obs) <- paste0('Y',Year.obs)
	
	update.DFEI.eff <- function(coef) {
	  coef <- rep(coef, num.cty) # Copying for 49 exio regions
	  dfei[, exio.index.global] <- dfei[, exio.index.global] %*% diag(coef)
	  return(dfei)}
	
	DFEI.obs <- lapply(coeff.obs, update.DFEI.eff)
	TFEI.obs <- lapply(DFEI.obs, function(d) {eigenMapMatMult(d, as.matrix(L_inverse)) })
	
	TFEI.icp.draw <- lapply(TFEI.obs, SetupSectorIntensities,  # TFEI.obs input as final.intensity.mat
	            mapping_list=lapply(final_alloc_list[[country]][[1]], func1), not_conv_idx=final_alloc_list[[country]][[2]], country=countrycode(country,"iso3c", "iso2c"),
	            type='final', pri.intensity.mat=indirect_E_int)
	
	TFEI.icp.mean <- sapply(TFEI.icp.draw, colMeans)
	
	return(data.frame(name=ICP_catnames,TFEI.icp.mean))
}

TFEI.ApplyKeyTechImprovement.EXIO <- function(country='IND', 
                                              dfei=dfei.exio, coeff=c(steel=0.27, alu=0.45, cement=0.8, brick=0.8)) {
  exio.index <- c(104, 108, 100, 101) # Same order as coeff
  exio.index.global <- as.vector(sapply(seq(0,exio.len-200,200), function(x) x+exio.index, simplify = "array"))
  n_years <- Year.end - Year.base
  coeff.ann <- coeff^(1/n_years)
  Year.obs <- c(Year.base, seq(2020, Year.end, 10))
  coeff.obs <- lapply(Year.obs, function(x) coeff.ann^(x-Year.base)) # Coefficients for observation years
  names(coeff.obs) <- paste0('Y',Year.obs)
  
  update.DFEI.eff <- function(coef) {
    coef <- rep(coef, num.cty) # Copying for 49 exio regions
    dfei[, exio.index.global] <- dfei[, exio.index.global] %*% diag(coef)
    return(dfei)}
  
  DFEI.obs <- lapply(coeff.obs, update.DFEI.eff)
  TFEI.obs <- lapply(DFEI.obs, function(d) {eigenMapMatMult(d, as.matrix(L_inverse)) })
  
  cty_place <- which(exio_ctys==countrycode(country, "iso3c", "iso2c"))
  cty_idx <- seq(200*(cty_place-1)+1, 200*cty_place)  # 200 EXIO commodities per country
  TFEI.exio.allregion <- sapply(TFEI.obs, colSums)[cty_idx,]
  
  # Match currency EUR 2007 MER to USD 2011 PPP
  TFEI.exio.allregion <- TFEI.exio.allregion / ifelse(country=="IND", (EXR_IND * CPI_ratio_IND / PPP_IND),
                                          ifelse(country=="BRA", (EXR_BRA * CPI_ratio_BRA / PPP_BRA), 
                                                 (EXR_ZAF * CPI_ratio_ZAF / PPP_ZAF))) 
  
  return(data.frame(name=EX_catnames,TFEI.exio.allregion))
}

# calls
# TFEI.ICP.IND.TMAR <- TFEI.ApplyKeyTechImprovement("IND", DFEI.TMAR) #x7
# TFEI.ICP.BRA.TMAR <- TFEI.ApplyKeyTechImprovement("BRA", DFEI.TMAR) #x7
# TFEI.ICP.ZAF.TMAR <- TFEI.ApplyKeyTechImprovement("ZAF", DFEI.TMAR) #x7
# Summurized as following:
# TFEI.ICP.IND <- lapply(DFEI.EXIO, function(x) {TFEI.ApplyKeyTechImprovement("IND", x)})
# TFEI.ICP.BRA <- lapply(DFEI.EXIO, function(x) {TFEI.ApplyKeyTechImprovement("BRA", x)})
# TFEI.ICP.ZAF <- lapply(DFEI.EXIO, function(x) {TFEI.ApplyKeyTechImprovement("ZAF", x)})

# a <- TFEI.ApplyKeyTechImprovement("IND", p_energy_int.prirow, coeff=c(0.1, 0.1, 0.1, 0.1)) #x7

# TFEI.EXIO.ts <- sapply(countries, TFEI.ApplyKeyTechImprovement.EXIO, dfei=dfei.exio, simplify=FALSE, USE.NAMES = TRUE) # Returns a list
# # TFEI.IND <- TFEI.ApplyKeyTechImprovement.EXIO("IND", p_energy_int.prirow) 
# # TFEI.BRA <- TFEI.ApplyKeyTechImprovement.EXIO("BRA", p_energy_int.prirow) 
# # TFEI.ZAF <- TFEI.ApplyKeyTechImprovement.EXIO("ZAF", p_energy_int.prirow) 
# # a <- TFEI.ApplyKeyTechImprovement("BRA", p_energy_int.prirow, coeff=c(0.1, 0.1, 0.1, 0.1)) #x7
# # a <- TFEI.ApplyKeyTechImprovement("ZAF", p_energy_int.prirow, coeff=c(0.1, 0.1, 0.1, 0.1)) #x7
# # b <- a %>% mutate(r.improv=Y2050/Y2008) %>% filter(name=="")
# 
# # Health/Education TFEI (long format)
# idx.HE <- 174:175
# a <- do.call("rbind", lapply(seq_along(TFEI.EXIO.ts), function(x, name, i) {
#   temp <- data.frame(t(x[[i]][idx.HE,-1])) 
#   names(temp) <- c("Education", "Health")
#   temp <- temp %>% mutate(Country=name[i], Year=names(x[[i]])[-1])
#   return(temp)
#   }, name=names(TFEI.EXIO.ts), x=TFEI.EXIO.ts))



# Return EXIO indirect intensity in MJ/USD2007
GetSpecificEXIOSectorIntensity <- function(cty, exio_sect) {
  cty_place <- which(exio_ctys==cty)
  # cty_idx_fd <- seq(7*(cty_place-1)+1, 7*cty_place)   # 7 final demand columns per country
  cty_idx_ex <- seq(200*(cty_place-1)+1, 200*cty_place)   # 7 final demand columns per country
  ex_idx <- which(EX_catnames==exio_sect)
  
  int <- colSums(indirect_E_int[,cty_idx_ex])[ex_idx] * EXR_EUR$r   # MJ/EUR to MJ/USD2007
  
  return(int)
}


### Below are for one-time uses.

# Format input tables (Intensity & Unit) for JM's component: Food, Clothing, Health/Education
FormatDLEInputTable.EXIO <- function(comp, tabl = "Intensity", carr="Total", val.list=TFEI.EXIO.ts, idx) {
  outtable <- data.frame(Country=character(), Scenario=character(), Year=integer(), Type=character(), Carrier=character(), TFEI=double())
  outlist <- list()
  if (tabl=="Intensity") {
    for (cty in names(val.list)) {
      val <- sapply(idx, function (i) {
        v.all <- lapply(val.list, function(x) {x[i,]})
        v <- as.numeric(v.all[[cty]][-1])
        return(v)
      })
      names(val) <- comp
      out <- data.frame(Country=cty, Scenario=rep("DLE.BAU", length(Year.obs)), 
                        Year=Year.obs, Type="OP", Carrier=carr, val[rep(1,length(Year.obs)),]) %>%
        rbind(data.frame(Country=cty, Scenario=rep("DLE.ACCEL", length(Year.obs)), 
                         Year=Year.obs, Type="OP", Carrier=carr, val[rep(1,length(Year.obs)),])) %>%
        rbind(data.frame(Country=cty, Scenario=rep("DLE.ACCEL.LCT", length(Year.obs)), 
                         Year=Year.obs, Type="OP", Carrier=carr, val)) %>%
        rbind(data.frame(Country=cty, Scenario=rep("DLE.ACCEL.LCT.BHV", length(Year.obs)), 
                         Year=Year.obs, Type="OP", Carrier=carr, val)) 
      names(out)[-(1:5)] <- comp  # All the non-numeric columns
      outlist[[cty]] <- out
    }
  }

  outtable <- do.call(rbind, outlist)
  return(outtable)
}

# Add more component to the input table
AddComponentToDLETable <- function(comp, tabl="Intensity", val.list, scenario, type="OP", carr="total", table) {
  if(!(comp %in% names(table))) {
    outlist <- list()
    for (cty in names(val.list)) {
      temp <- data.frame(Country=cty, Scenario=scenario, Type=type, Carrier=carr, Val=val.list[[cty]])  
      if (dim(temp)[1]==length(Year.obs)) {temp <- temp %>% mutate(Year=Year.obs)}  # If val.list[[cty]] is a series, add Year column (for join).
      outlist[[cty]] <- temp
    }
    
    val.table <- do.call(rbind, outlist)
    
    a <- table %>% left_join(val.table)
    names(a)[dim(a)[2]] <- comp
  }
  else {
    for (cty in names(val.list)) {
      table <- table %>% mutate_cond(Country==cty & Scenario==scenario & Type==type & Carrier==carr, !!comp:=as.numeric(val.list[[cty]]))
    }
    a <- table
  }
  return(a)
}

# Fill in the cells not filled specifically
FillEmptyCellsinDLEinputTable <- function(table, type="Intensity") {
  
  # First get ACCEL numbers if available, then if NA, get BAU numbers (in case of Food especially)
  repl <- table %>% filter(Scenario=="DLE.ACCEL")
  repl[is.na(repl)] <- (table %>% filter(Scenario=="DLE.BAU"))[is.na(repl)]
  
  if (type=="Intensity") {
    repl.copy <- table %>% select(Country, Scenario, Year, Type, Carrier) %>% left_join(repl %>% select(-Scenario))
  }
  else {
    repl.copy <- table %>% select(Country, Scenario, Year, Type) %>% left_join(repl %>% select(-Scenario))
  }
  table[is.na(table)] <- repl.copy[is.na(table)] 
  return(table)
}

### Read in the raw EXIO3 energy extensions and format them in a consistent way. (with 69 carriers)
HarmonizeEXIO3ExtensionFormat <- function(raw.ext) {
  # pei.nature.raw <- data.frame(name=label.S$name[idx.PE.nature], mat=raw.ext[idx.PE.nature, ]) %>% 
  #   mutate(name=gsub("Nature Inputs: ", "", name)) %>% right_join(data.frame(name=carrier.name.fin)) 
  # pei.USE.raw <- data.frame(name=label.S$name[idx.USE], mat=raw.ext[idx.USE, ]) %>% 
  #   mutate(name=gsub("Energy Carrier Use ", "", name)) %>% right_join(data.frame(name=carrier.name.pr)) %>% 
  #   right_join(data.frame(name=carrier.name.fin))  # Only the primary carriers, but keep the 69x9800 dimension
  # ei.SUPL.raw <- data.frame(name=label.S$name[idx.SUPL], mat=raw.ext[idx.SUPL, ]) %>% 
  #   mutate(name=gsub("Energy Carrier Supply ", "", name)) %>% right_join(data.frame(name=carrier.name.fin)) # All the 69 carriers
  # 
  # pei.nature <- as.matrix(pei.nature.raw %>% select(-name))
  # pei.nature[is.na(pei.nature)] <- 0
  # pei.USE <- as.matrix(pei.USE.raw %>% select(-name))
  # pei.USE[is.na(pei.USE)] <- 0
  # ei.SUPL <- as.matrix(ei.SUPL.raw %>% select(-name))
  # ei.SUPL[is.na(ei.SUPL)] <- 0
  
  fei.NENE.raw <- data.frame(name=label.S$name[idx.FE.NENE], mat=raw.ext[idx.FE.NENE, ]) %>% 
    mutate(name=gsub("Energy Carrier Net NENE ", "", name))
  fei.NTRA.raw <- data.frame(name=label.S$name[idx.FE.NTRA], mat=raw.ext[idx.FE.NTRA, ]) %>% 
    mutate(name=gsub("Energy Carrier Net NTRA ", "", name))
  fei.TAVI.raw <- data.frame(name=label.S$name[idx.FE.TAVI], mat=raw.ext[idx.FE.TAVI, ]) %>% 
    mutate(name=gsub("Energy Carrier Net TAVI ", "", name))
  fei.TMAR.raw <- data.frame(name=label.S$name[idx.FE.TMAR], mat=raw.ext[idx.FE.TMAR, ]) %>% 
    mutate(name=gsub("Energy Carrier Net TMAR ", "", name))
  fei.TOTH.raw <- data.frame(name=label.S$name[idx.FE.TOTH], mat=raw.ext[idx.FE.TOTH, ]) %>% 
    mutate(name=gsub("Energy Carrier Net TOTH ", "", name))
  fei.TRAI.raw <- data.frame(name=label.S$name[idx.FE.TRAI], mat=raw.ext[idx.FE.TRAI, ]) %>% 
    mutate(name=gsub("Energy Carrier Net TRAI ", "", name))
  fei.TROA.raw <- data.frame(name=label.S$name[idx.FE.TROA], mat=raw.ext[idx.FE.TROA, ]) %>% 
    mutate(name=gsub("Energy Carrier Net TROA ", "", name))
  fei.LOSS.raw <- data.frame(name=label.S$name[idx.FE.LOSS], mat=raw.ext[idx.FE.LOSS, ]) %>% 
    mutate(name=gsub("Energy Carrier Net LOSS ", "", name))
  
  fei.NENE <- data.frame(name=carriers.Arkz) %>% full_join(fei.NENE.raw) %>% slice(1:69) %>% select(-name)
  fei.NTRA <- data.frame(name=carriers.Arkz) %>% full_join(fei.NTRA.raw) %>% slice(1:69) %>% select(-name)
  fei.TAVI <- data.frame(name=carriers.Arkz) %>% full_join(fei.TAVI.raw) %>% slice(1:69) %>% select(-name)
  fei.TMAR <- data.frame(name=carriers.Arkz) %>% full_join(fei.TMAR.raw) %>% slice(1:69) %>% select(-name)
  fei.TOTH <- data.frame(name=carriers.Arkz) %>% full_join(fei.TOTH.raw) %>% slice(1:69) %>% select(-name)
  fei.TRAI <- data.frame(name=carriers.Arkz) %>% full_join(fei.TRAI.raw) %>% slice(1:69) %>% select(-name)
  fei.TROA <- data.frame(name=carriers.Arkz) %>% full_join(fei.TROA.raw) %>% slice(1:69) %>% select(-name)
  fei.LOSS <- data.frame(name=carriers.Arkz) %>% full_join(fei.LOSS.raw) %>% slice(1:69) %>% select(-name)
  
  fei.NENE[is.na(fei.NENE)] <- 0
  fei.NTRA[is.na(fei.NTRA)] <- 0
  fei.TAVI[is.na(fei.TAVI)] <- 0
  fei.TMAR[is.na(fei.TMAR)] <- 0
  fei.TOTH[is.na(fei.TOTH)] <- 0
  fei.TRAI[is.na(fei.TRAI)] <- 0
  fei.TROA[is.na(fei.TROA)] <- 0
  fei.LOSS[is.na(fei.LOSS)] <- 0
  
  fei.exio <- as.matrix(           fei.NTRA + fei.TAVI + fei.TMAR + fei.TOTH + fei.TRAI + fei.TROA           ) # w/o fei.NENE: NENE is about manufacturing process. Not about energy use & wellbeing
  nei.exio <- as.matrix(fei.NENE + fei.NTRA + fei.TAVI + fei.TMAR + fei.TOTH + fei.TRAI + fei.TROA + fei.LOSS)
  
  fei.elec <- fei.exio
  fei.elec[-idx.Elec.carrier,] <- 0
  
  fei.non.elec <- fei.exio
  fei.non.elec[idx.Elec.carrier,] <- 0
  
  fei.sub <- list(fei.NENE, fei.NTRA, fei.TAVI, fei.TMAR, fei.TOTH, fei.TRAI, fei.TROA, fei.LOSS) 
  names(fei.sub) <- c("NENE", "NTRA", "TAVI", "TMAR", "TOTH", "TRAI", "TROA", "LOSS")
  
  output <- list(fei.exio, fei.elec, fei.non.elec, fei.sub, 
  							#	pei.nature, pei.USE, ei.SUPL, 
  							 nei.exio)
  
  return(output)
}

# To convert inconsistet column names
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
