
### Material extension?
key.sects <- c(Oil=28, Cement=101, Steel=104, Aluminum=108)
# idx.key.sects <- as.vector(sapply(seq(0,exio.len-200,200), function(x) x+key.sects, simplify = "array"))
idx.key.sects <- list()
for (i in 1:length(key.sects)) {
  idx.key.sects[[i]] <- as.vector(sapply(seq(0,exio.len-200,200), function(x) x+key.sects[i], simplify = "array"))
}

# Embodied material use in all EXIO products (material EUR/product EUR)
collap <- function(x) {colSums(L_inverse[x,])}
tfmi.monetary.exio <- t(sapply(idx.key.sects, collap))   # 4x9800   
row.names(tfmi.monetary.exio) <- names(key.sects)

# Total monetary output of the sectors (M.EUR) (basic price)
raw.x.2010 <- unlist(read.csv(paste0(EXIO3_path, "x_2010.csv"), header = FALSE))
M.EUR.2010 <- sapply(idx.key.sects, function(x) {sum(raw.x.2010[x])}) 

# Total global consumption 

# Global cement production and consumption 3.3e9 ton (2010) & 4.2e9 ton (2015) : http://www.globalcement.com/magazine/articles/490-cement-101-an-introduction-to-the-worlds-most-important-building-material
# Global steel consumption 1.408e9 ton (2010) & 1.622e9 ton (2017) : http://www.ereport.ru/en/stat.php?razdel=metal&count=st&table=cons  & http://www.sxcoal.com/news/4568880/info/en 
# Global Alu consumption 40.052e6 ton (2010) http://www.ereport.ru/en/stat.php?razdel=metal&count=al&table=cons
# Global crude oil consumption 365*88.3e6 = 32.2e9 barrel (2010) : http://www.ereport.ru/en/stat.php?razdel=metal&count=oi&table=cons

qty.2010 <- c(32.2e9, 3.3e9, 1.408e9, 40.052e6)  # Barrel, ton, ton, ton


# (Sort of) Global price (basic price)
price.2010 <- M.EUR.2010*1e6/qty.2010  # EUR/barrel or ton consumed
names(price.2010) <- names(key.sects)


# Embodied material intensity in EXIO (for Health and Edu)
tfmi.phy.exio <- diag(1/price.2010) %*% tfmi.monetary.exio # Barrel or ton / product EUR


# Derive material intensities in ICP (Barrel or ton/USD)
mat.int.icp.list <- list()
for (i in 1:length(key.sects)) {
  list[mat.int.icp.list$BRA[[names(key.sects)[i]]], BRA.alloc.m, NC_BRA.m, BRA_FD_adj.m] <- DeriveIntensities('BRA', 'final', final.intensity.mat=tfmi.phy.exio[i,,drop=F])
  list[mat.int.icp.list$IND[[names(key.sects)[i]]], IND_alloc.m, NC_IND.m, IND_FD_adj.m] <- DeriveIntensities('IND', 'final', final.intensity.mat=tfmi.phy.exio[i,,drop=F])
  list[mat.int.icp.list$ZAF[[names(key.sects)[i]]], ZAF_alloc.m, NC_ZAF.m, ZAF_FD_adj.m] <- DeriveIntensities('ZAF', 'final', final.intensity.mat=tfmi.phy.exio[i,,drop=F])
}


# Embodied material consumtpion from food in 2010 (Barrel or ton)
tot.mat.food <- list(
  BRA = sapply(mat.int.icp.list$BRA, function (mat.int.cty) {
    mean(rowSums(mat.int.cty[, idx.food.icp] %*% diag(BRA_FD_ICP_io.yr[idx.food.icp, 1]))) / scaler_BRA
  }),
  IND = sapply(mat.int.icp.list$IND, function (mat.int.cty) {
    mean(rowSums(mat.int.cty[, idx.food.icp] %*% diag(IND_FD_ICP_io.yr[idx.food.icp, 1]))) / scaler_IND
  }),
  ZAF = sapply(mat.int.icp.list$ZAF, function (mat.int.cty) {
    mean(rowSums(mat.int.cty[, idx.food.icp] %*% diag(ZAF_FD_ICP_io.yr[idx.food.icp, 1]))) / scaler_ZAF
  })
)


# Embodied material consumtpion from clothing in 2010 (Barrel or ton)
idx.clothing.all <- c(idx.clothing.icp, idx.footwear.icp)
tot.mat.clothing <- list(
  BRA = sapply(mat.int.icp.list$BRA, function (mat.int.cty) {
    mean(rowSums(mat.int.cty[, idx.clothing.all] %*% diag(BRA_FD_ICP_io.yr[idx.clothing.all, 1]))) / scaler_BRA
  }),
  IND = sapply(mat.int.icp.list$IND, function (mat.int.cty) {
    mean(rowSums(mat.int.cty[, idx.clothing.all] %*% diag(IND_FD_ICP_io.yr[idx.clothing.all, 1]))) / scaler_IND
  }),
  ZAF = sapply(mat.int.icp.list$ZAF, function (mat.int.cty) {
    mean(rowSums(mat.int.cty[, idx.clothing.all] %*% diag(ZAF_FD_ICP_io.yr[idx.clothing.all, 1]))) / scaler_ZAF
  })
)


# Total kcal of food
tot.kcal <- list(
  BRA = kcal.ssp2 %>% filter(Region=="BRA" & Year==2010) %>% select(Val) %>% as.numeric() * BRA_pop_io.yr *365,
  IND = kcal.ssp2 %>% filter(Region=="IND" & Year==2010) %>% select(Val) %>% as.numeric() * IND_pop_io.yr *365,
  ZAF = kcal.ssp2 %>% filter(Region=="ZAF" & Year==2010) %>% select(Val) %>% as.numeric() * ZAF_pop_io.yr *365
)

# Total kg of clothing
tot.kg.clothing <- list(
  BRA = tot.clothing.kg.BRA,
  IND = tot.clothing.kg.IND,
  ZAF = tot.clothing.kg.ZAF
)

tfmi.food <- t(mapply(function(x, y) {x*1e3/y}, tot.mat.food, tot.kcal))   # mBarrel or kg / kcal food
tfmi.clothing <- t(mapply(function(x, y) {x*1e3/y}, tot.mat.clothing, tot.kg.clothing))  # mBarrel or kg / kg clothing

# Intensity (mBarrel or kg/EUR 2007 MER to mBarrel or kg/USD 2011 PPP)
tfmi.health <- t(data.frame(BRA = tfmi.phy.exio[,BRA_idx_ex[idx.health.exio]] / (EXR_BRA * CPI_BRA / PPP_BRA) * 1e3,
                          IND = tfmi.phy.exio[,IND_idx_ex[idx.health.exio]] / (EXR_IND * CPI_IND / PPP_IND) * 1e3,
                          ZAF = tfmi.phy.exio[,ZAF_idx_ex[idx.health.exio]] / (EXR_ZAF * CPI_ZAF / PPP_ZAF) * 1e3))

colnames(tfmi.health) <- colnames(tfmi.food)
tfmi.education <- t(data.frame(BRA = tfmi.phy.exio[,BRA_idx_ex[idx.education.exio]] / (EXR_BRA * CPI_BRA / PPP_BRA) * 1e3, 
                             IND = tfmi.phy.exio[,IND_idx_ex[idx.education.exio]] / (EXR_IND * CPI_IND / PPP_IND) * 1e3,
                             ZAF = tfmi.phy.exio[,ZAF_idx_ex[idx.education.exio]] / (EXR_ZAF * CPI_ZAF / PPP_ZAF) * 1e3))
colnames(tfmi.education) <- colnames(tfmi.food)

# Reshape
tfmi.food <- data.frame(tfmi.food) %>% add_rownames("Country") %>% gather(Material, Food, -Country)
tfmi.clothing <- data.frame(tfmi.clothing) %>% add_rownames("Country")  %>% gather(Material, Clothing, -Country)
tfmi.health <- data.frame(tfmi.health) %>% add_rownames("Country")  %>% gather(Material, Health, -Country)
tfmi.education <- data.frame(tfmi.education) %>% add_rownames("Country") %>% gather(Material, Educ, -Country)

tfmi.io <- tfmi.food %>% left_join(tfmi.clothing) %>% left_join(tfmi.health) %>% left_join(tfmi.education) %>% mutate(Type="OP")

material.int.AM <- read_xlsx(paste0(result_path, "LCA_analysis/material_quantity_all_comp.xlsx")) %>% mutate(AC.R=AC, AC.U=AC) %>% select(-AC)

material.con <- energy.tot %>% filter(Type=="CON") %>% select(Scenario:Year) %>% left_join(material.int.AM) 
material.con <- material.con %>% select(intersect(names(energy.tot %>% rename(Material=Carrier)), names(material.con)))

material.con.form <- unit.tot %>% filter(Carrier=="elec") %>% rename(Material=Carrier) %>% arrange(Scenario, Country, Type, Year)
material.con.form[,] <- NA
material.con.form <- do.call(rbind, replicate(4, material.con.form, simplify=FALSE)) # Replicat 4 times for 4 material types
material.con.form[, which(names(material.int.all) %in% names(material.con))] <- material.con

material.op <- energy.tot %>% filter(Type=="OP") %>% select(Scenario:Year) %>% left_join(tfmi.io) 
material.op <- material.op %>% select(intersect(names(energy.tot %>% rename(Material=Carrier)), names(material.op)))

material.op.form <- unit.tot %>% filter(Carrier=="elec") %>% rename(Material=Carrier) %>% arrange(Scenario, Country, Type, Year)
material.op.form[,] <- NA
material.op.form <- do.call(rbind, replicate(4, material.op.form, simplify=FALSE)) # Replicat 4 times for 4 material types
material.op.form[, which(names(material.int.all) %in% names(material.op))] <- material.op

material.int.all <- rbind(material.con.form, material.op.form) %>% arrange(Scenario, Country, Type, Material, Year)

unit.tot.material <- material.int.all %>% select(Scenario:Material) %>% left_join(unit.tot %>% filter(Carrier=="elec") %>% select(-Carrier)) %>% select(names(material.int.all))
material.calc <- (unit.tot.material %>% select(-(Scenario:Material))) * (material.int.all %>% select(-(Scenario:Material))) %>%
  mutate(Mobility=Mobility*1e3, Roads=Roads/1e6) * 1e6 / 1e3 # Barrel or ton
material.tot <- data.frame(unit.tot.material %>% select(Scenario:Material), material.calc) %>% mutate(total=rowSums(.[6:24], na.rm=TRUE))
material.tot[is.na(material.tot)] <- 0
material.tot.sum <- material.tot %>% group_by(Scenario, Country, Year, Material) %>% 
  select(-Type) %>% summarise_all(sum, na.rm=TRUE)

xlsx::write.xlsx(as.data.frame(material.tot), paste0(path.integ, "Result - Total material", uncertainty.case, ".xlsx"), sheetName="kBarrel or ton by comp")
xlsx::write.xlsx(as.data.frame(material.tot.sum), paste0(path.integ, "Result - Total material", uncertainty.case, ".xlsx"), sheetName="Aggregated ", append=TRUE)

xlsx::write.xlsx(as.data.frame(material.int.all), paste0(path.integ, "Input - Intensity & Units - Material", uncertainty.case, ".xlsx"), sheetName="Material Intensity (TFEI)")
xlsx::write.xlsx(as.data.frame(unit.tot.material), paste0(path.integ, "Input - Intensity & Units - Material", uncertainty.case, ".xlsx"), sheetName="Unit", append=TRUE)


