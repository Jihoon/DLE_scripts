
cu_eqs=read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/cu_eq.csv")
states=read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/states.csv")
# valid_column_names <- make.names(names=names(food_nutrients), unique=TRUE, allow_ = TRUE)

hh_sum=selectDBdata(SURVEY, ID, HH_SIZE, REGION, AGE, SOCIAL_GROUP, RELIGION, MALE, MALE_ADULT, MALE_MINOR, EDUC_YEARS, MINOR, DWELL_STATUS, WORKER_TYPE, OCCUPATION, EXPENDITURE, WEIGHT, URBAN, tables='IND1_HH')
hh_sum=hh_sum%>%
  mutate(exp_percap = expenditure/hh_size)

#create income groups, which is used to join cluster assignments to items
urb_grp=hh_sum %>%
  filter(urban==1) %>%
  filter(is.finite(exp_percap)) %>%
  arrange(exp_percap) %>%
  mutate(inc_grp=cut(exp_percap,breaks=c(0,1.4*365,2.8*365,5.6*365,max(exp_percap)*365),labels=F))

rur_grp=hh_sum %>%
  filter(urban==0) %>%
  filter(is.finite(exp_percap)) %>%
  arrange(exp_percap) %>%
  mutate(inc_grp=cut(exp_percap,breaks=c(0,0.95*365,1.9*365,3.8*365,max(exp_percap)*365),labels=F))

hh_sum<-rbind(urb_grp,rur_grp) 

hh_sum=hh_sum%>%
  mutate(female_adult=hh_size-minor-male_adult,female_minor=minor-male_minor)%>%
  mutate(cu_eq_MA=getcu("male_adult")*male_adult, cu_eq_FA=female_adult*getcu("female_adult"), 
         cu_eq_MM=male_minor*getcu("male_minor"), cu_eq_FM=female_minor*getcu("female_minor"))%>%
  left_join(states)%>%
  mutate(cluster=paste0(zone,as.character(urban)))

hh_dem_cts=hh_sum %>%
  # select(cluster,cu_eq_MA,cu_eq_FA,cu_eq_MM,cu_eq_FM, weight)%>%
  group_by(cluster, inc_grp) %>%
  summarise(cu_eq_MA_avg=weighted.mean(cu_eq_MA, weight, na.rm=TRUE),
            cu_eq_FA_avg=weighted.mean(cu_eq_FA, weight, na.rm=TRUE),
            cu_eq_MM_avg=weighted.mean(cu_eq_MM, weight, na.rm=TRUE),
            cu_eq_FM_avg=weighted.mean(cu_eq_FM, weight, na.rm=TRUE),
            MA_avg=weighted.mean(male_adult, weight, na.rm=TRUE),
            FA_avg=weighted.mean(female_adult, weight, na.rm=TRUE),
            MM_avg=weighted.mean(male_minor, weight, na.rm=TRUE),
            FM_avg=weighted.mean(female_minor, weight, na.rm=TRUE),
            MA_tot=sum(male_adult*weight, na.rm=TRUE),
            FA_tot=sum(female_adult*weight, na.rm=TRUE),
            MM_tot=sum(male_minor*weight, na.rm=TRUE),
            FM_tot=sum(female_minor*weight, na.rm=TRUE),
            n_hh = sum(weight)) %>%
  filter(inc_grp %in% inc_grps)%>%
  mutate(clsname=paste0(cluster,"_",as.character(inc_grp))) 

hh_size_cls <- hh_dem_cts %>% ungroup(cluster) %>% select(clsname, MA_avg, FA_avg, MM_avg, FM_avg)  # Num persons


#calculate avg prices and get energy intensity by cluster
food_items = selectDBdata(SURVEY, ID, ITEM, CODE, UNIT, QTY_TOT, VAL_TOT_ORG, tables='IND1_FOOD')
hh_map=selectDBdata(CODE, ICP_CODE, tables='IND1_MAP')

#For food items that are entered by number, alter quantity to kg based on IND_FOOD_AVG_WT look up table
food_items = food_items %>%
  left_join(avg_wt) %>%
  transform(qty_tot=ifelse(is.na(avg_wt), qty_tot, qty_tot*avg_wt))

# food_nutrients = food_nutrients %>%
#   select(item, food_grp, energy,protein, vita, iron, zinc)

food_items = food_items %>%
  left_join(hh_map)

food_avgs_national=food_items%>%
  filter(is.finite(qty_tot)) %>%
  mutate(avg_price=val_tot_org/qty_tot)%>%
  inner_join(hh_sum%>% select(survey, id, hh_size, cluster, weight, urban, inc_grp, region)) %>%
  group_by(item, code) %>%
  summarise(avg_price=weighted.mean(avg_price, qty_tot, na.rm=T), qty_tot=sum(weight * qty_tot, na.rm=T) / sum(hh_sum$weight)) %>%
  left_join(food_nutrients_org)

food_avgs_grp=food_items%>%
  filter(is.finite(qty_tot)) %>%
  mutate(avg_price=val_tot_org/qty_tot)%>%
  inner_join(hh_sum%>% select(survey, id, hh_size, cluster, weight, urban, inc_grp, region)) %>%
  left_join(food_group%>%select(-code)) %>%
  group_by(urban, cluster, inc_grp, food_grp, item) %>%
  summarise_each(.,funs(weighted.mean(., weight=qty_tot, na.rm=T)),avg_price) %>% filter(!is.na(food_grp))

food_price_indiv <- food_items%>%
  filter(is.finite(qty_tot)) %>%
  mutate(avg_price=val_tot_org/qty_tot)%>%
  inner_join(hh_sum%>% select(survey, id, hh_size, cluster, weight, urban, inc_grp, region)) 


# National total cost and emission
list[Cost, Emission] <- GetTOtalQuantities()  # M.USD/yr   &   kTon CO2e/yr


# Infeasible (manually put together..) - previous data
# inf1 <- c("N1_1", "S1_1", "W0_1", "W1_1")             # "te_min" 
# inf2 <- c("N0_1", "N1_1", "S1_1", "W0_1", "W1_1")    # "te_min_cap"
# inf3 <- c("S1_1", "W0_1", "W1_1")    # "te_min_nogrp" & "te_min_khes"

# Infeasible (manually put together..) - new data (IFCT 2017 + FAO EF)
inf1 <- c("N0_1", "N1_1", "S1_1", "W0_1", "W1_1")             # "te_min", "te_min_cap", "te_min_nogrp" & "te_min_khes"
inf2 <- "S1_1"  # "min_pds

# Gap total
tc_gap <- Cost %>% mutate_cond(row.names(.) %in% inf1, te_min=NA, te_min_cap=NA, te_min_nogrp=NA, te_min_khes=NA) %>% 
  mutate_cond(row.names(.) %in% inf2, te_min_pds=NA) %>%
  # mutate_cond(row.names(.) %in% inf2, te_min_cap=NA) %>% 
  # mutate_cond(row.names(.) %in% inf3, te_min_nogrp=NA, te_min_khes=NA) %>% 
  mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs(.-base)) %>% 
  mutate_cond(is.na(te_min), te_min=te_min_cost) %>%
  mutate_cond(is.na(te_min_cap), te_min_cap=te_min_cost) %>% 
  mutate_cond(is.na(te_min_pds), te_min_pds=te_min_cost) %>% 
  mutate_cond(is.na(te_min_nogrp), te_min_nogrp=te_min_cost) %>% 
  mutate_cond(is.na(te_min_khes), te_min_khes=te_min_cost) %>% 
  rbind(colSums(.)) 
tc_gap[abs(tc_gap) < 1] <- 0
row.names(tc_gap) <- c(row.names(Cost), "tot")

# in kTon CO2e/yr
te_gap <- Emission %>% mutate_cond(row.names(.) %in% inf1, te_min=NA, te_min_cap=NA, te_min_nogrp=NA, te_min_khes=NA) %>% 
  mutate_cond(row.names(.) %in% inf2, te_min_pds=NA) %>%
  # mutate_cond(row.names(.) %in% inf2, te_min_cap=NA) %>% 
  # mutate_cond(row.names(.) %in% inf3, te_min_nogrp=NA, te_min_khes=NA) %>% 
  mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs(.-base)) %>% 
  mutate_cond(is.na(te_min), te_min=te_min_cost) %>%
  mutate_cond(is.na(te_min_cap), te_min_cap=te_min_cost) %>% 
  mutate_cond(is.na(te_min_pds), te_min_pds=te_min_cost) %>% 
  mutate_cond(is.na(te_min_nogrp), te_min_nogrp=te_min_cost) %>% 
  mutate_cond(is.na(te_min_khes), te_min_khes=te_min_cost) %>% 
  rbind(colSums(.))
te_gap[abs(te_gap) < 1] <- 0
row.names(te_gap) <- c(row.names(Cost), "tot")

# data frame for plotting
Cost_pl <- Cost %>% mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs((.-base)/base)) %>% 
  mutate(zone = row.names(Cost)) %>%
  mutate_cond(zone %in% inf1, te_min=te_min_cost, te_min_cap=te_min_cost, te_min_nogrp=te_min_cost, te_min_khes=te_min_cost) %>%
  mutate_cond(zone %in% inf2, te_min_pds=te_min_cost) %>%
  # mutate_cond(zone %in% inf2, te_min_cap=te_min_cost) %>%
  # mutate_cond(zone %in% inf3, te_min_nogrp=te_min_cost, te_min_khes=te_min_cost) %>% 
  gather("scenario", "expenditure", 2:10) %>% arrange(zone) %>% mutate_cond(abs(expenditure) < 1e-10, expenditure=0) # USD/year
Cost_pl <- cbind(do.call("rbind", lapply(strsplit(Cost_pl$zone, ""), '[', -3)), Cost_pl) 
names(Cost_pl)[1:3] <- c("state", "urban", "inc")

Emission_pl <- Emission %>% mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs((.-base)/base)) %>% 
  mutate(zone = row.names(Emission)) %>%
  mutate_cond(zone %in% inf1, te_min=te_min_cost, te_min_cap=te_min_cost, te_min_nogrp=te_min_cost, te_min_khes=te_min_cost) %>%
  mutate_cond(zone %in% inf2, te_min_pds=te_min_cost) %>%
  # mutate_cond(zone %in% inf2, te_min_cap=te_min_cost) %>%
  # mutate_cond(zone %in% inf3, te_min_nogrp=te_min_cost, te_min_khes=te_min_cost) %>% 
  gather("scenario", "kgCO2e", 2:10) %>% arrange(zone) %>% mutate_cond(abs(kgCO2e) < 1e-10, kgCO2e=0) # kgCO2e/year
Emission_pl <- cbind(do.call("rbind", lapply(strsplit(Emission_pl$zone, ""), '[', -3)), Emission_pl) 
names(Emission_pl)[1:3] <- c("state", "urban", "inc")

Infeasibles <- Cost_pl %>% select(zone, scenario) %>% mutate(inf="Feasible") %>%
  mutate_cond(zone %in% inf1 & (scenario=="te_min" | scenario=="te_min_cap" | scenario=="te_min_nogrp" | scenario=="te_min_khes"), 
              inf="Infeasible") %>%
  mutate_cond(zone %in% inf2 & scenario=="te_min_pds", inf="Infeasible") 

Cost_pl <- Cost_pl %>% left_join(Infeasibles)
Emission_pl <- Emission_pl %>% left_join(Infeasibles)

# Total non-CO2 emission per capita for te_min_cap 
EmissionPerCap <- Emission %>% select(base, te_min_cap, te_min_cost, n_hh) %>%   # kTon CO2e/yr
  mutate(zone = row.names(Emission)) %>%
  mutate_cond(zone %in% inf1, te_min_cap=te_min_cost) 
EmissionPerCap <- cbind(do.call("rbind", lapply(strsplit(EmissionPerCap$zone, ""), '[', -3)), EmissionPerCap) 
names(EmissionPerCap)[1:3] <- c("state", "urban", "inc")
EmissionPerCap <- EmissionPerCap %>% select(-te_min_cost, -zone) %>% group_by(inc) %>% 
  summarise(totE_base = sum(base), totE_opt = sum(te_min_cap), n_hh = sum(n_hh)) %>% cbind(pop_inc) %>%
# EmissionPerCap <- EmissionPerCap %>% rbind(c(0, colSums(EmissionPerCap[,-1]))) 
  mutate(perCap_base=totE_base/pop_inc*1e6, perCap_opt=totE_opt/pop_inc*1e6)  # kgCO2e
sum(EmissionPerCap$totE_base) / sum(EmissionPerCap$pop_inc)*1e6
sum(EmissionPerCap$totE_opt) / sum(EmissionPerCap$pop_inc)*1e6
sum(EmissionPerCap$totE_opt) /  *1e6
write.table(EmissionPerCap[,6:7], "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)




Consumption_kg <- GetTOtalQuantities_kg()  # kg/yr in NSS categories
Consumption_kg <- Consumption_kg %>% left_join(food_group) %>% select(-food_grp) %>% arrange(code) %>% 
  left_join(food_avgs_national %>% select(code, avg_price))
# Consumption_price <- Consumption_kg %>% mutate_at(vars(contains("min")), funs(.*avg_price))
# Total food expenditure (USD 2007 MER)
Consumption_price <- Consumption_kg %>% mutate_at(vars(contains("min")), 
                                                  funs(.*avg_price * PPP_IND / CPI_ratio_IND / EXR_IND )) #/ IND_con_grwth 

# Get hh size & n_hh by four income groups 
a <- hh_sum %>% group_by(inc_grp) %>%
  summarise(MA_avg=weighted.mean(male_adult, weight, na.rm=TRUE),
            FA_avg=weighted.mean(female_adult, weight, na.rm=TRUE),
            MM_avg=weighted.mean(male_minor, weight, na.rm=TRUE),
            FM_avg=weighted.mean(female_minor, weight, na.rm=TRUE),
            n_hh = sum(weight))
hh_size_inc <- rowSums(a[,2:5])

n_hh_inc <- hh_dem_cts %>% group_by(inc_grp) %>% summarise(n_hh = sum(n_hh))
pop_inc <- hh_size_inc*n_hh_inc$n_hh 
pop_share_inc <- pop_inc / sum(pop_inc)

# Survey total food consumption (USD 2007 MER)
# combined with the baseline from the optimization data
food_baseline_NSS <- IND_FD_code[1:331,] %>% select(item, total, CODE) %>% 
  left_join(select(Consumption_price, starts_with("te_min_cap"), code), by=c("CODE" = "code")) %>%
  mutate(total = total * PPP_IND / CPI_ratio_IND / EXR_IND  ) #/ IND_con_grwth
food_baseline_NSS <- food_baseline_NSS %>% 
  mutate_cond(CODE >= 280 & CODE <=290, total=0) %>% 
  mutate_cond(CODE >= 325, total=0) %>% 
  mutate_cond(is.na(te_min_cap_base), te_min_cap_base=total, te_min_cap_opt_tot=total,
              te_min_cap_opt_1=total*pop_share_inc[1], te_min_cap_opt_2=total*pop_share_inc[2],
              te_min_cap_opt_3=total*pop_share_inc[3], te_min_cap_opt_4=total*pop_share_inc[4]) %>%
  rename(NSS = total, Opt_baseline = te_min_cap_base, Opt_result = te_min_cap_opt_tot, 
         Opt_result1 = te_min_cap_opt_1, Opt_result2 = te_min_cap_opt_2, Opt_result3 = te_min_cap_opt_3, Opt_result4 = te_min_cap_opt_4)
colSums(food_baseline_NSS[,2:9])

### Deriving GJ/cap from food

# 1. Only from the Optimization items (114 of them)
a <- data.frame(code=as.numeric(row.names(CES_ICP_IND))) %>% left_join(Consumption_price) 
a[is.na(a)] <- 0
a.mat <- as.matrix(a %>% select(-item, -group, -code))
Consumption_ICP <- data.frame(t(CES_ICP_IND) %*% a.mat)   # $/yr in ICP categories
Consumption_ICP <- Consumption_ICP %>% select(starts_with("te_min_cap"))

colSums(Consumption_ICP * apply(IND_intensity, 2, mean)[1:151] / 1000) / c(sum(pop_inc), sum(pop_inc), pop_inc[1], pop_inc[4])

# 2. Including those food items excluded from the optimization
fd.mat <- as.matrix(food_baseline_NSS[,c(2,4:9)])
fd.mat[is.na(fd.mat)] <- 0
Consumption_base_ICP <- data.frame(t(CES_ICP_IND) %*% fd.mat)   # $/yr in ICP categories
foodEperCapita <- colSums(Consumption_base_ICP * apply(IND_intensity, 2, mean)[1:151] / 1000, na.rm=TRUE) / 
  c(sum(pop_inc), sum(pop_inc), sum(pop_inc), pop_inc)
foodEperCapita.sd <- colSums(Consumption_base_ICP * apply(IND_intensity, 2, sd)[1:151] / 1000, na.rm=TRUE) / 
  c(sum(pop_inc), sum(pop_inc), sum(pop_inc), pop_inc)
write.table(foodEperCapita, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
write.table(foodEperCapita.sd, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)


# Survey total food consumption (USD 2007 MER)
# food_baseline_NSS <- IND_FD_code[1:150,2]
# compare_baseline_food <- cbind((IND_FD_ICP_usd2007)[1:45,1]*1e6 / scaler_IND, Consumption_ICP[1:45,1])
# compare_baseline_food[compare_baseline_food[,2]==0,]
