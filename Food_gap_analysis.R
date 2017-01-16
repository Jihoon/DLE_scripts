getcu= function(group) {
  x = cu_eqs %>%
    filter(group==Group) %>%
    select(cu_eq)
  return(as.numeric(x))
}

cu_eqs=read.csv("C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/cu_eq.csv")
states=read.csv("C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/states.csv")
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



### Get total cost and emission (default output from GAMS run)
GetTOtalQuantities <- function() {
  
  n_cls <- length((scenarios)[[1]])
  n_scene <- length(scenarios)
  
  TotC <- matrix(, nrow = n_cls, ncol = 0)
  TotE <- matrix(, nrow = n_cls, ncol = 0)
  
  for (j in 1:n_scene) {
    params_opt = read_excel('C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/diet_gms/Parameter_outputs_inc.xlsx', 
                            sheet=names(scenarios)[j])
    if (j==1) {
      TotC <- cbind(TotC, t(params_opt[c(1,2),-1]))
      TotE <- cbind(TotE, t(params_opt[c(3,4),-1]))
    }  else  {
      TotC <- cbind(TotC, t(params_opt[2,-1]))
      TotE <- cbind(TotE, t(params_opt[4,-1]))
    }
  }
  
  TotC <- data.frame(TotC, hh_dem_cts %>% ungroup() %>% select(n_hh))
  TotE <- data.frame(TotE, hh_dem_cts %>% ungroup() %>% select(n_hh))
  
  names(TotC) <- c("base", names(scenarios), "n_hh")
  names(TotE) <- c("base", names(scenarios), "n_hh")
  
  TotC <- TotC %>% mutate_at(1:(n_scene+1), funs(.*n_hh/1e6))  # M.USD/yr
  TotE <- TotE %>% mutate_at(1:(n_scene+1), funs(.*n_hh/1e6))  # kTon CO2e/yr
  
  row.names(TotC) <- names((scenarios)[[1]])
  row.names(TotE) <- names((scenarios)[[1]])
  
  return(list(TotC, TotE))
}

list[Cost, Emission] <- GetTOtalQuantities()  # M.USD/yr   &   kTon CO2e/yr


# Infeasible (manually put together..)
inf1 <- c("N1_1", "S1_1", "W0_1", "W1_1")             # "te_min" 
inf2 <- c("N0_1", "N1_1", "S1_1", "W0_1", "W1_1")    # "te_min_cap"
inf3 <- c("S1_1", "W0_1", "W1_1")    # "te_min_nogrp" & "te_min_khes"

# Gap total
tc_gap <- Cost %>% mutate_cond(row.names(.) %in% inf1, te_min=NA) %>% 
  mutate_cond(row.names(.) %in% inf2, te_min_cap=NA) %>% 
  mutate_cond(row.names(.) %in% inf3, te_min_nogrp=NA, te_min_khes=NA) %>% 
  mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs(.-base)) %>% 
  mutate_cond(is.na(te_min), te_min=te_min_cost) %>%
  mutate_cond(is.na(te_min_cap), te_min_cap=te_min_cost) %>% 
  mutate_cond(is.na(te_min_nogrp), te_min_nogrp=te_min_cost) %>% 
  mutate_cond(is.na(te_min_khes), te_min_khes=te_min_cost) %>% 
  rbind(colSums(.)) 
tc_gap[abs(tc_gap) < 1] <- 0
row.names(tc_gap) <- c(row.names(Cost), "tot")

te_gap <- Emission %>% mutate_cond(row.names(.) %in% inf1, te_min=NA) %>% 
  mutate_cond(row.names(.) %in% inf2, te_min_cap=NA) %>% 
  mutate_cond(row.names(.) %in% inf3, te_min_nogrp=NA, te_min_khes=NA) %>% 
  mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs(.-base)) %>% 
  mutate_cond(is.na(te_min), te_min=te_min_cost) %>%
  mutate_cond(is.na(te_min_cap), te_min_cap=te_min_cost) %>% 
  mutate_cond(is.na(te_min_nogrp), te_min_nogrp=te_min_cost) %>% 
  mutate_cond(is.na(te_min_khes), te_min_khes=te_min_cost) %>% 
  rbind(colSums(.))
te_gap[abs(te_gap) < 1] <- 0
row.names(te_gap) <- c(row.names(Cost), "tot")

# data frame for plotting
Cost_pl <- Cost %>% mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs((.-base)/base)) %>% 
  mutate(zone = row.names(Cost)) %>%
  mutate_cond(zone %in% inf1, te_min=te_min_cost) %>%
  mutate_cond(zone %in% inf2, te_min_cap=te_min_cost) %>%
  mutate_cond(zone %in% inf3, te_min_nogrp=te_min_cost, te_min_khes=te_min_cost) %>% 
  gather("scenario", "expenditure", 2:10) %>% arrange(zone) %>% mutate_cond(abs(expenditure) < 1e-10, expenditure=0) # USD/year
Cost_pl <- cbind(do.call("rbind", lapply(strsplit(Cost_pl$zone, ""), '[', -3)), Cost_pl) 
names(Cost_pl)[1:3] <- c("state", "urban", "inc")

Emission_pl <- Emission %>% mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs((.-base)/base)) %>% 
  mutate(zone = row.names(Emission)) %>%
  mutate_cond(zone %in% inf1, te_min=te_min_cost) %>%
  mutate_cond(zone %in% inf2, te_min_cap=te_min_cost) %>%
  mutate_cond(zone %in% inf3, te_min_nogrp=te_min_cost, te_min_khes=te_min_cost) %>% 
  gather("scenario", "kgCO2e", 2:10) %>% arrange(zone) %>% mutate_cond(abs(kgCO2e) < 1e-10, kgCO2e=0) # kgCO2e/year
Emission_pl <- cbind(do.call("rbind", lapply(strsplit(Emission_pl$zone, ""), '[', -3)), Emission_pl) 
names(Emission_pl)[1:3] <- c("state", "urban", "inc")

Infeasibles <- Cost_pl %>% select(zone, scenario) %>% mutate(inf="Feasible") %>%
  mutate_cond(zone %in% inf1 & scenario=="te_min", inf="Infeasible") %>%
  mutate_cond(zone %in% inf2 & scenario=="te_min_cap", inf="Infeasible") 

Cost_pl <- Cost_pl %>% left_join(Infeasibles)
Emission_pl <- Emission_pl %>% left_join(Infeasibles)

### Nutritional info set

food_nutrients_org = read_excel('C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/NSS_food_items-VitA.xlsx', sheet='NSS_food_items_values')
valid_column_names <- make.names(names=names(food_nutrients_org), unique=TRUE, allow_ = TRUE)
names(food_nutrients_org) <- valid_column_names

food_nutrients_org = food_nutrients_org %>% select(item, energy,protein, vita, iron, zinc) %>% right_join(food_wgrp) %>%
  mutate_cond(item=="Kharbooza", zinc=0) %>% arrange(item) %>% mutate_if(is.numeric, funs(.*10)) %>%  # nutrients per kg
  select(item, energy, protein, iron, zinc, vita)

food_nutrients_new = read_excel('C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/NSS_food_items-VitA.xlsx', sheet='NSS_food_items_new')
valid_column_names <- make.names(names=names(food_nutrients_new), unique=TRUE, allow_ = TRUE)
names(food_nutrients_new) <- valid_column_names

food_nutrients_new = food_nutrients_new %>% select(item, energy,protein, vita, iron, zinc) %>% right_join(food_wgrp) %>%
  mutate_cond(item=="Kharbooza", zinc=0) %>% arrange(item) %>% mutate_if(is.numeric, funs(.*10))  %>%  # nutrients per kg
  select(item, energy, protein, iron, zinc, vita)
### National sum



### Get total food kg consumptions
GetTOtalQuantities_kg <- function() {
  
  # n_fooditem <- dim(items_to_optimize)[1]
  n_scene <- length(scenarios)
  
  Tot_kg <- matrix(, nrow = n_fooditem, ncol = 0)
  hh_count <- rep(as.matrix(hh_dem_cts %>% ungroup() %>% select(n_hh)), each=2)
  
  for (j in 1:n_scene) {
    consum_opt = read_excel('C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/diet_gms/Kg_outputs_inc.xlsx', 
                            sheet=names(scenarios)[j])   # kg total per food item of a household
    
    if (j == 5) {  # To deal with those infeasible groups (<- replace them with the counterparts from "te_min_cost" scenario)
      replace_inf <- read_excel('C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/diet_gms/Kg_outputs_inc.xlsx', 
                                sheet="te_min_cost")   # kg total per food item of a household
      
      vars <- paste0(inf2, "kg_opt")
      consum_opt[,vars] <- replace_inf[,vars]
    }
    
    kg <- consum_opt[,-1]
    sum_kg <- sweep(kg, 2, hh_count, '*')
    base_tot <- rowSums(sum_kg[,seq(1, ncol(sum_kg), by = 2)])        # national total kg at the baseline (survey)
    opt_tot <- rowSums(sum_kg[,seq(2, ncol(sum_kg), by = 2)])         # national total kg optimized 
    opt_tot_low_inc <- rowSums(sum_kg[,seq(2, ncol(sum_kg), by = 8)])  # total kg for lowest income group
    opt_tot_hi_inc <- rowSums(sum_kg[,seq(8, ncol(sum_kg), by = 8)])   # total kg for highest income group
    
    tot <- data.frame(base_tot, opt_tot, opt_tot_low_inc, opt_tot_hi_inc)
    names(tot) <- paste0(names(scenarios)[j], "_", c("base", "opt_tot", "opt_1", "opt_4"))
    
    Tot_kg <- cbind(Tot_kg, tot)
  }
  
  return(cbind(food_wgrp,Tot_kg))
}

Consumption_kg <- GetTOtalQuantities_kg()  # kg/yr in NSS categories
Consumption_kg <- Consumption_kg %>% left_join(food_group) %>% select(-food_grp) %>% arrange(code) %>% 
  left_join(food_avgs_national %>% select(code, avg_price))
Consumption_price <- Consumption_kg %>% mutate_at(vars(contains("min")), funs(.*avg_price))

a<-data.frame(code=as.numeric(row.names(CES_ICP_IND))) %>% left_join(Consumption_price) 
a[is.na(a)] <- 0
a.mat <- as.matrix(a %>% select(-item, -group, -code))
Consumption_ICP <- data.frame(t(CES_ICP_IND) %*% a.mat)   # $/yr in ICP categories
Consumption_ICP <- Consumption_ICP %>% select(starts_with("te_min_cap"))
colSums(Consumption_ICP * apply(IND_intensity, 2, mean)[1:151] / 1000) / c(IND_pop_2007, IND_pop_2007, b$n_hh[1]*4.5, b$n_hh[4]*4.5)

b <- hh_dem_cts %>% group_by(inc_grp) %>% summarise(n_hh = sum(n_hh))
