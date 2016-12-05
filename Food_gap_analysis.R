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
            FM_tot=sum(female_minor*weight, na.rm=TRUE)) %>%
  filter(inc_grp %in% inc_grps)%>%
  mutate(clsname=paste0(cluster,"_",as.character(inc_grp))) %>%
  mutate(n_hh = FM_tot/FM_avg)

hh_size_cls <- hh_dem_cts %>% ungroup(cluster) %>% select(clsname, MA_avg, FA_avg, MM_avg, FM_avg)


### Get total
GetTOtalQuantities <- function() {
  
  n_cls <- length((scenarios)[[1]])
  n_scene <- length(scenarios)
  
  TotC <- matrix(, nrow = n_cls, ncol = 0)
  TotE <- matrix(, nrow = n_cls, ncol = 0)
  
  for (j in 1:n_scene) {
    params_opt = read_excel('C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/diet_gms/Parameter_outputs_inc - new nutr grp 2.xlsx', 
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

list[b,bb] <- GetTOtalQuantities()


### Nutritional info set

food_nutrients_org = read_excel('C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/NSS_food_items-VitA.xlsx', sheet='NSS_food_items')
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
