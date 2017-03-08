###########################################
### Initialization + Import Parameters  ###
###########################################

workdir <- "C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/"

# setwd("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food")

# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_HH_All.Rda")    #IND_HH_Alldata
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_Food_All.Rda")  #IND_FOOD_Alldata
# load("./Saved tables/IND_intensities.Rda")


### Average weight per unit (10 items)
avg_wt <- selectDBdata(ITEM, AVG_WT, tables='IND_FOOD_AVG_WT')


### Indian Dietary Reference Intakes (DRI)
DRI <- read.csv(paste0(workdir, "DRI-India.csv"), header=TRUE)  # India specific
DRI <- reshape(DRI, idvar = "Group", timevar = "Nutrient", v.names="DRI", direction = "wide") %>% 
  mutate_at(vars(contains('DRI')), funs(yr=.*365))


### Consumption units
CU <- read.csv(paste0(workdir, "cu_eq.csv"), header=TRUE)  


### Average household size (in CU)
states <- read.csv(paste0(workdir, "states.csv"), header=TRUE) %>% rename(State=region)
a <- xlsx::read.xlsx(paste0(workdir, "NSS_food_diagnostics_VitA.xlsx"), 2, startRow = 3 , endRow = 272) %>%
  left_join(states) %>% select(State, Urban..Rural, Male.Adults, Female.Adults, Male.minors, Female.minors, Total.Pop, zone)
hh_size <- a %>% mutate(cluster=paste0(zone, Urban..Rural)) %>% select(-zone, -Urban..Rural) %>% group_by(cluster) %>% 
  summarise(MA=weighted.mean(Male.Adults,Total.Pop), FA=weighted.mean(Female.Adults,Total.Pop),
            MM=weighted.mean(Male.minors,Total.Pop), FM=weighted.mean(Female.minors,Total.Pop)) 

# Average consumption unit by cluster and by hh member
cu <- data.frame(cluster= hh_size$cluster, as.matrix(hh_size[,-1]) %*% diag(CU$cu_eq))

# Income groups to analyze
inc_grps <- c(1,2,3,4)  # all groups


########################
### From NRao's Init ###
########################

cu_eqs=read.csv(paste0(workdir, "cu_eq.csv"))
states=read.csv(paste0(workdir, "states.csv"))
# valid_column_names <- make.names(names=names(food_nutrients), unique=TRUE, allow_ = TRUE)

hh_sum=selectDBdata(SURVEY, ID, HH_SIZE, REGION, AGE, SOCIAL_GROUP, RELIGION, MALE, MALE_ADULT, MALE_MINOR, EDUC_YEARS, MINOR, DWELL_STATUS, WORKER_TYPE, OCCUPATION, EXPENDITURE, WEIGHT, URBAN, tables='IND1_HH')
hh_sum=hh_sum%>%
  mutate(exp_percap = expenditure/hh_size)

#create income groups, which is used to join cluster assignments to items
urb_grp=hh_sum %>%
  filter(urban==1) %>%
  filter(is.finite(exp_percap)) %>%
  arrange(exp_percap) %>%
  mutate(inc_grp=cut(exp_percap,breaks=c(0,1.4*365,2.8*365,5.6*365,max(exp_percap)*365),labels=FALSE))

rur_grp=hh_sum %>%
  filter(urban==0) %>%
  filter(is.finite(exp_percap)) %>%
  arrange(exp_percap) %>%
  mutate(inc_grp=cut(exp_percap,breaks=c(0,0.95*365,1.9*365,3.8*365,max(exp_percap)*365),labels=FALSE))

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



########################
### Food information ###
########################


### Price data by food item (By-cluster)

# price is in $/kg (by 8 clusters (region-urbanity))
food_by_cluster <- read.csv(paste0(workdir, "food_item_details-CU.csv"), 
                            header=TRUE, stringsAsFactors = FALSE)[,-1] %>% filter(!is.na(energy))
n_cluster <- length(unique(food_by_cluster$cluster))   # 8 for now
# We run optimization for these 114 items (exclude all-NA nutrition items).
items_to_optimize <- food_by_cluster %>% filter(cluster=="E0") %>% select(item, code) %>% arrange(item)  
price_by_cls <- food_by_cluster %>% select(cluster, code, item, avg_price) %>% 
  spread(key=cluster, value = avg_price, sep='_p') %>% arrange(item) %>% select(-code)
names(price_by_cls)[-1] <- paste0("price_", unique(food_by_cluster$cluster))

# price is in $/kg (by 16 clusters (region-urbanity-income))
food_by_cls_inc <- read.csv(paste0(workdir, "food_item_details-inc.csv"), 
                            header=TRUE, stringsAsFactors = FALSE)[,-1] %>% filter(!is.na(energy)) %>% 
  mutate(cls_inc = paste0(cluster,"_",inc_grp)) %>% filter(inc_grp %in% inc_grps)    # Only top & bottom income group
n_cls_inc <- length(unique(food_by_cls_inc$cls_inc))  # 16 for now
price_by_cls_inc <- food_by_cls_inc %>% select(cls_inc, code, item, avg_price) %>% 
  spread(key=cls_inc, value = avg_price, sep='_p') %>% arrange(item) %>% select(-code)
names(price_by_cls_inc)[-1] <- paste0("price_", unique(food_by_cls_inc$cls_inc))

# How to treat zero or negligible items
# Set price to zero and set cnosumptions to zero
# And turn on the cell in the 'ignore' list
price_by_cls_inc[is.na(price_by_cls_inc)] <- 0   # could be INF but it appears that GAMS does not process Inf


### Define food groups (two tiers)
# 1. 20 or so food groups (less aggregate) by N. Rao
food_group <- food_by_cluster %>%  filter(cluster=="E0") %>% select(item, code, food_grp) %>% right_join(items_to_optimize)
food_group$food_grp[is.na(food_group$food_grp)] <- "etc"

# 2. Five 'wide' food groups
food_wgrp <- openxlsx::read.xlsx(paste0(workdir, "NSS_food_items-VitA.xlsx"), 1) %>% 
  select(item, group=food_grp_wdds) %>% arrange(item) %>% filter(item %in% items_to_optimize$item) %>%
  mutate(groupa=ifelse(is.na(group), "OTH", group)) %>%
  mutate(groupa=ifelse(groupa %in% c("OFV", "VAFV", "DLGV"), "FV", groupa)) %>%
  mutate(groupa=ifelse(groupa %in% c("LNS", "E"), "PRTN", groupa)) %>% select(-group) %>% rename(group=groupa)

# Fix some items (garlic, onion, ginger, radish)
food_wgrp <- food_wgrp %>% mutate(group=replace(group, item %in% c("Ginger", "Onion", "Garlic", "Singara", "Khesari"), "OTH")) %>%
  mutate(group=replace(group, item == "Radish", "FV")) %>%
  mutate(group=replace(group, item == "Groundnut", "PRTN"))

grp_names <- unique(food_wgrp$group)
group_map <- data.frame(food_wgrp, grp=matrix(0, ncol=length(grp_names), nrow=dim(food_wgrp)[1]))
for(i in 1:length(grp_names)) {
  group_map[,2+i] <- as.numeric(group_map$group==grp_names[i])
}
group_map <- group_map %>% select(-group)
names(group_map)[-1] <- grp_names

group_map_pt <- group_map %>% mutate(PRTN = PRTN+MF) %>% mutate(MF=0)
group_map_kh <- group_map_pt %>% mutate_cond(item=="Khesari", PRTN = 1, OTH = 0)


### Nutritional data by food item (By-cluster)

# Main scenario
food_nutrients_new = read_excel('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/NSS_food_items-VitA.xlsx',
                                sheet='NSS_food_items_2017')

# Sensitivity with better rice nutrition
# food_nutrients_new = read_excel('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/NSS_food_items-VitA.xlsx',
#                                 sheet='NSS_food_items_2017_rice')
valid_column_names <- make.names(names=names(food_nutrients_new), unique=TRUE, allow_ = TRUE)
names(food_nutrients_new) <- valid_column_names

food_nutrients_new = food_nutrients_new %>% select(item, energy,protein, vita, iron, zinc) %>% right_join(food_wgrp) %>%
  mutate_cond(item=="Kharbooza", zinc=0) %>% arrange(item) %>% mutate_if(is.numeric, funs(.*10))  %>%  # nutrients per kg
  select(item, energy, protein, iron, zinc, vita)

# All food item-specific information
food_summary <- food_nutrients_new %>% left_join(food_wgrp)
n_fooditem <- dim(food_nutrients_new)[1]   # 114 for now



#calculate avg prices and get energy intensity by cluster
food_items = selectDBdata(SURVEY, ID, ITEM, CODE, UNIT, QTY_TOT, VAL_TOT, tables='IND1_FOOD')
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
  mutate(avg_price=val_tot/qty_tot)%>%
  inner_join(hh_sum%>% select(survey, id, hh_size, cluster, weight, urban, inc_grp, region)) %>%
  group_by(item, code) %>%
  summarise(avg_price=weighted.mean(avg_price, qty_tot, na.rm=T), qty_tot=sum(weight * qty_tot, na.rm=T) / sum(hh_sum$weight)) %>%
  left_join(food_summary)

food_avgs_grp=food_items%>%
  filter(is.finite(qty_tot)) %>%
  mutate(avg_price=val_tot/qty_tot)%>%
  inner_join(hh_sum%>% select(survey, id, hh_size, cluster, weight, urban, inc_grp, region)) %>%
  left_join(food_group%>%select(-code)) %>%
  group_by(urban, cluster, inc_grp, food_grp, item) %>%
  summarise_each(.,funs(weighted.mean(., weight=qty_tot, na.rm=T)),avg_price) %>% filter(!is.na(food_grp))

food_price_indiv <- food_items%>%
  filter(is.finite(qty_tot)) %>%
  mutate(avg_price=val_tot/qty_tot)%>%
  inner_join(hh_sum%>% select(survey, id, hh_size, cluster, weight, urban, inc_grp, region))

