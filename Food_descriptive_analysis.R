###########################################
### Initialization + Import Parameters  ###
###########################################


setwd("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food")

# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_HH_All.Rda")    #IND_HH_Alldata
# load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_Food_All.Rda")  #IND_FOOD_Alldata
# load("./Saved tables/IND_intensities.Rda")


### Average weight per unit (10 items)
avg_wt <- selectDBdata(ITEM, AVG_WT, tables='IND_FOOD_AVG_WT')


### Indian Dietary Reference Intakes (DRI)
DRI <- read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/DRI-India.csv", header=TRUE)  # India specific
DRI <- reshape(DRI, idvar = "Group", timevar = "Nutrient", v.names="DRI", direction = "wide") %>% 
  mutate_at(vars(contains('DRI')), funs(yr=.*365))


### Consumption units
CU <- read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/cu_eq.csv", header=TRUE)  


### Average household size (in CU)
states <- read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/states.csv", header=TRUE) %>% rename(State=region)
a <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/NSS_food_diagnostics_VitA.xlsx", 2, startRow = 3 , endRow = 272) %>%
  left_join(states) %>% select(State, Urban..Rural, Male.Adults, Female.Adults, Male.minors, Female.minors, Total.Pop, zone)
hh_size <- a %>% mutate(cluster=paste0(zone, Urban..Rural)) %>% select(-zone, -Urban..Rural) %>% group_by(cluster) %>% 
  summarise(MA=weighted.mean(Male.Adults,Total.Pop), FA=weighted.mean(Female.Adults,Total.Pop),
            MM=weighted.mean(Male.minors,Total.Pop), FM=weighted.mean(Female.minors,Total.Pop)) 

# Average consumption unit by cluster and by hh member
cu <- data.frame(cluster= hh_size$cluster, as.matrix(hh_size[,-1]) %*% diag(CU$cu_eq))



########################
### Food information ###
########################


### Price data by food item (By-cluster)

# price is in $/kg (by 8 clusters (region-urbanity))
food_by_cluster <- read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/food_item_details-CU.csv", 
                            header=TRUE, stringsAsFactors = FALSE)[,-1] %>% filter(!is.na(energy))
n_cluster <- length(unique(food_by_cluster$cluster))   # 8 for now
# We run optimization for these 114 items (exclude all-NA nutrition items).
items_to_optimize <- food_by_cluster %>% filter(cluster=="E0") %>% select(item, code) %>% arrange(item)  
price_by_cls <- food_by_cluster %>% select(cluster, code, item, avg_price) %>% 
  spread(key=cluster, value = avg_price, sep='_p') %>% arrange(item) %>% select(-code)
names(price_by_cls)[-1] <- paste0("price_", unique(food_by_cluster$cluster))

# price is in $/kg (by 16 clusters (region-urbanity-income))
inc_grps <- c(1,2,3,4)
food_by_cls_inc <- read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/food_item_details-inc.csv", 
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
food_wgrp <- read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/NSS_food_items-VitA.xlsx", 1) %>% 
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

food_nutrients_org = read_excel('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/NSS_food_items-VitA.xlsx', sheet='NSS_food_items_values')
valid_column_names <- make.names(names=names(food_nutrients_org), unique=TRUE, allow_ = TRUE)
names(food_nutrients_org) <- valid_column_names

food_nutrients_org = food_nutrients_org %>% select(item, energy,protein, vita, iron, zinc) %>% right_join(food_wgrp) %>%
  mutate_cond(item=="Kharbooza", zinc=0) %>% arrange(item) %>% mutate_if(is.numeric, funs(.*10)) %>%  # nutrients per kg
  select(item, energy, protein, iron, zinc, vita)

food_nutrients_new = read_excel('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/NSS_food_items-VitA.xlsx', sheet='NSS_food_items_2017')
# food_nutrients_new = read_excel('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/NSS_food_items-VitA.xlsx', sheet='NSS_food_items_2017_rice')
valid_column_names <- make.names(names=names(food_nutrients_new), unique=TRUE, allow_ = TRUE)
names(food_nutrients_new) <- valid_column_names

food_nutrients_new = food_nutrients_new %>% select(item, energy,protein, vita, iron, zinc) %>% right_join(food_wgrp) %>%
  mutate_cond(item=="Kharbooza", zinc=0) %>% arrange(item) %>% mutate_if(is.numeric, funs(.*10))  %>%  # nutrients per kg
  select(item, energy, protein, iron, zinc, vita)



# All food item-specific information
food_summary <- food_nutrients_new %>% left_join(food_wgrp)
n_fooditem <- dim(food_nutrients_new)[1]   # 114 for now


###########################
### HH food consumption ###
###########################

### Cluster expenditure import (by 8 cluster)
exp_by_cls_org <- data.frame(t(read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/cluster_diets_exp-CU.csv", 
                                        header=TRUE, check.names = FALSE, stringsAsFactors = F)[,c(-1,-2)])) 
# exp_by_cls_org <- exp_by_cls_org %>% mutate(item=row.names(exp_by_cls_org)) %>% select(item, everything()) %>%
#   left_join(food_wgrp) 
# exp_by_cls <- exp_by_cls_org %>% filter(item %in% items_to_optimize$item) %>% select(item, group, everything()) 
# names(exp_by_cls)[-c(1,2)] <- paste0("expen_", unique(food_by_cluster$cluster))
exp_by_cls_org <- exp_by_cls_org %>% mutate(item=row.names(exp_by_cls_org)) %>% select(item, everything()) 
exp_by_cls <- exp_by_cls_org %>% filter(item %in% items_to_optimize$item) 
names(exp_by_cls)[-1] <- paste0("expen_", unique(food_by_cluster$cluster))
exp_by_cls[price_by_cls==0] <- 0

### Cluster expenditure import (by 16 cluster)
exp_by_cls_inc <- data.frame(t(read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/cluster_diets_exp-inc.csv", 
                                        header=TRUE, check.names = FALSE, stringsAsFactors = F)[,c(-1:-2)] %>% 
                                 filter(inc_grp %in% inc_grps) %>% select(-1) )) 
# exp_by_cls_inc <- exp_by_cls_inc %>% mutate(item=row.names(exp_by_cls_inc)) %>% select(item, everything()) %>%
#   left_join(food_wgrp) 
# exp_by_cls_inc <- exp_by_cls_inc %>% filter(item %in% items_to_optimize$item) %>% select(item, group, everything()) 
# names(exp_by_cls_inc)[-c(1,2)] <- paste0("expen_", unique(food_by_cls_inc$cls_inc))
exp_by_cls_inc <- exp_by_cls_inc %>% mutate(item=row.names(exp_by_cls_inc)) %>% select(item, everything()) 
exp_by_cls_inc <- exp_by_cls_inc %>% filter(item %in% items_to_optimize$item) 
names(exp_by_cls_inc)[-1] <- paste0("expen_", unique(food_by_cls_inc$cls_inc))
exp_by_cls_inc[price_by_cls_inc==0] <- 0

### kg consumption (by 8 cluster)
kg_by_cls <- exp_by_cls 
kg_by_cls[,-1] <- kg_by_cls[,-1] / select(price_by_cls, contains("price_"))
names(kg_by_cls)[-1] <- paste0("kg_", unique(food_by_cluster$cluster))
kg_by_cls[price_by_cls==0] <- 0

### kg consumption (by 16 cluster)
kg_by_cls_inc <- exp_by_cls_inc
kg_by_cls_inc[,-1] <- kg_by_cls_inc[,-1] / select(price_by_cls_inc, contains("price_"))
names(kg_by_cls_inc)[-1] <- paste0("kg_", unique(food_by_cls_inc$cls_inc))
kg_by_cls_inc[price_by_cls_inc==0] <- 0


### Combine all by-cluster information into one
kg_by_cls <- kg_by_cls %>% left_join(kg_by_cls_inc)
exp_by_cls <- exp_by_cls %>% left_join(exp_by_cls_inc)
# kg_by_cls[is.na(kg_by_cls)] <- 0
# kg_by_cls[is.infinite(kg_by_cls)] <- 0

# Aggregation by food group (for visualization (for now))
# kg_food_group <- kg_by_cls %>% left_join(food_group) %>% group_by(food_grp) %>% summarise_if(is.numeric, sum) %>% select(-code) %>%
#   melt(value.name = "kg")
# exp_food_group <- exp_by_cls %>% left_join(food_group) %>% group_by(food_grp) %>% summarise_if(is.numeric, sum) %>% select(-code) %>%
#   melt(value.name = "exp")
# 
# kg_food_wgroup <- kg_by_cls %>% group_by(group) %>% summarise_if(is.numeric, sum) %>% 
#   melt(value.name = "kg")
# 
# ggplot(kg_food_group, aes(food_grp, kg)) +   
#   geom_bar(aes(fill = variable), position = "dodge", stat="identity") + labs(x='Food group',y="Consumption [kg/yr]")
# ggplot(exp_food_group, aes(food_grp, exp)) +   
#   geom_bar(aes(fill = variable), position = "dodge", stat="identity") + labs(x='Food group',y ="Expenditure [$/yr]")

cal_base <- kg_by_cls %>% left_join(food_nutrients_new%>%select(item, energy)) %>% left_join(food_wgrp)
# calcalc <- cal_base %>%
#   mutate_at(vars(starts_with("kg_")), funs(.*energy)) %>% group_by(group) %>% summarise_if(is.numeric, sum) 

# calorie share by food group
# calshare <- cal_base %>% 
#   mutate(kcal=energy*kg_E0) %>% group_by(group) %>%
#   summarise(kcalsum=sum(kcal)) %>% mutate(share=kcalsum/sum(kcalsum))

### Deal with the negligible food items in each diet
# calculate calorie shares within SS-C group
# set shares as percentage integer to ignore <1% items
# If this pct==0, we mark them unavailable in the region.
# calcalc_ss <- cal_base %>% filter(group=="SS-C") %>%
#   mutate_at(vars(matches("_1|_4")), funs(cal=.*energy)) %>% select(item, ends_with("cal")) %>%
#   mutate_if(is.numeric, funs( round(./sum(.)*100)))   
calsum_grp <- cal_base %>% 
  mutate_at(vars(starts_with("kg")), funs("kcal"=energy*.)) %>% group_by(group) %>% select(item, group, ends_with("kcal")) %>%
  summarise_if(is.numeric, funs(sum=sum)) 
calpct_grp <- cal_base %>% 
  mutate_at(vars(starts_with("kg")), funs("kcal"=energy*.)) %>% group_by(group) %>% select(item, group, ends_with("kcal")) %>% 
  left_join(calsum_grp) %>% arrange(group) 
n_cls <- (dim(calpct_grp)[2]-2)/2
calpct_grp[,3:(n_cls+2)] <- round(calpct_grp[,3:(n_cls+2)] / calpct_grp[,(n_cls+3):length(calpct_grp)] *100)
calpct_grp <- calpct_grp %>% select(-ends_with("sum"))

 

# Both poor & rich population within the same zone end up with the same 'not available' food items.
# lowinc_col <- seq(grep("E0_2", names(calpct_grp)), length(calpct_grp), 2)
# hiinc_col <- lowinc_col+1

a <- data.frame( t(calpct_grp[,-1:-10]) )
a <- cbind(do.call("rbind", lapply(strsplit(substr(row.names(a), 4, 7), ""), '[', -3)), a) 
names(a)[1:3] <- c("state", "urban", "inc")
a <- a %>% group_by(state, urban) %>% summarise_all(mean)
a <- a[rep(seq(1,dim(a)[1]), each=4),]
a <- data.frame(t(a[,-1:-3]))
names(a) <- names(calpct_grp[,-1:-10])
calpct_grp[,-1:-10] <- a

# b <- (calpct_grp[,lowinc_col]+calpct_grp[,hiinc_col])/2
# calpct_grp[,lowinc_col] <- b
# calpct_grp[,hiinc_col] <- b
calpct_grp <- calpct_grp %>% arrange(item) %>% ungroup(group) %>% select(-group)

# Identify those items to ignore in the optimization
idx_nocon <- which(calpct_grp ==0, arr.ind = TRUE)

# replace price levels arbitrarily high to exclude them from getting assigned
# b <- price_by_cls_inc %>% left_join(food_wgrp) %>% arrange(group) %>% filter(group=="SS-C") %>% select(-group)
# b[idx_nocon] <- 1e6
# 
# price_by_cls_inc <- price_by_cls_inc %>% left_join(food_wgrp) %>% arrange(group) %>% filter(group!="SS-C") %>% select(-group) %>%
#   rbind(b)

price_by_cls <- price_by_cls %>% left_join(price_by_cls_inc)
ignore <- price_by_cls
ignore[,-1] <- 0
ignore[idx_nocon] <- 1
names(ignore)[-1] <- c(paste0("ign_", unique(food_by_cluster$cluster)), paste0("ign_", unique(food_by_cls_inc$cls_inc)))
View(ignore %>% left_join(food_wgrp) %>% select(item, group, everything()))

##########################
### Emission from food ###
##########################

### LCA EF by Tilman et al. (not used for now)

# emission <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/Food emission/nature13959-s1.xlsx", 1,
#                       startRow = 6, endRow = 560, header=FALSE, colClasses=c("character", "numeric", "numeric", "numeric"))
# names(emission) <- c("group", "per_gPRTN", "per_srv", "per_kcal")  # in gCeq
# emission[,2:4] <- emission[,2:4] #* 44/12
# emission <- emission %>% group_by(group) %>% summarise_all(funs(mean = mean, se=std.error))


### Non-CO2 EF from Hugo before Feb 2017 (wrong values)
ef_crop_old <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Emission_factors_ESM.xlsx", 1, startRow=2,
                colIndex = c(2,5), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X5)
ef_lvstock_old <- data.frame(xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Emission_factors_ESM.xlsx", 2,
                              colIndex = 8, header = FALSE, rowIndex=c(11:16), stringsAsFactors=FALSE),
                         xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Emission_factors_ESM.xlsx", 2,
                              colIndex = 5, header = FALSE, rowIndex=c(29:34))) %>% rename(item=X8, ef=X5)

### Non-CO2 EF from Hugo after Feb 2017 (corrected values)
ef_crop <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_25jan17.xlsx", 1, startRow=2,
                           colIndex = c(2,8), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X8)
ef_lvstock <- data.frame(xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_25jan17.xlsx", 2,
                                         colIndex = 11, header = FALSE, rowIndex=c(11:16), stringsAsFactors=FALSE),
                         xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_25jan17.xlsx", 2,
                                         colIndex = 6, header = FALSE, rowIndex=c(30:35)))
names(ef_lvstock) <- c("item", "ef")

ef_esm <- rbind.fill(ef_crop, ef_lvstock)
ef_esm <- ef_esm %>% mutate(ef = ifelse(grepl("Rice",item), ef+0.66, ef)) %>% arrange(item) %>% rename(item_esm=item, ef_nonco2=ef)


### EF mapping with extraction rate and edible rate
ef_all <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Emission_factors_mapping.xlsx", 1, colIndex=c(1:6),
                           stringsAsFactors=FALSE) %>% left_join(ef_esm) 
ef_all <- ef_all %>% mutate(ef_nonco2 = ifelse(grepl("Fish",item), 1.13, ef_nonco2))   # Based on Tilman et al. and 227g/serving (fish) http://www.health.state.mn.us/divs/eh/fish/eating/serving.html
ef_all <- ef_all %>% mutate(ef_per_kg_eaten=ef_nonco2/extraction/edible)  # CO2e per edible kg

food_summary <- food_summary %>% left_join(ef_all %>% select(item, extraction, edible, ef_per_kg_eaten))
price_by_cls_ed <- price_by_cls %>% left_join(ef_all %>% select(item, edible)) %>% 
  mutate_at(vars(starts_with("price")), funs(./edible))   # price per edible kg


### Emission data comparison (for Rice/Wheat)

# Rice/Wheat total consumption from NSS
# rice_wheat <- (IND_FOOD_Alldata %>% filter(str_detect(item, 'Rice') | str_detect(item, 'Wheat'))) %>% select(id, item, contains("tot")) %>%
#   rename(hhid=id)
# rice_wheat <- rice_wheat %>% left_join(IND_HH %>% select(hhid, weight)) %>% 
#   group_by(item) %>% summarise(tot_kg=sum(qty_tot*weight, na.rm=TRUE) / scaler_IND, tot_usd=sum(val_tot_org*weight, na.rm=TRUE)/ scaler_IND) %>%
#   mutate(tot_usd = tot_usd * PPP_IND / CPI_ratio_IND / EXR_IND) %>% mutate(price = tot_usd/tot_kg)
# 
# # Emission intensity from EXIO
# IND_em_int <- data.frame(indirect_em_int[,IND_idx_ex] * EXR_EUR$r / 1000) # kg/M.EUR to g/USD2007
# IND_rice_wheat <- IND_em_int[,c(1,2)] 
# names(IND_rice_wheat) <- c("Rice", "Wheat")  # Paddy rice and wheat. 
# row.names(IND_rice_wheat) <- GHG_item       # g/USD2007
# IND_rice_wheat[c(2,5),] <- IND_rice_wheat[c(2,5),] * 25  # Converting to CO2e/USD (CH4)
# IND_rice_wheat[c(3,6),] <- IND_rice_wheat[c(3,6),] * 298 # Converting to CO2e/USD (N2O)
# em_per_kg <- IND_rice_wheat %>% mutate(em_rc = Rice*rice_wheat$price[1], em_wh = Wheat*rice_wheat$price[3])  # g emission/kg grain
# colSums(em_per_kg)


### Derive effectiveness metrics

food_corr <- food_nutrients_new %>% left_join(food_avgs_national %>% select(item, avg_price, qty_tot)) %>% 
  left_join(ef_all %>% select(item, abbr, ef_per_kg_eaten, edible, group)) 
# mutate(usd_per_kg_eaten=avg_price/edible)  # $ per edible kg

nutri_cost <- food_corr %>% mutate(p_protein = protein/avg_price, p_iron = iron/avg_price,
                                   p_zinc = zinc/avg_price, p_vita  = vita/avg_price , p_cal = energy/avg_price,
                                   e_protein = protein/ef_per_kg_eaten, e_iron = iron/ef_per_kg_eaten,
                                   e_zinc = zinc/ef_per_kg_eaten, e_vita  = vita/ef_per_kg_eaten , e_cal = energy/ef_per_kg_eaten) %>%
  mutate(calintake = energy * qty_tot * edible/365)


################
### Plotting ###
################


### Plot cost & emission effectiveness of food items

nutri_cost <- nutri_cost %>% filter(group != "OTH") %>% left_join(food_avgs_national %>% select(item, avg_price, qty_tot))
gram <- nutri_cost %>% filter(grepl("Gram", item)) %>% mutate(abbr="Gram") %>% group_by(abbr) %>%
  summarise(avg_price=weighted.mean(avg_price, qty_tot), qty_tot=sum(qty_tot), calintake=sum(calintake)) #%>% summarise_if(is.numeric, first)
nutri_cost[grepl("Gram", nutri_cost$item), c("abbr", "avg_price", "qty_tot", "calintake")] <- gram
nutri_cost <- nutri_cost %>% mutate_cond(abbr=="Gram", 
                           p_protein = protein/avg_price, p_iron = iron/avg_price,
                           p_zinc = zinc/avg_price, p_vita = vita/avg_price , p_cal = energy/avg_price)
nutri_cost <- nutri_cost[!grepl("Gram, split|Gram, whole", nutri_cost$item),] 

calrange <- c(min((nutri_cost$calintake)), max((nutri_cost$calintake)))
n_slice <- 30

library("ggrepel")
library(colorRamps)

pl1 <- ggplot(data=nutri_cost %>% arrange(protein) %>% slice((n()-n_slice+1):n()), aes(x=1/p_protein, y=1000/e_protein)) +
  geom_point(size=3, aes(color=calintake)) + 
  scale_color_gradientn(colours=matlab.like2(200), trans = "log", 
                        name = "kCal/day", limits=calrange, breaks=c(0.1, 2.5, 50.0, 1000)) +
  # theme(legend.position="right") + 
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  labs(x='$/g Protein',y="gCO2e/g Protein") +
  geom_text_repel(data=nutri_cost %>% arrange(protein) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3)

pl2 <- ggplot(data=nutri_cost %>% arrange(zinc) %>% slice((n()-n_slice+1):n()), aes(x=1/p_zinc, y=1000/e_zinc)) +
  geom_point(size=3, aes(color=calintake))+ 
  scale_color_gradientn(colours=matlab.like2(200), trans = "log", 
                        name = "kCal/day", limits=calrange, breaks=c(0.1, 2.5, 50.0, 1000)) + 
  theme(legend.position="none") + 
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  labs(x='$/mg Zinc',y="gCO2e/mg Zinc")+
  geom_text_repel(data=nutri_cost %>% arrange(zinc) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3)

pl3 <- ggplot(data=nutri_cost %>% arrange(iron) %>% slice((n()-n_slice+1):n()), aes(x=1/p_iron, y=1000/e_iron))+
  geom_point(size=3, aes(color=calintake))+ 
  scale_color_gradientn(colours=matlab.like2(200), trans = "log", 
                        name = "kCal/day", limits=calrange, breaks=c(0.1, 2.5, 50.0, 1000)) + 
  theme(legend.position="none") +
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  labs(x='$/mg Iron',y="gCO2e/mg Iron")+
  geom_text_repel(data=nutri_cost %>% arrange(iron) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3)

pl4 <- ggplot(data=nutri_cost %>% arrange(vita) %>% slice((n()-n_slice+1):n()), aes(x=1/p_vita, y=1000/e_vita))+
  geom_point(size=3, aes(color=calintake))+ 
  scale_color_gradientn(colours=matlab.like2(200), trans = "log", 
                        name = "kCal/day", limits=calrange, breaks=c(0.1, 2.5, 50.0, 1000)) + 
  theme(legend.position="none") + 
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  labs(x='$/ug Vitamin A',y="gCO2e/ug Vitamin A")+
  geom_text_repel(data=nutri_cost %>% arrange(vita) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3)

pl5 <- ggplot(data=nutri_cost %>% arrange(energy) %>% slice((n()-n_slice+1):n()), aes(x=1/p_cal, y=1000/e_cal))+
  geom_point(size=3, aes(color=calintake))+ 
  scale_color_gradientn(colours=matlab.like2(200), trans = "log", 
                        name = "kCal/day", limits=calrange, breaks=c(0.1, 2.5, 50.0, 1000)) + 
  theme(legend.position="none") +
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  labs(x='$/kcal',y="gCO2e/kcal")+
  geom_text_repel(data=nutri_cost %>% arrange(energy) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3)

# grid.arrange(pl1, pl2, pl3, pl4, pl5, nrow=2, ncol=3)
pdf(file = "C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/Food item effectiveness - tot cal.pdf", width = 17, height = 10)
grid_arrange_shared_legend(pl1, pl2, pl3, pl4, pl5, nrow=2, ncol=3, position="right")
dev.off()



p1 <- ggplot(data=nutri_cost %>% arrange(protein) %>% slice((n()-n_slice+1):n()), aes(x=1/p_protein, y=1000/e_protein)) +
  geom_point(size=3, aes(color=protein))+ 
  scale_color_gradientn(colours=matlab.like2(200), name = "g Protein/kg food", breaks=c(150, 200, 250)) + 
  theme(legend.position="bottom") +
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  theme(axis.title = element_text(size = rel(1.5))) +
  labs(x='$/g Protein',y="gCO2e/g Protein")+
  geom_text_repel(data=nutri_cost %>% arrange(protein) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3.5)

p2 <- ggplot(data=nutri_cost %>% arrange(zinc) %>% slice((n()-n_slice+1):n()), aes(x=1/p_zinc, y=1000/e_zinc))+
  geom_point(size=3, aes(color=zinc))+ 
  scale_color_gradientn(colours=matlab.like2(200), name = "mg zinc/kg food", breaks=c(20, 40, 60)) + 
  theme(legend.position="bottom") + 
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  theme(axis.title = element_text(size = rel(1.5))) +
  labs(x='$/mg Zinc',y="gCO2e/mg Zinc")+
  geom_text_repel(data=nutri_cost %>% arrange(zinc) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3.5)

p3 <- ggplot(data=nutri_cost %>% arrange(iron) %>% slice((n()-n_slice+1):n()), aes(x=1/p_iron, y=1000/e_iron))+
  geom_point(size=3, aes(color=iron))+ 
  scale_color_gradientn(colours=matlab.like2(200), name = "mg iron/kg food", breaks=c(30, 60, 90)) + 
  theme(legend.position="bottom") +
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  theme(axis.title = element_text(size = rel(1.5))) +
  labs(x='$/mg Iron',y="gCO2e/mg Iron")+
  geom_text_repel(data=nutri_cost %>% arrange(iron) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3.5)

p4 <- ggplot(data=nutri_cost %>% arrange(vita) %>% slice((n()-n_slice+1):n()), aes(x=1/p_vita, y=1000/e_vita))+
  geom_point(size=3, aes(color=vita))+ 
  scale_color_gradientn(colours=matlab.like2(200), trans='log', name = "ug Vitamin A/kg food", breaks=c(250, 1000, 4000, 16000)) + 
  theme(legend.position="bottom") + 
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  theme(axis.title = element_text(size = rel(1.5))) +
  labs(x='$/ug Vitamin A',y="gCO2e/ug Vitamin A")+
  geom_text_repel(data=nutri_cost %>% arrange(vita) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3.5)

p5 <- ggplot(data=nutri_cost %>% arrange(energy) %>% slice((n()-n_slice+1):n()), aes(x=1/p_cal, y=1000/e_cal))+
  geom_point(size=3, aes(color=energy))+ 
  scale_color_gradientn(colours=matlab.like2(200), name = "kcal/kg food", breaks=c(4000, 5000, 6000)) + 
  theme(legend.position="bottom") +
  scale_x_continuous(labels = waiver(), trans = "log10") +
  scale_y_continuous(labels = waiver(), trans = "log10") +
  theme(axis.title = element_text(size = rel(1.5))) +
  labs(x='$/kcal',y="gCO2e/kcal")+
  geom_text_repel(data=nutri_cost %>% arrange(energy) %>% slice((n()-n_slice+1):n()), aes(label=abbr), size = 3.5)

pdf(file = "C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/Food item effectiveness - nutr content.pdf", width = 15, height = 10)
# grid.arrange(p1, p2, p3, p4, p5, nrow=2, ncol=3)
grid.arrange(p1, p2, p4, p3, nrow=2, ncol=2)
dev.off()



### Price summary plot by cluster
items_to_plot <- c("Rice, non-PDS", "Rice, PDS", "Wheat/atta, non-PDS", "Wheat/atta, PDS", "Milk, liquid", "Potato", "Coconut",
                    "Arhar, tur", "Jowar and its products", "Bajra and its products", "Maize and its products",
                    "Masur", "Moong", "Gram, split", "Gram, whole", "Banana", "Fish, prawn", "Chicken", "Beef/buffalo meat")
# items_to_plot <- c("whole wheat", "rice", "pulses & legumes", "milk products", "roots & tubers", "Meat and poultry", "leafy vegetables")
# price_plot <- food_avgs_grp %>% filter(item %in% itemss_to_plot) %>% mutate(cls=paste0(cluster,"_",inc_grp))

price_plot <- food_price_indiv %>% mutate(grouped = item) %>% left_join(food_wgrp) %>% 
  # mutate_cond(grepl("Rice", item), grouped="Rice") %>%
  # mutate_cond(grepl("Wheat", item), grouped="Wheat") %>%
  mutate_cond(grepl("Gram", item), grouped="Gram") %>%
  filter(item %in% items_to_plot) 
price_plot <- price_plot %>% group_by(id, grouped) %>% 
  summarise(price = weighted.mean(avg_price, qty_tot), item=first(item), group=first(group)) %>% 
  left_join(hh_sum%>% select(id, hh_size, cluster, weight, urban, inc_grp, region)) 

# Specify orders x-axis
# price_plot$item_fac <- factor(price_plot$grouped, 
#                               levels=c("Rice", "Wheat", "Milk, liquid", "Potato", "Coconut",
#                                        "Arhar, tur", "Jowar and its products", "Bajra and its products", "Maize and its products",
#                                        "Masur", "Gram", "Moong", "Banana", "Fish, prawn", "Chicken", "Beef/buffalo meat"),
#                               labels=c("Rice", "Wheat", "Milk", "Potato", "Coconut", "Arhar", "Jowar", 
#                                        "Bajra", "Maize", "Masur", "Gram", "Moong", "Banana", "Fish", "Chicken", "Beef"))
price_plot$item_fac <- factor(price_plot$grouped, 
                              levels=c("Rice, non-PDS", "Rice, PDS", "Wheat/atta, non-PDS", "Wheat/atta, PDS", "Milk, liquid", "Potato", "Coconut",
                                       "Arhar, tur", "Jowar and its products", "Bajra and its products", "Maize and its products",
                                       "Masur", "Gram", "Moong", "Banana", "Fish, prawn", "Chicken", "Beef/buffalo meat"),
                              labels=c("Rice, n-PDS", "Rice, PDS", "Wheat, n-PDS", "Wheat, PDS", "Milk", "Potato", "Coconut", "Arhar", "Jowar", 
                                       "Bajra", "Maize", "Masur", "Gram", "Moong", "Banana", "Fish", "Chicken", "Beef"))

pr_plot <- ggplot(price_plot, aes(x = item_fac, weight=weight, fill=group))
pr_plot <- pr_plot + geom_boxplot(aes(y = price), outlier.size=NA) + ylim(0, 12) +
  labs(x="Major food items", y="$ per kg (PPP)")
  # facet_grid(.~urban)
pr_plot
ggsave("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/Food price compare.pdf", 
       pr_plot, width=25, height=15, unit="cm")


### Total emission/cost summary plot by cluster
excluded <- c("te_min_khes", "tc_min_nobf", "tc_min", "te_min") # , "tc_min_cap"
p_tc <- ggplot(Cost_pl %>% filter(!scenario %in% excluded), aes(x = state, y=expenditure*100, shape=scenario)) 
p_tc <- p_tc + geom_point(size=3) +  #geom_point(size=3, aes(shape=inf)) +
  # scale_colour_manual(values = rainbow(9)) +
  scale_shape_manual(values = c(2, 0, 20, 3, 1)) +
  labs(x="state", y="% change from baseline", title="Expenditures on food") +
  facet_grid(urban~inc, labeller=label_both) 
  

p_te <- ggplot(Emission_pl%>% filter(!scenario %in% excluded), aes(x = state, y=kgCO2e*100, shape=scenario)) 
p_te <- p_te + geom_point(size=3) +  #geom_point(size=3, aes(shape=inf)) +
  # scale_colour_manual(values = rainbow(9)) +
  scale_shape_manual(values = c(2, 0, 20, 3, 1)) +
  ylim(-75, 15) +
  labs(x="state", y="% change from baseline", title="Non-CO2 emissions from food consumed") +
  facet_grid(urban~inc, labeller=label_both)

pdf(file = "C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/Optimization results - new EF and nutr 2017.pdf", width = 9, height = 11)
grid.arrange(p_tc, p_te, nrow=2, ncol=1)
dev.off()




### Top 10 cost effective food items (Tentative) - For the call with Ruth
# nutrient_cost <- food_summary %>% 
#   mutate_at(vars(starts_with("cluster")), funs("p_protein"=./protein, "p_iron"=./iron, "p_zinc"=./zinc, "p_vita"=./vita)) %>%
#   select(-code)
# 
# a1 <- nutrient_cost %>% arrange(cluster_p1_p_protein) %>% slice(1:10) 
# a1 <- transform(a1, item = reorder(item, cluster_p1_p_protein))
# a2 <- nutrient_cost %>% arrange(cluster_p1_p_iron) %>% slice(1:10) 
# a2 <- transform(a2, item = reorder(item, cluster_p1_p_iron))
# a3 <- nutrient_cost %>% arrange(cluster_p1_p_zinc) %>% slice(1:10) 
# a3 <- transform(a3, item = reorder(item, cluster_p1_p_zinc))
# a4 <- nutrient_cost %>% arrange(cluster_p1_p_vita) %>% slice(1:10) 
# a4 <- transform(a4, item = reorder(item, cluster_p1_p_vita))
# 
# p1 <- ggplot(a1, aes(x=item, y=1/cluster_p1_p_protein)) + geom_bar(stat="identity") + 
#   theme(axis.text.x=element_text(angle=45, size=10, hjust=1), axis.title.x=element_blank()) + ylab("g Protein per $ spent") 
# p2 <- ggplot(a2, aes(x=item, y=1/cluster_p1_p_iron)) + geom_bar(stat="identity") + 
#   theme(axis.text.x=element_text(angle=45, size=10, hjust=1), axis.title.x=element_blank()) + ylab("mg Iron per $ spent") 
# p3 <- ggplot(a3, aes(x=item, y=1/cluster_p1_p_zinc)) + geom_bar(stat="identity") + 
#   theme(axis.text.x=element_text(angle=45, size=10, hjust=1), axis.title.x=element_blank()) + ylab("mg Zinc per $ spent") 
# p4 <- ggplot(a4, aes(x=item, y=1/cluster_p1_p_vita)) + geom_bar(stat="identity") + 
#   theme(axis.text.x=element_text(angle=45, size=10, hjust=1), axis.title.x=element_blank()) + ylab("ug Vitamin A per $ spent") 
# grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)



### Function for grid legend

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}