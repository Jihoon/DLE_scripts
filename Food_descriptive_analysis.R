########################
### Food information ###
########################

# Average weight per unit (10 items)
avg_wt <- selectDBdata(ITEM, AVG_WT, tables='IND_FOOD_AVG_WT')

# Indian Dietary Reference Intakes (DRI)
DRI <- read.csv("C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/DRI-India.csv", header=TRUE)  # India specific
DRI <- reshape(DRI, idvar = "Group", timevar = "Nutrient", v.names="DRI", direction = "wide") %>% 
  mutate_at(vars(contains('DRI')), funs(yr=.*365))


### By-cluster
# price in $/kg
food_by_cluster <- read.csv("C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/food_item_details_PCB083_1.csv", 
                            header=TRUE, stringsAsFactors = FALSE)[,-1] %>% filter(!is.na(energy))
n_cluster <- max(food_by_cluster$cluster)
# Run optimization for these 114 items (exclude all-NA nutrition items).
items_to_optimize <- food_by_cluster %>% filter(cluster==1) %>% select(item, code) %>% arrange(item)  
price_by_cls <- food_by_cluster %>% select(cluster, code, item, avg_price) %>% 
  spread(key=cluster, value = avg_price, sep='_p') %>% arrange(item)
food_group <- food_by_cluster %>%  filter(cluster==1) %>% select(item, code, food_grp) %>% right_join(items_to_optimize)
food_group$food_grp[is.na(food_group$food_grp)] <- "etc"

# Nutritional values
nutrients <- read.csv("C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/food_item_details_PCB083_1.csv", header=TRUE, 
                      stringsAsFactors = F)[,-1] %>%
  filter(!is.na(energy)) %>% filter(cluster==1) %>% select(item, code, en_int, energy, protein, iron, zinc, vita) %>%
  mutate_at(vars(energy, protein, iron, zinc, vita), funs(.*10)) %>%  # Nutritional values per kg
  arrange(item)

nutrients <- nutrients %>% left_join(food_wgrp)


### Five food groups
food_wgrp <- read.xlsx("C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/NSS_food_items-VitA.xlsx", 1) %>% 
  select(item, group=food_grp_wdds) %>% arrange(item) %>% filter(item %in% items_to_optimize$item) %>%
  mutate(groupa=ifelse(is.na(group), "OTH", group)) %>%
  mutate(groupa=ifelse(groupa %in% c("OFV", "VAFV", "DLGV"), "FV", groupa)) %>%
  mutate(groupa=ifelse(groupa %in% c("LNS", "E"), "PRTN", groupa)) %>% select(-group) %>% rename(group=groupa)

# Fix some items (garlic, onion, ginger)
food_wgrp <- food_wgrp %>% mutate(group=replace(group, item %in% c("Ginger", "Onion", "Garlic"), "OTH")) %>%
  mutate(group=replace(group, item == "Radish", "FV"))

grp_names <- unique(food_wgrp$group)
# grp_names <- grp_names[-4]  # Remove OTH for now
group_map <- data.frame(food_wgrp, grp=matrix(0, ncol=length(grp_names), nrow=dim(food_wgrp)[1]))
for(i in 1:length(grp_names)) {
  group_map[,2+i] <- as.numeric(group_map$group==grp_names[i])
}
group_map <- group_map %>% select(-group)
names(group_map)[-1] <- grp_names



# Effectiveness data

food_corr <- nutrients %>% left_join(price_by_cls %>% select(code, cluster_p6)) %>% mutate(MJ_per_kg = en_int*cluster_p6)

nutri_cost <- food_corr %>% mutate(p_protein = protein/cluster_p6, p_iron = iron/cluster_p6,
                                   p_zinc = zinc/cluster_p6, p_vita  = vita/cluster_p6 , p_cal = energy/cluster_p6,
                                   e_protein = protein/MJ_per_kg, e_iron = iron/MJ_per_kg,
                                   e_zinc = zinc/MJ_per_kg, e_vita  = vita/MJ_per_kg , e_cal = energy/MJ_per_kg)


##########################
### Emission from food ###
##########################

# LCA EF by Tilman et al. (not used for now)

emission <- xlsx::read.xlsx("C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/Food emission/nature13959-s1.xlsx", 1,
                      startRow = 6, endRow = 560, header=FALSE, colClasses=c("character", "numeric", "numeric", "numeric"))
names(emission) <- c("group", "per_gPRTN", "per_srv", "per_kcal")  # in gCeq
emission[,2:4] <- emission[,2:4] #* 44/12
emission <- emission %>% group_by(group) %>% summarise_all(funs(mean = mean, se=std.error))


### Emission data comparison (for Rice/Wheat)
rice_wheat <- (IND_FOOD_Alldata %>% filter(str_detect(item, 'Rice') | str_detect(item, 'Wheat'))) %>% select(id, item, contains("tot")) %>%
  rename(hhid=id)
rice_wheat <- rice_wheat %>% left_join(IND_HH %>% select(hhid, weight)) %>% 
  group_by(item) %>% summarise(tot_kg=sum(qty_tot*weight, na.rm=TRUE) / scaler_IND, tot_usd=sum(val_tot_org*weight, na.rm=TRUE)/ scaler_IND) %>%
  mutate(tot_usd = tot_usd * PPP_IND / CPI_ratio_IND / EXR_IND) %>% mutate(price = tot_usd/tot_kg)

IND_em_int <- data.frame(indirect_em_int[,IND_idx_ex] * EXR_EUR$r / 1000) # kg/M.EUR to g/USD2007
IND_rice_wheat <- IND_em_int[,c(1,2)] 
names(IND_rice_wheat) <- c("Rice", "Wheat")  # Paddy rice and wheat. 
row.names(IND_rice_wheat) <- GHG_item       # g/USD2007
IND_rice_wheat[c(2,5),] <- IND_rice_wheat[c(2,5),] * 25  # Converting to CO2e/USD (CH4)
IND_rice_wheat[c(3,6),] <- IND_rice_wheat[c(3,6),] * 298 # Converting to CO2e/USD (N2O)
em_per_kg <- IND_rice_wheat %>% mutate(em_rc = Rice*rice_wheat$price[1], em_wh = Wheat*rice_wheat$price[3])  # g emission/kg grain
colSums(em_per_kg)



################
### Plotting ###
################

# Plot cost/energy effectiveness

p1 <- ggplot(data=food_corr, aes(x=protein, y=cluster_p6, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='Protein g/kg',y="$/kg") +
  geom_text(aes(label=item),hjust=0, vjust=0, size=3)
p2 <- ggplot(data=food_corr, aes(x=zinc, y=cluster_p6, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='Zinc mg/kg',y="$/kg")
p3 <- ggplot(data=food_corr, aes(x=iron, y=cluster_p6, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='Iron mg/kg',y="$/kg")
p4 <- ggplot(data=food_corr, aes(x=vita, y=cluster_p6, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='VitA ug/kg',y="$/kg") 
p5 <- ggplot(data=food_corr, aes(x=protein, y=MJ_per_kg, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='Protein g/kg',y="MJ/kg") +
  geom_text(aes(label=item),hjust=0, vjust=0, size=3)
p6 <- ggplot(data=food_corr, aes(x=zinc, y=MJ_per_kg, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='Zinc g/kg',y="MJ/kg")
p7 <- ggplot(data=food_corr, aes(x=iron, y=MJ_per_kg, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='Iron g/kg',y="MJ/kg")
p8 <- ggplot(data=food_corr, aes(x=vita, y=MJ_per_kg, fill=group))+
  geom_point(aes(shape=group)) + labs(x='VitA g/kg',y="MJ/kg")
p9 <- ggplot(data=food_corr, aes(x=protein, y=energy, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='Protein g/kg',y="kcal/kg") +
  geom_text(aes(label=item),hjust=0, vjust=0, size=3)
p10 <- ggplot(data=food_corr, aes(x=zinc, y=energy, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='Zinc g/kg',y="kcal/kg")
p11 <- ggplot(data=food_corr, aes(x=iron, y=energy, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='Iron g/kg',y="kcal/kg")
p12 <- ggplot(data=food_corr, aes(x=vita, y=energy, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='VitA g/kg',y="kcal/kg")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, nrow=3, ncol=4)


nutri_cost <- nutri_cost %>% filter(!(item %in% c("Wheat/atta, PDS","Rice, PDS", "Sugar, PDS")))

pl1 <- ggplot(data=nutri_cost, aes(x=p_protein, y=e_protein, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='g Protein/$',y="g Protein/MJ")+
  geom_text(data=nutri_cost %>% filter(p_protein > 60), aes(label=item),hjust=1, vjust=0, size=3)

pl2 <- ggplot(data=nutri_cost, aes(x=p_zinc, y=e_zinc, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='mg Zinc/$',y="mg Zinc/MJ")+
  geom_text(data=nutri_cost %>% filter(p_zinc > 10), aes(label=item),hjust=1, vjust=0, size=3)

pl3 <- ggplot(data=nutri_cost, aes(x=p_iron, y=e_iron, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='mg Iron/$',y="mg Iron/MJ")+
  geom_text(data=nutri_cost %>% filter(p_iron > 30), aes(label=item),hjust=1, vjust=0, size=3)

pl4 <- ggplot(data=nutri_cost, aes(x=p_vita, y=e_vita, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='ug VitA/$',y="ug VitA/MJ")+
  geom_text(data=nutri_cost %>% filter(p_vita > 2500), aes(label=item),hjust=1, vjust=0, size=3) 

pl5 <- ggplot(data=nutri_cost, aes(x=p_cal, y=e_cal, fill=group))+
  geom_point(aes(shape=group)) + theme(legend.position="none") + labs(x='kcal/$',y="kcal/MJ")+
  geom_text(data=nutri_cost %>% filter(p_cal > 1500), aes(label=item),hjust=1, vjust=0, size=3)

grid.arrange(pl1, pl2, pl3, pl4, pl5, nrow=2, ncol=3)


