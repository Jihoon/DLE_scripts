sourcedir <- "H:/MyDocuments/IO work/DLE_scripts/"
source(paste0(sourcedir, "Food_init.R"))

runname <- "devmin100" #  "fert" #

###########################
### HH food consumption ###
###########################

### Cluster expenditure import (by 8 cluster)
exp_by_cls_org <- data.frame(t(read.csv(paste0(workdir, "cluster_diets_exp-CU.csv"), 
                                        header=TRUE, check.names = FALSE, stringsAsFactors = F)[,c(-1,-2)])) 
exp_by_cls_org <- exp_by_cls_org %>% mutate(item=row.names(exp_by_cls_org)) %>% select(item, everything()) 
exp_by_cls <- exp_by_cls_org %>% filter(item %in% items_to_optimize$item) 
names(exp_by_cls)[-1] <- paste0("expen_", unique(food_by_cluster$cluster))
exp_by_cls[price_by_cls==0] <- 0

### Cluster expenditure import (by 16 cluster)
exp_by_cls_inc <- data.frame(t(read.csv(paste0(workdir, "cluster_diets_exp-inc.csv"), 
                                        header=TRUE, check.names = FALSE, stringsAsFactors = F)[,c(-1:-2)] %>% 
                                 filter(inc_grp %in% inc_grps) %>% select(-1) )) 
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

a <- data.frame( t(calpct_grp[,-1:-10]) )
a <- cbind(do.call("rbind", lapply(strsplit(substr(row.names(a), 4, 7), ""), '[', -3)), a) 
names(a)[1:3] <- c("state", "urban", "inc")
a <- a %>% group_by(state, urban) %>% summarise_all(mean)
a <- a[rep(seq(1,dim(a)[1]), each=4),]
a <- data.frame(t(a[,-1:-3]))
names(a) <- names(calpct_grp[,-1:-10])
calpct_grp[,-1:-10] <- a


calpct_grp <- calpct_grp %>% arrange(item) %>% ungroup(group) %>% select(-group)

# Identify those items to ignore in the optimization
idx_nocon <- which(calpct_grp ==0, arr.ind = TRUE)

price_by_cls <- price_by_cls %>% left_join(price_by_cls_inc)
ignore <- price_by_cls
ignore[,-1] <- 0
ignore[idx_nocon] <- 1
names(ignore)[-1] <- c(paste0("ign_", unique(food_by_cluster$cluster)), paste0("ign_", unique(food_by_cls_inc$cls_inc)))
# View(ignore %>% left_join(food_wgrp) %>% select(item, group, everything()))

##########################
### Emission from food ###
##########################

### Non-CO2 EF from Hugo before Feb 2017 (wrong values)
ef_crop_old <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Emission_factors_ESM.xlsx", 1, startRow=2,
                colIndex = c(2,5), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X5)
ef_lvstock_old <- data.frame(xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Emission_factors_ESM.xlsx", 2,
                              colIndex = 8, header = FALSE, rowIndex=c(11:16), stringsAsFactors=FALSE),
                         xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Emission_factors_ESM.xlsx", 2,
                              colIndex = 5, header = FALSE, rowIndex=c(29:34))) %>% rename(item=X8, ef=X5)

### Non-CO2 EF from Hugo after Feb 2017 (corrected values) w/ new fertilizer values
# ef_crop <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_25jan17.xlsx", 1, startRow=2,
#                            colIndex = c(2,8), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X8)
ef_crop <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_25jan17.xlsx", 2, startRow=2,
                           colIndex = c(2,10), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X10)
# a <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_28feb17.xlsx", 3, startRow=2,
#                            colIndex = c(2,10), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X10)
ef_lvstock <- data.frame(xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_25jan17.xlsx", 3,
                                         colIndex = 11, header = FALSE, rowIndex=c(11:16), stringsAsFactors=FALSE),
                         xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_25jan17.xlsx", 3,
                                         colIndex = 6, header = FALSE, rowIndex=c(30:35)))
names(ef_lvstock) <- c("item", "ef")

rice_ch4 <- 1.0 # 0.66  kgCO2e/kg rice
ef_esm <- rbind.fill(ef_crop, ef_lvstock)
ef_esm <- ef_esm %>% mutate(ef = ifelse(grepl("paddy",item, ignore.case = TRUE), ef+rice_ch4, ef)) %>% arrange(item) %>% rename(item_esm=item, ef_nonco2=ef)


### EF mapping with extraction rate and edible rate w/ updated mapping (incorporating )
ef_all <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Emission_factors_mapping.xlsx", 2, colIndex=c(1:6),
                           stringsAsFactors=FALSE) %>% left_join(ef_esm) 
ef_all <- ef_all %>% mutate(ef_nonco2 = ifelse(grepl("Fish",item), 1.13, ef_nonco2))   # Based on Tilman et al. and 227g/serving (fish) http://www.health.state.mn.us/divs/eh/fish/eating/serving.html
ef_all <- ef_all %>% mutate(ef_per_kg_purchased=ef_nonco2/extraction, 
                            ef_per_kg_eaten=ef_nonco2/extraction/edible) %>% rename(ef_base = ef_nonco2)  # CO2e per edible kg

# write.csv(ef_all, paste0(workdir, "NSS_food_emission_factors.csv"))

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
pdf(file = paste0(workdir, "Figures/Food item effectiveness-tot cal-", runname, ".pdf"), width = 17, height = 10)
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

pdf(file = paste0(workdir, "Figures/Food item effectiveness-nutr content-", runname, ".pdf"), width = 15, height = 10)
grid.arrange(p1, p2, p3, p4, p5, nrow=2, ncol=3)
# grid.arrange(p1, p2, p4, p3, nrow=2, ncol=2)
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
rm(food_price_indiv)
gc()
# Specify orders x-axis
price_plot$item_fac <- factor(price_plot$grouped, 
                              levels=c("Rice, non-PDS", "Rice, PDS", "Wheat/atta, non-PDS", "Wheat/atta, PDS", "Milk, liquid", "Potato", "Coconut",
                                       "Arhar, tur", "Jowar and its products", "Bajra and its products", "Maize and its products",
                                       "Masur", "Gram", "Moong", "Banana", "Fish, prawn", "Chicken", "Beef/buffalo meat"),
                              labels=c("Rice,nPDS", "Rce,PDS", "Wheat,nPDS", "Wht,PDS", "Milk", "Potato", "Coconut", "Arhar", "Jowar", 
                                       "Bajra", "Maize", "Masur", "Gram", "Moong", "Banana", "Fish", "Chicken", "Beef"))

pr_plot <- ggplot(price_plot, aes(x = item_fac, weight=weight, fill=group))
pr_plot <- pr_plot + geom_boxplot(aes(y = price), outlier.size=NA) + ylim(0, 12) +
  labs(x="Major food items", y="$ per kg (PPP)")
  # facet_grid(.~urban)
pr_plot
ggsave(paste0(workdir, "Figures/Food price compare-", runname, ".pdf"), 
       pr_plot, width=25, height=15, unit="cm")


# compare rice/Wheat PDS price
rice_wheat <- selectDBdata(ID, ITEM, VAL_TOT, VAL_TOT, VAL_OWN, VAL_FREE, VAL_BAR, 
                           QTY_TOT, QTY_OWN, QTY_FREE, QTY_BAR, tables="IND1_FOOD") %>% 
  filter(grepl('Rice|Wheat', item) ) %>% 
  mutate(price_org = val_tot/qty_tot, price_adj = val_tot/qty_tot, 
         price_mkt = (val_tot - val_own - val_free - val_bar)/(qty_tot - qty_own - qty_free - qty_bar)) %>% 
  left_join(selectDBdata(ID, WEIGHT, tables='IND1_HH'))
rice_wheat_price <-  rice_wheat %>% group_by(item) %>% summarise(price_avg=weighted.mean(price_org, weight, na.rm=TRUE),
                                                                 price_mkt=weighted.mean(price_mkt, weight, na.rm=TRUE))

ggplot(rice_wheat %>% filter(grepl(' PDS', item)), aes(x = item, weight=weight)) + 
  geom_boxplot(aes(y = price_org)) + ylab("USD/kg")

