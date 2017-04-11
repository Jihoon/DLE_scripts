# RDA table
write.table(dris %>% spread(key = Nutrient, value = DRI), "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

# Food item summary
library(Hmisc)
a <- food_summary %>% select(-extraction) %>% select(item, group, everything()) %>% select(-edible, everything())  %>% arrange(group)
names(a) <- capitalize(names(a))
names(a)[3:9] <- c("Calorie (kcal/kg)",	"Protein (g/kg)",	"Iron (mg/kg)",	"Zinc (mg/kg)",	
                   "Vitamin A (μg/kg)", "Emission factor (kgCO2e/kg)", "%kg Edible")
write.table(a, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

# Get the population pct for the infeasible population
a<-hh_anal_out %>% group_by(zone, urban, inc_grp) %>% 
  summarise(pop = sum(total_pop_excl_outeaters)) %>% 
  mutate(pct = pop/sum(a$pop)) %>% 
  mutate(zn = paste0(zone, urban, '_', inc_grp)) %>% filter(zn %in% zone_infsbl[[1]])
sum(a$pop)
sum(a$pct)

a<-hh_sum %>%  # hh_sum from Food_init.R
  # select(cluster,cu_eq_MA,cu_eq_FA,cu_eq_MM,cu_eq_FM, weight)%>%
  group_by(cluster, inc_grp) %>%
  summarise(totexp=sum(expenditure*weight, na.rm=TRUE)/1e6, hhtotexp=weighted.mean(expenditure, weight, na.rm=TRUE)) %>% 
  mutate(clsname=paste0(cluster,"_",as.character(inc_grp))) %>% ungroup() %>% 
  select(clsname, everything()) %>% select(-cluster, -inc_grp)
tot.pop <- hh_anal_out %>% ungroup() %>% mutate(clsname = paste0(as.character(zone), urban, '_', inc_grp)) %>% 
  group_by(clsname) %>% summarise(pop = sum(total_pop_excl_outeaters))
a <- a %>%
  right_join(data.frame(clsname=row.names(tc_gap), tc_gap)) %>% rename(totfoodexp_base = base) %>% 
  mutate(hhfoodexp_base = totfoodexp_base/n_hh*1e6) %>% select(clsname, totexp, hhtotexp, totfoodexp_base, hhfoodexp_base, n_hh, everything()) %>%
  mutate_cond(is.na(totexp), totexp = sum(a$totexp, na.rm=TRUE), hhtotexp=totexp/n_hh*1e6) %>% left_join(tot.pop)


# Get calorie share among cereals after optimization
kgfile <- paste0('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/Results/kg_outputs-', runname, '.xlsx')
kg_base <- read_excel(kgfile, sheet="te_min_cap") %>% select(1, seq(2, length(.), 2))
kg_opt <- read_excel(kgfile, sheet="te_min_cap") %>% select(seq(1, length(.), 2))
kg_devmin <- read_excel(kgfile, sheet="dev_min") %>% select(seq(1, length(.), 2))
kg_optcost <- read_excel(kgfile, sheet="tc_min_cap") %>% select(seq(1, length(.), 2))

tot_kg_base <- data.frame(item=kg_base$item, kg=rowSums(as.matrix(kg_base[,-1]) %*% diag(tc_gap$n_hh[1:32]/tc_gap$n_hh[33])))%>% 
  left_join(food_summary %>% select(item, group, energy)) %>% mutate(kcal=kg*energy)
tot_kg_opt <- data.frame(item=kg_opt$item, kg=rowSums(as.matrix(kg_opt[,-1]) %*% diag(tc_gap$n_hh[1:32]/tc_gap$n_hh[33]))) %>% 
  left_join(food_summary %>% select(item, group, energy)) %>% mutate(kcal=kg*energy)
tot_kg_devmin <- data.frame(item=kg_devmin$item, kg=rowSums(as.matrix(kg_devmin[,-1]) %*% diag(tc_gap$n_hh[1:32]/tc_gap$n_hh[33]))) %>% 
  left_join(food_summary %>% select(item, group, energy)) %>% mutate(kcal=kg*energy)
tot_kg_mincost <- data.frame(item=kg_optcost$item, kg=rowSums(as.matrix(kg_optcost[,-1]) %*% diag(tc_gap$n_hh[1:32]/tc_gap$n_hh[33]))) %>% 
  left_join(food_summary %>% select(item, group, energy)) %>% mutate(kcal=kg*energy)

sum(tot_kg_base$kcal)
sum(tot_kg_opt$kcal)
sum(tot_kg_devmin$kcal)
sum(tot_kg_mincost$kcal)

kcalshare_base <- tot_kg_base %>% group_by(group) %>% mutate(kcalpct=kcal/sum(kcal)) %>% arrange(group) %>% 
  filter(group=="SS-C") %>% select(item, kcalpct) %>% 
  mutate(type="Baseline") %>% arrange(kcalpct) %>% slice(11:20) 
kcalshare_opt <- tot_kg_opt %>% group_by(group) %>% mutate(kcalpct=kcal/sum(kcal)) %>% arrange(group) %>% 
  filter(group=="SS-C") %>% select(item, kcalpct) %>% 
  mutate(type="Reference") %>% arrange(kcalpct) %>% filter(item %in% kcalshare_base$item) 
kcalshare_devmin <- tot_kg_devmin %>% group_by(group) %>% mutate(kcalpct=kcal/sum(kcal)) %>% arrange(group) %>% 
  filter(group=="SS-C") %>% select(item, kcalpct) %>% 
  mutate(type="DevMin") %>% arrange(kcalpct) %>% filter(item %in% kcalshare_base$item) 
kcalshare_mincost <- tot_kg_mincost %>% group_by(group) %>% mutate(kcalpct=kcal/sum(kcal)) %>% arrange(group) %>% 
  filter(group=="SS-C") %>% select(item, kcalpct) %>% 
  mutate(type="MinCost") %>% arrange(kcalpct) %>% filter(item %in% kcalshare_base$item) 


# Group calorie shares and total emission (kTon CO2e/yr) in each zone
tot_cal_base <- kg_base %>% left_join(food_summary %>% select(item, group, energy)) %>% mutate_at(vars(matches("_base")), funs(cal=.*energy)) %>%
  group_by(group) %>% summarise_at(vars(matches("_cal")), sum) %>% mutate_if(is.numeric, funs(./sum(.)))
group_name <- tot_cal_base$group
tot_cal_base <- data.frame(t(tot_cal_base[-1]))
names(tot_cal_base) <- group_name
table.out <- for_shannon %>% select(-shannon) %>% arrange(zone, urban, inc_grp) %>% ungroup() %>% 
  cbind(tot_cal_base, 'kgCO2e/cap'=Emission$base*1e6/tot.pop$pop) %>% arrange(zone, inc_grp, urban) 
write.table(table.out, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

# kcalshare_base <- kcalshare_base %>% filter(group=="SS-C") %>% select(item, kcalpct) %>% 
#   mutate(type="Baseline") %>% arrange(kcalpct) %>% slice(11:20) 
# kcalshare_opt <- kcalshare_opt %>% filter(group=="SS-C") %>% select(item, kcalpct) %>% 
#   mutate(type="Reference") %>% arrange(kcalpct) %>% filter(item %in% kcalshare_base$item) 
# kcalshare_devmin <- kcalshare_opt %>% filter(group=="SS-C") %>% select(item, kcalpct) %>% 
#   mutate(type="Reference") %>% arrange(kcalpct) %>% filter(item %in% kcalshare_base$item)
# kcalshare_mincost <- kcalshare_opt %>% filter(group=="SS-C") %>% select(item, kcalpct) %>% 
#   mutate(type="Reference") %>% arrange(kcalpct) %>% filter(item %in% kcalshare_base$item)


ss_calshare <- rbind(kcalshare_base, kcalshare_opt, kcalshare_devmin, kcalshare_mincost) %>% 
  mutate_cond(item=="Rice, non-PDS" | item=="Rice, PDS", item="Rice")%>% 
  mutate_cond(item=="Wheat/atta, non-PDS" | item=="Wheat/atta, PDS", item="Wheat")

ss_calshare$item_fac <- factor(ss_calshare$item, 
                              levels=c("Rice", "Wheat", "Potato", "Jowar and its products", 
                                       "Bajra and its products", "Maize and its products", "Suji, rawa", "Ragi and its products"),
                              labels=c("Rice", "Wheat", "Potato", "Jowar", "Bajra", "Maize", "Suji", "Ragi"))
ss_calshare$Scenario <- factor(ss_calshare$Scenario,
                               levels=c("Baseline", "Reference", "MinCost", "DevMin"),
                               ordered=FALSE)
ss_calshare <- ss_calshare %>% group_by(item_fac, type) %>% summarise(kcalpct = sum(kcalpct)) %>% arrange(type, item_fac) %>% rename(Scenario = type)

pdf(file = paste0(workdir, "Figures/Calorie share from cereal-", runname, ".pdf"), width = 9, height = 7)
ggplot(ss_calshare, aes(item_fac, kcalpct, fill=Scenario)) +
  geom_bar(stat="identity", position="dodge") + xlab("Major cereal") + ylab("Calorie share") +
  # scale_fill_brewer(palette="Greys")
  scale_fill_manual(values=c('#252525','#bdbdbd','#969696','#636363'))
  # colorRampPalette(brewer.pal(9, "Greys"))(4)
dev.off()
  

# Region-state set
# Region_state <- zones %>% rename(State=region, Region=zone) %>% arrange(Region) %>% group_by(Region) %>% 
#   mutate(num=paste0("name" , str_pad(row_number(), 2, pad="0"))) %>%
#   select(num, Region, State) %>% spread(num, State) %>% mutate(States=paste())

Region_state <- zones %>% rename(State=region, Region=zone) %>% arrange(Region, State) %>% group_by(Region) %>% summarise(vector=paste(State, collapse=", "))
write.table(Region_state, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

Shannon <- for_shannon %>% mutate(urban=factor(urban, labels=c("R", "U"))) %>% 
  rename(Region=zone, "Income Group"=inc_grp, 'Urban/Rural'=urban, 'Shannon\'s Index'=shannon) 
write.table(Shannon, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

# kg total for dev_min
kg_devmin <- read_excel(kgfile, sheet="dev_min") %>% select(seq(1, length(.), 2))
tot_kg_devmin <- data.frame(item=kg_devmin$item, kg=rowSums(as.matrix(kg_devmin[,-1]) %*% diag(tc_gap$n_hh[1:32]/tc_gap$n_hh[33]))) %>% 
  left_join(food_summary %>% select(item, group, energy)) %>% mutate(kcal=kg*energy)
diff_devmin <- cbind(tot_kg_base %>% select(item, kg, group), tot_kg_devmin %>% select(kg) %>% rename(kg_devmin=kg)) %>%
  mutate(kg_diff = kg-kg_devmin, pct_diff = kg_diff/kg*100) %>% left_join(food_summary %>% select(-extraction, -edible, -ef_per_kg_eaten)) %>% 
  left_join(price_by_cls_inc %>% cbind(avgprc=rowMeans(price_by_cls_inc %>% select(-item))) %>% select(item, avgprc))
