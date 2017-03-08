# RDA table
write.table(dris %>% spread(key = Nutrient, value = DRI), "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

# Food item summary
library(Hmisc)
a <- food_summary %>% select(-extraction, -edible) %>% select(item, group, everything()) %>% arrange(group)
names(a) <- capitalize(names(a))
names(a)[7:8] <- c("Vitamin A", "Emission factor")
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
a <- a %>%
  right_join(data.frame(clsname=row.names(tc_gap), tc_gap)) %>% rename(totfoodexp_base = base) %>% 
  mutate(hhfoodexp_base = totfoodexp_base/n_hh*1e6) %>% select(clsname, totexp, hhtotexp, totfoodexp_base, hhfoodexp_base, n_hh, everything()) %>%
  mutate_cond(is.na(totexp), totexp = sum(a$totexp, na.rm=TRUE), hhtotexp=totexp/n_hh*1e6) 


# Get calorie share among cereals after optimization
kgfile <- paste0('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/Results/kg_outputs-', runname, '.xlsx')
kg_opt <- read_excel(kgfile, sheet="te_min_cap") %>% select(seq(1, length(.), 2))
kg_base <- read_excel(kgfile, sheet="te_min_cap") %>% select(1, seq(2, length(.), 2))
tot_kg_opt <- data.frame(item=kg_opt$item, kg=rowSums(as.matrix(kg_opt[,-1]) %*% diag(tc_gap$n_hh[1:32]/tc_gap$n_hh[33]))) %>% 
  left_join(food_summary %>% select(item, group, energy)) %>% mutate(kcal=kg*energy)
tot_kg_base <- data.frame(item=kg_base$item, kg=rowSums(as.matrix(kg_base[,-1]) %*% diag(tc_gap$n_hh[1:32]/tc_gap$n_hh[33])))%>% 
  left_join(food_summary %>% select(item, group, energy)) %>% mutate(kcal=kg*energy)
sum(tot_kg_opt$kcal)
sum(tot_kg_base$kcal)
kcalshare_opt <- tot_kg_opt %>% group_by(group) %>% mutate(kcalpct=kcal/sum(kcal)) %>% arrange(group)
kcalshare_base <- tot_kg_base %>% group_by(group) %>% mutate(kcalpct=kcal/sum(kcal)) %>% arrange(group)

kcalshare_base <- kcalshare_base %>% filter(group=="SS-C") %>% select(item, kcalpct) %>% 
  mutate(type="Baseline") %>% arrange(kcalpct) %>% slice(11:20) 
kcalshare_opt <- kcalshare_opt %>% filter(group=="SS-C") %>% select(item, kcalpct) %>% 
  mutate(type="Reference") %>% arrange(kcalpct) %>% filter(item %in% kcalshare_base$item) 

ss_calshare <- rbind(kcalshare_base, kcalshare_opt) %>% 
  mutate_cond(item=="Rice, non-PDS" | item=="Rice, PDS", item="Rice")%>% 
  mutate_cond(item=="Wheat/atta, non-PDS" | item=="Wheat/atta, PDS", item="Wheat")

ss_calshare$item_fac <- factor(ss_calshare$item, 
                              levels=c("Rice", "Wheat", "Potato", "Jowar and its products", 
                                       "Bajra and its products", "Maize and its products", "Suji, rawa", "Ragi and its products"),
                              labels=c("Rice", "Wheat", "Potato", "Jowar", "Bajra", "Maize", "Suji", "Ragi"))

ss_calshare <- ss_calshare %>% group_by(item_fac, type) %>% summarise(kcalpct = sum(kcalpct)) %>% arrange(type, item_fac) %>% rename(Scenario = type)

pdf(file = paste0(workdir, "Figures/Calorie share from cereal-", runname, ".pdf"), width = 9, height = 7)
ggplot(ss_calshare, aes(item_fac, kcalpct, fill=Scenario)) +
  geom_bar(stat="identity", position="dodge") + xlab("Major cereal") + ylab("Calorie share")
dev.off()
  

