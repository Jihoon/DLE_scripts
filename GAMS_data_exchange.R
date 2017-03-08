igdx("C:/GAMS/win64/24.4")


### Run Food_descriptive_analysis.R before this!!


#########################
### Status quo review ###
#########################

# status_quo <- food_summary %>% mutate(consumption=kg_by_cls$exp_cls6) %>%
#   mutate_at(vars(energy, protein, iron, zinc, vita), funs(yr=.*consumption))


########################
### Run optimization ###
########################

work_path <- "C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/"
# work_path <- "C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/diet_gms/"

### Debug note: An issue with the execution of RunFoodOpt in series
# !!! This still problem remains to be resolved. !!!
# Potential reasons - I haven't yet figured out the specific reasons. 
#           It appears that the gdxrrw package is not fully tested 
#           or the GAMS command line runs are not for consecutive runs
#           or there is a mismatch in running time in GAMS and R's serial calls.
# Symptom - This run frequently and inconsistently spits out flag=3 or 6 for certain zones, for which cases I had to run the same line multiple times until it executes without that problem.
# Remedy - The delay functions before or after the gams() run may help, but not 100% sure.
#          Another temporary remedy I am using is to set up a break point when those flags are detected, re-run the problematic zones when it stops at the break points, and continue (with the next() commented out temporarily).
# Note - I've contacted the developer, who said it is just a wrapper and we need to rely on GAMS documentation for any problems.

scenarios <- list()
nutr <- food_nutrients_new  # food_nutrients_org
scenarios[[1]] <- RunFoodOpt("tc_min", cluster="reg-urb-inc", nutr)   # min_totcost w/ nutri by reg-urb-inc
scenarios[[2]] <- RunFoodOpt("tc_min_cap", cluster="reg-urb-inc", nutr)   # min_totcost w/ nutri by reg-urb-inc
scenarios[[3]] <- RunFoodOpt("tc_min_nobf", cluster="reg-urb-inc", nutr)   # min_totcost w/ nutri by reg-urb-inc (no beef)

scenarios[[4]] <- RunFoodOpt("te_min", cluster="reg-urb-inc", nutr)   # min_totcost w/ nutri by reg-urb-inc
scenarios[[5]] <- RunFoodOpt("te_min_cap", cluster="reg-urb-inc", nutr)   # min_totcost w/ nutri by reg-urb-inc
scenarios[[6]] <- RunFoodOpt("te_min_pds", cluster="reg-urb-inc", nutr)   # min_totcost w/ nutri by reg-urb-inc (no PDS vs non-PDS fixed r)
scenarios[[7]] <- RunFoodOpt("te_min_cost", cluster="reg-urb-inc", nutr)   # min_totcost w/ nutri by reg-urb-inc (no cost capping)
scenarios[[8]] <- RunFoodOpt("te_min_nogrp", cluster="reg-urb-inc", nutr)   # min_totcost w/ nutri by reg-urb-inc (trade between meat and prtn)
scenarios[[9]] <- RunFoodOpt("dev_min", cluster="reg-urb-inc", nutr)   # min consumption (kg) deviation
names(scenarios) <- c("tc_min", "tc_min_cap", "tc_min_nobf", "te_min", "te_min_cap",
                      "te_min_pds", "te_min_cost", "te_min_nogrp", "dev_min")
# scenarios[[9]] <- RunFoodOpt("te_min_khes", cluster="reg-urb-inc", nutr)   # min_totcost w/ nutri by reg-urb-inc (allow khesari)
# names(scenarios) <- c("tc_min", "tc_min_cap", "tc_min_nobf", "te_min", "te_min_cap", 
#                       "te_min_pds", "te_min_cost", "te_min_nogrp", "te_min_khes")

scenarios_main_devmin100 <- scenarios
OrganizeOptOutputs(scenarios)
OrganizeOptOutputs(scenarios_rice_sens)
OrganizeOptOutputs(scenarios_main)
OrganizeOptOutputs(scenarios_main_devmin)

save(scenarios_main, file="C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/scenarios_main.Rda")
save(scenarios_main_prc, file="C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/scenarios_main_prc.Rda")
save(scenarios_main_frt, file="C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/scenarios_main_frt.Rda")
save(scenarios_rice_sens, file="C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/scenarios_rice_sens.Rda")
save(scenarios_main_devmin100, file="C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/scenarios_main_devmin100.Rda")

# Test purpose (certain scenario and certain zone)
WriteOptGDX("dev_min", cluster="reg-urb-inc", zone="E0_2", nutr)


# Identify contributions to cost/emission decrease
a <- l_consum[[2]] %>% left_join(price_by_cls) %>% left_join(ef_all %>% select(item, ef_per_kg_eaten))
sav_contribution <- data.frame(item=ef_all$item)
for (i in unique(food_by_cluster$cluster)) {
  mat <- a %>% select(item, contains(i), ef_per_kg_eaten) %>% 
    mutate(Base=rowSums(select(.,contains("_base"))), Opt=rowSums(select(.,contains("_opt")))) %>%
    select(item, Base, Opt, contains("price"), ef_per_kg_eaten) %>%
    mutate(cost_sav = (.[[2]]-.[[3]])*.[[4]], emi_sav = (.[[2]]-.[[3]])*.[[5]]) %>%
    mutate(cost_sav_share = cost_sav / sum(cost_sav), emi_sav_share = emi_sav / sum(emi_sav))
  names(mat)[2:3] <- c(paste0("Base_",i), paste0("Opt_",i))
  
  mat <- mat %>% select(item, cost_sav, emi_sav) 
  names(mat)[-1] <- c(paste0("C_sav_",i), paste0("E_sav_",i))
  sav_contribution <- sav_contribution %>% left_join(mat)
    
  # mat <- a %>% select(item, contains(i), ef_per_kg_eaten) %>% select(-ends_with("FA"), -ends_with("FM"), -ends_with("MM")) %>%
  #   mutate(cost_sav = (.[[2]]-.[[3]])*.[[4]], emi_sav = (.[[2]]-.[[3]])*.[[5]]) %>%
  #   mutate(cost_sav_share = cost_sav / sum(cost_sav), emi_sav_share = emi_sav / sum(emi_sav))
  # mat <- mat %>% select(item, cost_sav_share, emi_sav_share) 
  # names(mat)[-1] <- c(paste0("C_sav_share_",i), paste0("E_sav_share_",i))
  # sav_contribution <- sav_contribution %>% left_join(mat)
}
sav_contribution <- sav_contribution %>% 
  # mutate_if(is.numeric,as.integer) %>% 
  arrange(desc(C_sav_E0))
# sav_contribution <- sav_contribution %>% rbind(colSums(sav_contribution[,-1]))

xlsx::write.xlsx(sav_contribution, "Saving_contribution.xlsx", row.names = FALSE, col.names = TRUE)




# snapshot <- cbind(snapshot, food_group)
# snapshot_group <- snapshot %>% select(-code, -num) %>% group_by(food_grp) %>% summarise_if(is.numeric, sum) %>%
#   melt(value.name = "kg")
snapshot_sum <- cbind(snapshot, food_wgrp) %>% left_join(nutrients) %>%
  mutate_at(c(2:6), funs("kcal"= .*energy)) %>% 
  mutate_at(c(2:6), funs("prtn"= .*protein)) %>% 
  mutate_at(c(2:6), funs("iron"= .*iron)) %>% 
  mutate_at(c(2:6), funs("zinc"= .*zinc)) %>% 
  mutate_at(c(2:6), funs("vita"= .*vita)) %>% 
  select(item, group, everything()) %>% select(-num, -energy, -code, -en_int, -protein, -iron, -zinc, -vita)
tot_cluster <- as.data.frame(matrix(colSums(snapshot_sum[,-c(1,2)], na.rm=TRUE), nrow=5)) # Check the total kg and kcal per food group
names(tot_cluster) <-c("kg", "kcal", "protein", "iron", "zinc", "vita")
  
foodgroup_kg <- snapshot_sum %>% group_by(group) %>% summarise_at(c(3:7), sum) %>%
  melt(value.name = "kg") 
foodgroup_kcal <- snapshot_sum %>% group_by(group) %>% summarise_at(c(8:12), sum) %>%
  melt(value.name = "kcal") 
fooditem_kg <- snapshot_sum %>% select(-group, -ends_with("kcal")) %>% melt(value.name = "kg") 
fooditem_kcal <- snapshot_sum %>% select(-group, -c(3:7)) %>% melt(value.name = "kcal") 

# snapshot_group %>% group_by(group) %>% summarise(sum) %>%
#   melt(value.name = "kg")

ggplot(foodgroup_kg, aes(group, kg)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + labs(x='Food group',y = "Consumption [kg/yr]")
ggplot(foodgroup_kcal, aes(group, kcal)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + labs(x='Food group',y = "Calorie [kcal/yr]")
ggplot(fooditem_kg, aes(item, kg)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + labs(x='Food item',y = "Consumption [kg/yr]")
ggplot(fooditem_kcal, aes(item, kcal)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") + labs(x='Food item',y = "Calorie [kcal/yr]")

# result6[,2:6] <- result6[,2:6] - result6[,c(6,6,6,6,6)]
# result6[abs(result6) < 1e-10] <- 0

cons_cmin <- data.frame(result_opt[[i]]$val)

library(Matrix)
a <- sparseMatrix(cons_cmin$X1, cons_cmin$X2, x = cons_cmin$X3)
cons_cmin <- data.frame(item=food_price$item, as.matrix(a))
colnames(cons_cmin)[-1] <- hh_member
aa <- food_summary %>% left_join(cons_cmin) %>% 
  mutate_at(vars(contains('-')), funs(cal=.*energy, protein=.*protein, zinc=.*zinc, iron=.*iron, vita=.*vita))
sum_nutrients <- aa %>% ungroup() %>% select(contains('-')) %>% colSums(.)









### Food overall (139 items from IND1)
# Price per item
food_price <- IND_FOOD_Alldata %>% 
  filter(!is.na(qty_tot)) %>% 
  select(id, item, code, unit, val_tot_org, qty_tot) %>% 
  filter(code %in% items_to_optimize$code) %>%  # Select only the food items with non-NA nutritional data
  left_join(avg_wt) %>% left_join(IND_HH_Alldata %>% select(id, weight)) %>%
  mutate_if(!is.na(avg_wt), kg_tot=avg_wt*qty_tot) %>%
  mutate_if(is.na(kg_tot), kg_tot=qty_tot) %>%
  mutate(unit_price_kg = val_tot_org/kg_tot) %>% group_by(item, code) %>% 
  summarise(avg_price_kg = weighted.mean(unit_price_kg, weight, na.rm=T), 
            sd=as.numeric(sqrt(weighted.var(unit_price_kg, weight, na.rm=T)))) %>% arrange(item)

food_summary <- food_price %>% left_join(nutrients) %>% filter(!is.na(code)) %>% arrange(item)

### Test: Pick one example household
hh1 <- IND_HH_Alldata %>% arrange(expenditure/hh_size) %>% slice(7)    

hh_food <- IND_FOOD_Alldata %>% filter(id==hh1$id & !is.na(qty_tot)) %>% 
  select(item, code, unit, val_tot_org, qty_tot) %>% 
  left_join(avg_wt) %>% mutate(kg_tot=ifelse(is.na(avg_wt), qty_tot, avg_wt*qty_tot)) %>% select(-qty_tot, -avg_wt, -unit)

hh_food_per_cap <- hh_food %>% mutate(val_tot_org = val_tot_org/hh1$hh_size, kg_tot = kg_tot/hh1$hh_size)

# Food intake of the sample hh
food_summary_hh <- food_summary %>% left_join(hh_food) %>% select(-sd)
ref_intake <- t(food_summary_hh$kg_tot/hh1$hh_size)[c(1,1,1,1),] 
ref_intake[is.na(ref_intake)] <- 0


#### One sample household example
# Reformat for wgdx
idx <- 1:length(food_price$item)
pr <- cbind(idx, food_summary$avg_price_kg)
c <- cbind(idx, food_summary$energy)
e <- cbind(idx, food_summary$en_int)
nn <- cbind(1:length(pop_groups[[1]]), as.numeric(hh1 %>% select(m_adult, f_adult, m_child, f_child)))

# Define the GAMS entities
f  <- list(name='f',  type='set', uels=food_items, ts='Food groups')
a  <- list(name='a',  type='parameter', dim=2, form='full', uels=c(food_items, nut_groups), 
           val= as.matrix(food_summary %>% ungroup() %>% select(protein, iron, zinc, vita)), 
           ts="nutritive value of foods (mg/g per kg)", domains=c("f", "n"))
pr <- list(name='pr', type='parameter', dim=1, form='sparse', uels=food_items, 
           val= pr, # ordered alphabetically
           ts="price of food f ($ per kg)", domains="f")
c  <- list(name='c',  type='parameter', dim=1, form='sparse', uels=food_items, 
           val= c, 
           ts="calorie content of food f (kcal per kg)", domains="f")
e  <- list(name='e',  type='parameter', dim=1, form='sparse', uels=food_items, 
           val= e, 
           ts="energy intensity of food f (GJ per kg)", domains="f")
r  <- list(name='r',  type='parameter', dim=2, form='full', uels=c(pop_groups, food_items), 
           val= ref_intake, 
           ts="current level of food f intake by person of type p (kg per person)", domains=c("p", "f"))
nn <- list(name='nn', type='parameter', dim=1, form='sparse', uels=pop_groups, 
           val= nn, 
           ts="number of people of type p (num)", domains="p")

wgdx("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/DLE_data_hh.gdx", f, a, pr, c, e, r, nn)
