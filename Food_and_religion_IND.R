food_ind1 = selectDBdata(SURVEY, ID, ITEM, UNIT, QTY_TOT, VAL_TOT, tables='IND1_FOOD')
# hh_sum=selectDBdata(SURVEY, ID, HH_SIZE, REGION, AGE, SOCIAL_GROUP, RELIGION, MALE, EDUC_YEARS, MINOR, DWELL_STATUS, WORKER_TYPE, OCCUPATION, EXPENDITURE, WEIGHT, URBAN, tables='IND1_HH')

food_religion = food_ind1 %>%
  left_join(food_avg_wt) %>%
  transform(qty_tot=ifelse(is.na(avg_wt), qty_tot, qty_tot*avg_wt))

food_religion = food_religion %>%
  filter(!is.na(qty_tot)) %>%
  # left_join(food_nutrients) %>%
  inner_join(hh_sum %>% filter(!is.na(religion)) %>% select(survey, id, hh_size, weight, religion)) 

milk <- food_religion %>% filter(item=="Milk, liquid") 
milk_sum <- milk %>% group_by(religion) %>% 
  summarise(milk_usd = weighted.mean(val_tot/hh_size, weight), milk_liter = weighted.mean(qty_tot/hh_size,weight))
relig_sum <- hh_sum %>% group_by(religion) %>% 
  summarise(tot_pop = sum(hh_size *weight), tot_hh = sum(weight), avg_hh_size = weighted.mean(hh_size, weight))
milk_sum <- milk_sum %>% left_join(relig_sum)

write.table(milk_sum, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)

milk_plot <- ggplot(milk, aes(x = religion, weight=weight)) + 
  geom_boxplot(aes(y = qty_tot/hh_size)) + ylab("Milk liter per capita")
milk_plot


hh_sum_BRA=selectDBdata(SURVEY, ID, HH_SIZE, RACE, REGION, AGE, MALE, EDUC_YEARS, MINOR, DWELL_STATUS, EXPENDITURE, WEIGHT, URBAN, tables='BRA1_HH')
hh_sum_ZAF=selectDBdata(SURVEY, ID, HH_SIZE, RACE, REGION, AGE, MALE, EDUC_YEARS, MINOR, DWELL_STATUS, EXPENDITURE, WEIGHT, URBAN, tables='ZAF1_HH')

hh_BRA <- hh_sum_BRA %>% group_by(race) %>% 
  summarise(tot_pop = sum(hh_size *weight), hh_size = weighted.mean(hh_size, weight))
hh_ZAF <- hh_sum_ZAF %>% group_by(race) %>% 
  summarise(tot_pop = sum(hh_size *weight), hh_size = weighted.mean(hh_size, weight))

view(hh_BRA)
view(hh_ZAF)
