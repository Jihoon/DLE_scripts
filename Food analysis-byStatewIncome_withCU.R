# Creator: NRao, Sept 2016
# This code takes the food consumption data from the Oracle DB (shown below), right now for India,
# and calculates the macro- and micro-nutrient content of each food item and creates a summary by state/urb-rur of totals

setwd('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food')


#extract tables from Oracle. 
  food_items = selectDBdata(SURVEY, ID, ITEM, UNIT, QTY_TOT, VAL_TOT, tables='IND1_FOOD')
  food_avg_wt = selectDBdata(ITEM, AVG_WT, tables='IND_FOOD_AVG_WT')
  hh_sum = selectDBdata(SURVEY, ID, HH_SIZE, REGION, AGE, SOCIAL_GROUP, RELIGION, MALE, MALE_ADULT, MALE_MINOR, EDUC_YEARS, MINOR, DWELL_STATUS, WORKER_TYPE, OCCUPATION, EXPENDITURE, WEIGHT, URBAN, tables='IND1_HH')
  # food_nutrients = read_excel('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/NSS_food_items-VitA.xlsx', sheet='NSS_food_items')
  food_nutrients = xlsx::read.xlsx('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/NSS_food_items-VitA.xlsx',
                                 sheetName='NSS_food_items_2017', endRow=152)
  dris = read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/DRI-India.csv")
  cu_eqs = read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/cu_eq.csv")
  zones = read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/states.csv")
  
  #get emissions factors
  # ef_crop <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Emission_factors_ESM.xlsx", 1, startRow=2,
  #                            colIndex = c(2,5), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X5)
  # ef_lvstock <- data.frame(xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Emission_factors_ESM.xlsx", 2,
  #                                          colIndex = 8, header = FALSE, rowIndex=c(11:16), stringsAsFactors=FALSE),
  #                          xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Emission_factors_ESM.xlsx", 2,
  #                                          colIndex = 5, header = FALSE, rowIndex=c(29:34))) %>% rename(item=X8, ef=X5)
  
  ### Non-CO2 EF from Hugo after Feb 2017 (corrected values) w/ new fertilizer values
  # ef_crop <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_25jan17.xlsx", 1, startRow=2,
  #                            colIndex = c(2,8), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X8)
  ef_crop <- xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_28feb17.xlsx", 2, startRow=2,
                             colIndex = c(2,10), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X10)
  ef_lvstock <- data.frame(xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_28feb17.xlsx", 3,
                                           colIndex = 11, header = FALSE, rowIndex=c(11:16), stringsAsFactors=FALSE),
                           xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_28feb17.xlsx", 3,
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
  
  
  valid_column_names <- make.names(names=names(food_nutrients), unique=TRUE, allow_ = TRUE)
  names(food_nutrients) <- valid_column_names
  
  #create income groups, based on $1.25/day, $2/day and $5/day, adjusted to $2010PPP ($1.37, $2.73, $5.46 )
  #  hh_sum=hh_sum %>%
  #    filter(is.finite(exp_percap)) %>%
  #   mutate(inc_grp=cut(exp_percap,breaks=c(0,1.37*365,2.73*365,5.46*365,max(exp_percap)*365),labels=c(1:4)))
  hh_sum=hh_sum%>%
    mutate(exp_percap = expenditure/hh_size)
  
  urb_grp=hh_sum %>%
    filter(urban==1) %>%
    filter(is.finite(exp_percap)) %>%
    arrange(exp_percap) %>%
    mutate(inc_grp=as.integer(cut(exp_percap,breaks=c(0,1.4*365,2.8*365,5.6*365,max(exp_percap)*365),labels=c(1:4))))
  
  rur_grp=hh_sum %>%
    filter(urban==0) %>%
    filter(is.finite(exp_percap)) %>%
    arrange(exp_percap) %>%
    mutate(inc_grp=as.integer(cut(exp_percap,breaks=c(0,0.95*365,1.9*365,3.8*365,max(exp_percap)*365),labels=c(1:4))))
  
  hh_sum<-rbind(urb_grp,rur_grp)
  
  #create female and female minor counts and consumption equiv units for later use
  # create consumption unit equivalent for hh, using: 1 for male (avg of sedentary and moderate work), 0.77 for females
  # and 0.60 for children, both m and f. The source is Nat Nutrition Medical Board, 1981. (Text box 6.1.1)
  hh_sum=hh_sum%>%
    mutate(female_adult=hh_size-minor-male_adult,female_minor=minor-male_minor)%>%
    mutate(cu_eq=(male_adult*getcu("male_adult")+female_adult*getcu("female_adult")+male_minor*getcu("male_minor")+female_minor*getcu("female_minor")))
  
  #summary data by state, urb-rur and inc grp, for presentation
  hh_dem_cts=hh_sum %>%
    group_by(region, urban, inc_grp) %>%
    summarise(cu_eq_avg=weighted.mean(cu_eq, weight, na.rm=TRUE))
  
  zone_cts=hh_sum%>%
    left_join(zones)%>%
    group_by(zone, urban, inc_grp) %>%
    summarise(cu_eq_avg=weighted.mean(cu_eq, weight, na.rm=TRUE),tot_cu=sum(cu_eq*weight))
  
  #####################################  
  
  ###Food item analysis
  #For food items that are entered by number, alter quantity to kg based on IND_FOOD_AVG_WT look up table
   food_items =food_items %>%
     left_join(food_avg_wt) %>%
     transform(qty_tot=ifelse(is.na(avg_wt), qty_tot, qty_tot*avg_wt))
               
   food_nutrients = food_nutrients %>%
      select(item, food_grp_wdds, energy,protein, vita, iron, zinc)
   
    #calculate total nutrients per food item per cons equiv per day, based on unit consumption and per unit nutrient content. 
    #Note that nutrient data are always given per 100 g, while the survey data are per kg. 
  food_items = food_items %>%
    filter(!is.na(qty_tot)) %>%
    left_join(food_nutrients) %>%
    left_join(ef_all)%>%
    inner_join(hh_sum%>% select(survey, id, hh_size, weight, urban, region, inc_grp,cu_eq)) %>%
    mutate(energy_tot=qty_tot*energy*10/365/cu_eq, protein_tot=qty_tot*protein*10/365/cu_eq,vita_tot=qty_tot*vita*10/365/cu_eq, iron_tot=qty_tot*iron*10/365/cu_eq, zinc_tot=qty_tot*zinc*10/365/cu_eq, em_tot=ef_per_kg_eaten*qty_tot/365/cu_eq)
  
  # this gives us total total nutrients by household 
  food_total = food_items %>%
    group_by(survey, id, region, urban, weight, inc_grp) %>%
    summarise_each(.,funs(sum(., na.rm=TRUE)),energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot, val_tot, em_tot) %>%
    dplyr::rename(food_exp=val_tot)
  
  cereals_totals=food_items%>%
    select(survey,region, urban, weight, inc_grp, id, item, food_grp_wdds, cu_eq, energy_tot)%>%
    filter(food_grp_wdds=="SS-C")%>%
    transform(item=ifelse(grepl("Wheat"  ,item)&grepl("PDS",item),"Wheat",
                          ifelse(grepl("Rice",item)&grepl("PDS",item),"Rice",item)))%>%
    group_by(survey, id, region, urban, weight, inc_grp,cu_eq, item) %>%
    summarise_each(.,funs(sum(., na.rm=TRUE)),energy_tot)%>%
    spread(key=item,value=energy_tot)
  
  names(cereals_totals)<-gsub(" ","_",names(cereals_totals))
    
  cereals_totals=cereals_totals%>%
    group_by(survey, region, urban, inc_grp)%>% 
     summarise_each(funs(sum(.*weight*cu_eq,na.rm=TRUE)),Bread, Maida,Ragi_and_its_products, Potato, 
                    Small_millets_and_their_products, Bajra_and_its_products, Rice, Wheat, Jowar_and_its_products,Barley_and_its_products )
  
  food_grp_totals=food_items%>%
    select(survey,region, urban, weight, inc_grp, id, item, food_grp_wdds, cu_eq,energy_tot)%>%
    group_by(survey, region, urban, inc_grp, food_grp_wdds) %>%
    summarise_each(.,funs(sum(.*weight*cu_eq, na.rm=TRUE)),energy_tot)%>%
    spread(key=food_grp_wdds,value=energy_tot)%>%
    left_join(cereals_totals)
  
  names(food_grp_totals)<-gsub("-","_",names(food_grp_totals))
  
  food_grp_totals=food_grp_totals%>%
    mutate(wheat_sh=Wheat/SS_C,Rice_sh=Rice/SS_C,Maida_sh=Maida/SS_C,Bajra_sh=Bajra_and_its_products/SS_C,
    Jowar_sh=Jowar_and_its_products/SS_C, Barley_sh=Barley_and_its_products/SS_C,Potato_sh=Potato/SS_C,  
    Ragi_sh=Ragi_and_its_products/SS_C,Small_millets_sh=Small_millets_and_their_products/SS_C)
     
  food_total = food_total %>%
    group_by(survey, id) %>%
    summarise_each(.,funs(sum(., na.rm=TRUE)),energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot, food_exp, em_tot)
    
  # subset those who eat out, and mark those whose eat out share of total exp is >20%.
  outeaters = food_items %>%
    filter(., grepl("Cooked|processed food",item)) %>%
    select(survey, id, item, val_tot) %>%
    group_by(survey, id) %>%
    summarise_each(.,funs(sum(., na.rm=TRUE)), val_tot) %>%
    dplyr::rename(eatout_exp=val_tot) %>%
    left_join(food_total%>%select(survey, id,food_exp)) %>%
    mutate(eatout_share=eatout_exp/food_exp) %>%
    ##Don't want to include HH in nutritional stats if they eat out a lot. Include if (expenditure) share is < 20% 
    mutate(eatout_exclude=ifelse(eatout_share>0.2,1,0))
  
  
  ################ Calculate household deficiencies ##############################
  
   # add food nutrients to other household summary characteristics for use in analysis
  hh_sum= hh_sum %>%
    left_join(food_total) %>% filter(!is.na(energy_tot)) %>%
    left_join(outeaters) %>%
    #make sure those who don't eat out are not NA, so that they are included in future selections
    transform(eatout_exclude=ifelse(is.na(eatout_exclude),0,eatout_exclude))  %>%
#    mutate(exp_percap = expenditure/hh_size) %>%
    mutate(energy_tot_weatout=ifelse(is.na(eatout_share),energy_tot, energy_tot/(1-eatout_share)),
           protein_tot_weatout=ifelse(is.na(eatout_share),protein_tot, protein_tot/(1-eatout_share)),
           zinc_tot_weatout=ifelse(is.na(eatout_share),zinc_tot, zinc_tot/(1-eatout_share)),
           iron_tot_weatout=ifelse(is.na(eatout_share),iron_tot, iron_tot/(1-eatout_share)),
           vita_tot_weatout=ifelse(is.na(eatout_share),vita_tot, vita_tot/(1-eatout_share)),
           em_tot_weatout=ifelse(is.na(eatout_share),em_tot, em_tot/(1-eatout_share)))
  
 
  # keep track of total pop by region, with and without eatout folks
  hh_tots_excl_outeaters=hh_sum %>%
    filter(!eatout_exclude) %>%
    group_by(region, urban, inc_grp) %>%
    summarise(total_pop_excl_outeaters= sum(hh_size*weight))
  
  hh_tots=hh_sum %>%
    group_by(region, urban, inc_grp) %>%
    summarise(total_pop= sum(hh_size*weight))
  
  #create DRIs and deficiency checks based on normative thresholds from Indian RDA, 2010
        # but only for those who don't eat out too much, we don't have that nutritional content.
        #count deficit only if 10% below DRI
   
   hh_sum=hh_sum%>%
    filter(!eatout_exclude) %>%
   mutate(cal_gap_ma=get_gap("male_adult","calorie",energy_tot_weatout),
            cal_gap_fa= get_gap("female_adult","calorie",energy_tot_weatout),
            cal_gap_mm=get_gap("male_minor","calorie",energy_tot_weatout),
            cal_gap_fm=get_gap("female_minor","calorie",energy_tot_weatout))%>%
     mutate(cal_gap=(cal_gap_ma+cal_gap_fa+ cal_gap_mm+cal_gap_fm)/4,
            cal_def=ifelse(cal_gap>0.1,1,0),
            cal_surp=ifelse(cal_gap<0,1,0))%>%
     
     mutate(zinc_gap_ma=get_gap("male_adult","zinc",zinc_tot_weatout),
           zinc_gap_fa= get_gap("female_adult","zinc",zinc_tot_weatout),
           zinc_gap_mm=get_gap("male_minor","zinc",zinc_tot_weatout),
           zinc_gap_fm=get_gap("female_minor","zinc",zinc_tot_weatout))%>%
    mutate(zinc_gap=(zinc_gap_ma+zinc_gap_fa+ zinc_gap_mm+zinc_gap_fm)/4,
           zinc_def=ifelse(zinc_gap>0.1,1,0),
           zinc_surp=ifelse(zinc_gap<0,1,0))%>%
    
   mutate(iron_gap_ma=get_gap("male_adult","iron",iron_tot_weatout),
          iron_gap_fa= get_gap("female_adult","iron",iron_tot_weatout),
          iron_gap_mm=get_gap("male_minor","iron",iron_tot_weatout),
          iron_gap_fm=get_gap("female_minor","iron",iron_tot_weatout))%>%
     mutate(iron_gap=(iron_gap_ma+iron_gap_fa+ iron_gap_mm+iron_gap_fm)/4,
            iron_def=ifelse(iron_gap>0.1,1,0),
            iron_surp=ifelse(iron_gap<0,1,0))%>%
   
   mutate(protein_gap_ma=get_gap("male_adult","protein",protein_tot_weatout),
          protein_gap_fa= get_gap("female_adult","protein",protein_tot_weatout),
          protein_gap_mm=get_gap("male_minor","protein",protein_tot_weatout),
          protein_gap_fm=get_gap("female_minor","protein",protein_tot_weatout))%>%
     mutate(protein_gap=(protein_gap_ma+protein_gap_fa+ protein_gap_mm+protein_gap_fm)/4,
            protein_def=ifelse(protein_gap>0.1,1,0),
            protein_surp=ifelse(protein_gap<0,1,0))%>%
   
   mutate(vita_gap_ma=get_gap("male_adult","vita",vita_tot_weatout),
          vita_gap_fa= get_gap("female_adult","vita",vita_tot_weatout),
          vita_gap_mm=get_gap("male_minor","vita",vita_tot_weatout),
          vita_gap_fm=get_gap("female_minor","vita",vita_tot_weatout))%>%
     mutate(vita_gap=(vita_gap_ma+vita_gap_fa+ vita_gap_mm+vita_gap_fm)/4,
            vita_def=ifelse(vita_gap>0.1,1,0),
            vita_surp=ifelse(vita_gap<0,1,0))
   
  # wtd average nutrient data for states by urban/rural
  hh_anal=hh_sum %>%
    group_by(region, urban, inc_grp) %>%
    summarise_each(funs(weighted.mean(., w=weight, na.rm=TRUE)),
                   energy_tot_weatout, protein_tot_weatout, vita_tot_weatout, iron_tot_weatout, zinc_tot_weatout, em_tot_weatout)
  
  # wtd average gaps in nutrients by state, income grp and urban/rural for those with deficiency
  hh_zinc_gap_counts=hh_sum %>%
    filter(zinc_def==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(zinc_gap_avg=weighted.mean(zinc_gap, weight, na.rm=TRUE),
                zinc_def_ct= sum(zinc_def*hh_size*weight, na.rm=TRUE))
  
  hh_zinc_surp_counts=hh_sum %>%
    filter(zinc_surp==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(zinc_surp_avg=weighted.mean(-zinc_gap, weight, na.rm=TRUE),
              zinc_surp_ct= sum(zinc_surp*hh_size*weight, na.rm=TRUE))
  
  hh_cal_gap_counts=hh_sum %>%
    filter(cal_def==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(cal_gap_avg=weighted.mean(cal_gap, weight, na.rm=TRUE),
              cal_def_ct= sum(cal_def*hh_size*weight, na.rm=TRUE))
  
  hh_cal_surp_counts=hh_sum %>%
    filter(cal_surp==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(cal_surp_avg=weighted.mean(-cal_gap, weight, na.rm=TRUE),
              cal_surp_ct= sum(cal_surp*hh_size*weight, na.rm=TRUE))
  
  hh_iron_gap_counts=hh_sum %>%
    filter(iron_def==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(iron_gap_avg=weighted.mean(iron_gap, weight, na.rm=TRUE),
              iron_def_ct= sum(iron_def*hh_size*weight, na.rm=TRUE))
  
  hh_iron_surp_counts=hh_sum %>%
    filter(iron_surp==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(iron_surp_avg=weighted.mean(-iron_gap, weight, na.rm=TRUE),
              iron_surp_ct= sum(iron_surp*hh_size*weight, na.rm=TRUE))
  
  hh_protein_gap_counts=hh_sum %>%
    filter(protein_def==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(protein_gap_avg=weighted.mean(protein_gap, weight, na.rm=TRUE),
              protein_def_ct= sum(protein_def*hh_size*weight, na.rm=TRUE))
  
  hh_protein_surp_counts=hh_sum %>%
    filter(protein_surp==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(protein_surp_avg=weighted.mean(-protein_gap, weight, na.rm=TRUE),
              protein_surp_ct= sum(protein_surp*hh_size*weight, na.rm=TRUE))
  
  hh_vita_gap_counts=hh_sum %>%
    filter(vita_def==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(vita_gap_avg=weighted.mean(vita_gap, weight, na.rm=TRUE),
              vita_def_ct= sum(vita_def*hh_size*weight, na.rm=TRUE))
  
  hh_vita_surp_counts=hh_sum %>%
    filter(vita_surp==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(vita_surp_avg=weighted.mean(-vita_gap, weight, na.rm=TRUE),
              vita_surp_ct= sum(vita_surp*hh_size*weight, na.rm=TRUE))
  

  hh_anal=hh_anal%>%
          left_join(hh_zinc_gap_counts) %>%
          left_join(hh_cal_gap_counts) %>%
          left_join(hh_protein_gap_counts) %>%
          left_join(hh_vita_gap_counts) %>%
          left_join(hh_iron_gap_counts) %>%
          left_join(hh_zinc_surp_counts) %>%
          left_join(hh_cal_surp_counts) %>%
          left_join(hh_iron_surp_counts) %>%
          left_join(hh_protein_surp_counts) %>%
          left_join(hh_vita_surp_counts) %>%
          left_join(hh_tots) %>%
          left_join(hh_tots_excl_outeaters) %>%
          left_join(hh_dem_cts)%>%
          left_join(food_grp_totals)  #used later in charts to relate deficiencies to cereal shares
    
  #Create percentages for output
  hh_anal= hh_anal %>%
    mutate(protein_def_pct=protein_def_ct/total_pop_excl_outeaters) %>%
    mutate(vita_def_pct=vita_def_ct/total_pop_excl_outeaters) %>%
    mutate(zinc_def_pct=zinc_def_ct/total_pop_excl_outeaters) %>%
    mutate(cal_def_pct=cal_def_ct/total_pop_excl_outeaters) %>%
    mutate(iron_def_pct=iron_def_ct/total_pop_excl_outeaters)%>%
    mutate(protein_surp_pct=protein_surp_ct/total_pop_excl_outeaters) %>%
    mutate(vita_surp_pct=vita_surp_ct/total_pop_excl_outeaters) %>%
    mutate(zinc_surp_pct=zinc_surp_ct/total_pop_excl_outeaters) %>%
    mutate(cal_surp_pct=cal_surp_ct/total_pop_excl_outeaters) %>%
    mutate(iron_surp_pct=iron_surp_ct/total_pop_excl_outeaters)
  
  hh_anal= hh_anal %>%
    left_join(zones)

#order for output
hh_anal_out=  hh_anal %>%
    filter(!is.na(region)) %>%
    select(zone, region, urban, inc_grp, cu_eq_avg, energy_tot_weatout, protein_tot_weatout, vita_tot_weatout, iron_tot_weatout, zinc_tot_weatout, 
           cal_gap_avg, cal_def_pct,cal_surp_avg, cal_surp_pct,
           protein_gap_avg, protein_def_pct,protein_surp_avg, protein_surp_pct, 
           iron_gap_avg,iron_def_pct, iron_surp_avg, iron_surp_pct,  
           zinc_gap_avg, zinc_def_pct,zinc_surp_avg, zinc_surp_pct, 
           vita_gap_avg, vita_def_pct,vita_surp_avg, vita_surp_pct,
           total_pop_excl_outeaters,
           em_tot_weatout)

hh_anal_out[is.na(hh_anal_out)]<-0  

inc_grp_tots=hh_anal%>%
  group_by(urban, inc_grp) %>% 
  summarise(protein_ct_tot=sum(protein_def_ct, na.rm=T)/1E6,
            vita_ct_tot=sum(vita_def_ct, na.rm=T)/1E6,
            iron_ct_tot=sum(iron_def_ct, na.rm=T)/1E6,
            zinc_ct_tot=sum(zinc_def_ct, na.rm=T)/1E6,
            totpop=sum(total_pop_excl_outeaters)/1E6,
            avgcal=weighted.mean(energy_tot_weatout,weight=weight, na.rm=T)
            )

################Food item by zone analysis ####################################

food_items=food_items%>%
  transform(item=ifelse(grepl("Wheat",item)&grepl("PDS",item),"Wheat",ifelse(grepl("Rice",item)&grepl("PDS",item),"Rice",item)))%>%
  transform(item=ifelse(grepl("Other|Baby",item)&food_grp_wdds=="SS-C","Other",item))%>%
  left_join(zones)

food_zone_totals = food_items %>%
  #  transform(food_grp_wdds=ifelse(grepl("V",food_grp_wdds),"FV",ifelse(grepl("NA",food_grp_wdds),"OTH",food_grp_wdds)))%>%
  group_by(zone, urban, inc_grp, food_grp_wdds) %>%
  summarise_each(.,funs(sum(.*weight*cu_eq,na.rm=TRUE)),energy_tot)%>%
  dplyr::rename(grp_energy=energy_tot)%>%
  #spread(key=food_grp_wdds,value=grp_energy)%>%
  left_join(zone_cts)%>%select(-cu_eq_avg)%>%
  transform(grp_energy=grp_energy/tot_cu)


food_items_totals = food_items %>%
  group_by(zone, urban, inc_grp, food_grp_wdds, item) %>%
  summarise_each(.,funs(sum(.*weight*cu_eq, na.rm=TRUE)),energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot) %>%
  dplyr::rename(item_energy=energy_tot,item_protein=protein_tot,item_vita=vita_tot, item_iron=iron_tot,item_zinc=zinc_tot)%>%
  left_join(food_zone_totals)%>%
  mutate(item_energy_share=item_energy/grp_energy/tot_cu)%>%
  arrange(zone,urban,inc_grp,food_grp_wdds,desc(item_energy_share))


for_shannon=food_items_totals%>%
  filter(food_grp_wdds=="SS-C")%>%
  group_by(zone, inc_grp,urban)%>%
  summarise(shannon=-1*sum(item_energy_share*log(item_energy_share)))

for_vita=food_items_totals%>%
  #filter(item=="Mango"| item=="Carrot")%>%
  group_by(zone, inc_grp,urban)%>%
  summarise(tot_vita=sum(item_vita/tot_cu))

food_group_totals = food_items %>%
  #  transform(food_grp_wdds=ifelse(grepl("V",food_grp_wdds),"FV",ifelse(grepl("NA",food_grp_wdds),"OTH",food_grp_wdds)))%>%
  group_by(zone, urban, inc_grp, food_grp_wdds) %>%
  summarise_each(.,funs(sum(.*weight*cu_eq,na.rm=TRUE)),energy_tot)%>%
  dplyr::rename(grp_energy=energy_tot)%>%
  #spread(key=food_grp_wdds,value=grp_energy)%>%
  left_join(zone_cts)%>%select(-cu_eq_avg)%>%
  transform(grp_energy=grp_energy/tot_cu)

write.csv(for_shannon,'shannon_indices_newdata.csv')


##############################################
#do T-tests to see if avg nutrient consumption levels are different in urban and rural, first all-India then by inc_grp
nat_prot_result<-t.test(protein_tot~urban,data=hh_sum)
nat_zinc_result<-t.test(zinc_tot~urban,data=hh_sum)
nat_iron_result<-t.test(iron_tot~urban,data=hh_sum)
nat_vita_result<-t.test(vita_tot~urban,data=hh_sum)
nat_energy_result<-t.test(energy_tot~urban,data=hh_sum)

energy_result<-sapply(unique(hh_sum$inc_grp),function(inc)c(result=t.test(energy_tot~urban,hh_sum[hh_sum$inc_grp==inc,])))
energy_result<-data.frame(energy_result)
prot_result<-sapply(unique(hh_sum$inc_grp),function(inc)c(result=t.test(protein_tot~urban,hh_sum[hh_sum$inc_grp==inc,])))
prot_result<-data.frame(prot_result)
iron_result<-sapply(unique(hh_sum$inc_grp),function(inc)c(result=t.test(iron_tot~urban,hh_sum[hh_sum$inc_grp==inc,])))
iron_result<-data.frame(iron_result)
zinc_result<-sapply(unique(hh_sum$inc_grp),function(inc)c(result=t.test(zinc_tot~urban,hh_sum[hh_sum$inc_grp==inc,])))
zinc_result<-data.frame(zinc_result)
vita_result<-sapply(unique(hh_sum$inc_grp),function(inc)c(result=t.test(vita_tot~urban,hh_sum[hh_sum$inc_grp==inc,])))
vita_result<-data.frame(vita_result)
#############################################


  require(ggplot2)
  
  hh_sum=hh_sum%>%
    mutate(loc_inc=paste0(inc_grp,urban))%>%
    arrange(loc_inc)
   
  #Cal
  pdf(file = paste0(workdir, "Figures/Average micronutrient consumption - Cal.pdf"), width = 10, height = 10)
  ggplot(hh_sum[hh_sum$energy_tot<10000,],aes(x=loc_inc, y=energy_tot))+geom_rect(xmin=-Inf,xmax=Inf,ymin=1813,ymax=2525,fill='pink')+geom_boxplot(outlier.colour=NA)+scale_x_discrete(labels=c("Inc1-R","Inc1-U","Inc2-R","Inc2-U","Inc3-R","Inc3-U","Inc4-R","Inc4-U"))+coord_cartesian(ylim=c(500,4500))+theme(text=element_text(size=20))+ylab("Calories/day per CU")+xlab("Income Group/Urban-Rural")  
  dev.off()
  
  #Protein
  pdf(file = paste0(workdir, "Figures/Average micronutrient consumption - Protein.pdf"), width = 10, height = 10)
  ggplot(hh_sum[hh_sum$energy_tot<10000,],aes(x=loc_inc, y=protein_tot))+geom_rect(xmin=-Inf,xmax=Inf,ymin=40,ymax=60,fill='pink')+geom_boxplot(outlier.colour=NA)+scale_x_discrete(labels=c("Inc1-R","Inc1-U","Inc2-R","Inc2-U","Inc3-R","Inc3-U","Inc4-R","Inc4-U")) +coord_cartesian(ylim=c(0,150))+theme(text=element_text(size=20))+ylab("Protein g/day per CU")+xlab("Income Group/Urban-Rural")  
  dev.off()
  
  #Iron
  pdf(file = paste0(workdir, "Figures/Average micronutrient consumption - Iron.pdf"), width = 10, height = 10)
  ggplot(hh_sum[hh_sum$energy_tot<10000,],aes(x=loc_inc, y=iron_tot))+geom_rect(xmin=-Inf,xmax=Inf,ymin=17,ymax=23,fill='pink')+geom_boxplot(outlier.colour=NA)+scale_x_discrete(labels=c("Inc1-R","Inc1-U","Inc2-R","Inc2-U","Inc3-R","Inc3-U","Inc4-R","Inc4-U")) +coord_cartesian(ylim=c(0,50))+theme(text=element_text(size=20))+ylab("Iron mg/day per CU")+xlab("Income Group/Urban-Rural")  
  dev.off()
  
  #vita
  pdf(file = paste0(workdir, "Figures/Average micronutrient consumption - vita.pdf"), width = 10, height = 10)
  ggplot(hh_sum[hh_sum$energy_tot<10000,],aes(x=loc_inc, y=vita_tot))+geom_hline(yintercept=600)+geom_boxplot(outlier.colour=NA)+scale_x_discrete(labels=c("Inc1-R","Inc1-U","Inc2-R","Inc2-U","Inc3-R","Inc3-U","Inc4-R","Inc4-U")) +coord_cartesian(ylim=c(0,1500)) +theme(text=element_text(size=20)) +ylab("Vit A mcg/day per CU")+xlab("Income Group/Urban-Rural")
  dev.off()
  
  #zinc
  pdf(file = paste0(workdir, "Figures/Average micronutrient consumption - zinc.pdf"), width = 10, height = 10)
  ggplot(hh_sum[hh_sum$energy_tot<10000,],aes(x=loc_inc, y=zinc_tot))+geom_rect(xmin=-Inf,xmax=Inf,ymin=9,ymax=12,fill='pink')+geom_boxplot(outlier.colour=NA)+scale_x_discrete(labels=c("Inc1-R","Inc1-U","Inc2-R","Inc2-U","Inc3-R","Inc3-U","Inc4-R","Inc4-U")) +coord_cartesian(ylim=c(0,25))+theme(text=element_text(size=20))+ylab("Zinc mg/day per CU")+xlab("Income Group/Urban-Rural")  
  dev.off()
  
  #rice share vs iron deficiency plot
  hh_anal$inc_grp <- factor(hh_anal$inc_grp)
  p<-ggplot(hh_anal,aes(Rice_sh,iron_def_pct))+geom_point(aes(colour=zone, shape=inc_grp))
  pdf(file = paste0(workdir, "Figures/Share of population with iron deficiency - Rice.pdf"), width = 7, height = 10)
  p
  dev.off()
  
  p<-ggplot(hh_anal,aes(wheat_sh,iron_def_pct))+geom_point(aes(colour=zone, shape=inc_grp))
  pdf(file = paste0(workdir, "Figures/Share of population with iron deficiency - Wheat.pdf"), width = 7, height = 10)
  p
  dev.off()
  
  #bar graph showing cereal shares by zone and income group
  plot_cer_shs=food_items_totals%>%
    filter(food_grp_wdds=="SS-C", (inc_grp==1 | inc_grp==4))%>%
    transform(item=ifelse(grepl("Jowar",item),"Jowar",
                          ifelse(grepl("Ragi",item),"Ragi",
                                 ifelse(grepl("Bajra",item),"Bajra",
                                        ifelse(grepl("Maize",item),"Maize",
                                               ifelse(grepl("Small millets",item),"Small millets",
                                                      ifelse(grepl("substitutes",item),"Cereal subs",
                                                             ifelse(grepl("Suji",item),"Suji",
                                                                    ifelse(grepl("Sewai",item),"Sewai",
                                                                           ifelse(grepl("Barley",item),"Barley",item))))))))))
  
  u_r<-c(`0`="Rural",`1`="Urban")
  inc_grp_labels<-c(`1`="Lowest income",`4`="Highest income")       
  
  #plot cereal calorie shares by zone for urban/rural richest/poorest
  q<-ggplot(plot_cer_shs,aes(item,item_energy_share))+geom_bar(aes(fill=zone),position="dodge",stat="identity")
  q+facet_grid(urban ~ inc_grp,labeller=labeller(urban=as_labeller(u_r),inc_grp=as_labeller(inc_grp_labels)))+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(y="Cereals calorie shares",x="")
  
  #plot food grp calorie shares
  u_r<-c(`0`="Rural",`1`="Urban")
  inc_grp_labels<-c(`1`="Lowest income",`4`="Highest income")       
  r<-ggplot(food_group_totals%>%filter(inc_grp==1|inc_grp==4),aes(food_grp_wdds,grp_energy))+geom_bar(aes(fill=zone),position="dodge",stat="identity")
  r+facet_grid(urban ~ inc_grp,labeller=labeller(urban=as_labeller(u_r),inc_grp=as_labeller(inc_grp_labels)))+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(y="Food group calories per CU",x="")
  
  
  ##################################
  
  write.csv(hh_anal_out,'NSS_cu_diagnostics_bystate_inc_newdata.csv')
  write.csv(food_group_totals,'NSS_cu_foodgrp_diagnostics_newdata.csv')
  write.csv(food_nutrients,'NSS_cu_food_nutrients_newdata.csv')
  write.csv(inc_grp_tots,'inc_grp_cu_tots_newdata.csv')