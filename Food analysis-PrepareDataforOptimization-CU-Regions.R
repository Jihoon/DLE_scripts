# Creator: NRao, Sept 2016
# This code takes the food consumption data from the Oracle DB (shown below), right now for India,
# and calculates the macro- and micro-nutrient content of each food item and creates a summary by state/urb-rur of totals

setwd('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food')

#connect to database
require(RColorBrewer)
require(foreign)
require(RJDBC)
require(readxl)
require(Hmisc)
require(rms)
require(doParallel)
require(data.table)
require(plyr)  # Needed by 'caret'; must be loaded before dplyr
require(dplyr)
require(tidyr)
require(caret)
require(gbm)
require(ggplot2)
require(gridExtra)
require(stringr)
require(stringi)
require(grid)
require(scales)
require(readxl)
require(xlsx)

options(java.parameters = "-Xmx96g")  # Arbitrarily set to 96GB; doesn't seem to matter if too high
drv = JDBC("oracle.jdbc.driver.OracleDriver","P:/ene.model/Rcodes/Common_files/ojdbc6.jar", identifier.quote="\"") 
db_conn = dbConnect(drv, "jdbc:oracle:thin:@gp3.iiasa.ac.at:1521:gp3", "hh_data", "hh_data")

# function to select tables to extract - called below
selectDBdata = function(..., tables, lower=TRUE, conn=NULL, fun=NULL) {
  
  # If database connection is not specified, use 'db_conn' object in global environment

  if (is.null(conn)) conn=get('db_conn', envir=globalenv())
  
  # Coerce arguments to upper case for compatability with Oracle DB
  vars = toupper(as.character(substitute(list(...)))[-1])
  tables = toupper(tables)
  
  # Identify partial matching table names if none specified explicitly
  # Return error message if feasible table(s) cannot be identified
  tbs = dbFetch(dbSendQuery(conn,"select TABLE_NAME from user_tables"))[[1]]
  if (length(tables)==1) {
    if (!tables %in% tbs) tables = sort(grep(paste0("*",tables,"$"), tbs, value=T))
  }
  if (length(tables)==0 | any(!tables %in% tbs)) stop("Cannot find tables that match given argument(s)")
  
  # Extract column names associated with each table
  col_names = dbFetch(dbSendQuery(conn, paste(
    "SELECT table_name, column_name
    FROM USER_TAB_COLUMNS
    ",paste0("WHERE table_name IN ('", paste(tables,collapse="','"),"')"))))
  
  # Check if 'variable' argument is valid; if not, return error indicating erroneous value(s)
  miss = setdiff(vars, col_names$COLUMN_NAME)
  if (length(miss)>0) stop("Variable(s) not present in the table(s): ", paste(miss,collapse=","))
  
  # Fetch data, pulling only the set of suitable column names
  # Bind (append) individual tables via 'bind_rows'
  out = bind_rows(lapply(tables, function(x) {
    if (length(vars)==0) {
      y = dbReadTable(conn, x)
    } else {
      v = intersect(vars, filter(col_names, TABLE_NAME==x)$COLUMN_NAME)
      y = dbFetch(dbSendQuery(conn, paste("SELECT",paste(v, collapse=","),"FROM",x)))
    }
    if (lower) names(y) = tolower(names(y))
    if (!is.null(fun)) y = fun(y)
    return(y)
  }))
  
  return(out)
}

# Function to return the household-specific nutrient DRIs given composition of age and adult/minor 
getDRI= function(group, nutrient) {
  x = dris %>%
    filter(group==Group & nutrient==Nutrient) %>%
    select(DRI)
  return(as.numeric(x))
}

#function that returns the cons eq of the hh member (male adult=1, fem ad=0.77, children=0.60)
getcu= function(group) {
  x = cu_eqs %>%
    filter(group==Group) %>%
    select(cu_eq)
  return(as.numeric(x))
}
#function returns the nutritional gap per cu-eq for a given nutrient and member type, where positive indicates intake deficiency
get_gap=function(group,nutrient,amount) {
  
  dri=getDRI(group,nutrient)
  consum= getcu(group)*amount
  x = (dri-consum)/dri
  
  return(x)
}
#extract tables from Oracle. 
  # food_nutrients = read_excel('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Foo/NSS_food_items-VitA.xlsx', sheet='NSS_food_items')
  food_nutrients = xlsx::read.xlsx('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/NSS_food_items-VitA.xlsx', 
                                   sheetName='NSS_food_items_2017', endRow=152)
  food_items = selectDBdata(SURVEY, ID, ITEM, CODE, UNIT, QTY_TOT, VAL_TOT, tables='IND1_FOOD')
  food_avg_wt=selectDBdata(ITEM, AVG_WT, tables='IND_FOOD_AVG_WT')
  hh_sum=selectDBdata(SURVEY, ID, HH_SIZE, REGION, AGE, SOCIAL_GROUP, RELIGION, MALE, MALE_ADULT, MALE_MINOR, EDUC_YEARS, MINOR, DWELL_STATUS, WORKER_TYPE, OCCUPATION, EXPENDITURE, WEIGHT, URBAN, tables='IND1_HH')
  hh_map=selectDBdata(CODE, ICP_CODE, tables='IND1_MAP')
  en_ints=selectDBdata(COUNTRY, ICP_CODE, MEAN, tables='PRI_E_INT')
  cu_eqs=read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/cu_eq.csv")
  dris=read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/DRI-India.csv")
  states=read.csv("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/states.csv")
    valid_column_names <- make.names(names=names(food_nutrients), unique=TRUE, allow_ = TRUE)
  names(food_nutrients) <- valid_column_names
  
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
  
  #create female and female minor counts and consumption equiv units for later use
  # create consumption unit equivalent for hh, using: 1 for male (avg of sedentary and moderate work), 0.77 for females
  # and 0.60 for children, both m and f. The source is Nat Nutrition Medical Board, 1981. (Text box 6.1.1)
  hh_sum=hh_sum%>%
    mutate(female_adult=hh_size-minor-male_adult,female_minor=minor-male_minor)%>%
    # mutate(cu_eq=(male_adult+female_adult*0.77+minor*0.60))%>%
    mutate(cu_eq=(male_adult*getcu("male_adult")+female_adult*getcu("female_adult")+male_minor*getcu("male_minor")+female_minor*getcu("female_minor"))) %>%
    left_join(states)%>%
    mutate(cluster=paste0(zone,as.character(urban)))
 
  #summary data by cluster, for presentation
  hh_dem_cts=hh_sum %>%
    select(cluster,cu_eq, weight)%>%
    group_by(cluster) %>%
    summarise(cu_eq_avg=weighted.mean(cu_eq, weight, na.rm=TRUE))
  
   #For food items that are entered by number, alter quantity to kg based on IND_FOOD_AVG_WT look up table
   food_items =food_items %>%
     left_join(food_avg_wt) %>%
     transform(qty_tot=ifelse(is.na(avg_wt), qty_tot, qty_tot*avg_wt))
 
   food_nutrients = food_nutrients %>%
      select(item, food_grp, energy,protein, vita, iron, zinc)
   
   food_items = food_items %>%
     left_join(hh_map)

   #calculate avg prices and get energy intensity by cluster     
 food_avgs=food_items%>%
   filter(is.finite(qty_tot)) %>%
   mutate(avg_price=val_tot/qty_tot)%>%
   inner_join(hh_sum%>% select(survey, id, hh_size, cluster,weight, urban, inc_grp, region)) %>%
   group_by(cluster, inc_grp, icp_code, item, code) %>%
   summarise_each(.,funs(weighted.mean(., weight=qty_tot,na.rm=T)),avg_price) %>%
   left_join(en_ints%>%filter(country=='IND')%>%select(-country))%>%
    rename(en_int=mean)%>%
   left_join(food_nutrients)
  
    #calculate total nutrients per food item per person per day based on unit consumption and per unit nutrient content. 
    #Note that nutrient data are always given per 100 g, while the survey data are per kg. 
  food_items = food_items %>%
    filter(!is.na(qty_tot)) %>%
     left_join(food_nutrients) %>%  
   inner_join(hh_sum%>% select(survey, cluster, id, hh_size, weight, urban, as.integer(inc_grp), region, cu_eq)) %>%
    mutate(energy_tot=qty_tot*energy*10/365/cu_eq, 
           protein_tot=qty_tot*protein*10/365/cu_eq,
           vita_tot=qty_tot*vita*10/365/cu_eq, 
           iron_tot=qty_tot*iron*10/365/cu_eq, 
           zinc_tot=qty_tot*zinc*10/365/cu_eq,
           food_exp_percap=val_tot/cu_eq)
  
  # this gives us total total nutrients by household 
  food_total = food_items %>%
    group_by(survey, cluster, id, region, urban, weight, inc_grp, food_grp) %>%
    summarise_each(.,funs(sum(., na.rm=TRUE)),energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot, val_tot) %>%
    dplyr::rename(food_exp=val_tot)

  food_total = food_total %>%
    group_by(survey, id) %>%
    summarise_each(.,funs(sum(., na.rm=TRUE)),energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot, food_exp)
  
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
  
  
  item_anal_cluster=food_items %>%
    left_join(outeaters) %>%
    filter(!eatout_exclude) %>%
    group_by(cluster,inc_grp, item) %>%
    summarise_each(funs(weighted.mean(., w=weight, na.rm=TRUE)),energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot) %>%
    filter(energy_tot>200| protein_tot>5| iron_tot>2| zinc_tot>1|vita_tot>60)
  
  
  hh_sum= hh_sum %>%
    left_join(food_total) %>%
    left_join(outeaters) %>%
    #make sure those who don't eat out are not NA, so that they are included in future selections
    transform(eatout_exclude=ifelse(is.na(eatout_exclude),0,eatout_exclude))  %>%

    mutate(energy_tot_weatout=ifelse(is.na(eatout_share),energy_tot, energy_tot/(1-eatout_share)),
     protein_tot_weatout=ifelse(is.na(eatout_share),protein_tot, protein_tot/(1-eatout_share)),
     zinc_tot_weatout=ifelse(is.na(eatout_share),zinc_tot, zinc_tot/(1-eatout_share)),
     iron_tot_weatout=ifelse(is.na(eatout_share),iron_tot, iron_tot/(1-eatout_share)),
     vita_tot_weatout=ifelse(is.na(eatout_share),vita_tot, vita_tot/(1-eatout_share)))
  
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
  
    
    food_energy_byitem = food_items %>%
      left_join(hh_sum%>%select(survey, id,cal_def, protein_def,iron_def,zinc_def,vita_def))%>%
      filter(cal_def==1 | protein_def==1 | iron_def==1 | zinc_def==1 | vita_def==1)%>%
    select(cluster, inc_grp, id, item,  energy_tot)%>%
    spread(item,energy_tot)
  
    food_energy_byitem[is.na(food_energy_byitem)]<-0
   # mutate(tot_energy=food_energy_byitem%>%select(-cluster,-id)%>%rowSums(.,na.rm=T))%>%
    
    food_energy_byitem = food_energy_byitem %>%
      select(-id)%>%
      group_by(cluster, inc_grp) %>%
    summarise_each(.,funs(mean(., na.rm=TRUE)))%>%
      left_join(hh_dem_cts)
     # filter(is.finite(energy_tot))%>%
   
    food_exp_byitem = food_items %>%
      select(cluster, inc_grp, id, item,  food_exp_percap)%>%
      spread(item,food_exp_percap)
    
    food_exp_byitem[is.na(food_exp_byitem)]<-0
    # mutate(tot_energy=food_energy_byitem%>%select(-cluster,-id)%>%rowSums(.,na.rm=T))%>%
    
    food_exp_byitem = food_exp_byitem %>%
      select(-id)%>%
      group_by(cluster, inc_grp) %>%
      summarise_each(.,funs(mean(., na.rm=TRUE)))
   
  
#now create representative diets by cluster, weighted average of members
#     food_byclust=food_total%>% 
#     group_by(cluster) %>%
#     summarise_each(.,funs(mean(.,na.rm=TRUE)),energy_tot, protein_tot, iron_tot, zinc_tot, food_exp_percap)
#     
    write.csv(food_avgs,'food_item_details-inc.csv')
    write.csv(food_energy_byitem,'cluster_diets_energy-inc.csv')
    write.csv(food_exp_byitem,'cluster_diets_exp-inc.csv')
    write.csv(item_anal_cluster,'cluster_item_diagnostics-inc.csv')
    