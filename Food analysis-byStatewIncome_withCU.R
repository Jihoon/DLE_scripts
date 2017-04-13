# Created by NRao, Sept 2016, and maintained by JMin
# This code takes the food consumption data from the Oracle DB (shown below), right now for India,
# and calculates the macro- and micro-nutrient content of each food item and creates a summary by state/urb-rur of totals

# Increase java heap size
options(java.parameters = "-Xmx96g")  # Arbitrarily set to 96GB; doesn't seem to matter if too high

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


#############################
### Set working directory ###
#############################

  setwd('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food')


############################
### Function definitions ###  
#############################
  
  # Function to select tables to extract - called below
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
  
  # Function to return the household-specific nutrient DRIs given composition of age and adult/minor ###
  getDRI= function(group, nutrient) {
    x = dris %>%
      filter(group==Group & nutrient==Nutrient) %>%
      select(DRI)
    return(as.numeric(x))
  }
  
  # Function that returns the cons eq of the hh member (male adult=1, fem ad=0.77, children=0.60)
  getcu= function(group) {
    x = cu_eqs %>%
      filter(group==Group) %>%
      select(cu_eq)
    return(as.numeric(x))
  }
  
  # function returns the nutritional gap per cu-eq for a given nutrient and member type, where positive indicates intake deficiency
  get_gap=function(group,nutrient,amount) {
    
    dri = getDRI(group, nutrient)
    consum = getcu(group)*amount
    x = (dri-consum)/dri
    
    return(x)
  }
  
  
###################
### Data import ###
###################

  # Extract required data from Oracle DB and other spreadsheets
  
  # Set up the connection to the DB
  drv = JDBC("oracle.jdbc.driver.OracleDriver","P:/ene.model/Rcodes/Common_files/ojdbc6.jar", identifier.quote="\"") 
  db_conn = dbConnect(drv, "jdbc:oracle:thin:@gp3.iiasa.ac.at:1521:gp3", "hh_data", "hh_data")
  
  
  # 'IND1' is for NSS 2011-2012.
  
  # Food consumption by household
  food_items = selectDBdata(SURVEY, ID, ITEM, UNIT, QTY_TOT, VAL_TOT, tables='IND1_FOOD')
  
  # Average weight per food item sold by number (egg, orange, coconut, etc)
  food_avg_wt = selectDBdata(ITEM, AVG_WT, tables='IND_FOOD_AVG_WT')
  
  # Household demographics
  hh_sum = selectDBdata(SURVEY, ID, HH_SIZE, REGION, AGE, SOCIAL_GROUP, RELIGION, MALE, MALE_ADULT, MALE_MINOR, EDUC_YEARS, MINOR, DWELL_STATUS, WORKER_TYPE, OCCUPATION, EXPENDITURE, WEIGHT, URBAN, tables='IND1_HH')
  
  # Nutrient content by food item (updated with 2017 table)
  food_nutrients = xlsx::read.xlsx('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/NSS_food_items-VitA.xlsx', 
                             sheetName='NSS_food_items_2017', endRow=152)
  
  # Make valid column names for food_nutrients matrix in case they are not
  valid_column_names <- make.names(names=names(food_nutrients), unique=TRUE, allow_ = TRUE)
  names(food_nutrients) <- valid_column_names
  
  # Dietary Reference Intakes (DRI), consumption unit by household member, definition of Indian regions/zones
  dris = read.csv("DRI-India.csv")
  cu_eqs = read.csv("cu_eq.csv")
  zones = read.csv("states.csv")


  # Read in emissions factors by food item ###
  
  # Non-CO2 EF compiled from Gerber et al. (2013) and India-specific fertilizer data (http://inputsurvey.dacnet.nic.in/databasehome.aspx)
  # These EFs are kgCO2e per kg of production unit (carcass weight, paddy crop, etc)
  ef_crop <- xlsx::read.xlsx("../../Data/Food/Em_factors_Narasimha_new_28feb17.xlsx", 2, startRow=2,
                         colIndex = c(2,10), header = FALSE, stringsAsFactors=FALSE) %>% rename(item=X2, ef=X10)
  ef_lvstock <- data.frame(xlsx::read.xlsx("C:/Users/min/SharePoint/T/WS2 - Documents/Data/Food/Em_factors_Narasimha_new_28feb17.xlsx", 3,
                                       colIndex = 11, header = FALSE, rowIndex=c(11:16), stringsAsFactors=FALSE),
                       xlsx::read.xlsx("../../Data/Food/Em_factors_Narasimha_new_28feb17.xlsx", 3,
                                       colIndex = 6, header = FALSE, rowIndex=c(30:35)))
  names(ef_lvstock) <- c("item", "ef")
  
  # Methane EF for rice
  rice_ch4 <- 1.0 # 0.66  kgCO2e/kg rice
  
  # Combine EFs for all food items and add CH4 EF for rice
  ef_org <- rbind.fill(ef_crop, ef_lvstock)
  ef_org <- ef_org %>% mutate(ef = ifelse(grepl("paddy",item, ignore.case = TRUE), ef+rice_ch4, ef)) %>% arrange(item) %>% rename(item_esm=item, ef_nonco2=ef)
  
  # Merge data on extraction rate, edible portion ratio, and the mapping between NSS items and source items with the original EFs from above
  ef_all <- xlsx::read.xlsx("../../Data/Food/Emission_factors_mapping.xlsx", 2, colIndex=c(1:6),
                        stringsAsFactors=FALSE) %>% left_join(ef_org) 
  ef_all <- ef_all %>% mutate(ef_nonco2 = ifelse(grepl("Fish",item), 1.13, ef_nonco2))   # Update 'fish' item Based on Tilman et al. and 227g/serving (fish) http://www.health.state.mn.us/divs/eh/fish/eating/serving.html
  
  # Convert EF per production weight to EF per weight consumed 
  ef_all <- ef_all %>% mutate(ef_per_kg_purchased=ef_nonco2/extraction, 
                          ef_per_kg_eaten=ef_nonco2/extraction/edible) %>% rename(ef_base = ef_nonco2)  # CO2e per edible kg
  
  
  
############################  
### Define income groups ###
############################  
  
  # Create income group variable, based on $1.25/day, $2/day and $5/day, adjusted to $2010PPP ($1.37, $2.73, $5.46) ###
  hh_sum = hh_sum %>%
    mutate(exp_percap = expenditure/hh_size)

  urb_grp = hh_sum %>%
    filter(urban==1) %>%
    filter(is.finite(exp_percap)) %>%
    arrange(exp_percap) %>%
    mutate(inc_grp=as.integer(cut(exp_percap,breaks=c(0, 1.4*365, 2.8*365, 5.6*365, max(exp_percap)*365),labels=c(1:4))))

  rur_grp = hh_sum %>%
    filter(urban==0) %>%
    filter(is.finite(exp_percap)) %>%
    arrange(exp_percap) %>%
    mutate(inc_grp=as.integer(cut(exp_percap,breaks=c(0, 0.95*365, 1.9*365, 3.8*365, max(exp_percap)*365),labels=c(1:4))))

  hh_sum<-rbind(urb_grp,rur_grp)

  # Create female and female minor counts and consumption equiv units for later use ###
  # Consumption units are read by getcu() defined above.
  hh_sum = hh_sum %>%
    mutate(female_adult=hh_size-minor-male_adult, female_minor=minor-male_minor) %>%
    mutate(cu_eq=(male_adult*getcu("male_adult")+female_adult*getcu("female_adult")+
                    male_minor*getcu("male_minor")+female_minor*getcu("female_minor")))
  
  # Summarize CU for presentation
  # 1) by state, urb-rur and inc grp
  hh_dem_cts = hh_sum %>%
    group_by(region, urban, inc_grp) %>%
    summarise(cu_eq_avg=weighted.mean(cu_eq, weight, na.rm=TRUE))
  
  # 2) by zone(E/S/W/N), urb-rur and inc grp
  zone_cts = hh_sum %>%
    left_join(zones) %>%
    group_by(zone, urban, inc_grp) %>%
    summarise(cu_eq_avg=weighted.mean(cu_eq, weight, na.rm=TRUE),tot_cu=sum(cu_eq*weight))


##########################  
### Food item analysis ###
##########################  
  
  # For food items that are entered by number, alter quantity to kg based on IND_FOOD_AVG_WT look up table
  food_items = food_items %>%
    left_join(food_avg_wt) %>%
    transform(qty_tot=ifelse(is.na(avg_wt), qty_tot, qty_tot*avg_wt))
           
  food_nutrients = food_nutrients %>%
    select(item, food_grp_wdds, energy,protein, vita, iron, zinc)
  
  # Calculate total nutrients per food item per cons equiv per day, based on unit consumption and per unit nutrient content. 
  # Note that nutrient data are always given per 100 g, while the survey data are per kg. 
  food_items = food_items %>%
    filter(!is.na(qty_tot)) %>%
    left_join(food_nutrients) %>%
    left_join(ef_all) %>%
    inner_join(hh_sum %>% select(survey, id, hh_size, weight, urban, region, inc_grp, cu_eq)) %>%
    mutate(energy_tot=qty_tot*energy*10/365/cu_eq, protein_tot=qty_tot*protein*10/365/cu_eq,
           vita_tot=qty_tot*vita*10/365/cu_eq, iron_tot=qty_tot*iron*10/365/cu_eq, 
           zinc_tot=qty_tot*zinc*10/365/cu_eq, em_tot=ef_per_kg_eaten*qty_tot/365/cu_eq)

  # Get total nutrients, expenditure, emissions by household (per CU)
  food_total = food_items %>%
    group_by(survey, id, region, urban, weight, inc_grp) %>%
    summarise_each(., funs(sum(., na.rm=TRUE)), energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot, val_tot, em_tot) %>%
    dplyr::rename(food_exp=val_tot)

  # Get calorie total by cereal item and by household (per CU)
  cereals_totals=food_items %>%
    select(survey, region, urban, weight, inc_grp, id, item, food_grp_wdds, cu_eq, energy_tot) %>%
    filter(food_grp_wdds=="SS-C") %>%
    transform(item=ifelse(grepl("Wheat",item)&grepl("PDS",item),"Wheat",
                        ifelse(grepl("Rice",item)&grepl("PDS",item),"Rice",item))) %>%
    group_by(survey, id, region, urban, weight, inc_grp,cu_eq, item) %>%
    summarise_each(.,funs(sum(., na.rm=TRUE)), energy_tot) %>%
    spread(key=item, value=energy_tot)
  # Give proper R names for cereals_totals
  names(cereals_totals) <- gsub(" ","_",names(cereals_totals))

  # Summarize calorie total from major cereal items by state, urb-rur and inc grp (per CU)
  cereals_totals = cereals_totals %>%
    group_by(survey, region, urban, inc_grp) %>% 
    summarise_each(funs(sum(.*weight*cu_eq,na.rm=TRUE)), Bread, Maida, Ragi_and_its_products, Potato, 
                  Small_millets_and_their_products, Bajra_and_its_products, Rice, Wheat, Jowar_and_its_products, Barley_and_its_products )

  # Summarize calorie total from each food group by state, urb-rur and inc grp (per CU)   
  food_grp_totals = food_items %>%
    select(survey, region, urban, weight, inc_grp, id, item, food_grp_wdds, cu_eq, energy_tot) %>%
    group_by(survey, region, urban, inc_grp, food_grp_wdds) %>%
    summarise_each(., funs(sum(.*weight*cu_eq, na.rm=TRUE)), energy_tot) %>%
    spread(key=food_grp_wdds,value=energy_tot) %>%
    left_join(cereals_totals)
  names(food_grp_totals)<-gsub("-","_",names(food_grp_totals))
  
  # Get calorie share of each cereal item among total cereal (SS-C) calories
  food_grp_totals = food_grp_totals%>%
    mutate(wheat_sh=Wheat/SS_C,Rice_sh=Rice/SS_C, Maida_sh=Maida/SS_C, Bajra_sh=Bajra_and_its_products/SS_C,
    Jowar_sh=Jowar_and_its_products/SS_C, Barley_sh=Barley_and_its_products/SS_C, Potato_sh=Potato/SS_C,  
    Ragi_sh=Ragi_and_its_products/SS_C, Small_millets_sh=Small_millets_and_their_products/SS_C)
  
  # Not neccessary 
  food_total = food_total %>%
    group_by(survey, id) %>%
    summarise_each(.,funs(sum(., na.rm=TRUE)),energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot, food_exp, em_tot)

  # subset those who eat out, and mark those whose eat out share of total exp is >20%.
  outeaters = food_items %>%
    filter(grepl("Cooked|processed food",item)) %>%
    select(survey, id, item, val_tot) %>%
    group_by(survey, id) %>%
    summarise_each(.,funs(sum(., na.rm=TRUE)), val_tot) %>%
    dplyr::rename(eatout_exp=val_tot) %>%
    left_join(food_total %>% select(survey, id, food_exp)) %>%
    mutate(eatout_share=eatout_exp/food_exp) %>%
    ##Don't want to include HH in nutritional stats if they eat out a lot. Include if (expenditure) share is < 20% 
    mutate(eatout_exclude=ifelse(eatout_share>0.2, 1, 0)) %>% select(-region, -urban, -weight)


########################################
### Calculate household deficiencies ###
########################################
  
  # Add food nutrients to other household summary characteristics for use in analysis
  hh_sum = hh_sum %>%
    left_join(food_total) %>% filter(!is.na(energy_tot)) %>%
    left_join(outeaters) %>%
    # Make sure those who don't eat out are not NA, so that they are included in future selections
    transform(eatout_exclude=ifelse(is.na(eatout_exclude), 0, eatout_exclude))  %>%
    # Assume nutritional contents of each household's eatout food is identical to its average diet
    # and add in the nutrient intake from the eatout portion of the hh
    mutate(energy_tot_weatout=ifelse(is.na(eatout_share), energy_tot, energy_tot/(1-eatout_share)),
         protein_tot_weatout=ifelse(is.na(eatout_share), protein_tot, protein_tot/(1-eatout_share)),
         zinc_tot_weatout=ifelse(is.na(eatout_share), zinc_tot, zinc_tot/(1-eatout_share)),
         iron_tot_weatout=ifelse(is.na(eatout_share), iron_tot, iron_tot/(1-eatout_share)),
         vita_tot_weatout=ifelse(is.na(eatout_share), vita_tot, vita_tot/(1-eatout_share)),
         em_tot_weatout=ifelse(is.na(eatout_share), em_tot, em_tot/(1-eatout_share)))
    
  # keep track of total pop by region, with and without eatout folks
  # without outeaters
  hh_tots_excl_outeaters = hh_sum %>%
    filter(!eatout_exclude) %>%
    group_by(region, urban, inc_grp) %>%
    summarise(total_pop_excl_outeaters = sum(hh_size*weight))
  
  # with outeaters
  hh_tots = hh_sum %>%
    group_by(region, urban, inc_grp) %>%
    summarise(total_pop = sum(hh_size*weight))

  # Create DRIs and deficiency checks based on normative thresholds from Indian RDA, 2010
  # but only for those who don't eat out too much (eatout expenditure share < 20%), we don't have that nutritional content.
  # Count households with deficit only if 10% below the households' DRI 
  # *_tot_weatout values are per CU.
  # *_gap_* values are per person.
  hh_sum = hh_sum %>%
    filter(!eatout_exclude) %>%
    mutate(cal_gap_ma=get_gap("male_adult", "calorie", energy_tot_weatout),
           cal_gap_fa=get_gap("female_adult", "calorie", energy_tot_weatout),
           cal_gap_mm=get_gap("male_minor", "calorie", energy_tot_weatout),
           cal_gap_fm=get_gap("female_minor","calorie", energy_tot_weatout)) %>%
    mutate(cal_gap=(cal_gap_ma*male_adult+cal_gap_fa*female_adult+cal_gap_mm*male_minor+cal_gap_fm*female_minor)/hh_size, # avg gap per hh member
           cal_def=ifelse(cal_gap>0.1, 1, 0),
           cal_surp=ifelse(cal_gap<0, 1, 0)) %>%
    
    mutate(zinc_gap_ma=get_gap("male_adult", "zinc", zinc_tot_weatout),
           zinc_gap_fa=get_gap("female_adult", "zinc", zinc_tot_weatout),
           zinc_gap_mm=get_gap("male_minor", "zinc", zinc_tot_weatout),
           zinc_gap_fm=get_gap("female_minor", "zinc", zinc_tot_weatout)) %>%
    mutate(zinc_gap=(zinc_gap_ma*male_adult+zinc_gap_fa*female_adult+zinc_gap_mm*male_minor+zinc_gap_fm*female_minor)/hh_size, # avg gap per hh member
           zinc_def=ifelse(zinc_gap>0.1, 1, 0),
           zinc_surp=ifelse(zinc_gap<0, 1, 0)) %>%
    
    mutate(iron_gap_ma=get_gap("male_adult", "iron", iron_tot_weatout),
           iron_gap_fa= get_gap("female_adult", "iron", iron_tot_weatout),
           iron_gap_mm=get_gap("male_minor", "iron", iron_tot_weatout),
           iron_gap_fm=get_gap("female_minor", "iron", iron_tot_weatout)) %>%
    mutate(iron_gap=(iron_gap_ma*male_adult+iron_gap_fa*female_adult+iron_gap_mm*male_minor+iron_gap_fm*female_minor)/hh_size, # avg gap per hh member
           iron_def=ifelse(iron_gap>0.1, 1, 0),
           iron_surp=ifelse(iron_gap<0, 1, 0)) %>%
    
    mutate(protein_gap_ma=get_gap("male_adult", "protein", protein_tot_weatout),
           protein_gap_fa= get_gap("female_adult", "protein", protein_tot_weatout),
           protein_gap_mm=get_gap("male_minor", "protein", protein_tot_weatout),
           protein_gap_fm=get_gap("female_minor", "protein", protein_tot_weatout)) %>%
    mutate(protein_gap=(protein_gap_ma*male_adult+protein_gap_fa*female_adult+protein_gap_mm*male_minor+protein_gap_fm*female_minor)/hh_size, # avg gap per hh member
           protein_def=ifelse(protein_gap>0.1, 1, 0),
           protein_surp=ifelse(protein_gap<0, 1, 0)) %>%
    
    mutate(vita_gap_ma=get_gap("male_adult", "vita", vita_tot_weatout),
           vita_gap_fa= get_gap("female_adult", "vita", vita_tot_weatout),
           vita_gap_mm=get_gap("male_minor", "vita", vita_tot_weatout),
           vita_gap_fm=get_gap("female_minor", "vita", vita_tot_weatout))%>%
    mutate(vita_gap=(vita_gap_ma*male_adult+vita_gap_fa*female_adult+vita_gap_mm*male_minor+vita_gap_fm*female_minor)/hh_size, # avg gap per hh member
           vita_def=ifelse(vita_gap>0.1, 1, 0),
           vita_surp=ifelse(vita_gap<0, 1, 0))

  # Get wtd average nutrient data for states by urban/rural and income group (without outeaters)
  hh_anal = hh_sum %>%
    group_by(region, urban, inc_grp) %>%
    summarise_each(funs(weighted.mean(., w=weight, na.rm=TRUE)),
                 energy_tot_weatout, protein_tot_weatout, vita_tot_weatout, iron_tot_weatout, zinc_tot_weatout, em_tot_weatout)
    
  # Get wtd average gaps in nutrients by state, income grp and urban/rural for those with deficiency
  hh_zinc_gap_counts = hh_sum %>%
    filter(zinc_def==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(zinc_gap_avg=weighted.mean(zinc_gap, hh_size*weight, na.rm=TRUE),
              zinc_def_ct= sum(zinc_def*hh_size*weight, na.rm=TRUE))

  hh_zinc_surp_counts = hh_sum %>%
    filter(zinc_surp==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(zinc_surp_avg=weighted.mean(-zinc_gap, hh_size*weight, na.rm=TRUE),
              zinc_surp_ct= sum(zinc_surp*hh_size*weight, na.rm=TRUE))
  
  hh_cal_gap_counts = hh_sum %>%
    filter(cal_def==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(cal_gap_avg=weighted.mean(cal_gap, hh_size*weight, na.rm=TRUE),
              cal_def_ct= sum(cal_def*hh_size*weight, na.rm=TRUE))
    
  hh_cal_surp_counts = hh_sum %>%
    filter(cal_surp==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(cal_surp_avg=weighted.mean(-cal_gap, hh_size*weight, na.rm=TRUE),
              cal_surp_ct= sum(cal_surp*hh_size*weight, na.rm=TRUE))
  
  hh_iron_gap_counts = hh_sum %>%
    filter(iron_def==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(iron_gap_avg=weighted.mean(iron_gap, hh_size*weight, na.rm=TRUE),
              iron_def_ct= sum(iron_def*hh_size*weight, na.rm=TRUE))
  
  hh_iron_surp_counts = hh_sum %>%
    filter(iron_surp==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(iron_surp_avg=weighted.mean(-iron_gap, hh_size*weight, na.rm=TRUE),
              iron_surp_ct= sum(iron_surp*hh_size*weight, na.rm=TRUE))
  
  hh_protein_gap_counts = hh_sum %>%
    filter(protein_def==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(protein_gap_avg=weighted.mean(protein_gap, hh_size*weight, na.rm=TRUE),
              protein_def_ct= sum(protein_def*hh_size*weight, na.rm=TRUE))
  
  hh_protein_surp_counts = hh_sum %>%
    filter(protein_surp==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(protein_surp_avg=weighted.mean(-protein_gap, hh_size*weight, na.rm=TRUE),
              protein_surp_ct= sum(protein_surp*hh_size*weight, na.rm=TRUE))
  
  hh_vita_gap_counts = hh_sum %>%
    filter(vita_def==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(vita_gap_avg=weighted.mean(vita_gap, hh_size*weight, na.rm=TRUE),
              vita_def_ct= sum(vita_def*hh_size*weight, na.rm=TRUE))
  
  hh_vita_surp_counts = hh_sum %>%
    filter(vita_surp==1)%>%
    group_by(region, urban, inc_grp) %>%
    summarise(vita_surp_avg=weighted.mean(-vita_gap, hh_size*weight, na.rm=TRUE),
              vita_surp_ct= sum(vita_surp*hh_size*weight, na.rm=TRUE))

  # Combine all analysis results into hh_anal
  hh_anal = hh_anal%>%
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
    left_join(hh_dem_cts) %>%
    left_join(food_grp_totals)  #used later in charts to relate deficiencies to cereal shares

  # Create percentages of deficient population for output (by state, income grp and urban/rural)
  hh_anal = hh_anal %>%
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

  # Select variables for output
  hh_anal_out = hh_anal %>%
    filter(!is.na(region)) %>%
    select(zone, region, urban, inc_grp, cu_eq_avg, energy_tot_weatout, protein_tot_weatout, vita_tot_weatout, iron_tot_weatout, zinc_tot_weatout, 
           cal_gap_avg, cal_def_pct,cal_surp_avg, cal_surp_pct,
           protein_gap_avg, protein_def_pct,protein_surp_avg, protein_surp_pct, 
           iron_gap_avg,iron_def_pct, iron_surp_avg, iron_surp_pct,  
           zinc_gap_avg, zinc_def_pct,zinc_surp_avg, zinc_surp_pct, 
           vita_gap_avg, vita_def_pct,vita_surp_avg, vita_surp_pct,
           total_pop_excl_outeaters,
           em_tot_weatout)
  # Replace NA with zeros
  hh_anal_out[is.na(hh_anal_out)]<-0  

  # Sum deficient population counts by income group and urb-rur
  inc_grp_tots = hh_anal %>%
    group_by(urban, inc_grp) %>% 
    summarise(protein_ct_tot=sum(protein_def_ct, na.rm=T)/1E6,
              vita_ct_tot=sum(vita_def_ct, na.rm=T)/1E6,
              iron_ct_tot=sum(iron_def_ct, na.rm=T)/1E6,
              zinc_ct_tot=sum(zinc_def_ct, na.rm=T)/1E6,
              cal_ct_tot=sum(cal_def_ct, na.rm=T)/1E6,
              totpop=sum(total_pop_excl_outeaters)/1E6,
              avgcal=weighted.mean(energy_tot_weatout,weight=weight, na.rm=T)
              )

  
##################################
### Food item by zone analysis ###
##################################
  
  # Adjust item names if necessary
  food_items = food_items %>%
    transform(item=ifelse(grepl("Wheat",item)&grepl("PDS",item),"Wheat",ifelse(grepl("Rice",item)&grepl("PDS",item),"Rice",item))) %>%
    transform(item=ifelse(grepl("Other|Baby",item)&food_grp_wdds=="SS-C","Other",item)) %>%
    left_join(zones)

  # Get calorie total per CU by zone, urb-rur, income group, and food group
  food_group_totals = food_items %>%
    group_by(zone, urban, inc_grp, food_grp_wdds) %>%
    summarise_each(.,funs(sum(.*weight*cu_eq, na.rm=TRUE)), energy_tot) %>%
    dplyr::rename(grp_energy=energy_tot) %>%
    left_join(zone_cts) %>% select(-cu_eq_avg) %>%
    transform(grp_energy=grp_energy/tot_cu)
  
  # Get calorie share of each food item in each food group
  food_items_totals = food_items %>%
    group_by(zone, urban, inc_grp, food_grp_wdds, item) %>%
    summarise_each(.,funs(sum(.*weight*cu_eq, na.rm=TRUE)), energy_tot, protein_tot, vita_tot, iron_tot, zinc_tot) %>%
    dplyr::rename(item_energy=energy_tot, item_protein=protein_tot, item_vita=vita_tot, item_iron=iron_tot, item_zinc=zinc_tot) %>%
    left_join(food_group_totals) %>%
    mutate(item_energy_share=item_energy/grp_energy/tot_cu) %>%
    arrange(zone,urban,inc_grp,food_grp_wdds,desc(item_energy_share))
  
  # Derive Shannon diversity index of cereal diet (by zone, urb-rur, and income group)
  for_shannon = food_items_totals %>%
    filter(food_grp_wdds=="SS-C") %>%
    group_by(zone, inc_grp, urban) %>%
    summarise(shannon=-1*sum(item_energy_share*log(item_energy_share)))

  write.csv(for_shannon,'shannon_indices_newdata.csv')

  
#######################
### Weighted t-test ###
#######################  

  library(weights)

  # Do weighted T-tests to see if avg nutrient consumption levels are different in urban and rural, first all-India then by inc_grp
  nat_prot_result<-wtd.t.test(hh_sum$protein_tot, hh_sum$urban, weight=hh_sum$weight)
  nat_zinc_result<-wtd.t.test(hh_sum$zinc_tot, hh_sum$urban, weight=hh_sum$weight)
  nat_iron_result<-wtd.t.test(hh_sum$iron_tot, hh_sum$urban, weight=hh_sum$weight)
  nat_vita_result<-wtd.t.test(hh_sum$vita_tot, hh_sum$urban, weight=hh_sum$weight)
  nat_energy_result<-wtd.t.test(hh_sum$energy_tot, hh_sum$urban, weight=hh_sum$weight)
  
  energy_result<-sapply(unique(hh_sum$inc_grp),function(inc)c(result=nat_energy_result$coefficients))
  energy_result<-data.frame(energy_result)
  prot_result<-sapply(unique(hh_sum$inc_grp),function(inc)c(result=nat_prot_result$coefficients))
  prot_result<-data.frame(prot_result)
  iron_result<-sapply(unique(hh_sum$inc_grp),function(inc)c(result=nat_iron_result$coefficients))
  iron_result<-data.frame(iron_result)
  zinc_result<-sapply(unique(hh_sum$inc_grp),function(inc)c(result=nat_zinc_result$coefficients))
  zinc_result<-data.frame(zinc_result)
  vita_result<-sapply(unique(hh_sum$inc_grp),function(inc)c(result=nat_vita_result$coefficients))
  vita_result<-data.frame(vita_result)
  

####################  
### Plot results ###
####################  
  
  require(ggplot2)
  
  hh_sum = hh_sum %>%
    mutate(loc_inc=paste0(inc_grp,urban)) %>%
    arrange(loc_inc)
  
  # T-test result
  
  # Calorie
  pdf(file = paste0(workdir, "Figures/Average micronutrient consumption - Cal.pdf"), width = 12, height = 10)
  p_val <- as.matrix(energy_result[row.names(energy_result)=="result.p.value",])
  p_val <- data.frame(x=1:4, y=c(2500, 2600, 2700, 2900), pval= paste0("p=",format(round(as.numeric(p_val), 3), nsmall=3)), stringsAsFactors=FALSE)
  ggplot(hh_sum[hh_sum$energy_tot<10000,],aes(x=factor(inc_grp), y=energy_tot, fill=factor(urban)))+
    geom_rect(xmin=-Inf, xmax=Inf, ymin=1813, ymax=2525, fill='pink')+
    geom_boxplot(position=position_dodge(width=0.8), aes(weight=weight), outlier.colour=NA, coef = 0)+
    scale_fill_manual(name="Urban-Rural", labels=c("Rural", "Urban"), values = c("white", "darkgrey")) +
    ylim(1500,3000) + ylab("Calories/day per CU") + xlab("Income Group") + scale_x_discrete(labels=c("1=Lowest", "2", "3", "4=Highest")) +
    theme(text=element_text(size=20), legend.title=element_blank())
  dev.off()
  
  # Protein
  pdf(file = paste0(workdir, "Figures/Average micronutrient consumption - Protein.pdf"), width = 10, height = 10)
  p_val <- as.matrix(prot_result[row.names(prot_result)=="result.p.value",])
  p_val <- data.frame(x=1:4, y=c(70, 75, 80, 85), pval= paste0("p=",format(round(as.numeric(p_val), 3), nsmall=3)), stringsAsFactors=FALSE)
  ggplot(hh_sum[hh_sum$energy_tot<10000,],aes(x=factor(inc_grp), y=protein_tot, fill=factor(urban)))+
    geom_rect(xmin=-Inf,xmax=Inf,ymin=40,ymax=60, fill='pink')+
    geom_boxplot(position=position_dodge(width=0.8), aes(weight=weight), outlier.colour=NA, coef = 0)+
    scale_fill_manual(guide=FALSE, values = c("white", "darkgrey")) +
    ylim(30,90) + ylab("Protein g/day per CU") + xlab("Income Group") + scale_x_discrete(labels=c("1=Lowest", "2", "3", "4=Highest")) +
    theme(text=element_text(size=20))
  dev.off()
  
  # Iron
  pdf(file = paste0(workdir, "Figures/Average micronutrient consumption - Iron.pdf"), width = 10, height = 10)
  p_val <- as.matrix(iron_result[row.names(iron_result)=="result.p.value",])
  p_val <- data.frame(x=1:4, y=c(20, 21, 22, 24), pval= paste0("p=",format(round(as.numeric(p_val), 3), nsmall=3)), stringsAsFactors=FALSE)
  ggplot(hh_sum[hh_sum$energy_tot<10000,],aes(x=factor(inc_grp), y=iron_tot, fill=factor(urban)))+
    geom_rect(xmin=-Inf,xmax=Inf,ymin=17,ymax=23, fill='pink')+
    geom_boxplot(position=position_dodge(width=0.8), aes(weight=weight), outlier.colour=NA, coef = 0)+
    scale_fill_manual(guide=FALSE, values = c("white", "darkgrey")) +
    ylim(5,30) + ylab("Iron mg/day per CU") + xlab("Income Group") + scale_x_discrete(labels=c("1=Lowest", "2", "3", "4=Highest")) +
    theme(text=element_text(size=20))
  dev.off()
  
  # Vit A
  pdf(file = paste0(workdir, "Figures/Average micronutrient consumption - vita.pdf"), width = 10, height = 10)
  p_val <- as.matrix(vita_result[row.names(vita_result)=="result.p.value",])
  p_val <- data.frame(x=1:4, y=c(750, 750, 800, 900), pval=paste0("p=",format(round(as.numeric(p_val), 3), nsmall=3)), stringsAsFactors=FALSE)
  ggplot(hh_sum[hh_sum$energy_tot<10000,],aes(x=factor(inc_grp), y=vita_tot, fill=factor(urban)))+
    geom_hline(yintercept=600)+
    geom_boxplot(position=position_dodge(width=0.8), aes(weight=weight), outlier.colour=NA, coef = 0)+
    scale_fill_manual(guide=FALSE, values = c("white", "darkgrey")) +
    ylim(0,1000) + ylab("Vit A mcg/day per CU") + xlab("Income Group") + scale_x_discrete(labels=c("1=Lowest", "2", "3", "4=Highest")) +
    theme(text=element_text(size=20))
  dev.off()
  
  # Zinc
  pdf(file = paste0(workdir, "Figures/Average micronutrient consumption - zinc.pdf"), width = 10, height = 10)
  p_val <- as.matrix(zinc_result[row.names(zinc_result)=="result.p.value",])
  p_val <- data.frame(x=1:4, y=c(13, 13, 13, 14), pval= paste0("p=",format(round(as.numeric(p_val), 3), nsmall=3)), stringsAsFactors=FALSE)
  ggplot(hh_sum[hh_sum$energy_tot<10000,],aes(x=factor(inc_grp), y=zinc_tot, fill=factor(urban)))+
    geom_rect(xmin=-Inf,xmax=Inf,ymin=9,ymax=12, fill='pink')+
    geom_boxplot(position=position_dodge(width=0.8), aes(weight=weight), outlier.colour=NA, coef = 0)+
    scale_fill_manual(guide=FALSE, values = c("white", "darkgrey")) +
    ylim(5,15) + ylab("Zinc mg/day per CU") + xlab("Income Group") + scale_x_discrete(labels=c("1=Lowest", "2", "3", "4=Highest")) +
    theme(text=element_text(size=20), plot.background = element_rect(fill = "transparent",colour = NA))
  dev.off()
  
  # Rice/wheat share vs iron deficiency plot
  hh_anal$inc_grp <- factor(hh_anal$inc_grp)
  p <- ggplot(hh_anal,aes(Rice_sh, iron_def_pct))+geom_point(aes(colour=zone, shape=inc_grp), size=3) +
    ylab("%POP in Iron Deficiency")+xlab("Calorie Share of Rice among All Cereals") +
    scale_shape_discrete(name="Income group", labels=c("1=Lowest", "2", "3", "4=Highest")) +
    scale_colour_discrete(name="Region", labels=c("East", "North", "South", "West"))
  pdf(file = paste0(workdir, "Figures/Share of population with iron deficiency - Rice.pdf"), width = 7, height = 10)
  p
  dev.off()
  
  p<-ggplot(hh_anal,aes(wheat_sh, iron_def_pct))+geom_point(aes(colour=zone, shape=inc_grp), size=3) +
    ylab("%POP in iron deficiency")+xlab("Calorie Share of Wheat among All Cereals") + theme(legend.position="none")
  pdf(file = paste0(workdir, "Figures/Share of population with iron deficiency - Wheat.pdf"), width = 7, height = 10)
  p
  dev.off()

  # Bar graph showing cereal shares by zone and income group
  plot_cer_shs = food_items_totals %>%
    filter(food_grp_wdds=="SS-C", (inc_grp==1 | inc_grp==4)) %>%
    transform(item=ifelse(grepl("Jowar",item),"Jowar",
                      ifelse(grepl("Ragi",item),"Ragi",
                             ifelse(grepl("Bajra",item),"Bajra",
                                    ifelse(grepl("Maize",item),"Maize",
                                           ifelse(grepl("Small millets",item),"Small millets",
                                                  ifelse(grepl("substitutes",item),"Cereal subs",
                                                         ifelse(grepl("Suji",item),"Suji",
                                                                ifelse(grepl("Sewai",item),"Sewai",
                                                                       ifelse(grepl("Barley",item),"Barley",item))))))))))

  # Plot cereal calorie shares by zone for urban/rural richest/poorest
  q <- ggplot(plot_cer_shs,aes(item,item_energy_share))+geom_bar(aes(fill=zone),position="dodge",stat="identity")
  q + facet_grid(urban ~ inc_grp,labeller=labeller(urban=as_labeller(u_r),inc_grp=as_labeller(inc_grp_labels)))+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(y="Cereals calorie shares",x="")
  
  # Plot food grp calorie shares
  u_r <- c(`0`="Rural",`1`="Urban")
  inc_grp_labels <- c(`1`="Lowest income",`4`="Highest income")       
  r <- ggplot(food_group_totals%>%filter(inc_grp==1|inc_grp==4),aes(food_grp_wdds,grp_energy))+geom_bar(aes(fill=zone),position="dodge",stat="identity")
  r + facet_grid(urban ~ inc_grp,labeller=labeller(urban=as_labeller(u_r),inc_grp=as_labeller(inc_grp_labels)))+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(y="Food group calories per CU",x="")
  

#########################  
### Save output files ###
#########################  
  
  write.csv(hh_anal_out,'NSS_cu_diagnostics_bystate_inc.csv')
  write.csv(food_group_totals,'NSS_cu_foodgrp_diagnostics.csv')
  write.csv(food_nutrients,'NSS_cu_food_nutrients.csv')
  write.csv(inc_grp_tots,'inc_grp_cu_tots.csv')
