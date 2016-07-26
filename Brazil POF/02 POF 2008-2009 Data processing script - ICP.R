# Process Brazil POF 2008-2009

#--------------------------
#--------------------------

# MANUAL SETTINGS:

# Specify survey code (used in Oracle DB) and survey name (used to lookup information in master documentation Excel file)
survey.code = "BRA0"
survey.name = "BRA POF 2008-2009 - ICP"

# Set working directory for survey
setwd("P:/ene.general/DecentLivingEnergy/Surveys/Brazil/POF 2008-2009 ICP/")

# Run all .R setup scripts in specified /Scripts directory
setup.scripts = list.files("P:/ene.general/DecentLivingEnergy/Surveys/Scripts/", pattern="*.R$", full.names=T, ignore.case=T)
invisible(lapply(setup.scripts, source))

# File path to .RData object containing monthly PPP adustment factors for desired base year
# Extract uniform PPP adjustment factor to apply to all values
# Annualized values in survey already inflated/deflated to January 2009 prices in local currency
ppp_path = "P:/ene.general/DecentLivingEnergy/Surveys/Consumer Prices/Monthly PPP adjustment factors for base year 2010.RData"
ppp_fact = filter(readRDS(ppp_path), iso3=="BRA", year==2009, month==1)$ppp_fact

# Excel file linking survey's item codes to item names
ce_code = read_excel("Documentation/BRA POF 2008-2009 CE Codes ICP.xlsx") %>% data.table(key="code")

# Excel file linking state numeric codes to state name
geo_code = read_excel("Documentation/BRA POF 2008-2009 Geographic Codes.xlsx")

#--------------------------
#--------------------------

# Function to convert character vectors in data frame to numeric (via type.convert), when possible
char2num = function(d) {
  d[] = lapply(d, function(x) if (class(x)=="character") type.convert(x, as.is=T) else x)  # Convert character to numeric, if possible
  return(d)
}

#--------------------------
#--------------------------

# Process household data
# Note: Uses post-stratification data to assign urban status

load("Data/t_domicilio_s.rda")
d = char2num(t_domicilio_s)

load("Data/poststr.rda")
post = char2num(poststr)

# Information used to define 'estrato' variable values that identify rural housholds
estrato.cats <- c( 7 , 3 , 9 , 3 , 9 , 4 , 6 , 13 , 10 , 24 , 9 , 10 , 16 , 9 , 8 , 22 , 28 , 10 , 31 , 31 , 19 , 14 , 19 , 9 , 11 , 18 , 8 )
names(estrato.cats) <- unique(d$cod_uf)

d1 = d %>%
  #mutate(id=paste0(cod_uf*1e6, num_seq*1e3, num_dv*100, cod_domc)) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(control=as.integer(paste0(cod_uf, str_pad(num_seq,3,pad=0), num_dv))) %>%  # Variable used to merge with post-stratification object
  left_join(post) %>%  # Join information from the post-stratification table
  rename(weight=fator_expansao2, income=renda_total) %>%  # fator_expansao2 is identical to post-stratifying weights using Census 2010 data
  mutate(income=income*12) %>%  # Original income variable is monthly HH income; this converts to annual
  mutate(date_int=as.Date("2008-05-19")+(perd_cod_p_visit_realm_em-1)*7) %>% # Raw variable gives number of weeks beginning May 19 2008
  left_join(geo_code) %>%   # cod_uf is numeric link to state name
  rename(region=reg1) %>%
  mutate(urban=ifelse(estrato >= estrato.cats[match(cod_uf, names(estrato.cats))] , 0 , 1 )) %>%  # If `estrato` value greater than or equal to the corresponding `estrato.cats` value, household is rural. Otherwise, urban.
  genvar(dwell_type, cod_tipo_domc, c('House','Apartment','Room'), 1:3) %>%
  genvar(dwell_status, cod_cond_ocup, c('Owned outright','Owned with mortgage','Rent-free (employer paid)','Rent-free (non-employer paid)','Other','Rent'), 1:6) %>%
  genvar(wall_mat, cod_material_parede, c('Brick','Wood','Thatching (sticks)','Wood','Straw','Other'), 1:6) %>%
  genvar(roof_mat, cod_material_cobertura, c('Tile','Concrete','Wood','Metal','Wood','Straw','Other'), 1:7) %>%
  genvar(floor_mat, cod_material_piso, c('Carpet','Ceramic, stone, tile','Wood','Concrete','Wood','Dirt','Other'), 1:7) %>%
  mutate(rooms=pmax(qtd_comodos_domc-qtd_banheiros,1)) %>%  # Total rooms minus number of bathrooms, minimum value of 1
  mutate_if(rede_15==1, elec_source="Electricity grid") %>%
  mutate_if(diesel_16==1, elec_source="Diesel generator") %>%
  mutate_if(solar_16==1, elec_source="Solar PV") %>%
  mutate_if(eolica_16==1, elec_source="Wind power") %>%
  mutate_if(agua_16==1, elec_source="Hydropower") %>%
  mutate_if(biodiesel_16==1, elec_source="Biodiesel") %>%
  mutate_if(sistema_misto_16==1, elec_source="Multiple sources") %>%
  mutate_if(outra_forma_17==1, elec_source="Other") %>%
  mutate_if(is.na(elec_source), elec_source="No electricity") %>%
  mutate(elec_access=as.integer(elec_source!="No electricity")) %>%
  genvar(water_source, cod_abast_agua, c('Municipal','Well or spring','Other'), 1:3) %>%
  mutate_if(cod_agua_comodo==1, water_source=paste0(water_source, ", piped")) %>%
  mutate_if(cod_agua_comodo==2, water_source=paste0(water_source, ", not piped")) %>%
  genvar(toilet_disposal, cod_esgoto_sanit, c('Sewage system','Septic tank','Rudimentary cesspit','Open ditch','River/lake/sea','Other','None'), 1:7) %>%
  mutate_if(gas_18==1, stove_type="Gas") %>%  # This could be LPG (most common) or piped gas
  mutate_if(lenha_18==1, stove_type="Wood") %>%
  mutate_if(carvao_18==1, stove_type="Coal") %>%
  mutate_if(energia_eletrica_18==1, stove_type="Electric") %>%
  mutate_if(outro_18==1, stove_type="Other") %>%
  select(id, weight, date_int, region, urban, income, dwell_type, dwell_status, rooms, wall_mat, roof_mat, floor_mat, elec_access, elec_source, water_source, toilet_disposal, stove_type)

#--------------------------
#--------------------------

### Process person data

load("Data/t_morador_s.rda")

d2 = char2num(t_morador_s) %>%
  # mutate(id=paste0(cod_uf*1e6, num_seq*1e3, num_dv*100, cod_domc)) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(cid=paste0(id,cod_unid_consumo)) %>%
  mutate(pid=paste0(id,cod_unid_consumo,num_informante)) %>%
  rename(weight=fator_expansao2, age=idade_anos, pheight=altura_imputado, pweight=peso_imputado) %>%
  genvar(male, cod_sexo, c(1,0), c(1,2)) %>%
  genvar(rel, cod_rel_pess_refe_uc, c('Head','Spouse','Child','Other relative','Other non-relative','Tenant','Domestic employee','Relative of domestic employee'), 1:8) %>%
  genvar(head, rel, T, "Head", other=F) %>%
  genvar(race, cod_cor_raca, c('White','Black','Yellow','Brown','Indigenous','Do not know'), c(1:5,9)) %>%
  genvar(educ_level, cod_nivel_instr, c('Nursery school','Preschool','Child literacy class','Adult literacy class','Old primary','Old gymnasium','Old classic, scientific','Regular basic education','Young and adult education for elementary school','Regular high school','Young and adult education for high school','Technological college','Pre-college','Undergraduate','Professional degree','Masters or doctorate'), 1:16) %>%
  left_join(educ_years) %>% # Add years of schooling completed
  mutate(student=as.integer(cod_curso_freq>0)) %>%
  mutate(earner=as.integer(cod_sit_receita==1)) %>%
  data.table(key = "id") %>%
  select(id, cid, pid, weight, head, rel, male, age, race, educ_level, educ_years, student, earner, pheight, pweight)

# Create household summary variables from person records
pp = d2[,list(
  hh_size = .N,
  male = male[head],
  age = age[head],
  race = race[head],
  educ_level = educ_level[head],
  educ_years = educ_years[head],
  minor = sum(age<18), # Number of minors (less than 18 years old)
  student = sum(student),
  earner = sum(earner),
  male_adult = sum(age>=18 & male==1),
  male_minor = sum(age<18 & male==1),
  age_adult = mean(age[age>=18]),  # Mean age of people over 18 (can be NA in case of minor head of household)
  age_minor = mean(age[age<18])  # Mean age of people under 18 (will be NA if no minors in household)
), by=id]

# If multiple household heads specified, retain information for eldest head only (male if age is a tie)
#check = pp[,list(n = .N), by=id] %>% filter(n>1)
pp = pp %>%
  arrange(id, -age, -male) %>%
  unique(by="id")

#--------------------------
#--------------------------

# Combine household level variables
hh = Reduce(function(...) left_join(...), list(d1,pp))

#--------------------------
#--------------------------

### Process household ownership of assets

load("Data/t_inventario_s.rda")

# Create data frame giving concordance between 'cod_item' code and common asset name
cod_item = c(101,201,301,401,501,601,701,801,901,1001,1101,1201,1301,1401,1501,1601,1701,1801,1901,2001,2101,2201,2301,2401,2501,2601,2701,2801,2901,3001,3101,3201) 
item = c('Stove','Freezer','Refrigerator, single door','Refrigerator, double door','Shower electric water heater','Blender',
         'Food processor','Grill','Vacuum cleaner','Toaster oven','Electric iron','Washing machine','Television, color',
         'Television, black and white','Stereo/Hi-Fi','Radio','Air conditioning','Electric fan','Sewing machine','Water filter',
         'Automobile','Bicycle','Motorcycle','Computer','Water purifier','Microwave','Satellite dish','DVD player','Clothes dryer',
         'Stand mixer','Hair dryer','Dishwasher')
item_names = data.frame(cod_item, item)

# Merge asset names, create variables for: number of asset owned, year of most recent acquisition, condition (new/used) of most recent acquisition
d = char2num(t_inventario_s) %>%
  #mutate(id=paste0(cod_uf*1e6, num_seq*1e3, num_dv*100, cod_domc)) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  left_join(item_names) %>%
  rename(code=cod_item) %>%
  mutate(n=ifelse(qtd_item==99, NA, qtd_item)) %>%
  mutate(year=ifelse(ano_aquisicao==9999, NA, ano_aquisicao)) %>% 
  genvar(cond, cod_estado, c("New","Used"), c(1,3)) %>%
  select(id, code, item, n, year, cond) %>%
  data.table(key=c("id","item"))

# In cases where multiple consumer units in household own an asset, need to aggregate at the household level
asset = d[,list(
  num = sum(n),  # Total number of each asset owned by household
  acq_cond = cond[which.max(year)],  # Condition (new/used) of asset when most recently acquired
  acq_year = max(year)  # Year when asset was most recently acquired
), by=key(d)]

# Gather data and add appropriate "unit" variable
asset = asset %>%
  mutate(pos=as.integer(num>0)) %>%
  mutate_if(!is.na(acq_year), pos=1) %>%
  filter(!is.na(pos)) %>%
  select(id, item, pos, num, acq_year, acq_cond)

#--------------------------
#--------------------------

# Load food expenditures (weekly diary)

load("Data/t_caderneta_despesa_s.rda")

food = char2num(t_caderneta_despesa_s) %>%
  #mutate(id=paste0(cod_uf*1e6, num_seq*1e3, num_dv*100, cod_domc)) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(code=paste0(prod_num_quadro_grupo_pro, str_pad(cod_item,width=5,pad=0))) %>%
  mutate(period=7, kg=quant_kg, unit="kg") %>%  # Food consumption reported in one-week diary; all quantities are kg
  mutate(value=val_despesa_corrigido*ppp_fact*365/period) %>%  # Inflation-adjusted value (Jan 2009) converted to $PPP in specified base year
  mutate(kg=kg*365/period) %>% 
  mutate_if(kg==0, kg=NA) %>%  # This ensures that NA is returned for item where quantities are not possible
  data.table(key="code") %>%
  merge(ce_code) %>%  # Will drop any codes in 'd' not found in 'ce_code'
  group_by(id,item,unit) %>%
  summarise(val_tot=sum(value), qty_tot=sum(kg)) %>%  # Sum values by household and item
  filter(val_tot>0 | is.na(val_tot))

#--------------------------
#--------------------------

# Combine individual non-food expenditure files and process as single data frame
load("Data/t_despesa_90dias_s.rda")  # 3-month housing
load("Data/t_despesa_12meses_s.rda")  # 12-month housing
load("Data/t_despesa_individual_s.rda")  # Name???
load("Data/t_despesa_veiculo_s.rda")  # Vehicle expenditures
load("Data/t_outras_despesas_s.rda")  # Other expenditures
load("Data/t_aluguel_estimado_s.rda")  # Other expenditures

d = bind_rows(
  char2num(t_despesa_90dias_s),
  char2num(t_despesa_12meses_s),
  char2num(t_despesa_individual_s),
  char2num(t_despesa_veiculo_s),
  char2num(t_outras_despesas_s),
  char2num(t_aluguel_estimado_s)
)

cons = d %>%
  #mutate(id=paste0(cod_uf*1e6, num_seq*1e3, num_dv*100, cod_domc)) %>%
  mutate(id=as.character(cod_uf*1e6 + num_seq*1e3 + num_dv*100 + cod_domc)) %>%
  mutate(code=paste0(str_pad(num_quadro,width=2,pad=0), str_pad(cod_item,width=5,pad=0))) %>%
  mutate(period=365/fator_anual) %>%
  mutate(value=val_despesa_corrigido*ppp_fact*365/period, qty=quantidade_final*365/period) %>%  # Inflation-adjusted value (Jan 2009) converted to $PPP in specified base year
  data.table(key="code") %>%
  merge(ce_code) %>%  # Will drop any codes in 'd' not found in 'ce_code'
  group_by(id,item) %>%
  summarise(val_tot=sum(value), qty_tot=sum(qty)) %>%  # Sum and annualize values
  filter(val_tot>0 | is.na(val_tot))

# Extract FUEL data
fuel.items = c('Electricity','Pipeline natural gas','LPG','Kerosene','Sawdust','Ethanol, non-transport','Diesel, non-transport','Gasoline, non-transport','Coal','Firewood','Ethanol, transport','Gasoline, transport','Diesel, transport','CNG, transport')
fuel.units = c('kWh','m3','kg','l','kg','l','l','l','kg','kg','l','l','l','l')  # COAL AND FIREWOOD UNITS (assuming kg for now...); CNG, assume liter???
fuel.data = data.frame(item=fuel.items, unit=fuel.units, stringsAsFactors=F)  
fuel.data  # Check alignment of items and units...
fuel = cons %>% 
  merge(fuel.data, by='item') %>% 
  rename(fuel = item)

# Extract OTHCON data
# This excludes fuel items AND any tax-related items
othcon = cons %>% 
  filter(item %in% fuel.data$item==FALSE) %>% 
  filter(!grepl('taxes', tolower(item))) %>% 
  select(id, item, val_tot)

#--------------------------
#--------------------------

# Convert fuel data 'unit' variable and associated quantities to standardized units (e.g. kg, liter, kWh, etc.)
# Uses custom function 'standardize_unit' (built upon conv_unit in package:birk)
temp = lapply(unique(fuel$unit), function(x) {
  if (!is.na(x)) {
    conv = standardize_unit(1, x)  # Determine conversion factor for given unit
    dset = fuel %>%
      filter(unit==x) %>%
      mutate(unit=conv$unit, qty_tot=qty_tot*conv$x)  # Convert all unit values to standard unit AND adjust quantity values using conversion factor
  } else {
    dset = filter(fuel, is.na(unit))
  }
  return(dset)
})
fuel = bind_rows(temp)

#--------------------------
#--------------------------

# Calculate consumption and expenditure totals for each household and add results to household summary data

# Use of custom function 'bind_rows2' is analagous to gdata::smartbind but faster
# Argument fill=0 ensures correct treatment of any explicit NA values when calculating totals below
temp = bind_rows2(list(food, fuel, othcon), fill=0)

# Consumption includes all consumption values
con_total = temp %>%
  group_by(id) %>%
  summarise(consumption=sum(val_tot))

# Expenditure EXCLUDES imputed rents
exp_total = temp %>%
  filter(item %in% "Imputed rent"==FALSE) %>%
  group_by(id) %>%
  summarise(expenditure=sum(val_tot))

# Income is pre-calculated in the household data; convert it to PPP
inc_total = hh %>%
  select(id, income) %>%
  mutate(income = income*ppp_fact) # Convert original, deflated income to $PPP in specified base year

# Add consumption and expenditure totals to HH file
hh = hh %>% 
  select(-one_of(c("income","expenditure","consumption"))) %>%
  left_join(inc_total) %>%
  left_join(con_total) %>%
  left_join(exp_total)

gc()

#--------------------------
#--------------------------

# Construct alternative 'elec_accessX' variables
hh = hh %>%
  left_join(fuel %>% 
              filter(fuel=="Electricity") %>% 
              mutate(elec_consume=val_tot>0) %>% 
              select(id, elec_consume)
  ) %>%
  mutate_if(is.na(elec_consume), elec_consume=F) %>%
  mutate(elec_access1 = as.integer(elec_access==1),
         elec_access2 = as.integer(elec_access==1 | elec_consume==T))

#--------------------------
#--------------------------

# WRITE RESULTS TO DISK AND COMMIT DATA TO ORACLE DB
# NOTE: 'survey.code' object in global environment is used by both saveSurvey2Disk() and commitSurvey2DB()

# Final safety check before writing to disk and database (ensure unique household ID's)
if (nrow(hh) != length(unique(hh$id))) {
  message("STOP!!! Number of unique household ID's does NOT match number of HH records!!!")
} else {
  commitResults(survey.code)
}

#--------------------------
# END
#--------------------------