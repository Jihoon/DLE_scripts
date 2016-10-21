igdx("C:/GAMS/win64/24.4")
setwd(paste0(path.package('gdxrrw'),'/tests'))
info <- gdxInfo("trnsport", dump = F, returnDF = T)

setwd("H:/MyDocuments/IO work/DLE_scripts")

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_HH_All.Rda")    #IND_HH_Alldata
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_Food_All.Rda")  #IND_FOOD_Alldata
load("./Saved tables/IND_intensities.Rda")

### General

# Price per item
food_price <- IND_FOOD_Alldata %>% filter(!is.na(qty_tot)) %>% select(item, code, unit, val_tot, qty_tot) %>%
  mutate(unit_price = val_tot/qty_tot) %>% group_by(item, code) %>% 
  summarise(avg_price = mean(unit_price, na.rm=T), sd=sd(unit_price, na.rm=T), unit=first(unit)) %>% arrange(item)

# Nutritional values
nutrients <- read.csv("C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/NSS_food_nutrients.csv", header=TRUE)[,-1]
food_summary <- nutrients %>% left_join(food_price) %>% filter(!is.na(code)) %>% arrange(item)

# E intensity
idx_food <- 1:45
mat <- (CES_ICP_IND)[,idx_food]
mean_food_int <- apply(IND_intensity, 2, mean)[idx_food]
library(tibble)
inten_food <- data.frame(intensity=mat%*%mean_food_int) %>% rownames_to_column("code") %>% filter(intensity>0) %>% mutate(code=as.numeric(code))
food_summary <- food_summary  %>% left_join(inten_food)

### HH specific

IND_HH_Alldata <- IND_HH_Alldata %>% 
  mutate(m_adult=male_adult, f_adult=(hh_size-minor-male_adult), m_child=male_minor, f_child=minor-male_minor)
hh1 <- IND_HH_Alldata[1,]
hh_food <- IND_FOOD_Alldata %>% filter(id==hh1$id & !is.na(qty_tot)) %>% select(item, unit, val_tot, qty_tot)

# Food intake
food_summary_hh <- food_summary %>% left_join(hh_food) %>% select(-sd)
ref_intake <- t(food_summary_hh$qty_tot/hh1$hh_size)[c(1,1,1,1),]

# food_items <- list(c("cereals", "fruits", "vegetables", "veg-protein", "meat", "dairy", "oil"))  # Spaceholder
food_items <- list(food_price$item)
pop_groups <- list(c("male-adult", "female-adult", "male-child", "female-child"))
nut_groups <- list(c("protein", "zinc", "iron"))

# Reformat for wgdx
pr <- cbind(1:139, food_summary$avg_price)
c <- cbind(1:139, food_summary$energy)
e <- cbind(1:139, food_summary$intensity)
nn <- cbind(1:4, as.numeric(hh1 %>% select(m_adult, f_adult, m_child, f_child)))

# Define the GAMS entities
f  <- list(name='f',  type='set', uels=food_items, ts='Food groups')
a  <- list(name='a',  type='parameter', dim=2, form='full', uels=c(food_items, nut_groups), 
           val= as.matrix(food_summary %>% select(protein, iron, zinc)), 
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


wgdx("C:/Users/min/SharePoint/WS2 - Documents 1/Analysis/Food/diet_gms/DLE_data.gdx", f, a, pr, c, e, r, nn)

setwd(paste0(path.package('gdxrrw'),'/extdata'))
b<-rgdx("trnsport.gdx", list(name="d"))
b<-rgdx("ls01GamsSolu.gdx", list(name="fitted"))

fnData <- "eurodist.gdx"
fnSol <- "eurosol.gdx"
invisible(suppressWarnings(file.remove(fnData,fnSol)))
n <- attr(eurodist,"Size")
cities <- attr(eurodist,"Labels")
uu <- list(c(cities))
dd <- as.matrix(eurodist)

clst <- list(name='cities',type='set',uels=uu,
             ts='cities from stats::eurodist')
dlst <- list(name='dist', type='parameter', dim=2, form='full',
             ts='distance', val=dd, uels=c(uu,uu))
wgdx(fnData, clst, dlst)
gams('tsp_dse.gms')
