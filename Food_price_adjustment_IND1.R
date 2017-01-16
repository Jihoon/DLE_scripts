# Key items from \SharePoint\WS2 - Documents\Analysis\Food\Copy of NSS_food_diagnostics.xlsx
fooditems <- c("Wheat/atta, non-PDS", "Bajra and its products", "Wheat/atta, PDS", 
           "Rice, non-PDS", "Rice, PDS", "Maize and its products", "Ragi and its products", 
           "Jowar and its products", "Bajra and its products", "Wheat/atta, non-PDS", 
           "Wheat/atta, PDS", "Jowar and its products", "Eggs", "Ragi and its products", 
           "Baby food", "Maize and its products", "Rice, non-PDS", "Small millets and their products", 
           "Wheat/atta, non-PDS", "Eggs", "Bajra and its products", "Rice, non-PDS", 
           "Rice, PDS", "Jowar and its products", "Wheat/atta, PDS", "Maize and its products", "Milk, liquid")
fooditems <- unique(fooditems)
fooditems <- fooditems[-10]  # Remove 'baby food'

xlcFreeMemory()

load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_HH_All.Rda")
load(file="H:/MyDocuments/IO work/DLE_scripts/Saved tables/IND1_Food_All.Rda")
svy <- "IND1"
# IND_Food_org <- selectDBdata(SURVEY, ID, ITEM, VAL_TOT, QTY_TOT, UNIT, CODE, tables=c(paste0(svy, '_FOOD'))) 
# IND_HH_region <- selectDBdata(SURVEY, ID, REGION, URBAN, tables=c(paste0(svy, '_HH'))) 

IND_Food <- IND_FOOD_Alldata %>% select(survey, id, item, val_tot, qty_tot, unit, code) #IND_Food_org
IND_HH_region <- IND_HH_Alldata %>% select(survey, id, region, urban)

names(IND_Food)[2] <- "hhid"
names(IND_HH_region)[2] <- "hhid"
IND_Food <- filter(IND_Food, item %in% fooditems)

attach(IND_Food)
IND_Food$unit_price <- val_tot / qty_tot

items <- unique(IND_Food[,c("code", "item")])
items <- items %>% arrange(code)

IND_Food <- merge(IND_Food, IND_HH, by = "hhid", all.x=TRUE, all.y=FALSE)
# IND_table(IND_Food$item)

mean_p <- IND_Food %>% group_by(code) %>% summarise(mean_p = weighted.mean(unit_price, weight, na.rm = TRUE))
IND_Food <- IND_Food %>% left_join(mean_p) %>% mutate(val_totadj = qty_tot * mean_p, scale_p = unit_price/mean_p)
IND_Food$val_totadj <- val_tot * IND_Food$unit_price
IND_Food <- IND_Food %>% left_join(IND_HH_region)

detach(IND_Food)

# Just to check the PDS price differences across region
mean_pds_price <- IND_Food %>% filter(code == 101 | code ==107) %>% group_by(region, code) %>% 
  summarise(mean_p = weighted.mean(unit_price, weight, na.rm = TRUE)) %>% spread(code, mean_p)


# Plot price distributions by item
layout(matrix(1:11, ncol=1))
par(oma = c(1, 0, 1, 0), mar= c(1, 0, 0, 0))
for (i in sort(unique(IND_Food$code))) {
  a <- IND_Food %>% filter(code == i)
  hist(a$unit_price, seq(0,100,0.05), main=NULL, xlim=c(0, 5))    
  title(paste0(items[items$code==i,2], " (", i,")"), line=-2)
}

# Boxplot of price ratio to the mean
layout(matrix(1:11, ncol=1))
par(oma = c(1, 0, 1, 0), mar= c(1, 2, 0, 0))
for (i in sort(unique(IND_Food$code))) {
  a <- IND_Food %>% filter(code == i)
  boxplot(scale_p ~ decile, data=a, outline=FALSE, range=1.5, axes=FALSE, ylim=c(0,4))
  axis(side = 2, at=seq(0,4))
  title(paste0(items[items$code==i,2], " (", i,")"), line=-1)
}

dev.off()

IND_Food_update <- IND_Food_org %>% left_join(IND_Food[,c("hhid", "code", "scale_p")], by=c("id"="hhid", "code"))
IND_Food_update$scale_p[is.na(IND_Food_update$scale_p)] <- 1
IND_Food_update$val_tot_adj <- IND_Food_update$val_tot / IND_Food_update$scale_p
IND_Food_update <- rename(IND_Food_update, price_scale=scale_p)

# Adding the adj val column into IND1_food
scode <- "IND1"  # "BRA0"
# Reference _FOOD and _OTHCON tables to the CAT_MAP table
sql <- "ALTER TABLE IND1_FOOD ADD VAL_TOT_ADJ NUMBER"
tryCatch({dbSendUpdate(conn, sql)}, error=function(e){cat("SKIPPED ERROR :",conditionMessage(e), "\n")})

# Create a separate table with adjusted price column
writeDF2Oracle(IND_Food_update[,c("survey", "id", "item", "code", "price_scale", "val_tot_adj")], 
               "IND1_FOOD_PRC", primary.keys=c("survey", "id", "item", "code"))

# Update original FOOD table by joining the new price table
sql <- "UPDATE (SELECT a.VAL_TOT_ADJ as OLD, b.VAL_TOT_ADJ as NEW 
FROM IND1_FOOD a LEFT JOIN IND1_FOOD_PRC b 
ON (a.survey=b.survey AND a.id=b.id AND a.item=b.item AND a.code=b.code)) t 
SET t.OLD = t.NEW"
tryCatch({dbSendUpdate(db_conn, sql)}, error=function(e){cat("SKIPPED ERROR :",conditionMessage(e), "\n")})

sql <- "ALTER TABLE IND1_FOOD RENAME COLUMN VAL_TOT TO VAL_TOT_ORG" # ALTER TABLE IND1_FOOD DROP COLUMN VAL_TOT_ADJ"
tryCatch({dbSendUpdate(db_conn, sql)}, error=function(e){cat("SKIPPED ERROR :",conditionMessage(e), "\n")})

sql <- "ALTER TABLE IND1_FOOD RENAME COLUMN VAL_TOT_ADJ TO VAL_TOT" # ALTER TABLE IND1_FOOD DROP COLUMN "
tryCatch({dbSendUpdate(db_conn, sql)}, error=function(e){cat("SKIPPED ERROR :",conditionMessage(e), "\n")})

# Price normalization index by decile
IND_Food_org <- selectDBdata(SURVEY, ID, ITEM, VAL_TOT_ORG, VAL_TOT, QTY_TOT, UNIT, CODE, tables=c(paste0(svy, '_FOOD')))
names(IND_Food_org)[2] <- "hhid"
IND_Food_org <- filter(IND_Food_org, item %in% fooditems) %>% left_join(IND_HH)
IND_Food_org <- IND_Food_org %>% mutate(price_scale = val_tot_org / val_tot)

p_normal_idx <- IND_Food_org %>% group_by(decile, code) %>% summarise(mean_idx = weighted.mean(price_scale, weight, na.rm = TRUE)) %>% 
  left_join(food_group) %>% select(-food_grp) %>% filter(!is.na(decile)) %>% filter(code!=120)

ggplot(p_normal_idx, aes(x=decile, y=mean_idx, shape = item)) +   
  geom_line(aes(group = item)) + geom_point(size=4) +
  scale_shape_manual(values = c(0:9)) +
  labs(x="Decile", y="Price normalization index") + 
  scale_x_discrete(labels=1:10)
