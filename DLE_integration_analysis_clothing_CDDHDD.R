library(readr)
hdd.path <- "C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Analysis/Climate impacts and space conditioning/Data/output_by_state_BRAINDZAF/"


svy = "IND1"
IND_HH.CDDHDD <- selectDBdata(ID, WEIGHT, CONSUMPTION, HH_SIZE, EXPENDITURE, REGION, tables=c(paste0(svy, '_HH'))) %>%
  mutate_cond(region=="Orissa", region="Odisha")


# There were duplicate rows for Tamil Nadu, which had to be removed from the CSV files directly.
hdd.IND <- 
  # read_csv(paste0(hdd.path, "DoT_HDD_tas_18.3India.csv")) %>% 
  read_csv(paste0(hdd.path, "DoT_HDD_tas_30.0India.csv")) %>%
  # read_csv(paste0(hdd.path, "DoT_HDD_tas_18India.csv")) %>% 
  filter(var=="HDDsum" & stat=="PopWeightAv") %>% 
  select(-index, -var, -stat, -State) %>%
  rename(region=reg, HDD=value) %>%
  mutate_cond(region=="NCT of Delhi", region="Delhi")
cdd.IND <- read_csv(paste0(hdd.path, "DoT_CDD_tas_18.3India.csv")) %>% filter(var=="CDDsum" & stat=="PopWeightAv") %>%
  select(-index, -var, -stat, -State) %>%
  rename(region=reg, CDD=value) %>%
  mutate_cond(region=="NCT of Delhi", region="Delhi")

IND_HH.CDDHDD <- IND_HH.CDDHDD  %>% left_join(hdd.IND) %>% 
  # left_join(hdd.IND.18) %>% 
  left_join(cdd.IND)


avg.HDD.IND <- weighted.mean(IND_HH.CDDHDD$HDD, w=IND_HH.CDDHDD$weight*IND_HH.CDDHDD$hh_size, na.rm=TRUE) # Weighted national average 
# avg.HDD.18.IND <- weighted.mean(IND_HH.CDDHDD$HDD.18, w=IND_HH.CDDHDD$weight*IND_HH.CDDHDD$hh_size, na.rm=TRUE) # Weighted national average 
avg.CDD.IND <- weighted.mean(IND_HH.CDDHDD$CDD, w=IND_HH.CDDHDD$weight*IND_HH.CDDHDD$hh_size, na.rm=TRUE)
IND_HH.CDDHDD <- IND_HH.CDDHDD %>% 
  mutate_cond(is.nan(HDD), HDD=avg.HDD.IND) %>% 
  # mutate_cond(is.nan(HDD.18), HDD.18=avg.HDD.18.IND) %>% 
  mutate_cond(is.nan(CDD), CDD=avg.CDD.IND) %>% select(id, region, HDD, CDD) #, HDD.18)

# clothing.all from 
clothing.HDD <- clothing.all %>% full_join(footwear.all %>% select(id, weight.tot.f = weight.tot)) %>%
  left_join(IND_HH.CDDHDD) %>% 
  mutate_at(vars(weight.tot, income), funs(pcap=./hh_size)) %>% mutate(HDD2 = HDD^2)

# # Eyeballing data
# ggplot(clothing.HDD, aes(x=HDD, y=weight.tot)) +
#   geom_point(aes(colour = decile))
# ggplot(clothing.HDD, aes(x=HDD.18, y=weight.tot)) +
#   geom_point(aes(colour = decile))
# ggplot(clothing.HDD, aes(x=HDD, y=weight.tot.f)) +
#   geom_point(aes(colour = decile))

# Detected evident outliers. Need to remove them
outliers <- clothing.HDD %>% filter(weight.tot > 100000  | weight.tot.f > 50000) %>% select(id)
outlier_consumption <- IND_clothing %>% right_join(outliers)
clothing.HDD <- clothing.HDD %>% filter(!(id %in% outliers$id))

# Clothing kg model
# install_github("dgrtwo/broom")
# library(broom)

lm.clothing <- lm(weight.tot ~ expenditure + HDD + hh_size, clothing.HDD, weights = weight)
# lm.clothing <- lm(weight.tot ~ consumption + HDD + hh_size, clothing.HDD)
# lm.clothing.18 <- lm(weight.tot ~ expenditure + HDD.18 + hh_size, clothing.HDD, weights = weight)
lm.footwear <- lm(weight.tot.f ~ expenditure + HDD + hh_size, clothing.HDD, weights = weight)
summary(lm.clothing) 
# summary(lm.clothing.18) 
summary(lm.footwear) 

# Save for DLE-upscaling
saveRDS(lm.clothing, file="P:/ene.model/DLE_scaleup/Data/Clothing/lm.clothing.Rds")
saveRDS(lm.footwear, file="P:/ene.model/DLE_scaleup/Data/Clothing/lm.footwear.Rds")

# tidy.clothing <-tidy(lm.clothing)
# write.table(tidy.clothing, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)



clothing.region.income <- clothing.HDD %>% group_by(region, decile) %>% 
  summarise(weight.clothing = weighted.mean(weight.tot, w=weight), HDD = first(HDD), weight.pcap = weighted.mean(weight.tot_pcap, w=weight),
            # HDD.18 = first(HDD.18), 
            pop=sum(weight*hh_size)) #%>% mutate(weight.pcap = weight.clothing/pop)

ggplot(clothing.region.income, aes(x=HDD, y=weight.clothing)) +
  geom_point(aes(group = decile, colour=decile))
# ggplot(clothing.region.income, aes(x=HDD.18, y=weight.clothing)) +
#   geom_point(aes(group = decile, colour=decile))


# ggplot(clothing.region.income, aes(x=HDD.18, y=weight.clothing, size=weight, group = decile, colour=decile)) +
#   stat_sum() + scale_size_area(max_size = 12) + labs(x="HDD.18")

### Predicting for BRA and ZAF
hdd.BRA <- read_csv(paste0(hdd.path, "DoT_HDD_tas_30.0Brazil.csv")) %>% filter(var=="HDDsum" & stat=="PopWeightAv") %>% 
  select(-index, -var, -stat, -State) %>%
  rename(region=reg, HDD=value) %>%
  mutate_cond(region=="Espirito Santon", region="Espirito Santo") %>%
  mutate_cond(region=="Goiais", region="Goias")
svy = "BRA0"
BRA_HH.HDD <- selectDBdata(ID, WEIGHT, CONSUMPTION, HH_SIZE, EXPENDITURE, REGION, tables=c(paste0(svy, '_HH'))) %>%
  mutate(region=gsub("á|ã","a",region)) %>%
  mutate(region=gsub("ô","o",region)) %>%
  mutate(region=gsub("í","i",region)) %>%
  left_join(hdd.BRA) 
# avg.HDD.BRA <- weighted.mean(BRA_HH.HDD$HDD, w=BRA_HH.HDD$weight*BRA_HH.HDD$hh_size, na.rm=TRUE) # Weighted national average 
BRA_HH.HDD <- BRA_HH.HDD %>% mutate(weight.tot= predict(lm.clothing, BRA_HH.HDD), weight.tot.f= predict(lm.footwear, BRA_HH.HDD)) %>%
  mutate(weight.pcap = weight.tot/hh_size, weight.pcap.f = weight.tot.f/hh_size)

dle.clothing.pcap.BRA.HDD <- weighted.median(BRA_HH.HDD$weight.pcap, w=BRA_HH.HDD$weight)/1000
dle.footwear.pcap.BRA.HDD <- weighted.median(BRA_HH.HDD$weight.pcap.f, w=BRA_HH.HDD$weight)/1000

hdd.ZAF <- read_csv(paste0(hdd.path, "DoT_HDD_tas_30.0SouthAfrica.csv")) %>% filter(var=="HDDsum" & stat=="PopWeightAv") %>% 
  select(-index, -var, -stat, -State) %>%
  rename(region=reg, HDD=value)
svy = "ZAF1"
ZAF_HH.HDD <- selectDBdata(ID, WEIGHT, CONSUMPTION, HH_SIZE, EXPENDITURE, REGION, tables=c(paste0(svy, '_HH'))) %>%
  left_join(hdd.ZAF) 
# avg.HDD.ZAF <- weighted.mean(ZAF_HH.HDD$HDD, w=ZAF_HH.HDD$weight*ZAF_HH.HDD$hh_size, na.rm=TRUE) # Weighted national average 
ZAF_HH.HDD <- ZAF_HH.HDD %>% mutate(weight.tot= predict(lm.clothing, ZAF_HH.HDD), weight.tot.f= predict(lm.footwear, ZAF_HH.HDD)) %>%
  mutate(weight.pcap = weight.tot/hh_size, weight.pcap.f = weight.tot.f/hh_size)

dle.clothing.pcap.ZAF.HDD <- weighted.median(ZAF_HH.HDD$weight.pcap, w=ZAF_HH.HDD$weight)/1000
dle.footwear.pcap.ZAF.HDD <- weighted.median(ZAF_HH.HDD$weight.pcap.f, w=ZAF_HH.HDD$weight)/1000


### Plot in the DLE SI
dle.clothing.pcap.IND <- clothing.all %>% summarise(median = weighted.median(weight.tot/hh_size, weight, na.rm=TRUE)/1000) %>% as.numeric() # 1.3 kg/cap

temp.sum = data.frame(country=c('IND', 'BRA', 'ZAF'), 
           HDD.mean=c(avg.HDD.IND, 
                      BRA_HH.HDD %>% summarise(weighted.mean(HDD, w=weight)) %>% as.numeric(), 
                      ZAF_HH.HDD %>% summarise(weighted.mean(HDD, w=weight)) %>% as.numeric()),
           weight.pcap=c(dle.clothing.pcap.IND, dle.clothing.pcap.BRA.HDD, dle.clothing.pcap.ZAF.HDD))

ggplot() + 
  stat_sum(data=clothing.region.income, aes(x=HDD, y=weight.pcap/1000, size=pop, group = decile, colour=decile)) + 
  scale_size_area(max_size = 15, breaks=c(5e6, 1e7, 2e7), labels = c("5 mil" ,"10 mil", "20 mil")) + 
  labs(x="HDD.30", y="Per-capita clothing consumption (kg)", 
       colour="Consumption decile", size="Population") +
  geom_point(data=temp.sum, aes(x=HDD.mean, y=weight.pcap), size=3, shape=23, color='deepskyblue1', fill='deepskyblue1') +
  # scale_shape_manual(values=c(4), guide = FALSE) +
  geom_text(data=temp.sum, aes(x=HDD.mean, y=weight.pcap, label = country), nudge_x=230, fontface='bold', color='deepskyblue1')
