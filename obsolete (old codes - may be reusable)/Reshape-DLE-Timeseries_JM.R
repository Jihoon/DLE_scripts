require(base)
# require(raster)
require(readxl)
require(xlsx)
require(tidyr)
require(dplyr)
require(Hmisc)
require(graphics)
require(ggplot2)
require(gridExtra)
require(stringr)
require(viridis)

setwd('C:/Users/min/IIASA/DLE - Documents/WS2 - Documents/Analysis/Final results')
file='DLE Pathway construction.xlsx'

data_stock<-read_excel(file,sheet='Stock-Unit-TimeSeries', skip=0, col_names=T)
data_stock_long<-gather(data_stock, "year","stock_units",5:9)
data_stock_reshaped<-spread(data_stock_long, dle_comp, stock_units)

data_stock_reshaped=data_stock_reshaped%>%
  transform(year=as.integer(substr(year,2,5)))
#write.csv(data_stock_reshaped,'stock_timeseries.csv')


data_build<-read_excel(file,sheet='Construct-Unit-TimeSeries', skip=0, col_names=T)
data_build_long<-gather(data_build, "year","build_units",5:9)
data_build_reshaped<-spread(data_build_long, dle_comp, build_units)

data_build_reshaped=data_build_reshaped%>%
  transform(year=as.integer(substr(year,2,5)))
#write.csv(data_build_reshaped,'Build_timeseries.csv')

ssp<-read_excel(file,sheet='SocioeconomicProjections', skip=0, col_names=T)
ssp_long<-gather(ssp, "year","value",3:7)
ssp_reshaped<-spread(ssp_long, variable, value)

ssp_reshaped=ssp_reshaped%>%
  transform(year=as.integer(substr(year,2,5)))
#write.csv(ssp_reshaped,'ssp_timeseries.csv')

my_intens<-read_excel(file,sheet='CompileIntensities', skip=0, col_names=T)
my_intens_long<-gather(my_intens, "year","en_intensity",6:10)
my_intens_reshaped<-spread(my_intens_long, dle_comp, en_intensity)

my_intens_reshaped=my_intens_reshaped%>%
  transform(year=as.integer(substr(year,2,5)))
#write.csv(my_intens_reshaped,'NR_intensities_timeseries.csv')

### Pull in SSP country total pathways based on const energy intensity and IMAGE runs of SSP2-Ref-v9 and SS3-26-SPA0v9 ###
ctry_tots<-read_excel(file, sheet='GDP-energy')
ctry_tots_long=ctry_tots%>%
  gather("Year","gj_per_cap",3:7)%>%
  transform(Year=as.integer(substr(Year,2,5)))


########## Plotting ###########################
plot_agg<-read_excel('Result - Total final energy per cap.xlsx', sheet='Aggregate')
plot_elec<-read_excel('Result - Total final energy per cap.xlsx', sheet='GJ.pcap by comp')

plot_agg_rearr=plot_agg%>%
  select(-1)%>%
  select(-Tot.GJ.pcap)%>%  
  gather("dle_comp","gj_per_cap",5:23)%>%
  mutate(ord_Scen=factor(Scenario, levels=c("DLE.BAU","DLE.ACCEL","DLE.ACCEL.LCT", "DLE.ACCEL.LCT.BHV")))%>%
  mutate(ord_dlecomp=factor(dle_comp, levels=c("Food","Cln.ckg","Housing.R", "Housing.U","SpaceCond.R", "SpaceCond.U", "AC.R", "AC.U", "Lighting.R", "Lighting.U","Water", "Sanitation", "Clothing", "Fridge", "TV", "Health", "Educ", "Mobility", "Roads")))%>%
  arrange(ord_Scen, Country, Year, ord_dlecomp)

dle_ctry_tots=plot_agg_rearr%>%
  group_by(Scenario, Country, Year)%>%
  summarise(gj_per_cap=sum(gj_per_cap)) 

#write.csv(dle_ctry_tots,'DLE outputs for Bas.csv')

## Put together with DLE results ######
ctry_comp<-rbind(as.data.frame(dle_ctry_tots),ctry_tots_long)

plot_elec_rearr=plot_elec%>%
  select(-1)%>%
  gather("dle_comp","gj_per_cap",6:24)%>%
  mutate(ord_Scen=factor(Scenario, levels=c("DLE.BAU","DLE.ACCEL","DLE.ACCEL.LCT", "DLE.ACCEL.LCT.BHV")))%>%
  mutate(ord_dlecomp=factor(dle_comp, levels=c("Food","Cln.ckg","Housing.R", "Housing.U","SpaceCond.R", "SpaceCond.U", "AC.R", "AC.U", "Lighting.R", "Lighting.U","Water", "Sanitation", "Clothing", "Fridge", "TV", "Health", "Educ", "Mobility", "Roads")))%>%
  arrange(ord_Scen, Country, Year, ord_dlecomp)

plot_tots_agg=plot_agg_rearr%>%
  group_by(Scenario, ord_Scen,Country, Year, dle_comp, ord_dlecomp)%>%
  summarise(gj_per_cap=sum(gj_per_cap)) 

plot_opcon_agg=plot_agg_rearr%>%
  group_by(Scenario, ord_Scen,Country, Year, Type)%>%
  summarise(gj_per_cap=sum(gj_per_cap)) 



##### Overview plot, no LCT.BHV #########

### Color scale for components
library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(12, "Paired"))
CompColors <- rev(getPalette(length(levels(plot_agg_rearr$ord_dlecomp))))
names(CompColors) <- levels(plot_agg_rearr$ord_dlecomp)
CompColScale <- scale_fill_manual(values = CompColors)
###

plot_tots= plot_tots_agg%>%
  filter(ord_Scen!='DLE.ACCEL.LCT.BHV')
plot_opcon= plot_opcon_agg%>%
  filter(ord_Scen!='DLE.ACCEL.LCT.BHV')

ptotals<- ggplot(plot_tots,aes(x=Year, y=gj_per_cap, fill=dle_comp)) + geom_area(position='stack') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("Final energy (GJ per cap)") + 
  CompColScale

ptotals+ facet_grid(Country~ord_Scen)

popcon<- ggplot(plot_opcon,aes(x=Year, y=gj_per_cap, fill=Type)) + geom_area(position='stack') +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("Final energy (GJ per cap)")+ scale_color_viridis()

popcon+ facet_grid(Country~ord_Scen)

png(file = "DLEEnergyPathways.png", width = 634, height = 532)
ptotals+ facet_grid(Country~ord_Scen)+theme(text  = element_text(size = 15))
dev.off()

png(file = "DLE Op vs Con.png", width = 634, height = 532)
popcon+ facet_grid(Country~ord_Scen)+theme(text  = element_text(size = 15))
dev.off()
##### OP and CON detail plots #########

plot_cons=plot_agg_rearr%>%
  filter(Type=='CON', gj_per_cap>0)%>%
  filter(ord_Scen!='DLE.ACCEL.LCT.BHV')

pcon<- ggplot(plot_cons,aes(x=Year, y=gj_per_cap, fill=ord_dlecomp)) + 
  geom_area(position='stack') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Final energy (GJ per cap)")+ 
  CompColScale

pcon+ facet_grid(Country~ord_Scen)

png(file = "DLE Cons - good colors.png", width = 634, height = 532)
pcon+ facet_grid(Country~ord_Scen)+theme(text  = element_text(size = 15))
dev.off()

plot_ops=plot_agg_rearr%>%
  filter(Type=='OP', gj_per_cap>0)%>%
  filter(ord_Scen!='DLE.ACCEL.LCT.BHV')

pops<- ggplot(plot_ops,aes(x=Year, y=gj_per_cap, fill=ord_dlecomp)) + 
  geom_area(position='stack') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Final energy (GJ per cap)")+ 
  CompColScale

pops+ facet_grid(Country~ord_Scen)

png(file = "DLE Ops - good colors.png", width = 634, height = 532)
pops+ facet_grid(Country~ord_Scen)+theme(text  = element_text(size = 15))
dev.off()

#### Compare to GDP and SSP2 plots #######
ScenarioColors <- c(brewer.pal(9, "Blues")[c(5,7,8)], brewer.pal(9, "Set1")[c(1,3:5)])
names(ScenarioColors) <- c(rev(unique(plot_ctry$Scenario)), "DLE.ACCEL.LCT.BHV", 'DLE.BAU')
ScenarioColScale <- scale_color_manual(values = ScenarioColors)

plot_ctry=ctry_comp%>%
  filter(Scenario!='DLE.ACCEL.LCT.BHV' & Scenario!='DLE.BAU')
#filter(Scenario!='DLE.ACCEL.LCT.BHV' )
ctrys<- ggplot(plot_ctry,aes(x=Year, y=gj_per_cap, group=Scenario)) + 
  geom_line(aes(color=Scenario), size=1.5) +
  geom_text(data=plot_ctry %>% filter(Year==2015 & (Scenario=="DLE.ACCEL" | Scenario=="GDP") ), 
            aes(label=paste(format(gj_per_cap, digits = 1), "GJ/cap")), hjust = 0.14, vjust=1.5, check_overlap = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Final energy (GJ per cap)") +
  ScenarioColScale 

ctrys+ facet_grid(~Country)

png(file = "DLE vs GDP.png", width = 634, height = 532)
ctrys+ facet_grid(~Country)+theme(text  = element_text(size = 15))
dev.off()

#### Emissions plots #######
plot_em<-read_excel('Result - Total emissions per cap.xlsx', sheet='Aggregate')

plot_em_rearr=plot_em%>%
  select(Scenario, Country, Year, CO2e.ton.pcap)%>%
  filter(Scenario!='DLE.ACCEL.LCT')%>%
  arrange(Scenario, Country, Year)

pem<- ggplot(plot_em_rearr,aes(x=Year, y=CO2e.ton.pcap, group=Scenario)) + 
  geom_line(aes(color=Scenario), size=1.2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("GHG (tons) per capita") + ylim(0,10) +
  ScenarioColScale

pem+ facet_grid(~Country)

png(file = "DLE Emissions Pathways.png", width = 634, height = 532)
pem+ facet_grid(~Country)+theme(text  = element_text(size = 15))
dev.off()

