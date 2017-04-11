require(reldist)
require(data.table)
require(ineq)
require(plm)

r.GDP = 0.06
n_year <- 10
GDP = 1e6*(1+r.GDP)^(0:n_year)
# intensity = rep(1, 100) # rep(1, 100) * 
energy.base = 1000
percentiles <- 1000
elas.max <- 1  
elas.base <- elas.max  # Also assume elas.base = elas.max
elas.min <- 0.7
ginis <- c(0.30, 0.36, 0.55)

erf.inv = function(x) 2*(qnorm((x + 1)/2)/sqrt(2))

sdlogs <- sapply(ginis, FUN=erf.inv)

grid=seq(0,1,.1)
plot(grid,grid,type="l",xlab="Cumulative Population",ylab="Cumulative Income")
lines(Lc.lognorm, parameter=sdlogs[3], col="black")

income.pdf <- matrix(nrow = length(ginis), ncol = percentiles+1)
income.lag <- income.pdf

for (i in 1:length(ginis)) {
  income.pdf[i,] <- Lc.lognorm(seq(0, 1, 1/percentiles), parameter=sdlogs[i])
  lines(Lc.lognorm, parameter=sdlogs[i], col="black")
  income.lag[i,] <- lag(income.pdf[i,])
}
# a <- Lc.lognorm(seq(0,1,1e-2), parameter=sdlogs[3])

income.dist.base <- ((income.pdf - income.lag)[,-1] * GDP[1])[2,]  # only at gini=0.36
income.dist.end <- (income.pdf - income.lag)[,-1] * GDP[length(GDP)]

# percentage change in income from the lowest income hh
income.pct.base <- (1/income.dist.base[1]) %*% income.dist.base - 1   #income.dist.base / t(apply(income.dist.base, 1, lag)) 
income.pct.end <- diag(1/income.dist.end[,1]) %*% income.dist.end - 1   #income.dist.base / t(apply(income.dist.base, 1, lag))

# intens.highE <- income.pct.base # [kgCO2/cap]
# intens.lowE <- income.pct.base # [kgCO2/cap]

# energy distribution (absolute)
# energy.dist.base.hiE <- income.dist.base / GDP[1] * energy.base   # at elas=1, energy share = income share
# energy.dist.base.hiE <- (income.pct.base * elas.max + 1) / rowSums((income.pct.base * elas.max + 1)) * energy.base   # at elas=1, energy share = income share
# energy.dist.base.lowE <- (income.pct.base * elas.min + 1) / rowSums((income.pct.base * elas.min + 1)) * energy.base

# ene.scaler.hiE <- energy.base / rowSums(income.dist.base^elas.max) 
# ene.scaler.lowE <-energy.base / rowSums(income.dist.base^elas.min)
ene.scaler.base <- energy.base / sum(income.dist.base^elas.base)  # At the baseline gini=0.36

# energy.dist.base.hiE <- ene.scaler.base * income.dist.base^elas.max
# energy.dist.base.lowE <- ene.scaler.base * income.dist.base^elas.min
energy.dist.base <- ene.scaler.base * income.dist.base^elas.base

sum(energy.dist.base)

# intensity.base.hiE <- energy.dist.base.hiE/income.dist.base
# intensity.base.lowE <- energy.dist.base.lowE/income.dist.base
intensity.base <- energy.dist.base/income.dist.base

# energy.dist.end.hiE <- GetEnergy(income.dist.end, 1, income.dist.base[,1], energy.dist.base.hiE[,1])
# energy.dist.end.lowE <- GetEnergy(income.dist.end, elas.min, income.dist.base[,1], energy.dist.base.lowE[,1])

energy.dist.end.hiE <- ene.scaler.base * income.dist.end^elas.max
energy.dist.end.lowE <- ene.scaler.base * income.dist.end^elas.min

intensity.end.hiE <- energy.dist.end.hiE/income.dist.end
intensity.end.lowE <- energy.dist.end.lowE/income.dist.end

Ene.tot.end <- data.frame("High e"=rowSums(energy.dist.end.hiE), "Low e"=rowSums(energy.dist.end.lowE)) %>% rbind(c(elas.max, elas.min))
# Ene.tot.base <- data.frame("High e"=rowSums(energy.dist.base.hiE), "Low e"=rowSums(energy.dist.base.lowE)) %>% rbind(c(elas.max, elas.min))
row.names(Ene.tot.end) <- c("Gini30", "Gini36", "Gini55", "Elasticity")
print(Ene.tot.end)
print(paste("%Diff between Gini 30 and 55 =", (1-Ene.tot.end$Low.e[3]/Ene.tot.end$Low.e[1])*100))

# Income vs Intensity (one curve per one elasticity value)
a<-data.frame(inc=income.dist.base, int=intensity.base) %>% rbind(data.frame(inc=income.dist.end[1,], int=intensity.end.lowE[1,]))
# b<-data.frame(inc=income.dist.base[3,], int=intensity.base.lowE[3,]) %>% rbind(data.frame(inc=income.dist.end[3,], int=intensity.end.lowE[3,]))
ggplot(a, aes(inc, int)) + geom_point() + labs(title="Intensity") #+
#   # geom_point(data=b, aes(inc, int, color="grey")) #+ xlim(0,6) + ylim(4,16)
# b<-data.frame(inc=income.dist.base[1,], int=intensity.base.hiE[1,]) %>% rbind(data.frame(inc=income.dist.end[1,], int=intensity.end.hiE[1,]))
# ggplot(b, aes(inc, int)) + geom_point() + xlim(0,6) + ylim(3,16)

vecsize <- dim(income.dist.end)[2]

plot(intensity.end.lowE[1,seq(1,vecsize,vecsize/1e3)], ylab="Intensity (J/$)", xlab="Income percentile", main="Intensity 2050: Elasticity=0.8, Gini=0.30")
points(intensity.end.lowE[3,seq(1,vecsize,vecsize/1e3)], ylab="Intensity (J/$)", xlab="Income percentile", main="Intensity 2050: Elasticity=0.8, Gini=0.55")

plot(income.dist.end[1,seq(1,vecsize,vecsize/1e3)], ylab="Income ($)", xlab="Income percentile", main="Income 2050")
points(income.dist.end[3,seq(1,vecsize,vecsize/1e3)], ylab="Income ($)", xlab="Income percentile", main="Income 2050")

plot(energy.dist.end.lowE[1,seq(1,vecsize,vecsize/1e3)], ylab="Energy (J)", xlab="Income percentile", main="Energy 2050: Elasticity=0.8, Gini=0.30/0.55")
points(energy.dist.end.lowE[3,seq(1,vecsize,vecsize/1e3)])

plot(energy.dist.end.hiE[1,seq(1,vecsize,vecsize/1e3)], ylab="Energy (J)", xlab="Income percentile", main="Energy 2050: Elasticity=1, Gini=0.30/0.55")
points(energy.dist.end.hiE[3,seq(1,vecsize,vecsize/1e3)])

plot(energy.dist.base[seq(1,vecsize,vecsize/1e3)], ylab="Energy (J)", xlab="Income percentile", main="Energy 2011: Elasticity=1, Gini=0.36")

# GetEnergy <- function(income, elas, income.base, ene.base=1) {
#   ene <- ((income/income.base-1)*elas +1)*ene.base   
#   return(ene)
# }


### Country comparisons

# "NE.CON.PRVT.PC.KD"    "Household final consumption expenditure per capita (constant 2000 US$)"
# "NE.CON.PRVT.PP.KD"    "Household final consumption expenditure, PPP (constant 2005 international $)"

# "NY.GDP.PCAP.PP.KD"    "GDP per capita, PPP (constant 2005 international $)"                                
# "NY.GDP.PCAP.KD"       "GDP per capita (constant 2000 US$)"  

# "EG.USE.PCAP.KG.OE"    "Energy use (kg of oil equivalent per capita)"   
# "EG.USE.ELEC.KH.PC"    "Electric power consumption (kWh per capita)"

# SSP data import: Total GDP and Population in 2050
ssp.gdp.ppp <- read_excel("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Income inequality/SSP-GDP.ppp.xlsx", skip=0, col_names=TRUE) %>%
  select(Scenario, iso3c=Region, gdp.ppp=`2050`) %>%
  spread(Scenario, gdp.ppp)  # in 1e9 US$ 2005
names(ssp.gdp.ppp)[-1] <- paste0("gdp.",names(ssp.gdp.ppp)[-1])
ssp.pop <- read_excel("C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Income inequality/SSP-Population.xlsx",skip=0, col_names=TRUE) %>%
  select(Scenario, iso3c=Region, pop=`2050`) %>% mutate(pop=pop*1e6) %>%
  spread(Scenario, pop)   # number of people
names(ssp.pop)[-1] <- paste0("pop.",names(ssp.pop)[-1])


library("ggrepel")
y.start <- 1990
y.end <- 2013

# Historical data
CON.ENE.PCAP <- WDI(country = "all", 
                    indicator = c("NE.CON.PRVT.PP.KD",      # total expenditure (PPP)
                                  "EG.USE.PCAP.KG.OE",      # primary energy per cap
                                  # "NE.CON.PRVT.PC.KD",      # expenditure per capita (MER)
                                  # "EG.USE.ELEC.KH.PC",      # electricity per cap
                                  # "NY.GDP.PCAP.KD",         # GDP per capita (MER) 
                                  "NY.GDP.PCAP.PP.KD",      # GDP per capita (PPP) 
                                  "SP.POP.TOTL",            # Population
                                  "SI.POV.GINI"),           # Country Gini
                    start = y.start, end = y.end, extra = FALSE, cache = NULL) %>%
  mutate(con.ppp = NE.CON.PRVT.PP.KD/SP.POP.TOTL) %>%
  rename(ene = EG.USE.PCAP.KG.OE, 
         pop = SP.POP.TOTL, 
         #con.mer = NE.CON.PRVT.PC.KD, elec=EG.USE.ELEC.KH.PC, 
         gdp = NY.GDP.PCAP.PP.KD, 
         gini = SI.POV.GINI) %>% 
  mutate(inten.ppp = ene/con.ppp, 
         # inten.mer = ene/con.mer, el.inten = elec/con.ppp, el.inten.gdp = elec/gdp, 
         inten.gdp = ene/gdp) %>% 
  filter(!grepl('[0-9]',iso2c) & !grepl('X[A-Z]',iso2c)) %>%
  mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>% filter(inten.ppp<15 & !is.na(iso3c)) 

# ENE <- CON.ENE.PCAP %>% filter(!is.na(inten.ppp)) 
CON.ENE.PCAP <- CON.ENE.PCAP %>% filter(!is.na(gdp)) 

ggplot(CON.ENE.PCAP %>% arrange(iso3c, year), aes(x=gdp, y=inten.gdp, group = iso3c, color=iso3c)) + 
  geom_path(alpha=0.5, arrow = arrow(angle=30, type="closed", length=unit(0.25, "cm"))) + 
  geom_text_repel(data=CON.ENE.PCAP %>% filter(year==2013), aes(label=iso3c)) + theme(legend.position="none") #+ 
  # scale_x_log10() + scale_y_log10()
  
summary(plm(log(ene)~log(gdp), data=CON.ENE.PCAP, method="random", effect="time", index=c("iso3c", "year")))

SSP1.2050 <- ssp.gdp.ppp %>% left_join(ssp.pop) %>% select(iso3c, contains("SSP1"))
SSP2.2050 <- ssp.gdp.ppp %>% left_join(ssp.pop) %>% select(iso3c, contains("SSP2")) 
SSP3.2050 <- ssp.gdp.ppp %>% left_join(ssp.pop) %>% select(iso3c, contains("SSP3")) 
SSP4.2050 <- ssp.gdp.ppp %>% left_join(ssp.pop) %>% select(iso3c, contains("SSP4")) 
SSP5.2050 <- ssp.gdp.ppp %>% left_join(ssp.pop) %>% select(iso3c, contains("SSP5")) 

gini(SSP1.2050$gdp.SSP1/SSP1.2050$pop.SSP1, SSP1.2050$pop.SSP1)
gini(SSP2.2050$gdp.SSP2/SSP2.2050$pop.SSP2, SSP2.2050$pop.SSP2)
gini(SSP3.2050$gdp.SSP3/SSP3.2050$pop.SSP3, SSP3.2050$pop.SSP3)
gini(SSP4.2050$gdp.SSP4/SSP4.2050$pop.SSP4, SSP4.2050$pop.SSP4)
gini(SSP5.2050$gdp.SSP5/SSP5.2050$pop.SSP5, SSP5.2050$pop.SSP5)


# a <- WDI(country = "all", indicator = c("NE.CON.PRVT.PC.KD", "EG.USE.PCAP.KG.OE", "EG.USE.ELEC.KH.PC", "SP.POP.TOTL"), start = 1990, end = 2016, extra = FALSE, cache = NULL) %>%
#   rename(con = NE.CON.PRVT.PC.KD, ene = EG.USE.PCAP.KG.OE, pop=SP.POP.TOTL, elec=EG.USE.ELEC.KH.PC) %>% mutate(el.inten = elec/con) %>% 
#   filter(!grepl('[0-9]',iso2c) & !grepl('X[A-Z]',iso2c)) %>%
#   mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>% filter(inten<15 & !is.na(iso3c) & !is.na(el.inten))

e.gini <- vector()
# el.gini <- vector()
c.gini.ppp <- vector()
# c.gini.mer <- vector()
g.gini <- vector()

r.min <- 0.65
r.max <- 1.14
# r.min.mer <- 0.55
# r.max.mer <- 1

for(i in y.start:y.end) {
  ENE.yearly <- CON.ENE.PCAP %>% filter(year==i) %>% 
    left_join(SSP1.2050) %>% left_join(SSP2.2050) %>% left_join(SSP3.2050) %>% left_join(SSP4.2050) %>% left_join(SSP5.2050) %>%
    mutate(gdp.pcap.SSP1=gdp.SSP1/pop.SSP1*1e9, # billion $ -> $
           gdp.pcap.SSP2=gdp.SSP2/pop.SSP2*1e9, 
           gdp.pcap.SSP3=gdp.SSP3/pop.SSP3*1e9, 
           gdp.pcap.SSP4=gdp.SSP4/pop.SSP4*1e9, 
           gdp.pcap.SSP5=gdp.SSP5/pop.SSP5*1e9) %>%
    filter(!is.na(gdp.SSP1))  # having most countries in 2007
  # GDP.yearly <- GDP %>% filter(year==i)  # having most countries in 2007
  
  # tot.wealth.ssp <- ENE.yearly %>% summarise_at(vars(starts_with("gdp.SSP")), sum)
  # gdp.ref <- tot.wealth.ssp$gdp.SSP2   # Fixed reference GDP total for 2050 (SSP2)
  # 
  # # Scale the GDP/cap accordingly to make the total=gdp.ref (SSP2)
  # ENE.yearly <- ENE.yearly %>% mutate(gdp.pcap.SSP1=gdp.pcap.SSP1*gdp.ref/tot.wealth.ssp$gdp.SSP1,
  #                       gdp.pcap.SSP3=gdp.pcap.SSP3*gdp.ref/tot.wealth.ssp$gdp.SSP3,
  #                       gdp.pcap.SSP4=gdp.pcap.SSP4*gdp.ref/tot.wealth.ssp$gdp.SSP4,
  #                       gdp.pcap.SSP5=gdp.pcap.SSP5*gdp.ref/tot.wealth.ssp$gdp.SSP5)
  # Check whether the GDP total is identical
  ENE.yearly %>% summarise(sum(gdp.pcap.SSP5*pop.SSP5))
  
  e.gini <- append(e.gini, gini(ENE.yearly$ene, ENE.yearly$pop))
  c.gini.ppp <- append(c.gini.ppp, gini(ENE.yearly$con.ppp, ENE.yearly$pop))
  g.gini <- append(g.gini, gini(ENE.yearly$gdp, ENE.yearly$pop))
  
  # mean.exp <- weighted.mean(ENE.yearly$con.ppp, ENE.yearly$pop)
  mean.gdp <- weighted.mean(ENE.yearly$gdp, ENE.yearly$pop)
  
  # Plot - GDP distribution in the order of increasing wealth
  ggplot(ENE.yearly %>% arrange(gdp.pcap.SSP2) %>% mutate(num=1:length(gdp)), aes(x=num, y=gdp)) + geom_point() +
    geom_text_repel(aes(label=iso3c), size = 2.5) + ylab("GDP per cap (PPP$ 2005)") +
    geom_point(aes(x=num, y=gdp.pcap.SSP1, color="SSP1")) + 
    geom_point(aes(x=num, y=gdp.pcap.SSP2, color="SSP2")) + 
    geom_point(aes(x=num, y=gdp.pcap.SSP4, color="SSP4"))  
  
  # GDP-Gini comparison
  print(data.frame("2013"=gini(ENE.yearly$gdp, ENE.yearly$pop), 
          "gini.2050.SSP1"=gini(ENE.yearly$gdp.pcap.SSP1, ENE.yearly$pop.SSP1), 
          "2050 SSP2"=gini(ENE.yearly$gdp.pcap.SSP2, ENE.yearly$pop.SSP2), 
          # gini(ENE.yearly$gdp.pcap.SSP3, ENE.yearly$pop.SSP3),
          "2050 SSP4"=gini(ENE.yearly$gdp.pcap.SSP4, ENE.yearly$pop.SSP4) #,
          # gini(ENE.yearly$gdp.pcap.SSP5, ENE.yearly$pop.SSP5)
          ))
  
  # Primary energy vs. expenditure PPP
  # lm(log(ene)~log(con), CON.ENE.PCAP.yearly, weights=pop)
  # eqn <- lm(log(ene)~log(con.ppp), ENE.yearly, weights=pop)
  eqn <- lm(log(ene)~log(gdp), ENE.yearly, weights=pop)
  summary(eqn)
  elas <- eqn$coefficients[2]
  elas.min.cty <- 0.7
  elas.max.cty <- 1.1
  
  # ENE.yearly.adj.ppp <- ENE.yearly.adj %>% 
  #   mutate(ene.adj.eq=ene*con.adj.eq.ppp^elas/con.ppp^elas, 
  #          ene.adj.uneq=ene*con.adj.uneq.ppp^elas/con.ppp^elas,
  #          ene.adj.eq.min=ene*con.adj.eq.ppp^elas.min.cty/con.ppp^elas.min.cty, 
  #          ene.adj.uneq.min=ene*con.adj.uneq.ppp^elas.min.cty/con.ppp^elas.min.cty)
  # ENE.yearly.adj.ppp.varyelas <- ENE.yearly.adj %>% 
  #   mutate(ene.adj.eq=ene*con.adj.eq.ppp^elas/con.ppp^elas, 
  #          ene.adj.uneq=ene*con.adj.uneq.ppp^elas/con.ppp^elas,
  #          ene.adj.eq.min=ene*con.adj.eq.ppp^elas.min.cty/con.ppp^elas.min.cty, 
  #          ene.adj.uneq.min=ene*con.adj.uneq.ppp^elas.min.cty/con.ppp^elas.min.cty) %>%
  #   mutate_cond(con.ppp < mean.exp, ene.adj.eq.min=ene*con.adj.eq.ppp^elas.max.cty/con.ppp^elas.max.cty,
  #               ene.adj.uneq.min=ene*con.adj.uneq.ppp^elas.max.cty/con.ppp^elas.max.cty)
  
  # 2050 
  # Scenario 1: GDP allocation as is in SSPs
  ENE.yearly.adj <- ENE.yearly %>% 
    mutate(ene.base.SSP1=ene*gdp.pcap.SSP1^elas/gdp^elas, 
           ene.base.SSP2=ene*gdp.pcap.SSP2^elas/gdp^elas,
           ene.base.SSP4=ene*gdp.pcap.SSP4^elas/gdp^elas,
           ene.min.SSP1=ene*gdp.pcap.SSP1^elas.min.cty/gdp^elas.min.cty, # Assuming different C (const) for different elas values
           ene.min.SSP2=ene*gdp.pcap.SSP2^elas.min.cty/gdp^elas.min.cty,
           ene.min.SSP4=ene*gdp.pcap.SSP4^elas.min.cty/gdp^elas.min.cty,
           ene.max.SSP1=ene*gdp.pcap.SSP1^elas.max.cty/gdp^elas.max.cty, 
           ene.max.SSP2=ene*gdp.pcap.SSP2^elas.max.cty/gdp^elas.max.cty, 
           ene.max.SSP4=ene*gdp.pcap.SSP4^elas.max.cty/gdp^elas.max.cty)
  ENE.yearly.adj <- ENE.yearly.adj %>% select(country, iso2c, iso3c, starts_with("gdp"), starts_with("ene"), everything())
  
  # Scenario 2: clean - GDP allocation more to high intensity/poor countries (based on SSP4)
  # Consider only the more equal side of the world
  mean.gdp.SSP4 <- weighted.mean(ENE.yearly$gdp.pcap.SSP4, ENE.yearly$pop.SSP4)
  
  # r.dist <- 0.5
  # tot.gdp.allocate <- as.numeric(ENE.yearly %>% mutate(gdp.diff.total=(gdp.pcap.SSP4-mean.gdp.SSP4)*pop.SSP4) %>%
  #                              filter(gdp.diff.total>=0) %>% summarise(tot.gdp.allocate=sum(gdp.diff.total)*r.dist))
  ENE.yearly.adj.clean <- ENE.yearly %>% mutate(ene.tot = ene*pop, rich=(gdp.pcap.SSP4>=mean.gdp.SSP4)) %>% group_by(rich) %>%
    # mutate(ene.share=ene.tot/sum(ene.tot), gdp.pcap.clean=0) %>% arrange(rich, ene.share) %>% 
    mutate(int.share=inten.gdp/max(inten.gdp), int.share.inv=1/inten.gdp/max(1/inten.gdp), gdp.pcap.clean=0) %>% arrange(rich, int.share) %>% 
    mutate_cond(rich, gdp.pcap.clean = gdp.pcap.SSP4-(gdp.pcap.SSP4-mean.gdp.SSP4)*inten.gdp/max(inten.gdp)) %>%
    mutate_cond(!rich, gdp.pcap.clean = gdp.pcap.SSP4+(mean.gdp.SSP4-gdp.pcap.SSP4)/inten.gdp/max(1/inten.gdp)) %>%
    # mutate_cond(rich, gdp.pcap.clean = (gdp.pcap.SSP4-tot.gdp.allocate*ene.share/pop.SSP4)) %>%
    # mutate_cond(!rich, gdp.pcap.clean = pmin(gdp.pcap.SSP4+tot.gdp.allocate*int.share/pop.SSP4, mean.gdp.SSP4)) %>% 
    ungroup()
    
  ggplot(ENE.yearly.adj.clean %>% arrange(gdp.pcap.SSP4) %>% mutate(num=1:length(gdp.pcap.SSP4)), aes(x=num, y=gdp.pcap.SSP4)) + geom_point() +
    geom_point(aes(color="SSP4")) + ylab("GDP per cap (PPP$ 2005)") +
    geom_point(aes(x=num, y=gdp.pcap.clean, color="adj-clean")) + geom_text_repel(aes(x=num, y=gdp.pcap.clean, label=iso3c), size = 2.5) +
    labs(title="Cleaner world in 2050")
      
  ene.diff <- sum(ENE.yearly.adj.clean$gdp.pcap.clean*ENE.yearly.adj.clean$pop.SSP4) / sum(ENE.yearly.adj.clean$gdp.pcap.SSP4*ENE.yearly.adj.clean$pop.SSP4)
  
  ENE.yearly.adj.clean <- ENE.yearly.adj.clean %>% mutate(gdp.pcap.clean = gdp.pcap.clean/ene.diff) %>%
    mutate(ene.base.clean=ene*gdp.pcap.clean^elas/gdp^elas, 
           ene.base.SSP4=ene*gdp.pcap.SSP4^elas/gdp^elas,
           ene.min.clean=ene*gdp.pcap.clean^elas.min.cty/gdp^elas.min.cty, # Assuming different C (const) for different elas values
           ene.min.SSP4=ene*gdp.pcap.SSP4^elas.min.cty/gdp^elas.min.cty,
           ene.max.clean=ene*gdp.pcap.clean^elas.max.cty/gdp^elas.max.cty, 
           ene.max.SSP4=ene*gdp.pcap.SSP4^elas.max.cty/gdp^elas.max.cty)
  ENE.yearly.adj <- ENE.yearly.adj %>% select(country, iso2c, iso3c, starts_with("gdp"), starts_with("ene"), everything())
  gini(ENE.yearly.adj.clean$gdp.pcap.clean, ENE.yearly.adj.clean$pop.SSP4) 
  gini(ENE.yearly.adj.clean$gdp.pcap.SSP4, ENE.yearly.adj.clean$pop.SSP4) 
  
  # With measured elasticity 
  ENE.yearly.adj.clean %>% select(ene.base.clean, ene.base.SSP4, pop.SSP4) %>% 
    summarise_at(vars(starts_with("ene.base.")), funs(sum(.*pop.SSP4))) / 
    sum(ENE.yearly.adj.clean$ene.base.SSP4 * ENE.yearly.adj.clean$pop.SSP4)  
  # With measured elasticity 
  ENE.yearly.adj.clean %>% select(ene.min.clean, ene.min.SSP4, pop.SSP4) %>% 
    summarise_at(vars(starts_with("ene.min.")), funs(sum(.*pop.SSP4))) / 
    sum(ENE.yearly.adj.clean$ene.min.SSP4 * ENE.yearly.adj.clean$pop.SSP4)  
  # With measured elasticity 
  ENE.yearly.adj.clean %>% select(ene.max.clean, ene.max.SSP4, pop.SSP4) %>% 
    summarise_at(vars(starts_with("ene.max.")), funs(sum(.*pop.SSP4))) / 
    sum(ENE.yearly.adj.clean$ene.max.SSP4 * ENE.yearly.adj.clean$pop.SSP4)  
  
  # Scenario 3: Dirty - GDP allocation more to high intensity/poor countries (based on SSP4)
  # Consider only the more equal side of the world
  # mean.gdp.SSP4 <- weighted.mean(ENE.yearly$gdp.pcap.SSP4, ENE.yearly$pop.SSP4)
  
  ENE.yearly.adj.dirty <- ENE.yearly %>% mutate(ene.tot = ene*pop, rich=(gdp.pcap.SSP4>=mean.gdp.SSP4)) %>% group_by(rich) %>%
    # mutate(ene.share=ene.tot/sum(ene.tot), gdp.pcap.dirty=0) %>% arrange(rich, ene.share) %>% 
    mutate(int.share=inten.gdp/max(inten.gdp), int.share.inv=1/inten.gdp/max(1/inten.gdp), gdp.pcap.dirty=0) %>% arrange(rich, desc(int.share)) %>% 
    mutate_cond(!rich, gdp.pcap.dirty = gdp.pcap.SSP4+(mean.gdp.SSP4-gdp.pcap.SSP4)*inten.gdp/max(inten.gdp)) %>%
    mutate_cond(rich, gdp.pcap.dirty = gdp.pcap.SSP4-(gdp.pcap.SSP4-mean.gdp.SSP4)/inten.gdp/max(1/inten.gdp)) %>%
    # mutate_cond(rich, gdp.pcap.dirty = (gdp.pcap.SSP4-tot.gdp.allocate*ene.share/pop.SSP4)) %>%
    # mutate_cond(!rich, gdp.pcap.dirty = pmin(gdp.pcap.SSP4+tot.gdp.allocate*int.share/pop.SSP4, mean.gdp.SSP4)) %>% 
    ungroup()
  
  ggplot(ENE.yearly.adj.dirty %>% arrange(gdp.pcap.SSP4) %>% mutate(num=1:length(gdp.pcap.SSP4)), aes(x=num, y=gdp.pcap.SSP4)) + 
    geom_point(aes(color="SSP4")) + ylab("GDP per cap (PPP$ 2005)") +
    geom_point(aes(x=num, y=gdp.pcap.dirty, color="adj-dirty")) + geom_text_repel(aes(x=num, y=gdp.pcap.dirty, label=iso3c), size = 2.5) +
    labs(title="Dirtier world in 2050")
  
  ene.diff <- sum(ENE.yearly.adj.dirty$gdp.pcap.dirty*ENE.yearly.adj.dirty$pop.SSP4) /  
    sum(ENE.yearly.adj.dirty$gdp.pcap.SSP4*ENE.yearly.adj.dirty$pop.SSP4)
  
  ENE.yearly.adj.dirty <- ENE.yearly.adj.dirty %>% mutate(gdp.pcap.dirty = gdp.pcap.dirty/ene.diff) %>%
    mutate(ene.base.dirty=ene*gdp.pcap.dirty^elas/gdp^elas, 
           ene.base.SSP4=ene*gdp.pcap.SSP4^elas/gdp^elas,
           ene.min.dirty=ene*gdp.pcap.dirty^elas.min.cty/gdp^elas.min.cty, # Assuming different C (const) for different elas values
           ene.min.SSP4=ene*gdp.pcap.SSP4^elas.min.cty/gdp^elas.min.cty,
           ene.max.dirty=ene*gdp.pcap.dirty^elas.max.cty/gdp^elas.max.cty, 
           ene.max.SSP4=ene*gdp.pcap.SSP4^elas.max.cty/gdp^elas.max.cty)
  ENE.yearly.adj <- ENE.yearly.adj %>% select(country, iso2c, iso3c, starts_with("gdp"), starts_with("ene"), everything())
  gini(ENE.yearly.adj.dirty$gdp.pcap.dirty, ENE.yearly.adj.dirty$pop.SSP4) 
  gini(ENE.yearly.adj.dirty$gdp.pcap.SSP4, ENE.yearly.adj.dirty$pop.SSP4) 
  
  # With measured elasticity 
  ENE.yearly.adj.dirty %>% select(ene.base.dirty, ene.base.SSP4, pop.SSP4) %>% 
    summarise_at(vars(starts_with("ene.base.")), funs(sum(.*pop.SSP4))) / 
    sum(ENE.yearly.adj.dirty$ene.base.SSP4 * ENE.yearly.adj.dirty$pop.SSP4)  
  # With measured elasticity 
  ENE.yearly.adj.dirty %>% select(ene.min.dirty, ene.min.SSP4, pop.SSP4) %>% 
    summarise_at(vars(starts_with("ene.min.")), funs(sum(.*pop.SSP4))) / 
    sum(ENE.yearly.adj.dirty$ene.min.SSP4 * ENE.yearly.adj.dirty$pop.SSP4)  
  # With measured elasticity 
  ENE.yearly.adj.dirty %>% select(ene.max.dirty, ene.max.SSP4, pop.SSP4) %>% 
    summarise_at(vars(starts_with("ene.max.")), funs(sum(.*pop.SSP4))) / 
    sum(ENE.yearly.adj.dirty$ene.max.SSP4 * ENE.yearly.adj.dirty$pop.SSP4)  
  
  
  
  
  
  # Compare the energy change ratio
  # With measured elasticity 
  ENE.yearly.adj %>% select(ene.base.SSP1, ene.base.SSP4, pop.SSP1, pop.SSP4) %>% 
    summarise(ene.base.SSP1= sum(ene.base.SSP1*pop.SSP1), ene.base.SSP4= sum(ene.base.SSP4*pop.SSP4)) / 
    sum(ENE.yearly.adj$ene.base.SSP2 * ENE.yearly.adj$pop.SSP2)  
  # With elasticity = 1.1
  ENE.yearly.adj %>% select(ene.max.SSP1, ene.max.SSP4, pop.SSP1, pop.SSP4) %>% 
    summarise(ene.max.SSP1= sum(ene.max.SSP1*pop.SSP1), ene.max.SSP4= sum(ene.max.SSP4*pop.SSP4)) / 
    sum(ENE.yearly.adj$ene.max.SSP2 * ENE.yearly.adj$pop.SSP2)  
  # With elasticity = 0.7
  ENE.yearly.adj %>% select(ene.min.SSP1, ene.min.SSP4, pop.SSP1, pop.SSP4) %>% 
    summarise(ene.min.SSP1= sum(ene.min.SSP1*pop.SSP1), ene.min.SSP4= sum(ene.min.SSP4*pop.SSP4)) / 
    sum(ENE.yearly.adj$ene.min.SSP2 * ENE.yearly.adj$pop.SSP2)
  # ENE.yearly.adj.ppp.varyelas %>% select(ene, ene.adj.eq, ene.adj.uneq, ene.adj.eq.min, ene.adj.uneq.min, pop) %>% summarise_all(funs(sum(.*pop))) / 
  #   sum(ENE.yearly.adj$ene * ENE.yearly.adj$pop)
  
  # Plot - Energy intensity vs gdp/cap (SSP1)
  m <- ggplot(ENE.yearly.adj, aes(x=gdp, y=inten.gdp)) + geom_point() +
    geom_text(aes(label=iso3c), size = 2.5, hjust=1) + 
    geom_point(aes(x=gdp.pcap.SSP1, y=ene.base.SSP1/gdp.pcap.SSP1, color="measured elas")) + 
    geom_point(aes(x=gdp.pcap.SSP1, y=ene.min.SSP1/gdp.pcap.SSP1, color="min.elas = 0.7")) + 
    geom_point(aes(x=gdp.pcap.SSP1, y=ene.max.SSP1/gdp.pcap.SSP1, color="max.elas = 1.1")) +
    geom_text(aes(x=gdp.pcap.SSP1, y=ene.max.SSP1/gdp.pcap.SSP1, label=iso3c), size = 2.5, hjust=1) +
    labs(title="SSP1", x ="GDP/cap (PPP$ 2005)", y = "Energy intensity (kgoe/$)")
  m + geom_line(aes(x=gdp, y=exp(eqn$coefficients[1])*gdp^(eqn$coefficients[2]-1)))

  # Population bubble with energy/cap - Base elas
  ggplot(ENE.yearly.adj %>% filter(pop>5e7), aes(x=gdp, y=ene)) + 
    # geom_point(aes(size=pop, alpha=.02, color="Base 2013")) +
    scale_size(range = c(1,15)) +
    # geom_text(aes(label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP1, y=ene.base.SSP1, size=pop.SSP1, alpha=.02, color="SSP1 2050")) +
    geom_text(aes(x=gdp.pcap.SSP1, y=ene.base.SSP1, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP2, y=ene.base.SSP2, size=pop.SSP2, alpha=.02, color="SSP2 2050")) +
    geom_text(aes(x=gdp.pcap.SSP2, y=ene.base.SSP2, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.base.SSP4, size=pop.SSP4, alpha=.02, color="SSP4 2050")) +
    geom_text(aes(x=gdp.pcap.SSP4, y=ene.base.SSP4, label=iso3c), size = 2.5, hjust=1) +
    scale_alpha(guide = 'none') + labs(y="Energy per capita") + 
    scale_x_log10() + scale_y_log10()
  
  cty.scenario <- ENE.yearly.adj %>% 
    rename(gdp.pcap.SSP0=gdp, pop.SSP0=pop, ene.base.SSP0=ene) %>% 
    select(iso3c, contains("pop"), contains("base"), contains("gdp.pcap"), contains("ene"), -inten.gdp) %>% gather(key=category, value=val, -iso3c) 
  cty.scenario <- cty.scenario %>% filter(!grepl("SSP3|SSP5|min|max", category)) %>% arrange(iso3c, category) %>% 
    tidyr::extract(category, c("category", "scenario"), "([a-z.]+).(SSP[0-9])") %>%
    spread(category, val) #%>% mutate(gdp.tot=gdp.pcap*pop)
  
  # Show energy total change with arrow - Base elas
  # ggplot(cty.scenario %>% filter(scenario=="SSP2" | scenario=="SSP1"), aes(x=gdp.pcap, y=ene.base*pop, group = iso3c, color = iso3c)) + 
  #   geom_path(aes(alpha=.02), arrow = arrow(angle=12, type="closed", length=unit(0.25, "cm"))) +
  #   # scale_size(range = c(1,15)) +
  #   geom_text_repel(data=cty.scenario %>% filter(scenario=="SSP2"), aes(label=iso3c), size = 2.5) + 
  #   scale_alpha(guide = 'none')  + theme(legend.position="none") + 
  #   scale_x_log10() 
  
  # Show energy total change with arrow - Base elas
  ggplot(cty.scenario %>% filter(scenario!="SSP0"), aes(x=gdp.pcap, y=ene.base*pop, group = iso3c, color = iso3c)) + 
    geom_path(aes(alpha=.02), arrow = arrow(angle=12, type="closed", length=unit(0.25, "cm"))) +
    # scale_size(range = c(1,15)) +
    geom_text(data=cty.scenario %>% filter(scenario=="SSP2"), aes(label=iso3c), size = 2.5) + 
    scale_alpha(guide = 'none')  + theme(legend.position="none") + labs(y="Total energy") +
    scale_x_log10() 
  
  # Population bubble with tot energy - Base elas
  ggplot(ENE.yearly.adj %>% filter(pop>5e7), aes(x=gdp, y=ene*pop)) + 
    geom_point(aes(alpha=.02, color="Base 2013")) +
    scale_size(range = c(1,15)) +
    geom_text(aes(label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP1, y=ene.base.SSP1*pop.SSP1, alpha=.02, color="SSP1 2050")) +
    geom_text(aes(x=gdp.pcap.SSP1, y=ene.base.SSP1*pop.SSP1, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP2, y=ene.base.SSP2*pop.SSP2, alpha=.02, color="SSP2 2050")) +
    geom_text(aes(x=gdp.pcap.SSP2, y=ene.base.SSP2*pop.SSP2, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.base.SSP4*pop.SSP4, alpha=.02, color="SSP4 2050")) +
    geom_text(aes(x=gdp.pcap.SSP4, y=ene.base.SSP4*pop.SSP4, label=iso3c), size = 2.5, hjust=1) +
    scale_alpha(guide = 'none') + labs(y="Total primary energy") + 
    scale_x_log10() #+ scale_y_log10()
  
  # Population bubble with intensities - Base elas
  ggplot(ENE.yearly.adj %>% filter(pop>5e7), aes(x=gdp, y=inten.gdp)) + 
    # geom_point(aes(size=pop, alpha=.02, color="Base 2013")) +
    scale_size(range = c(1,15)) +
    # geom_text(aes(label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP1, y=ene.base.SSP1/gdp.pcap.SSP1, size=pop.SSP1, alpha=.02, color="SSP1 2050")) +
    geom_text(aes(x=gdp.pcap.SSP1, y=ene.base.SSP1/gdp.pcap.SSP1, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP2, y=ene.base.SSP2/gdp.pcap.SSP2, size=pop.SSP2, alpha=.02, color="SSP2 2050")) +
    geom_text(aes(x=gdp.pcap.SSP2, y=ene.base.SSP2/gdp.pcap.SSP2, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.base.SSP4/gdp.pcap.SSP4, size=pop.SSP4, alpha=.02, color="SSP4 2050")) +
    geom_text(aes(x=gdp.pcap.SSP4, y=ene.base.SSP4/gdp.pcap.SSP4, label=iso3c), size = 2.5, hjust=1) +
    geom_line(aes(x=gdp, y=exp(eqn$coefficients[1])*gdp^(eqn$coefficients[2]-1))) +
    labs(x="GDP per capita (in log)", y="Energy intensity (kgoe/$) (in log)") +
    scale_alpha(guide = 'none') + 
    scale_x_log10() + scale_y_log10()
  
  # Population bubble with intensities - Min elas
  ggplot(ENE.yearly.adj, aes(x=gdp, y=inten.gdp)) + 
    geom_point(aes(size=pop, alpha=.02)) +
    scale_size(range = c(1,10)) +
    geom_text(aes(label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP1, y=ene.min.SSP1/gdp.pcap.SSP1, size=pop.SSP1, alpha=.02, color="SSP1 (e=0.7)")) +
    geom_text(aes(x=gdp.pcap.SSP1, y=ene.min.SSP1/gdp.pcap.SSP1, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP2, y=ene.min.SSP2/gdp.pcap.SSP2, size=pop.SSP2, alpha=.02, color="SSP2 (e=0.7)")) +
    geom_text(aes(x=gdp.pcap.SSP2, y=ene.min.SSP2/gdp.pcap.SSP2, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.min.SSP4/gdp.pcap.SSP4, size=pop.SSP4, alpha=.02, color="SSP4 (e=0.7)")) +
    geom_text(aes(x=gdp.pcap.SSP4, y=ene.min.SSP4/gdp.pcap.SSP4, label=iso3c), size = 2.5, hjust=1) +
    geom_line(aes(x=gdp, y=exp(eqn$coefficients[1])*gdp^(eqn$coefficients[2]-1))) +
    # geom_line(aes(x=gdp, y=2000/gdp)) +
    ylim(0, 0.5)
 
  # Population bubble with intensities - Max elas
  ggplot(ENE.yearly.adj, aes(x=gdp, y=inten.gdp)) + 
    geom_point(aes(size=pop, alpha=.02)) +
    scale_size(range = c(1,10)) +
    geom_text(aes(label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP1, y=ene.max.SSP1/gdp.pcap.SSP1, size=pop.SSP1, alpha=.02, color="SSP1 (e=0.7)")) +
    geom_text(aes(x=gdp.pcap.SSP1, y=ene.max.SSP1/gdp.pcap.SSP1, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP2, y=ene.max.SSP2/gdp.pcap.SSP2, size=pop.SSP2, alpha=.02, color="SSP2 (e=0.7)")) +
    geom_text(aes(x=gdp.pcap.SSP2, y=ene.max.SSP2/gdp.pcap.SSP2, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.max.SSP4/gdp.pcap.SSP4, size=pop.SSP4, alpha=.02, color="SSP4 (e=0.7)")) +
    geom_text(aes(x=gdp.pcap.SSP4, y=ene.max.SSP4/gdp.pcap.SSP4, label=iso3c), size = 2.5, hjust=1) +
    geom_line(aes(x=gdp, y=exp(eqn$coefficients[1])*gdp^(eqn$coefficients[2]-1))) +
    # geom_line(aes(x=gdp, y=2000/gdp)) +
    ylim(0, 0.5)
  
  # Plot - Energy intensity vs gdp/cap (SSP4)
  m <- ggplot(ENE.yearly.adj, aes(x=gdp, y=inten.gdp)) + geom_point() +
    geom_text(aes(label=iso3c), size = 2.5, hjust=1) + 
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.base.SSP4/gdp.pcap.SSP4, color="measured elas")) + 
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.min.SSP4/gdp.pcap.SSP4, color="min.elas = 0.7")) + 
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.max.SSP4/gdp.pcap.SSP4, color="max.elas = 1.1")) +
    geom_text(aes(x=gdp.pcap.SSP4, y=ene.max.SSP4/gdp.pcap.SSP4, label=iso3c), size = 2.5, hjust=1) +
    labs(title="SSP4", x ="GDP/cap (PPP$ 2005)", y = "Energy intensity (kgoe/$)")
  m + geom_line(aes(x=gdp, y=exp(eqn$coefficients[1])*gdp^(eqn$coefficients[2]-1)))
  
  # Plot - Energy intensity vs gdp/cap (Clean)
  m <- ggplot(ENE.yearly.adj.clean, aes(x=gdp, y=ene/gdp)) + geom_point() +
    geom_text(aes(label=iso3c), size = 2.5, hjust=1) + 
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.base.SSP4/gdp.pcap.SSP4, color="measured elas 0.89 gdp unadj")) + 
    geom_point(aes(x=gdp.pcap.clean, y=ene.base.clean/gdp.pcap.clean, color="measured elas 0.89")) + 
    geom_point(aes(x=gdp.pcap.clean, y=ene.min.clean/gdp.pcap.clean, color="min.elas = 0.7")) + 
    geom_point(aes(x=gdp.pcap.clean, y=ene.max.clean/gdp.pcap.clean, color="max.elas = 1.1")) +
    geom_text(aes(x=gdp.pcap.clean, y=ene.max.clean/gdp.pcap.clean, label=iso3c), size = 2.5, hjust=1) +
    labs(title="Clean world", x ="GDP/cap (PPP$ 2005)", y = "Energy intensity (kgoe/$)")
  m + geom_line(aes(x=gdp, y=exp(eqn$coefficients[1])*gdp^(eqn$coefficients[2]-1)))
  
  # Plot - Energy intensity vs gdp/cap (Dirty)
  m <- ggplot(ENE.yearly.adj.dirty, aes(x=gdp, y=ene/gdp)) + geom_point() +
    geom_text(aes(label=iso3c), size = 2.5, hjust=1) + 
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.base.SSP4/gdp.pcap.SSP4, color="measured elas 0.89 gdp unadj")) + 
    geom_point(aes(x=gdp.pcap.dirty, y=ene.base.dirty/gdp.pcap.dirty, color="measured elas 0.89")) + 
    geom_point(aes(x=gdp.pcap.dirty, y=ene.min.dirty/gdp.pcap.dirty, color="min.elas = 0.7")) + 
    geom_point(aes(x=gdp.pcap.dirty, y=ene.max.dirty/gdp.pcap.dirty, color="max.elas = 1.1")) +
    geom_text(aes(x=gdp.pcap.dirty, y=ene.max.dirty/gdp.pcap.dirty, label=iso3c), size = 2.5, hjust=1) +
    labs(title="Dirty world", x ="GDP/cap (PPP$ 2005)", y = "Energy intensity (kgoe/$)")
  m + geom_line(aes(x=gdp, y=exp(eqn$coefficients[1])*gdp^(eqn$coefficients[2]-1)))
  
  ggplot(ENE.yearly.adj.clean, aes(x=gdp, y=ene/gdp)) + geom_point(aes(size=pop, alpha=.02)) +
    geom_text(aes(label=iso3c), size = 2.5, hjust=1) + 
    scale_size(range = c(1,10)) +
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.min.SSP4/gdp.pcap.SSP4, size=pop.SSP4, alpha=.02, color="SSP4 (e=0.7)")) + 
    geom_text(aes(x=gdp.pcap.SSP4, y=ene.min.SSP4/gdp.pcap.SSP4, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.clean, y=ene.min.clean/gdp.pcap.clean, size=pop.SSP4, alpha=.02, color="Clean (e=0.7)")) + 
    geom_text(aes(x=gdp.pcap.clean, y=ene.min.clean/gdp.pcap.clean, label=iso3c), size = 2.5, hjust=1) +
    geom_point(data=ENE.yearly.adj.dirty, aes(x=gdp.pcap.dirty, y=ene.min.dirty/gdp.pcap.dirty, size=pop.SSP4, alpha=.02, color="Dirty (e=0.7)")) +
    geom_text(data=ENE.yearly.adj.dirty, aes(x=gdp.pcap.dirty, y=ene.min.dirty/gdp.pcap.dirty, label=iso3c), size = 2.5, hjust=1) +
    labs(title="Min elasticity = 0.7", x ="GDP/cap (PPP$ 2005)", y = "Energy intensity (kgoe/$)")
  
  ggplot(ENE.yearly.adj.clean, aes(x=gdp, y=ene/gdp)) + geom_point(aes(size=pop, alpha=.02)) +
    geom_text(aes(label=iso3c), size = 2.5, hjust=1) + 
    scale_size(range = c(1,10)) +
    geom_point(aes(x=gdp.pcap.SSP4, y=ene.max.SSP4/gdp.pcap.SSP4, size=pop.SSP4, alpha=.02, color="SSP4 (e=1.1)")) + 
    geom_text(aes(x=gdp.pcap.SSP4, y=ene.max.SSP4/gdp.pcap.SSP4, label=iso3c), size = 2.5, hjust=1) +
    geom_point(aes(x=gdp.pcap.clean, y=ene.max.clean/gdp.pcap.clean, size=pop.SSP4, alpha=.02, color="Clean (e=1.1)")) + 
    geom_text(aes(x=gdp.pcap.clean, y=ene.max.clean/gdp.pcap.clean, label=iso3c), size = 2.5, hjust=1) +
    geom_point(data=ENE.yearly.adj.dirty, aes(x=gdp.pcap.dirty, y=ene.max.dirty/gdp.pcap.dirty, size=pop.SSP4, alpha=.02, color="Dirty (e=1.1)")) +
    geom_text(data=ENE.yearly.adj.dirty, aes(x=gdp.pcap.dirty, y=ene.max.dirty/gdp.pcap.dirty, label=iso3c), size = 2.5, hjust=1) +
    labs(title="Max elasticity = 1.1", x ="GDP/cap (PPP$ 2005)", y = "Energy intensity (kgoe/$)")
  
  # Step graphs comparing total energy (SSP4 vs clean/dirty, same Gini)
  # Total GDP in trillion $
  # Total energy in trillion kgoe
  cum.plot.data <- ENE.yearly.adj.dirty %>% select(iso3c, contains("SSP4"), contains("dirty")) %>% 
    left_join(ENE.yearly.adj.clean %>% select(iso3c, contains("clean"))) %>%
    mutate(int.base.SSP4=ene.base.SSP4/gdp.pcap.SSP4, int.base.dirty=ene.base.dirty/gdp.pcap.dirty, int.base.clean=ene.base.clean/gdp.pcap.clean) %>% 
    mutate(int.min.SSP4=ene.min.SSP4/gdp.pcap.SSP4, int.min.dirty=ene.min.dirty/gdp.pcap.dirty, int.min.clean=ene.min.clean/gdp.pcap.clean) %>% 
    mutate(int.max.SSP4=ene.max.SSP4/gdp.pcap.SSP4, int.max.dirty=ene.max.dirty/gdp.pcap.dirty, int.max.clean=ene.max.clean/gdp.pcap.clean) %>% 
    
    arrange(int.max.dirty) %>%
    mutate(tot.gdp.max.2050.dirty=gdp.pcap.dirty*pop.SSP4/1e12, cum.gdp.max.dirty = cumsum(tot.gdp.max.2050.dirty), cum.ene.max.dirty = cumsum(int.max.dirty*tot.gdp.max.2050.dirty)) %>%
    arrange(int.max.clean) %>%
    mutate(tot.gdp.max.2050.clean=gdp.pcap.clean*pop.SSP4/1e12, cum.gdp.max.clean = cumsum(tot.gdp.max.2050.clean), cum.ene.max.clean = cumsum(int.max.clean*tot.gdp.max.2050.clean)) %>% 
    arrange(int.max.SSP4) %>%
    mutate(tot.gdp.max.2050=gdp.pcap.SSP4*pop.SSP4/1e12, cum.gdp.max = cumsum(tot.gdp.max.2050), cum.ene.max = cumsum(int.max.SSP4*tot.gdp.max.2050)) %>%
    
    arrange(int.min.dirty) %>%
    mutate(tot.gdp.min.2050.dirty=gdp.pcap.dirty*pop.SSP4/1e12, cum.gdp.min.dirty = cumsum(tot.gdp.min.2050.dirty), cum.ene.min.dirty = cumsum(int.min.dirty*tot.gdp.min.2050.dirty)) %>%
    arrange(int.min.clean) %>%
    mutate(tot.gdp.min.2050.clean=gdp.pcap.clean*pop.SSP4/1e12, cum.gdp.min.clean = cumsum(tot.gdp.min.2050.clean), cum.ene.min.clean = cumsum(int.min.clean*tot.gdp.min.2050.clean)) %>% 
    arrange(int.min.SSP4) %>%
    mutate(tot.gdp.min.2050=gdp.pcap.SSP4*pop.SSP4/1e12, cum.gdp.min = cumsum(tot.gdp.min.2050), cum.ene.min = cumsum(int.min.SSP4*tot.gdp.min.2050)) %>%
    
    arrange(int.base.dirty) %>%
    mutate(tot.gdp.2050.dirty=gdp.pcap.dirty*pop.SSP4/1e12, cum.gdp.dirty = cumsum(tot.gdp.2050.dirty), cum.ene.dirty = cumsum(int.base.dirty*tot.gdp.2050.dirty)) %>%
    arrange(int.base.clean) %>%
    mutate(tot.gdp.2050.clean=gdp.pcap.clean*pop.SSP4/1e12, cum.gdp.clean = cumsum(tot.gdp.2050.clean), cum.ene.clean = cumsum(int.base.clean*tot.gdp.2050.clean)) %>% 
    arrange(int.base.SSP4) %>%
    mutate(tot.gdp.2050=gdp.pcap.SSP4*pop.SSP4/1e12, cum.gdp = cumsum(tot.gdp.2050), cum.ene = cumsum(int.base.SSP4*tot.gdp.2050)) # GDP in trillion $2005
  
  View(cum.plot.data %>% select(starts_with("cum.")))
  ggplot(cum.plot.data , aes(x=cum.gdp, y=int.base.SSP4)) + geom_step(aes(color="SSP4 (g=0.45, e=0.89)")) + 
    geom_step(aes(x=cum.gdp.dirty, y=int.base.dirty, color="Dirty (g=0.3, e=0.89)")) + 
    geom_step(aes(x=cum.gdp.clean, y=int.base.clean, color="Clean (g=0.3, e=0.89)")) +
    geom_text_repel(aes(label=iso3c), size = 2.5) +
    labs(y="Energy intensity (kgoe/$)", x="Total GDP (trillion PPP$ 2005)") +
    geom_text(aes(x=cum.gdp.dirty, y=int.base.dirty, label=formatC(cum.ene.dirty, digits = 1, format="f")), size = 2.5, hjust=1) +
    geom_text(aes(x=cum.gdp.clean, y=int.base.clean, label=formatC(cum.ene.clean, digits = 1, format="f")), size = 2.5, hjust=1)

  ggplot(cum.plot.data %>% arrange(tot.gdp.min.2050), aes(x=cum.gdp.min, y=cum.ene.min)) + 
    geom_step(aes(color="SSP4 (g=0.45, e=0.70)"), direction="vh") + 
    geom_text(data=cum.plot.data %>% filter(tot.gdp.min.2050>5), aes(label=iso3c), size = 2.5) +
    geom_step(aes(x=cum.gdp.min.dirty, y=cum.ene.min.dirty, color="Dirty (g=0.3, e=0.70)"), direction="vh") + 
    geom_text(data=cum.plot.data %>% filter(tot.gdp.min.2050>5), aes(x=cum.gdp.min.dirty, y=cum.ene.min.dirty, label=iso3c), size = 2.5) +
    geom_step(aes(x=cum.gdp.min.clean, y=cum.ene.min.clean, color="Clean (g=0.3, e=0.70)"), direction="vh") +
    geom_text(data=cum.plot.data %>% filter(tot.gdp.min.2050>5), aes(x=cum.gdp.min.clean, y=cum.ene.min.clean, label=iso3c), size = 2.5) +
    labs(y="Energy (kgoe)", x="Total GDP (trillion PPP$ 2005)") #+
    # geom_text(aes(x=cum.gdp.dirty, y=cum.ene.dirty, label=formatC(cum.ene.dirty, digits = 1, format="f")), size = 2.5, hjust=1) +
    # geom_text(aes(x=cum.gdp.clean, y=cum.ene.clean, label=formatC(cum.ene.clean, digits = 1, format="f")), size = 2.5, hjust=1)
  ggplot(cum.plot.data %>% arrange(tot.gdp.max.2050), aes(x=cum.gdp.max, y=cum.ene.max)) + 
    geom_step(aes(color="SSP4 (g=0.45, e=1.1)"), direction="vh") + 
    geom_text(data=cum.plot.data %>% filter(tot.gdp.max.2050>5), aes(label=iso3c), size = 2.5) +
    geom_step(aes(x=cum.gdp.max.dirty, y=cum.ene.max.dirty, color="Dirty (g=0.3, e=1.1)"), direction="vh") + 
    geom_text(data=cum.plot.data %>% filter(tot.gdp.max.2050>5), aes(x=cum.gdp.max.dirty, y=cum.ene.max.dirty, label=iso3c), size = 2.5) +
    geom_step(aes(x=cum.gdp.max.clean, y=cum.ene.max.clean, color="Clean (g=0.3, e=1.1)"), direction="vh") +
    geom_text(data=cum.plot.data %>% filter(tot.gdp.max.2050>5), aes(x=cum.gdp.max.clean, y=cum.ene.max.clean, label=iso3c), size = 2.5) +
    labs(y="Energy (kgoe)", x="Total GDP (trillion PPP$ 2005)") #+
  
  # Step graphs comparing total energy (SSP2 vs SSP1/SSP4)
  # Total GDP in trillion $
  # Total energy in trillion kgoe
  cum.plot.SSP <- ENE.yearly.adj %>% select(iso3c, contains("SSP1"), contains("SSP2"), contains("SSP4")) %>% 
    mutate(int.base.SSP4=ene.base.SSP4/gdp.pcap.SSP4, int.base.SSP1=ene.base.SSP1/gdp.pcap.SSP1, int.base.SSP2=ene.base.SSP2/gdp.pcap.SSP2) %>% 
    arrange(int.base.SSP4) %>%
    mutate(tot.gdp.SSP4=gdp.pcap.SSP4*pop.SSP4/1e12, cum.gdp.SSP4 = cumsum(tot.gdp.SSP4), cum.ene.SSP4 = cumsum(int.base.SSP4*tot.gdp.SSP4)) %>%
    arrange(int.base.SSP1) %>%
    mutate(tot.gdp.SSP1=gdp.pcap.SSP1*pop.SSP1/1e12, cum.gdp.SSP1 = cumsum(tot.gdp.SSP1), cum.ene.SSP1 = cumsum(int.base.SSP1*tot.gdp.SSP1)) %>% 
    arrange(int.base.SSP2) %>%
    mutate(tot.gdp.SSP2=gdp.pcap.SSP2*pop.SSP2/1e12, cum.gdp.SSP2 = cumsum(tot.gdp.SSP2), cum.ene.SSP2 = cumsum(int.base.SSP2*tot.gdp.SSP2)) # GDP in trillion $2005
  
  ggplot(cum.plot.SSP, aes(x=cum.gdp.SSP2, y=int.base.SSP2)) + geom_step(aes(color="SSP2 (g=0.36, e=0.89)")) + 
    geom_step(aes(x=cum.gdp.SSP4, y=int.base.SSP4, color="SSP4 (g=0.45, e=0.89)")) + 
    geom_step(aes(x=cum.gdp.SSP1, y=int.base.SSP1, color="SSP1 (g=0.3, e=0.89)")) +
    geom_text_repel(aes(label=iso3c), size = 2.5) +
    labs(y="Energy intensity (kgoe/$)", x="Total GDP (trillion PPP$ 2005)") +
    geom_text(aes(x=cum.gdp.SSP4, y=int.base.SSP4, label=formatC(cum.ene.SSP4, digits = 1, format="f")), size = 2.5, hjust=1) +
    geom_text(aes(x=cum.gdp.SSP1, y=int.base.SSP1, label=formatC(cum.ene.SSP1, digits = 1, format="f")), size = 2.5, hjust=1)
  
  cum.tot <- ENE.yearly.adj %>% select(iso3c, gdp, ene, pop, contains("SSP1"), contains("SSP2"), contains("SSP4")) %>% 
    mutate(tot.ene.now=ene*pop, tot.ene.SSP2=ene.base.SSP2*pop.SSP2, tot.ene.SSP1=ene.base.SSP1*pop.SSP1, tot.ene.SSP4=ene.base.SSP4*pop.SSP4) %>%
    arrange(gdp.pcap.SSP2) %>% #arrange(desc(tot.ene.SSP2)) %>%
    mutate(cum.ene.now = cumsum(tot.ene.now), cum.ene.SSP2 = cumsum(tot.ene.SSP2), 
           cum.ene.SSP1 = cumsum(tot.ene.SSP1), cum.ene.SSP4 = cumsum(tot.ene.SSP4), 
           cum.gdp.now = cumsum(gdp*pop/1e9), cum.gdp.SSP2 = cumsum(gdp.SSP2), # billion$
           cum.gdp.SSP1 = cumsum(gdp.SSP1), cum.gdp.SSP4 = cumsum(gdp.SSP4)) 
  cum.tot <- rbind(0,cum.tot) 
  ggplot(cum.tot, aes(x=cum.gdp.SSP1, y=cum.ene.SSP1, color="SSP1 (g=0.3, e=0.89)")) + geom_line() + geom_point(aes(shape="SSP1")) +
    geom_line(aes(x=cum.gdp.SSP2, y=cum.ene.SSP2, color="SSP2 (g=0.36, e=0.89)")) +
    geom_point(aes(x=cum.gdp.SSP2, y=cum.ene.SSP2, shape="SSP2")) +
    geom_line(aes(x=cum.gdp.SSP4, y=cum.ene.SSP4, color="SSP4 (g=0.45, e=0.89)")) +
    geom_point(aes(x=cum.gdp.SSP4, y=cum.ene.SSP4, shape="SSP4")) +
    geom_line(aes(x=cum.gdp.now, y=cum.ene.now, color="2013 (g=0.46)")) +
    geom_point(aes(x=cum.gdp.now, y=cum.ene.now, shape="2013")) +
    geom_text_repel(data=cum.tot %>% filter(gdp.SSP2>1000), aes(label=iso3c), size = 2.5) +
    labs(x="Total GDP in 2050 (b$)", y="Cumulative total energy  in 2050 (kgoe)") 
    # geom_text(aes(x=cum.gdp.SSP4, y=int.base.SSP4, label=formatC(cum.ene.SSP4, digits = 1, format="f")), size = 2.5, hjust=1) +
    # geom_text(aes(x=cum.gdp.SSP1, y=int.base.SSP1, label=formatC(cum.ene.SSP1, digits = 1, format="f")), size = 2.5, hjust=1)
  
  ggplot(cum.tot, aes(x=cum.gdp.SSP1-cum.gdp.now, y=cum.ene.SSP1-cum.ene.now, color="SSP1 (g=0.3, e=0.89)")) + geom_line() + geom_point(color="pink", aes(shape="SSP1")) +
    geom_line(aes(x=cum.gdp.SSP2-cum.gdp.now, y=cum.ene.SSP2-cum.ene.now, color="SSP2 (g=0.36, e=0.89)")) +
    geom_point(aes(x=cum.gdp.SSP2-cum.gdp.now, y=cum.ene.SSP2-cum.ene.now, shape="SSP2"), color="green") +
    geom_text_repel(data=cum.tot %>% filter(gdp.SSP2>1000), aes(x=cum.gdp.SSP2-cum.gdp.now, y=cum.ene.SSP2-cum.ene.now, label=iso3c), size = 2.5, color="red") +
    geom_line(aes(x=cum.gdp.SSP4-cum.gdp.now, y=cum.ene.SSP4-cum.ene.now, color="SSP4 (g=0.45, e=0.89)")) +
    geom_point(aes(x=cum.gdp.SSP4-cum.gdp.now, y=cum.ene.SSP4-cum.ene.now, shape="SSP4"), color="blue") +
    # geom_line(aes(x=1:dim(a)[1], y=cum.ene.now, color="2013 (g=0.46)")) +
    # geom_point(aes(x=1:dim(a)[1], y=cum.ene.now, shape="5")) +
    geom_text_repel(data=cum.tot %>% filter(gdp.SSP2>1000), aes(label=iso3c), size = 2.5, color="black") +
    labs(x="Total GDP difference from 2013 (b$)", y="Cumulative total energy difference from 2013 (kgoe)") +
    geom_line(aes(x=cum.gdp.SSP1-cum.gdp.now, y=sum(tot.ene.SSP1-tot.ene.now)/sum(gdp.SSP1-gdp*pop/1e9)*(cum.gdp.SSP1-cum.gdp.now)), color="yellow")
  
  cum.tot <- cum.tot %>% mutate(intensity.now=cum.ene.now/cum.gdp.now, intensity.SSP1=cum.ene.SSP1/cum.gdp.SSP1,
                                intensity.SSP2=cum.ene.SSP2/cum.gdp.SSP2, intensity.SSP4=cum.ene.SSP4/cum.gdp.SSP4,
                                intensity.SSP1.diff=(cum.ene.SSP1-cum.ene.now)/(cum.gdp.SSP1-cum.gdp.now),
                                intensity.SSP2.diff=(cum.ene.SSP2-cum.ene.now)/(cum.gdp.SSP2-cum.gdp.now), 
                                intensity.SSP4.diff=(cum.ene.SSP4-cum.ene.now)/(cum.gdp.SSP4-cum.gdp.now))
  # Plot - Total energy per cap vs gdp/cap
  # ggplot(ENE.yearly.adj %>% arrange(gdp) %>% mutate(num=1:length(gdp)), aes(x=num, y=ene)) + geom_point() +
  #   geom_text_repel(aes(label=iso3c), size = 2.5) + ylab("Primary energy per cap (kgoe)") +
  #   geom_point(aes(x=num, y=ene.adj.eq, color="Gini=0.3, elas=1")) + 
  #   geom_point(aes(x=num, y=ene.adj.eq.min, color="Gini=0.3, elas=worst"))
  ggplot(ENE.yearly.adj %>% arrange(gdp) %>% mutate(num=1:length(gdp)), aes(x=gdp, y=ene)) + geom_point() +
    geom_text_repel(aes(label=iso3c), size = 2.5) + ylab("Primary energy per cap (kgoe)") +
    geom_line(aes(x=con.ppp, y=exp(eqn$coefficients[1])*con.ppp^(eqn$coefficients[2])))
  
  n_group <- 10
  clean.poor <- ENE.yearly.adj.ppp %>% filter(con.ppp < mean.exp - 1000) %>% arrange(inten.ppp) %>% slice(1:n_group)
  dirty.poor <- ENE.yearly.adj.ppp %>% filter(con.ppp < mean.exp - 1000) %>% arrange(desc(inten.ppp)) %>% slice(1:n_group)
  clean.rich <- ENE.yearly.adj.ppp %>% filter(con.ppp >= mean.exp + 1000) %>% arrange(inten.ppp) %>% slice(1:n_group)
  dirty.rich <- ENE.yearly.adj.ppp %>% filter(con.ppp >= mean.exp + 1000) %>% arrange(desc(inten.ppp)) %>% slice(1:n_group)
  
  tot.decrease <- as.numeric(ENE.yearly.adj.ppp %>% mutate(con.diff.total=(con.ppp-con.adj.eq.ppp)*pop) %>% 
                               filter(con.diff.total>=0) %>% summarise(tot.decrease=sum(con.diff.total)))
  ENE.yearly.adj.ppp.dirty <- dirty.poor %>% mutate(con.ppp.tot = con.ppp*pop) %>% mutate(con.ppp.inc = tot.decrease * con.ppp.tot / sum(con.ppp.tot) / pop) %>% 
    mutate(con.adj.eq.ppp=con.ppp+con.ppp.inc) %>% select(-con.ppp.tot, -con.ppp.inc) %>% 
    rbind(ENE.yearly.adj.ppp %>% filter(!iso2c %in% dirty.poor$iso2c) %>% mutate_cond(con.ppp < mean.exp, con.adj.eq.ppp=con.ppp))
  ENE.yearly.adj.ppp.clean <- clean.poor %>% mutate(con.ppp.tot = con.ppp*pop) %>% mutate(con.ppp.inc = tot.decrease * con.ppp.tot / sum(con.ppp.tot) / pop) %>% 
    mutate(con.adj.eq.ppp=con.ppp+con.ppp.inc) %>% select(-con.ppp.tot, -con.ppp.inc) %>% 
    rbind(ENE.yearly.adj.ppp %>% filter(!iso2c %in% clean.poor$iso2c) %>% mutate_cond(con.ppp < mean.exp, con.adj.eq.ppp=con.ppp))
  
  print(c(sum(ENE.yearly.adj.ppp.clean$con.adj.eq.ppp), sum(ENE.yearly.adj.ppp.dirty$con.adj.eq.ppp)))
  
  # GDP difference between now and future
  gdp.diff <- ENE.yearly.adj %>% select(iso3c, gdp, contains("pop."), contains("gdp.pcap")) %>% mutate_at(vars(contains("gdp.pcap")), funs(.-gdp))
  gdp.diff <- gdp.diff %>% mutate_cond(gdp.pcap.SSP1 < 0, gdp.pcap.SSP1=0)
  Lc(gdp.diff$gdp.pcap.SSP1, gdp.diff$pop.SSP1, plot=TRUE)
  gini(ENE.yearly.adj$gdp.pcap.SSP1, ENE.yearly.adj$pop.SSP1)
  gini(gdp.diff$gdp.pcap.SSP1, gdp.diff$pop.SSP1)
  gini(ENE.yearly.adj$gdp.pcap.SSP2, ENE.yearly.adj$pop.SSP2)
  gini(gdp.diff$gdp.pcap.SSP2, gdp.diff$pop.SSP2)
  gini(ENE.yearly.adj$gdp.pcap.SSP4, ENE.yearly.adj$pop.SSP4)
  gini(gdp.diff$gdp.pcap.SSP4, gdp.diff$pop.SSP4)
}














### Obsolete routines


for(i in y.start:y.end) {
  ENE.yearly <- ENE %>% filter(year==i) %>% left_join(SSP1.2050) %>% left_join(SSP4.2050) # having most countries in 2007
  # ELEC.yearly <- ELEC %>% filter(year==i)  # having most countries in 2007
  GDP.yearly <- GDP %>% filter(year==i)  # having most countries in 2007
  
  # Lc(CON.ENE.PCAP.yearly$ene, CON.ENE.PCAP.yearly$pop, plot=T)
  # gini(CON.ENE.PCAP.yearly$ene, CON.ENE.PCAP.yearly$pop)
  e.gini <- append(e.gini, gini(ENE.yearly$ene, ENE.yearly$pop))
  # el.gini <- append(el.gini, gini(ELEC.yearly$elec, ENE.yearly$pop))
  c.gini.ppp <- append(c.gini.ppp, gini(ENE.yearly$con.ppp, ENE.yearly$pop))
  # c.gini.mer <- append(c.gini.mer, gini(ENE.yearly$con.mer, ENE.yearly$pop))
  g.gini <- append(g.gini, gini(GDP.yearly$gdp, ENE.yearly$pop))
  
  # print(paste0("Year=", i, ", Energy Gini=",gini(CON.ENE.PCAP.yearly$ene)))
  # print(paste0("Year=", i, ", Consumption Gini=",gini(CON.ENE.PCAP.yearly$con)))
  
  # eqn <- lm(log(elec)~log(con.ppp), ELEC.yearly, weights = pop)
  # m <- ggplot(ELEC.yearly, aes(x=con.ppp, y=el.inten)) + geom_point() +
  #   geom_text_repel(aes(label=iso3c), size = 2.5) 
  # m + geom_line(aes(x=con.ppp, y=exp(eqn$coefficients[1])*con.ppp^(eqn$coefficients[2]-1)))
  
  # eqn <- lm(log(ene)~log(gdp), ENE.yearly)
  # m <- ggplot(ENE.yearly, aes(x=gdp, y=e.inten)) + geom_point() +
  #   geom_text_repel(aes(label=iso3c), size = 2.5) 
  # m + geom_line(aes(x=gdp, y=exp(eqn$coefficients[1])*gdp^(eqn$coefficients[2]-1)))
  
  # eqn <- lm(log(elec)~log(gdp), ELEC.yearly)
  # m <- ggplot(ELEC.yearly, aes(x=gdp, y=el.inten)) + geom_point() +
  #   geom_text_repel(aes(label=iso3c), size = 2.5) 
  # m + geom_line(aes(x=gdp, y=exp(eqn$coefficients[1])*gdp^(eqn$coefficients[2]-1)))
  
  mean.exp <- weighted.mean(ENE.yearly$con.ppp, ENE.yearly$pop)
  ENE.yearly.adj <- ENE.yearly %>% 
    mutate(con.adj.eq.ppp = (con.ppp-mean.exp)*r.min+mean.exp, 
           con.adj.uneq.ppp = pmax(0, (con.ppp-mean.exp)*r.max+mean.exp)) # Need to be fixed to make the total unchanged
  # con.adj.eq.mer = (con.mer-mean(con.mer))*r.min.mer+mean(con.mer), 
  # con.adj.uneq.mer = pmax(0, (con.mer-mean(con.mer))*r.max.mer+mean(con.mer)))
  
  # colSums(ENE.yearly.adj %>% select(con.ppp, con.adj.eq.ppp, con.adj.uneq.ppp))
  ENE.yearly.adj %>% select(con.ppp, con.adj.eq.ppp, con.adj.uneq.ppp, pop) %>% summarise_all(funs(sum(.*pop)))
  # gini(ENE.yearly.adj$con.ppp, ENE.yearly.adj$pop)
  # gini(ENE.yearly.adj$con.adj.eq.ppp, ENE.yearly.adj$pop)
  # gini(ENE.yearly.adj$con.adj.uneq.ppp, ENE.yearly.adj$pop)
  
  # Plot - Expenditure distribution in the order of increasing wealth
  ggplot(ENE.yearly.adj %>% arrange(con.ppp) %>% mutate(num=1:length(con.ppp)), aes(x=num, y=con.ppp)) + geom_point() +
    geom_text_repel(aes(label=iso3c), size = 2.5) + ylab("Consumption per cap (PPP$ 2005)") +
    geom_point(aes(x=num, y=con.adj.eq.ppp, color="Gini=0.3")) + geom_point(aes(x=num, y=con.adj.uneq.ppp, color="Gini=0.55"))
  # print(c(gini(ENE.yearly.adj$con.adj.eq.ppp, ENE.yearly.adj$pop), gini(ENE.yearly.adj$con.adj.uneq.ppp, ENE.yearly.adj$pop)))
  
  # Primary energy vs. expenditure PPP
  # lm(log(ene)~log(con), CON.ENE.PCAP.yearly, weights=pop)
  eqn <- lm(log(ene)~log(con.ppp), ENE.yearly, weights=pop)
  elas <- eqn$coefficients[2]
  elas.min.cty <- 0.7
  elas.max.cty <- 1.1
  
  ENE.yearly.adj.ppp <- ENE.yearly.adj %>% 
    mutate(ene.adj.eq=ene*con.adj.eq.ppp^elas/con.ppp^elas, 
           ene.adj.uneq=ene*con.adj.uneq.ppp^elas/con.ppp^elas,
           ene.adj.eq.min=ene*con.adj.eq.ppp^elas.min.cty/con.ppp^elas.min.cty, 
           ene.adj.uneq.min=ene*con.adj.uneq.ppp^elas.min.cty/con.ppp^elas.min.cty)
  ENE.yearly.adj.ppp.varyelas <- ENE.yearly.adj %>% 
    mutate(ene.adj.eq=ene*con.adj.eq.ppp^elas/con.ppp^elas, 
           ene.adj.uneq=ene*con.adj.uneq.ppp^elas/con.ppp^elas,
           ene.adj.eq.min=ene*con.adj.eq.ppp^elas.min.cty/con.ppp^elas.min.cty, 
           ene.adj.uneq.min=ene*con.adj.uneq.ppp^elas.min.cty/con.ppp^elas.min.cty) %>%
    mutate_cond(con.ppp < mean.exp, ene.adj.eq.min=ene*con.adj.eq.ppp^elas.max.cty/con.ppp^elas.max.cty,
                ene.adj.uneq.min=ene*con.adj.uneq.ppp^elas.max.cty/con.ppp^elas.max.cty)
  # 2050 scenario
  ENE.yearly.adj.ppp <- ENE.yearly.adj %>% 
    mutate(ene.adj.eq=ene*con.adj.eq.ppp^elas/con.ppp^elas, 
           ene.adj.uneq=ene*con.adj.uneq.ppp^elas/con.ppp^elas,
           ene.adj.eq.min=ene*con.adj.eq.ppp^elas.min.cty/con.ppp^elas.min.cty, 
           ene.adj.uneq.min=ene*con.adj.uneq.ppp^elas.min.cty/con.ppp^elas.min.cty)
  
  n_group <- 10
  clean.poor <- ENE.yearly.adj.ppp %>% filter(con.ppp < mean.exp - 1000) %>% arrange(inten.ppp) %>% slice(1:n_group)
  dirty.poor <- ENE.yearly.adj.ppp %>% filter(con.ppp < mean.exp - 1000) %>% arrange(desc(inten.ppp)) %>% slice(1:n_group)
  clean.rich <- ENE.yearly.adj.ppp %>% filter(con.ppp >= mean.exp + 1000) %>% arrange(inten.ppp) %>% slice(1:n_group)
  dirty.rich <- ENE.yearly.adj.ppp %>% filter(con.ppp >= mean.exp + 1000) %>% arrange(desc(inten.ppp)) %>% slice(1:n_group)
  
  tot.decrease <- as.numeric(ENE.yearly.adj.ppp %>% mutate(con.diff.total=(con.ppp-con.adj.eq.ppp)*pop) %>% 
                               filter(con.diff.total>=0) %>% summarise(tot.decrease=sum(con.diff.total)))
  ENE.yearly.adj.ppp.dirty <- dirty.poor %>% mutate(con.ppp.tot = con.ppp*pop) %>% mutate(con.ppp.inc = tot.decrease * con.ppp.tot / sum(con.ppp.tot) / pop) %>% 
    mutate(con.adj.eq.ppp=con.ppp+con.ppp.inc) %>% select(-con.ppp.tot, -con.ppp.inc) %>% 
    rbind(ENE.yearly.adj.ppp %>% filter(!iso2c %in% dirty.poor$iso2c) %>% mutate_cond(con.ppp < mean.exp, con.adj.eq.ppp=con.ppp))
  ENE.yearly.adj.ppp.clean <- clean.poor %>% mutate(con.ppp.tot = con.ppp*pop) %>% mutate(con.ppp.inc = tot.decrease * con.ppp.tot / sum(con.ppp.tot) / pop) %>% 
    mutate(con.adj.eq.ppp=con.ppp+con.ppp.inc) %>% select(-con.ppp.tot, -con.ppp.inc) %>% 
    rbind(ENE.yearly.adj.ppp %>% filter(!iso2c %in% clean.poor$iso2c) %>% mutate_cond(con.ppp < mean.exp, con.adj.eq.ppp=con.ppp))
  
  print(c(sum(ENE.yearly.adj.ppp.clean$con.adj.eq.ppp), sum(ENE.yearly.adj.ppp.dirty$con.adj.eq.ppp)))
  # Compare the energy change ratio
  ENE.yearly.adj.ppp %>% select(ene, ene.adj.eq, ene.adj.uneq, ene.adj.eq.min, ene.adj.uneq.min, pop) %>% summarise_all(funs(sum(.*pop))) / 
    sum(ENE.yearly.adj$ene * ENE.yearly.adj$pop)  
  ENE.yearly.adj.ppp.varyelas %>% select(ene, ene.adj.eq, ene.adj.uneq, ene.adj.eq.min, ene.adj.uneq.min, pop) %>% summarise_all(funs(sum(.*pop))) / 
    sum(ENE.yearly.adj$ene * ENE.yearly.adj$pop)
  
  # Plot - Energy intensity vs expenditure
  m <- ggplot(ENE.yearly.adj.ppp.varyelas, aes(x=con.ppp, y=inten.ppp)) + geom_point() +
    geom_text_repel(aes(label=iso3c), size = 2.5) + 
    geom_point(aes(x=con.adj.eq.ppp, y=ene.adj.eq/con.adj.eq.ppp, color="measured elas")) + 
    geom_point(aes(x=con.adj.eq.ppp, y=ene.adj.eq.min/con.adj.eq.ppp, color="min.elas = 0.7"))
  m + geom_line(aes(x=con.ppp, y=exp(eqn$coefficients[1])*con.ppp^(eqn$coefficients[2]-1)))
  
  # Plot - Total energy per cap vs expenditure
  ggplot(ENE.yearly.adj.ppp.varyelas %>% arrange(con.ppp) %>% mutate(num=1:length(con.ppp)), aes(x=num, y=ene)) + geom_point() +
    geom_text_repel(aes(label=iso3c), size = 2.5) + ylab("Primary energy per cap (kgoe)") +
    geom_point(aes(x=num, y=ene.adj.eq, color="Gini=0.3, elas=1")) + geom_point(aes(x=num, y=ene.adj.eq.min, color="Gini=0.3, elas=worst"))
  ggplot(ENE.yearly.adj.ppp %>% arrange(con.ppp) %>% mutate(num=1:length(con.ppp)), aes(x=con.ppp, y=ene)) + geom_point() +
    geom_text_repel(aes(label=iso3c), size = 2.5) + ylab("Primary energy per cap (kgoe)") +
    geom_line(aes(x=con.ppp, y=exp(eqn$coefficients[1])*con.ppp^(eqn$coefficients[2])))
  
  # Primary energy vs. expenditure MER
  # lm(log(ene)~log(con), CON.ENE.PCAP.yearly, weights=pop)
  # eqn <- lm(log(ene)~log(con.mer), ENE.yearly, weights=pop)
  # elas <- eqn$coefficients[2]
  # elas.min.cty <- 0.7
  # Gini(ENE.yearly.adj$con.mer)
  # Gini(ENE.yearly.adj$con.adj.eq.mer)
  # Gini(ENE.yearly.adj$con.adj.uneq.mer)
  # ggplot(ENE.yearly.adj %>% arrange(con.mer) %>% mutate(num=1:length(con.mer)), aes(x=num, y=con.mer)) + geom_point() +
  #   geom_text_repel(aes(label=iso3c), size = 2.5) + ylab("Consumption per cap (MER$ 2000)") +
  #   geom_point(aes(x=num, y=con.adj.eq.mer, color="Gini=0.3")) + geom_point(aes(x=num, y=con.adj.uneq.mer, color="Gini=0.55"))
  # print(c(Gini(ENE.yearly.adj$con.adj.eq.mer), Gini(ENE.yearly.adj$con.adj.uneq.mer)))
  # 
  # ENE.yearly.adj.mer <- ENE.yearly.adj %>% 
  #   mutate(ene.adj.eq=ene*con.adj.eq.mer^elas/con.mer^elas, 
  #          ene.adj.uneq=ene*con.adj.uneq.mer^elas/con.mer^elas,
  #          ene.adj.eq.min=ene*con.adj.eq.mer^elas.min.cty/con.mer^elas.min.cty, 
  #          ene.adj.uneq.min=ene*con.adj.uneq.mer^elas.min.cty/con.mer^elas.min.cty)
  # colSums(ENE.yearly.adj.mer %>% select(ene, ene.adj.eq, ene.adj.uneq, ene.adj.eq.min, ene.adj.uneq.min)) / sum(ENE.yearly.adj$ene)
  # m <- ggplot(ENE.yearly.adj.mer, aes(x=con.mer, y=inten.mer)) + geom_point() +
  #   geom_text_repel(aes(label=iso3c), size = 2.5) + 
  #   geom_point(aes(x=con.adj.eq.mer, y=ene.adj.eq/con.adj.eq.mer, color="measured elas")) + 
  #   geom_point(aes(x=con.adj.eq.mer, y=ene.adj.eq.min/con.adj.eq.mer, color="min.elas = 0.7"))
  # m + geom_line(aes(x=con.mer, y=exp(eqn$coefficients[1])*con.mer^(eqn$coefficients[2]-1)))
  
}

# Showing e-, el- and c- gini changes in a plot
ggplot() + 
  geom_line(aes(x = y.start:y.end, y = e.gini, color = "Pr.Energy")) +
  geom_line(aes(x = y.start:y.end, y = c.gini, color = "Hh.Consumption/cap (PPP)")) +
  geom_line(aes(x = y.start:y.end, y = el.gini, color = "Electricity")) +
  geom_line(aes(x = y.start:y.end, y = g.gini, color = "GDP/cap (PPP)")) +
  ylab("Gini") + xlab("Year")
