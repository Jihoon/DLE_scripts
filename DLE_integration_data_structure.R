# df.dummy <- data.frame(year=Year.obs, setNames(data.frame(matrix(ncol = length(DLE.sectors), nrow = length(Year.obs))), DLE.sectors))
# df.unit <- list(TFEI=df.dummy, use.pcap=df.dummy, E.tot=df.dummy)
# df.cty <- list(sc.GDP=df.unit, sc.DLE.base=df.unit, sc.DLE.LCT=df.unit, sc.DLE.LCTBHV=df.unit, sc.BAU=df.unit)
# DLE.scenario <- list(IND=df.cty, BRA=df.cty, ZAF=df.cty)
# DLE.param <- list(IND=list(pop=Year.obs, gdp=Year.obs), 
#                   BRA=list(pop=Year.obs, gdp=Year.obs), 
#                   ZAF=list(pop=Year.obs, gdp=Year.obs)) 

# Classes for DLE structure
DLE.component <- setRefClass("DLE.component", 
            fields=list(TFEI.OP="data.frame", # Can be long or wide. may have elec/non-elec breakdown
                        TFEI.CON="data.frame",
                        # TFEI.OP="data.frame",
                        # TFEI.OP="data.frame",
                        # TFEI.OP="data.frame",
                        units="data.frame", 
                        FE.tot="numeric"),
            methods = list(
              getFE.tot = function() {
                FE.tot <<- (TFEI.OP+TFEI.CON) * units
              },
              populate = function(rollout, intensity) {
                units <<- rollout
                TFEI.OP <<- intensity %>% filter(OpCon=="OP") %>% select(-OpCon)
                TFEI.CON <<- intensity %>% filter(OpCon=="CON") %>% select(-OpCon)
              }
            ))
DLE.scenario <- setRefClass("DLE.scenario", 
            fields=list(Housing="DLE.component", # Can be long or wide 
                        Clothing="DLE.component",
                        Food="DLE.component",
                        Health="DLE.component",
                        Education="DLE.component",
                        Water="DLE.component",
                        Sanitation="DLE.component",
                        Cooking="DLE.component",
                        TV="DLE.component",
                        Fridge="DLE.component",
                        AC="DLE.component",
                        Cellphone="DLE.component",
                        Mobility="DLE.component", # Will be car/bus/train/etc later
                        # Aggregate sectors
                        Shelter="numeric",
                        Moility.all="numeric",
                        Appliance="numeric",
                        Food="numeric"
                        # More possibly
                        ),
            methods = list(
              aggregateComps = function() {
                Shelter <<- Housing$FE.tot + Clothing$FE.tot
                Appliance <<- Cooking$FE.tot + TV$FE.tot + Fridge$FE.tot + AC$FE.tot
              },
              populateScenario = function(rollout, intensity) {
                Housing$populate(rollout %>% select(Year, Housing), intensity %>% select(Year, Housing, OpCon, Carrier))
                Water$populate(rollout %>% select(Year, Water), intensity %>% select(Year, Water, OpCon, Carrier))
                Sanitation$populate(rollout %>% select(Year, Sanitation), intensity %>% select(Year, Sanitation, OpCon, Carrier))
                Cooking$populate(rollout %>% select(Year, Cooking), intensity %>% select(Year, Cooking, OpCon, Carrier))
                TV$populate(rollout %>% select(Year, TV), intensity %>% select(Year, TV, OpCon, Carrier))
                Fridge$populate(rollout %>% select(Year, Fridge), intensity %>% select(Year, Fridge, OpCon, Carrier))
                AC$populate(rollout %>% select(Year, AC), intensity %>% select(Year, AC, OpCon, Carrier))
                Cellphone$populate(rollout %>% select(Year, Cellphone), intensity %>% select(Year, Cellphone, OpCon, Carrier))
                Mobility$populate(rollout %>% select(Year, Mobility), intensity %>% select(Year, Mobility, OpCon, Carrier))
              }
            ))
DLE.country <- setRefClass("DLE.country",
            fields=list(Accel="DLE.scenario",
                        Accel.LCT="DLE.scenario",
                        Accel.LCT.BHV="DLE.scenario",
                        BAU="DLE.scenario",
                        GDP="numeric",
                        rollout="data.frame",
                        intensity="data.frame"),
            methods = list(
              populateData = function() {
                Accel$populateScenario(extractScenario("Accel", rollout), extractScenario("Accel", intensity))
                Accel.LCT$populateScenario(extractScenario("Accel.LCT", rollout), extractScenario("Accel.LCT", intensity))
                Accel.LCT.BHV$populateScenario(extractScenario("Accel.LCT.BHV", rollout), extractScenario("Accel.LCT.BHV", intensity))
                BAU$populateScenario(extractScenario("BAU", rollout), extractScenario("BAU", intensity))
              }
              
            ))
# Assume two table inputs: Rollout.mat, TFEI.mat
DLE.sc <- list(IND=DLE.country(rollout=Rollout.mat %>% filter(country=="IND"), intensity=TFEI.mat %>% filter(country=="IND")), 
               BRA=DLE.country(rollout=Rollout.mat %>% filter(country=="BRA"), intensity=TFEI.mat %>% filter(country=="BRA")),  
               # BRA=DLE.country(rollout=Rollout.mat %>% filter(country=="BRA"), intensity=TFEI.mat %>% filter(country=="BRA")),
               ZAF=DLE.country(rollout=Rollout.mat %>% filter(country=="ZAF"), intensity=TFEI.mat %>% filter(country=="ZAF")))

# extractScenario = function(df, field.name, match) {
#   return(df[df[[field.name]] == match,])
# }
# 
# 
# # Clothing
# # list.select(DLE.scenario$IND, use.pcap$Clothing)
# # list.update(DLE.scenario$IND, use.pcap$Clothing=1:5)
# # Function trial
# updateDLEscenario.use <- function(scenarios="All", sect=DLE.sectors, val) {
#   lapply(DLE.scenario, function(cty) {
#     if(scenarios=="All") lapply(cty, function(sc) {sc$use.pcap[, match(sect, names(sc$use.pcap))] <- val})
#     else {
#       match(scenarios, names(cty))
#     }
#   })
# }
# 




### Manually populating DLE.scenario
# Clothing
# Only the flow matters for trajectories 
# DLE.scenario$IND$sc.BAU$E.tot$Clothing <- mean(tot.clothing.IND) * dle.clothing.pcap.IND / avg.clothing.pcap.IND +
#   mean(tot.footwear.IND) * dle.footwear.pcap.IND / avg.footwear.pcap.IND
# DLE.scenario$BRA$sc.BAU$E.tot$Clothing <- mean(tot.clothing.BRA) * dle.clothing.pcap.BRA / avg.clothing.pcap.BRA +
#   mean(tot.footwear.BRA) * dle.footwear.pcap.BRA / avg.footwear.pcap.BRA
# DLE.scenario$ZAF$sc.BAU$E.tot$Clothing <- mean(tot.clothing.ZAF) * dle.clothing.pcap.ZAF / avg.clothing.pcap.ZAF +
#   mean(tot.footwear.ZAF) * dle.footwear.pcap.ZAF / avg.footwear.pcap.ZAF





 












geoSeq <- function(base, r, n) {
  return(base * (1+r)^(1:n))
}

