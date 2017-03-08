library(rlist)

scenarios <- scenarios_main_devmin
# scenarios <- scenarios_main_frt
# scenarios <- scenarios_main_devmin100

### Automating the infeasibility detection
for (i in 1:length(scenarios)) {
  print(names(subset(scenarios[[i]], infeasible>0)))
  print(names(scenarios)[i])
}

# Script to show elements of the list
unlist(list.select(scenarios[[9]], obj$val))
list.select(scenarios[[9]], base_cal$val)

paramfile <- paste0('C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/Results/Parameter_outputs-', runname, '.xlsx')
list[Cost, Emission] <- GetTOtalQuantities(paramfile, scenarios)  # M.USD/yr   &   kTon CO2e/yr

# Automatically detect infeasible zones and deal with them (replace them w/ a feasible scenario)
info_infsbl <- lapply(scenarios, function(x) names(subset(x, infeasible>0)))
zone_infsbl <- info_infsbl[lapply(info_infsbl, length) > 0]

tc_gap <- Cost
te_gap <- Emission
for (i in 1:length(zone_infsbl)) {
  col_infsbl <- match(names(zone_infsbl)[i], names(Cost)) 
  
  tc_gap[row.names(Cost) %in% zone_infsbl[[i]], col_infsbl] <- NA
  row_infsbl <- is.na(tc_gap[, col_infsbl])
  tc_gap[row_infsbl, col_infsbl]  <- tc_gap$tc_min_cap[row_infsbl]
  # tc_gap[row_infsbl, col_infsbl]  <- tc_gap$te_min_cost[row_infsbl]
  
  te_gap[row.names(Cost) %in% zone_infsbl[[i]], col_infsbl] <- NA
  te_gap[row_infsbl, col_infsbl]  <- te_gap$tc_min_cap[row_infsbl]
  # te_gap[row_infsbl, col_infsbl]  <- te_gap$te_min_cost[row_infsbl]
}
tc_gap <- tc_gap %>% mutate_at(vars(contains("_min")), funs(.-base)) 
# tc_gap <- tc_gap %>% mutate_at(vars(c(starts_with("tc"),starts_with("te"),starts_with("dev"))), funs(.-base)) 
tc_gap[abs(tc_gap) < 1] <- 0

te_gap <- te_gap %>% mutate_at(vars(contains("_min")), funs(.-base)) 
# te_gap <- te_gap %>% mutate_at(vars(c(starts_with("tc"),starts_with("te"),starts_with("dev"))), funs(.-base)) 
te_gap[abs(te_gap) < 1] <- 0

# data frame for plotting
Cost_pl <- tc_gap %>% mutate_at(vars(contains("_min")), funs(./base)) %>% 
  mutate(zone = row.names(Cost)) %>%
  gather("scenario", "expenditure", 2:10) %>% arrange(zone) %>% mutate_cond(abs(expenditure) < 1e-10, expenditure=0) # USD/year
Cost_pl <- cbind(do.call("rbind", lapply(strsplit(Cost_pl$zone, ""), '[', -3)), Cost_pl) 
names(Cost_pl)[1:3] <- c("state", "urban", "inc")

Emission_pl <- te_gap %>% mutate_at(vars(contains("_min")), funs(./base)) %>% 
  mutate(zone = row.names(Emission)) %>%
  gather("scenario", "kgCO2e", 2:10) %>% arrange(zone) %>% mutate_cond(abs(kgCO2e) < 1e-10, kgCO2e=0) # kgCO2e/year
Emission_pl <- cbind(do.call("rbind", lapply(strsplit(Emission_pl$zone, ""), '[', -3)), Emission_pl) 
names(Emission_pl)[1:3] <- c("state", "urban", "inc")

tc_gap <- tc_gap %>% rbind(colSums(.)) 
te_gap <- te_gap %>% rbind(colSums(.))  
row.names(tc_gap) <- c(row.names(Cost), "tot")
row.names(te_gap) <- c(row.names(Emission), "tot")

Infeasibles <- Cost_pl %>% select(zone, scenario) %>% mutate(inf="Feasible") 
for (i in 1:length(zone_infsbl)) {
  Infeasibles <- Infeasibles %>% mutate_cond(zone %in% zone_infsbl[[i]] & scenario %in% names(zone_infsbl)[i], inf="Infeasible")
}

Cost_pl <- Cost_pl %>% left_join(Infeasibles)
Emission_pl <- Emission_pl %>% left_join(Infeasibles)


# National total cost and emission
# Main scenario
# paramfile <- 'C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/Results/Parameter_outputs_inc.xlsx'
# list[Cost, Emission] <- GetTOtalQuantities(paramfile, scenarios_main_prc)  # M.USD/yr   &   kTon CO2e/yr
# 
# # Infeasible (manually put together..) - new data (IFCT 2017 + FAO EF)
# inf1 <- c("N0_1", "N1_1", "S1_1", "W0_1", "W1_1")             # "te_min", "te_min_cap", "te_min_nogrp" & "te_min_khes"
# inf2 <- "S1_1"  # "min_pds
# 
# 
# # Gap total
# tc_gap <- Cost %>% 
#   mutate_cond(row.names(.) %in% inf1, te_min=NA, te_min_cap=NA, te_min_nogrp=NA, te_min_khes=NA) %>%
#   mutate_cond(row.names(.) %in% inf2, te_min_pds=NA) %>%
#   mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs(.-base)) %>% 
#   mutate_cond(is.na(te_min), te_min=te_min_cost) %>%
#   mutate_cond(is.na(te_min_cap), te_min_cap=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_pds), te_min_pds=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_nogrp), te_min_nogrp=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_khes), te_min_khes=te_min_cost) %>% 
#   rbind(colSums(.)) 
# tc_gap[abs(tc_gap) < 1] <- 0
# row.names(tc_gap) <- c(row.names(Cost), "tot")
# 
# # in kTon CO2e/yr
# te_gap <- Emission %>% 
#   mutate_cond(row.names(.) %in% inf1, te_min=NA, te_min_cap=NA, te_min_nogrp=NA, te_min_khes=NA) %>%
#   mutate_cond(row.names(.) %in% inf2, te_min_pds=NA) %>%
#   mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs(.-base)) %>% 
#   mutate_cond(is.na(te_min), te_min=te_min_cost) %>%
#   mutate_cond(is.na(te_min_cap), te_min_cap=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_pds), te_min_pds=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_nogrp), te_min_nogrp=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_khes), te_min_khes=te_min_cost) %>% 
#   rbind(colSums(.))
# te_gap[abs(te_gap) < 1] <- 0
# row.names(te_gap) <- c(row.names(Cost), "tot")

# # data frame for plotting
# Cost_pl <- Cost %>% mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs((.-base)/base)) %>% 
#   mutate(zone = row.names(Cost)) %>%
#   mutate_cond(zone %in% inf1, te_min=te_min_cost, te_min_cap=te_min_cost, te_min_nogrp=te_min_cost, te_min_khes=te_min_cost) %>%
#   mutate_cond(zone %in% inf2, te_min_pds=te_min_cost) %>%
#   gather("scenario", "expenditure", 2:10) %>% arrange(zone) %>% mutate_cond(abs(expenditure) < 1e-10, expenditure=0) # USD/year
# Cost_pl <- cbind(do.call("rbind", lapply(strsplit(Cost_pl$zone, ""), '[', -3)), Cost_pl) 
# names(Cost_pl)[1:3] <- c("state", "urban", "inc")
# 
# Emission_pl <- Emission %>% mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs((.-base)/base)) %>% 
#   mutate(zone = row.names(Emission)) %>%
#   mutate_cond(zone %in% inf1, te_min=te_min_cost, te_min_cap=te_min_cost, te_min_nogrp=te_min_cost, te_min_khes=te_min_cost) %>%
#   mutate_cond(zone %in% inf2, te_min_pds=te_min_cost) %>%
#   gather("scenario", "kgCO2e", 2:10) %>% arrange(zone) %>% mutate_cond(abs(kgCO2e) < 1e-10, kgCO2e=0) # kgCO2e/year
# Emission_pl <- cbind(do.call("rbind", lapply(strsplit(Emission_pl$zone, ""), '[', -3)), Emission_pl) 
# names(Emission_pl)[1:3] <- c("state", "urban", "inc")
# 
# Infeasibles <- Cost_pl %>% select(zone, scenario) %>% mutate(inf="Feasible") %>%
#   mutate_cond(zone %in% inf1 & (scenario=="te_min" | scenario=="te_min_cap" | scenario=="te_min_nogrp" | scenario=="te_min_khes"), 
#               inf="Infeasible") %>%
#   mutate_cond(zone %in% inf2 & scenario=="te_min_pds", inf="Infeasible") 
# 
# 
# # Sensitivity scenarios
# paramfile <- 'C:/Users/min/SharePoint/T/WS2 - Documents/Analysis/Food/diet_gms/Results/Parameter_outputs_inc.xlsx'
# list[Cost, Emission] <- GetTOtalQuantities(paramfile, scenarios_rice_sens)  # M.USD/yr   &   kTon CO2e/yr
# 
# # Infeasible (manually put together..) - new data (IFCT 2017 + FAO EF) - rice sensitivity
# inf1 <- c("N0_1", "N1_1", "S1_1", "S1_2", "W0_1", "W1_1")             # "te_min"
# inf2 <- c("N0_1", "N0_2", "N1_1", "N1_2", "S0_2", "S1_1", "S1_2", "W0_1", "W1_1")             # "te_min_cap"
# inf3 <- c("N0_1", "N1_1", "S1_1", "W0_1", "W1_1")             # "te_min_nogrp" & "te_min_khes"
# inf4 <- c("S1_1", "W0_1", "W1_1")       # "te_min_pds"
# 
# # Infeasible (manually put together..) - previous data
# # inf1 <- c("N1_1", "S1_1", "W0_1", "W1_1")             # "te_min" 
# # inf2 <- c("N0_1", "N1_1", "S1_1", "W0_1", "W1_1")    # "te_min_cap"
# # inf3 <- c("S1_1", "W0_1", "W1_1")    # "te_min_nogrp" & "te_min_khes"
# 
# # Gap total
# tc_gap <- Cost %>% 
#   mutate_cond(row.names(.) %in% inf1, te_min=NA) %>%
#   mutate_cond(row.names(.) %in% inf2, te_min_cap=NA) %>%
#   mutate_cond(row.names(.) %in% inf3, te_min_nogrp=NA, te_min_khes=NA) %>%
#   mutate_cond(row.names(.) %in% inf4, te_min_pds=NA) %>%
#   mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs(.-base)) %>% 
#   mutate_cond(is.na(te_min), te_min=te_min_cost) %>%
#   mutate_cond(is.na(te_min_cap), te_min_cap=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_pds), te_min_pds=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_nogrp), te_min_nogrp=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_khes), te_min_khes=te_min_cost) %>% 
#   rbind(colSums(.)) 
# tc_gap[abs(tc_gap) < 1] <- 0
# row.names(tc_gap) <- c(row.names(Cost), "tot")
# 
# # in kTon CO2e/yr
# te_gap <- Emission %>% 
#   mutate_cond(row.names(.) %in% inf1, te_min=NA) %>%
#   mutate_cond(row.names(.) %in% inf2, te_min_cap=NA) %>%
#   mutate_cond(row.names(.) %in% inf3, te_min_nogrp=NA, te_min_khes=NA) %>%
#   mutate_cond(row.names(.) %in% inf4, te_min_pds=NA) %>%
#   mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs(.-base)) %>% 
#   mutate_cond(is.na(te_min), te_min=te_min_cost) %>%
#   mutate_cond(is.na(te_min_cap), te_min_cap=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_pds), te_min_pds=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_nogrp), te_min_nogrp=te_min_cost) %>% 
#   mutate_cond(is.na(te_min_khes), te_min_khes=te_min_cost) %>% 
#   rbind(colSums(.))
# te_gap[abs(te_gap) < 1] <- 0
# row.names(te_gap) <- c(row.names(Cost), "tot")
# 
# # data frame for plotting
# Cost_pl <- Cost %>% mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs((.-base)/base)) %>% 
#   mutate(zone = row.names(Cost)) %>%
#   mutate_cond(zone %in% inf1, te_min=te_min_cost) %>%
#   mutate_cond(zone %in% inf2, te_min_cap=te_min_cost) %>%
#   mutate_cond(zone %in% inf3, te_min_nogrp=te_min_cost, te_min_khes=te_min_cost) %>%
#   mutate_cond(zone %in% inf4, te_min_pds=te_min_cost) %>%
#   gather("scenario", "expenditure", 2:10) %>% arrange(zone) %>% mutate_cond(abs(expenditure) < 1e-10, expenditure=0) # USD/year
# Cost_pl <- cbind(do.call("rbind", lapply(strsplit(Cost_pl$zone, ""), '[', -3)), Cost_pl) 
# names(Cost_pl)[1:3] <- c("state", "urban", "inc")
# 
# Emission_pl <- Emission %>% mutate_at(vars(c(starts_with("tc"),starts_with("te"))), funs((.-base)/base)) %>% 
#   mutate(zone = row.names(Emission)) %>%
#   mutate_cond(zone %in% inf1, te_min=te_min_cost) %>%
#   mutate_cond(zone %in% inf2, te_min_cap=te_min_cost) %>%
#   mutate_cond(zone %in% inf3, te_min_nogrp=te_min_cost, te_min_khes=te_min_cost) %>%
#   mutate_cond(zone %in% inf4, te_min_pds=te_min_cost) %>%
#   gather("scenario", "kgCO2e", 2:10) %>% arrange(zone) %>% mutate_cond(abs(kgCO2e) < 1e-10, kgCO2e=0) # kgCO2e/year
# Emission_pl <- cbind(do.call("rbind", lapply(strsplit(Emission_pl$zone, ""), '[', -3)), Emission_pl) 
# names(Emission_pl)[1:3] <- c("state", "urban", "inc")
# 
# Infeasibles <- Cost_pl %>% select(zone, scenario) %>% mutate(inf="Feasible") %>%
#   mutate_cond(zone %in% inf1 & scenario=="te_min", inf="Infeasible") %>%
#   mutate_cond(zone %in% inf2 & scenario=="te_min_cap", inf="Infeasible") %>%
#   mutate_cond(zone %in% inf3 & (scenario=="te_min_nogrp" | scenario=="te_min_khes"), inf="Infeasible") %>%
#   mutate_cond(zone %in% inf4 & scenario=="te_min_pds", inf="Infeasible") 




### Total emission/cost summary plot by cluster
excluded <- c("te_min_khes", "tc_min_nobf", "tc_min", "te_min") # , "tc_min_cap"
p_tc <- ggplot(Cost_pl %>% filter(!scenario %in% excluded), aes(x = state, y=expenditure*100, shape=scenario)) 
p_tc <- p_tc + geom_point(size=3) +  #geom_point(size=3, aes(shape=inf)) +
  # scale_colour_manual(values = rainbow(9)) +
  scale_shape_manual(values = c(2, 0, 20, 3, 1)) +
  labs(x="state", y="% change from baseline", title="Expenditures on food") +
  facet_grid(urban~inc, labeller=label_both) 


p_te <- ggplot(Emission_pl%>% filter(!scenario %in% excluded), aes(x = state, y=kgCO2e*100, shape=scenario)) 
p_te <- p_te + geom_point(size=3) +  #geom_point(size=3, aes(shape=inf)) +
  # scale_colour_manual(values = rainbow(9)) +
  scale_shape_manual(values = c(2, 0, 20, 3, 1)) +
  # ylim(-75, 15) +
  labs(x="state", y="% change from baseline", title="Non-CO2 emissions from food consumed") +
  facet_grid(urban~inc, labeller=label_both)

# grid.arrange(p_tc, p_te, nrow=2, ncol=1)


pct_pl <- Cost_pl %>% rename(pctval = expenditure) %>% mutate(type="Cost") %>%
  rbind(Emission_pl %>% rename(pctval = kgCO2e) %>% mutate(type="Emission")) %>% arrange(state, urban, inc) %>%
  mutate(urb = factor(urban, label=c("Rural", "Urban"))) %>% 
  mutate(income = factor(inc, label=c("Income Group 1", "Income Group 2", "Income Group 3", "Income Group 4"))) %>% 
  filter(!scenario %in% excluded) %>%
  mutate(Scenario = 0) %>%
  mutate_cond(scenario=="te_min_cap", Scenario=1) %>%
  mutate_cond(scenario=="te_min_cost", Scenario=2) %>%
  mutate_cond(scenario=="te_min_pds", Scenario=3) %>%
  mutate_cond(scenario=="te_min_nogrp", Scenario=4) %>%
  mutate_cond(scenario=="tc_min_cap", Scenario=5) %>%
  mutate_cond(scenario=="dev_min", Scenario=6) %>%
  mutate(Scenario = factor(Scenario, label=c("Reference", "Ref+NoBudget", "Ref+PDS", "Ref+FlexDiet", "MinCost", "DevMin"))) %>%
  mutate_cond(inf=="Infeasible", pctval=NA)

Baseline <- data.frame(a = c(-Inf, Inf), b = 0, Baseline = factor(0, label="Baseline") )
 
pdf(file =  paste0(workdir, "Figures/Optimization results-newEF and nutr2017-", runname, ".pdf"), width = 9, height = 11)
ggplot(pct_pl %>% filter(!is.na(pctval))) +
  geom_point(size=3, aes(x = state, y=pctval*100, shape=Scenario, colour = pctval <=0)) +
  scale_colour_manual(name="Below 0", values = setNames(c('black','darkgrey'),c(TRUE, FALSE))) +
  # scale_y_continuous(breaks=seq(-50,250,50)) +
  # scale_shape_manual(values = c(2, 0, 20, 3, 1)) +
  scale_shape_manual(values = c(2, 0, 20, 3, 1, 8)) +
  labs(x="state", y="% change from baseline")+
  geom_line(aes(a, b, linetype = Baseline), Baseline, color='red') +
  facet_grid(income~urb+type, labeller=label_value)  
dev.off()








# Total non-CO2 emission per capita for te_min_cap 
EmissionPerCap <- Emission %>% select(base, te_min_cap, te_min_cost, n_hh) %>%   # kTon CO2e/yr
  mutate(zone = row.names(Emission)) %>%
  mutate_cond(zone %in% inf1, te_min_cap=te_min_cost) 
EmissionPerCap <- cbind(do.call("rbind", lapply(strsplit(EmissionPerCap$zone, ""), '[', -3)), EmissionPerCap) 
names(EmissionPerCap)[1:3] <- c("state", "urban", "inc")
EmissionPerCap <- EmissionPerCap %>% select(-te_min_cost, -zone) %>% group_by(inc) %>% 
  summarise(totE_base = sum(base), totE_opt = sum(te_min_cap), n_hh = sum(n_hh)) %>% cbind(pop_inc) %>%
# EmissionPerCap <- EmissionPerCap %>% rbind(c(0, colSums(EmissionPerCap[,-1]))) 
  mutate(perCap_base=totE_base/pop_inc*1e6, perCap_opt=totE_opt/pop_inc*1e6)  # kgCO2e
sum(EmissionPerCap$totE_base) / sum(EmissionPerCap$pop_inc)*1e6
sum(EmissionPerCap$totE_opt) / sum(EmissionPerCap$pop_inc)*1e6
# sum(EmissionPerCap$totE_opt) /  *1e6
write.table(EmissionPerCap[,6:7], "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)




Consumption_kg <- GetTOtalQuantities_kg()  # kg/yr in NSS categories
Consumption_kg <- Consumption_kg %>% left_join(food_group) %>% select(-food_grp) %>% arrange(code) %>% 
  left_join(food_avgs_national %>% select(code, avg_price))
# Consumption_price <- Consumption_kg %>% mutate_at(vars(contains("min")), funs(.*avg_price))
# Total food expenditure (USD 2007 MER)
Consumption_price <- Consumption_kg %>% mutate_at(vars(contains("min")), 
                                                  funs(.*avg_price * PPP_IND / CPI_ratio_IND / EXR_IND )) #/ IND_con_grwth 

# Get hh size & n_hh by four income groups 
a <- hh_sum %>% group_by(inc_grp) %>%
  summarise(MA_avg=weighted.mean(male_adult, weight, na.rm=TRUE),
            FA_avg=weighted.mean(female_adult, weight, na.rm=TRUE),
            MM_avg=weighted.mean(male_minor, weight, na.rm=TRUE),
            FM_avg=weighted.mean(female_minor, weight, na.rm=TRUE),
            n_hh = sum(weight))
hh_size_inc <- rowSums(a[,2:5])

n_hh_inc <- hh_dem_cts %>% group_by(inc_grp) %>% summarise(n_hh = sum(n_hh))
pop_inc <- hh_size_inc*n_hh_inc$n_hh 
pop_share_inc <- pop_inc / sum(pop_inc)

# Survey total food consumption (USD 2007 MER)
# combined with the baseline from the optimization data
food_baseline_NSS <- IND_FD_code[1:331,] %>% select(item, total, CODE) %>% 
  left_join(select(Consumption_price, starts_with("te_min_cap"), code), by=c("CODE" = "code")) %>%
  mutate(total = total * PPP_IND / CPI_ratio_IND / EXR_IND  ) #/ IND_con_grwth
food_baseline_NSS <- food_baseline_NSS %>% 
  mutate_cond(CODE >= 280 & CODE <=290, total=0) %>% 
  mutate_cond(CODE >= 325, total=0) %>% 
  mutate_cond(is.na(te_min_cap_base), te_min_cap_base=total, te_min_cap_opt_tot=total,
              te_min_cap_opt_1=total*pop_share_inc[1], te_min_cap_opt_2=total*pop_share_inc[2],
              te_min_cap_opt_3=total*pop_share_inc[3], te_min_cap_opt_4=total*pop_share_inc[4]) %>%
  rename(NSS = total, Opt_baseline = te_min_cap_base, Opt_result = te_min_cap_opt_tot, 
         Opt_result1 = te_min_cap_opt_1, Opt_result2 = te_min_cap_opt_2, Opt_result3 = te_min_cap_opt_3, Opt_result4 = te_min_cap_opt_4)
colSums(food_baseline_NSS[,2:9])

### Deriving GJ/cap from food

# 1. Only from the Optimization items (114 of them)
a <- data.frame(code=as.numeric(row.names(CES_ICP_IND))) %>% left_join(Consumption_price) 
a[is.na(a)] <- 0
a.mat <- as.matrix(a %>% select(-item, -group, -code))
Consumption_ICP <- data.frame(t(CES_ICP_IND) %*% a.mat)   # $/yr in ICP categories
Consumption_ICP <- Consumption_ICP %>% select(starts_with("te_min_cap"))

colSums(Consumption_ICP * apply(IND_intensity, 2, mean)[1:151] / 1000) / c(sum(pop_inc), sum(pop_inc), pop_inc[1], pop_inc[4])

# 2. Including those food items excluded from the optimization
fd.mat <- as.matrix(food_baseline_NSS[,c(2,4:9)])
fd.mat[is.na(fd.mat)] <- 0
Consumption_base_ICP <- data.frame(t(CES_ICP_IND) %*% fd.mat)   # $/yr in ICP categories
foodEperCapita <- colSums(Consumption_base_ICP * apply(IND_intensity, 2, mean)[1:151] / 1000, na.rm=TRUE) / 
  c(sum(pop_inc), sum(pop_inc), sum(pop_inc), pop_inc)
foodEperCapita.sd <- colSums(Consumption_base_ICP * apply(IND_intensity, 2, sd)[1:151] / 1000, na.rm=TRUE) / 
  c(sum(pop_inc), sum(pop_inc), sum(pop_inc), pop_inc)
write.table(foodEperCapita, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)
write.table(foodEperCapita.sd, "clipboard", sep="\t", row.names = FALSE, col.names = TRUE)


# Survey total food consumption (USD 2007 MER)
# food_baseline_NSS <- IND_FD_code[1:150,2]
# compare_baseline_food <- cbind((IND_FD_ICP_usd2007)[1:45,1]*1e6 / scaler_IND, Consumption_ICP[1:45,1])
# compare_baseline_food[compare_baseline_food[,2]==0,]
