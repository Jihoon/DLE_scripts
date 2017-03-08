igdx("C:/GAMS/win64/24.4")


# Returns a list of outputs for all clusters based on one scenario
RunFoodOpt <- function(scenario="tc_min", cluster="reg-urb", nutri) { #"reg-urb" or "reg-urb-inc"
  
  setwd(work_path)
  gms_file <- "DLE_diet_gdx_slack.gms"
  # gms_file <- "DLE_diet_gdx.gms"
  
  unlink("DLE_output_*")
  unlink("DLE_*.log")
  unlink("*PCB082_1*")
  
  # Init set names
  food_items <- list(items_to_optimize$item)
  food_groups <- list(grp_names)
  hh_member <- c("male-adult", "female-adult", "male-child", "female-child")
  pop_groups <- list(hh_member)
  nut_groups <- list(c("protein", "iron", "zinc", "vita"))
  
  # Constants (nutrients, energy intensity)
  # Re-format for wgdx
  idx <- 1:dim(items_to_optimize)[1]
  c <- cbind(idx, nutri$energy)
  e <- cbind(idx, ef_all$ef_per_kg_eaten)
  
  # Define the GAMS entities (lists)
  f  <- list(name='f',  type='set', uels=food_items, ts='Food items')
  fg  <- list(name='fg',  type='set', uels=food_groups, ts='Food groups')
  a  <- list(name='a',  type='parameter', dim=2, form='full', uels=c(food_items, nut_groups), 
             val= as.matrix(nutri %>% ungroup() %>% select(protein, iron, zinc, vita)), 
             ts="nutritive value of foods (mg/g per kg)", domains=c("f", "n"))
  c  <- list(name='c',  type='parameter', dim=1, form='sparse', uels=food_items, 
             val= c, 
             ts="calorie content of food f (kcal per kg)", domains="f")
  e  <- list(name='e',  type='parameter', dim=1, form='sparse', uels=food_items, 
             val= e, 
             ts="non-CO2e emission factor of food f (kgCO2e per kg)", domains="f")
  
  # For "te_min_nogrp" we combine meat/fish and other protein groups
  if (scenario=="te_min_nogrp") {
    grp_map <- group_map_pt
  } else if (scenario=="te_min_khes") {
    grp_map <- group_map_kh
  } else {
    grp_map <- group_map
  }
  
  grp <- list(name='grp',  type='parameter', dim=2, form='full', uels=c(food_items, food_groups), 
              val= as.matrix(grp_map %>% select(-item)), 
              ts="Mapping of food items to major groups", domains=c("f", "fg"))
  
  result_cluster <- list()
  
  if (cluster=="reg-urb") {
    clsnames <- unique(food_by_cluster$cluster)
  }  else if (cluster=="reg-urb-inc") {
    clsnames <- unique(food_by_cls_inc$cls_inc)
  }
  
  # Optimize by cluster
  for (i in clsnames) {
    
    hh_sz <- t(hh_size_cls%>%filter(clsname==i)%>%select(-clsname))
    
    nn <- cbind(1:length(pop_groups[[1]]), hh_sz)  # Assume one person per pop-group
    nn <- list(name='nn', type='parameter', dim=1, form='sparse', uels=pop_groups, 
               val= nn, 
               ts="number of people of type p (num)", domains="p")
    
    # Reformat for wgdx
    pr <- cbind(idx, eval(parse(text=paste0("price_by_cls$price_", i))))
    r <-  cbind(idx, eval(parse(text=paste0("kg_by_cls$kg_", i))))
    ig <-  cbind(idx, eval(parse(text=paste0("ignore$ign_", i))))
    
    # consumption per person
    r <- r[,c(2,2,2,2)] %*% diag(CU$cu_eq)  
    
    # Define the GAMS entities (lists)
    pr <- list(name='pr', type='parameter', dim=1, form='sparse', uels=food_items, 
               val= pr, # ordered alphabetically
               ts="price of food f ($ per kg)", domains="f")
    r  <- list(name='r',  type='parameter', dim=2, form='full', uels=c(food_items, pop_groups), 
               val= r, 
               ts="current level of food f intake by person of type p (kg per person)", domains=c("f", "p"))
    ig  <- list(name='ig', type='parameter', dim=1, form='sparse', uels=food_items, 
                val= ig, # ordered alphabetically
                ts="items to be ignored from the optimization because of too little consumption", domains="f")
    
    wgdx(paste0(work_path, "DLE_data.gdx"), f, fg, a, pr, c, e, r, grp, nn, ig)  # Not sure how I can use gdx with diff names in the .gms file
    # Sys.sleep(1)
    params <- paste0('DNLP=COUENNE LP=CPLEX Gdx=DLE_output_', i, ' LogOption=2 Logfile=DLE_log_', i, '.log //scenario=', scenario) #--INPUT=DLE_data.gdx 
    flag <- gams(paste(gms_file, params))
    
    if(flag) {
      print(paste0("Cluster ",i, ": Flag=", flag))
      # next()
    }
    
    result <- list()
    # Sys.sleep(0.2)
    
    result$obj <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="dvcon"))
    result$x <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="x"))
    result$base_tc <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="report_baseline_cost"))
    result$result_tc <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="report_result_cost"))
    result$base_tem <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="report_baseline_emission"))
    result$result_tem <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="report_result_emission"))
    result$base_nutri <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="report_baseline_nutri"))
    result$result_nutri <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="report_result_nutri"))
    result$base_cal <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="report_baseline_cal"))
    result$result_cal <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="report_result_cal"))
    result$base_kg <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="report_baseline_kg"))
    result$result_kg <- rgdx(paste0("DLE_output_", i, ".gdx"), list(name="report_result_kg"))
    result$infeasible <- sum(rgdx(paste0("DLE_output_", i, ".gdx"), list(name="s_cost_fix"))$val[,3]) +
      sum(rgdx(paste0("DLE_output_", i, ".gdx"), list(name="s_cal"))$val[,3]) +
      sum(rgdx(paste0("DLE_output_", i, ".gdx"), list(name="s_nutri"))$val[,3]) +
      sum(rgdx(paste0("DLE_output_", i, ".gdx"), list(name="s_maxcap"))$val[,3]) +
      sum(rgdx(paste0("DLE_output_", i, ".gdx"), list(name="s_mincap"))$val[,3])
    
    result_cluster[[i]] <- result
  }
  
  unlink("DLE_output_*")
  return(result_cluster)
  # Sys.sleep(2)
}

# Write GDX for one case (mainly for debugging)
WriteOptGDX <- function(scenario="tc_min", cluster="reg-urb-inc", zone="E0_1", nutri) { #"reg-urb" or "reg-urb-inc"
  
  setwd(work_path)
  gms_file <- "DLE_diet_gdx.gms"
  
  # Init set names
  food_items <- list(items_to_optimize$item)
  food_groups <- list(grp_names)
  hh_member <- c("male-adult", "female-adult", "male-child", "female-child")
  pop_groups <- list(hh_member)
  nut_groups <- list(c("protein", "iron", "zinc", "vita"))
  
  # Constants (nutrients, energy intensity)
  # Re-format for wgdx
  idx <- 1:dim(items_to_optimize)[1]
  c <- cbind(idx, nutri$energy)
  e <- cbind(idx, ef_all$ef_per_kg_eaten)
  
  # Define the GAMS entities (lists)
  f  <- list(name='f',  type='set', uels=food_items, ts='Food items')
  fg  <- list(name='fg',  type='set', uels=food_groups, ts='Food groups')
  a  <- list(name='a',  type='parameter', dim=2, form='full', uels=c(food_items, nut_groups), 
             val= as.matrix(nutri %>% ungroup() %>% select(protein, iron, zinc, vita)), 
             ts="nutritive value of foods (mg/g per kg)", domains=c("f", "n"))
  c  <- list(name='c',  type='parameter', dim=1, form='sparse', uels=food_items, 
             val= c, 
             ts="calorie content of food f (kcal per kg)", domains="f")
  e  <- list(name='e',  type='parameter', dim=1, form='sparse', uels=food_items, 
             val= e, 
             ts="non-CO2e emission factor of food f (kgCO2e per kg)", domains="f")
  
  # For "te_min_nogrp" we combine meat/fish and other protein groups
  if (scenario=="te_min_nogrp") {
    grp_map <- group_map_pt
  } else if (scenario=="te_min_khes") {
    grp_map <- group_map_kh
  } else {
    grp_map <- group_map
  }
  
  grp <- list(name='grp',  type='parameter', dim=2, form='full', uels=c(food_items, food_groups), 
              val= as.matrix(grp_map %>% select(-item)), 
              ts="Mapping of food items to major groups", domains=c("f", "fg"))
  
  result_cluster <- list()
  
  if (cluster=="reg-urb") {
    clsnames <- unique(food_by_cluster$cluster)
  }  else if (cluster=="reg-urb-inc") {
    clsnames <- unique(food_by_cls_inc$cls_inc)
  }
  
  hh_sz <- t(hh_size_cls%>%filter(clsname==zone)%>%select(-clsname))
  
  nn <- cbind(1:length(pop_groups[[1]]), hh_sz)  # Assume one person per pop-group
  nn <- list(name='nn', type='parameter', dim=1, form='sparse', uels=pop_groups, 
             val= nn, 
             ts="number of people of type p (num)", domains="p")
  
  # Reformat for wgdx
  pr <- cbind(idx, eval(parse(text=paste0("price_by_cls$price_", zone))))
  r <-  cbind(idx, eval(parse(text=paste0("kg_by_cls$kg_", zone))))
  ig <-  cbind(idx, eval(parse(text=paste0("ignore$ign_", zone))))
  
  # consumption per person
  r <- r[,c(2,2,2,2)] %*% diag(CU$cu_eq)  
  
  # Define the GAMS entities (lists)
  pr <- list(name='pr', type='parameter', dim=1, form='sparse', uels=food_items, 
             val= pr, # ordered alphabetically
             ts="price of food f ($ per kg)", domains="f")
  r  <- list(name='r',  type='parameter', dim=2, form='full', uels=c(food_items, pop_groups), 
             val= r, 
             ts="current level of food f intake by person of type p (kg per person)", domains=c("f", "p"))
  ig <- list(name='ig', type='parameter', dim=1, form='sparse', uels=food_items, 
             val= ig, # ordered alphabetically
             ts="items to be ignored from the optimization because of too little consumption", domains="f")
  
  wgdx(paste0(work_path, "DLE_data.gdx"), f, fg, a, pr, c, e, r, grp, nn, ig)  # Not sure how I can use gdx with diff names in the .gms file
}


OrganizeOptOutputs <- function(scenariosOpt) {
  
  l <- length(scenariosOpt)
  
  l_consum <- list()
  l_param <- list()
  l_kg <- list()
  l_nutr <- list()
  
  for(i in 1:l) {
    list[l_consum[[i]], l_param[[i]], l_nutr[[i]], l_kg[[i]]] <- OptimizationResultSummary(scenariosOpt[[i]])
    # only for MA now
    xlsx::write.xlsx(ArrConsumption(l_consum[[i]], "MA"), paste0(work_path, "Results/Consumption_outputs-", runname, ".xlsx"), names(scenariosOpt)[i], 
                     row.names = FALSE, col.names = TRUE, append = ifelse(i==1, FALSE, TRUE))
    xlsx::write.xlsx(l_param[[i]], paste0(work_path, "Results/Parameter_outputs-", runname, ".xlsx"), names(scenariosOpt)[i], 
                     row.names = TRUE, col.names = TRUE, append = ifelse(i==1, FALSE, TRUE))
    xlsx::write.xlsx(t(l_nutr[[i]]), paste0(work_path, "Results/Nutrient_outputs-", runname, ".xlsx"), names(scenariosOpt)[i], 
                     row.names = TRUE, col.names = TRUE, append = ifelse(i==1, FALSE, TRUE))
    xlsx::write.xlsx(l_kg[[i]], paste0(work_path, "Results/Kg_outputs-", runname, ".xlsx"), names(scenariosOpt)[i],
                     row.names = FALSE, col.names = TRUE, append = ifelse(i==1, FALSE, TRUE))
    print(paste0("Writing files... ", names(scenariosOpt)[i]))
    
    slacks <- unlist(list.select(scenariosOpt[[i]], infeasible))>0

    if (sum(slacks)>0) {
  # if (sum(l_consum[[i]][,-1] <0)) {
      # name_NF <- names(l_consum[[i]][,-1])[which((l_consum[[i]][,-1] <0), arr.ind = TRUE)[,2]]   
      # print(paste0("Infeasible at ", name_NF))}
      print(paste0("Infeasible at ", names(scenariosOpt[[i]])[slacks]))}
    Sys.sleep(3)
  }
  
  # Only when all entities in scenariosOpt are based on the same clusters.
  clsnames <- names(scenariosOpt[[1]]) 
  names(l_consum) <- names(scenariosOpt) 
  ll <- length(clsnames) # 8 or 16
  
  for(j in 1:ll) {
    conbycls <- CombineOutputByCLS(l_consum, clsnames[j], "MA")
    xlsx::write.xlsx(conbycls, paste0(work_path, "Results/Consumption_outputs-", runname, ".xlsx"), 
                     sheetName=clsnames[j], row.names = FALSE, col.names = TRUE, append=TRUE)
    print(paste0("Writing files... ", clsnames[j]))
    Sys.sleep(3)
  }
}

OptResultsForPlot <- function(scenariosOpt) {
  
  n_scene <- length(scenariosOpt)
  n_cls <- length((scenariosOpt)[[1]])
  
  # Total cost and emission for now (scenarios[[i]][[2]])
  TC <- matrix(, nrow = n_cls, ncol = 0)
  TE <- matrix(, nrow = n_cls, ncol = 0)
  
  for(i in 1:n_scene) {
    Scene <- OptimizationResultSummary(scenariosOpt[[i]])[[2]] 
    if (i==1) {
      TC <- cbind(TC, t(Scene[c(1,2),]))
      TE <- cbind(TE, t(Scene[c(3,4),]))
    } 
    else {
      TC <- cbind(TC, t(Scene[2,]))
      TE <- cbind(TE, t(Scene[4,]))
    }
  }
  
  TC <- data.frame(TC)
  TE <- data.frame(TE)
  row.names(TC) <- names((scenariosOpt)[[1]])
  row.names(TE) <- names((scenariosOpt)[[1]])
  names(TC) <- c("base", names(scenariosOpt))
  names(TE) <- c("base", names(scenariosOpt))
  
  # PlotBar(TC, TE)
  
  return(list(TC, TE))
}



# PlotBar <- function(cost, emission) {
#   
#   pl <- list()
#   
#   for(i in 1:dim(cost)[1]) {
#     
#   }
#   ggplot(data.m, aes(Names, value)) +   
#     geom_bar(aes(fill = variable), position = "dodge", stat="identity")
#   
#   p4 <- ggplot(data=nutri_cost %>% arrange(vita) %>% slice((n()-n_slice+1):n()), aes(x=1/p_vita, y=1000/e_vita))+
#     geom_point(size=3, aes(color=vita))+ 
#     scale_color_gradientn(colours=matlab.like2(200), trans='log', name = "ug Vitamin A/kg food", breaks=c(250, 1000, 4000, 16000)) + 
#     theme(legend.position="bottom") + 
#     scale_x_continuous(labels = waiver(), trans = "log10") +
#     scale_y_continuous(labels = waiver(), trans = "log10") +
#     labs(x='$/ug Vitamin A',y="gCO2e/ug Vitamin A")+
#     geom_text(data=nutri_cost %>% arrange(vita) %>% slice((n()-n_slice+1):n()), aes(label=abbr), hjust=0, vjust=0, size=3) 
#   


ArrConsumption <- function(consumption, type) {
  idx <- grep(type, names(consumption))
  wgrp <- food_wgrp %>% mutate_cond(item=="Khesari", group="PRTN")
  return(consumption[,c(1,idx)] %>% left_join(wgrp) %>% arrange(group) %>% filter(group!="OTH")) %>% select(item, group, everything())
}

CombineOutputByCLS <- function(consumption_l, cls, type) {
  
  n <- length(consumption_l)
  # a <- matrix(0, nrow = n_fooditem, ncol = n+2) # ncol needs to have item and baseline consumption.
  for(i in 1:n) {
    a <- ArrConsumption(consumption_l[[i]], type)
    idx <- grep(paste0(cls,"_opt_",type), names(a))
    if (i==1) {b <- a[,c(1,2,idx-1)]}
    b <- cbind(b, a[,idx])  
  }
  names(b)[c(-1,-2)] <- c("base",names(consumption_l))
  return(b)  
}


OptimizationResultSummary <- function(result_opt) {
  
  consumption <- data.frame(matrix(ncol = 0, nrow = n_fooditem))
  params_opt <- data.frame(matrix(ncol = 0, nrow = 4))
  kg_opt <- data.frame(matrix(ncol = 0, nrow = n_fooditem))
  nutri <- data.frame(matrix(ncol = 0, nrow = 4))
  
  for (i in names(result_opt)) {
    
    # Result consumption
    snapshot <- data.frame(result_opt[[i]]$x$val) %>% spread(key=X2, value = X3, sep="_") %>%
      rename(num=X1) %>% right_join(data.frame(num=1:n_fooditem))
    snapshot[is.na(snapshot)] <- 0
    
    # Baseline consumption
    baseline <- rep.col(eval(parse(text=paste0("kg_by_cls$kg_", i))), 4) %*% diag(CU$cu_eq)
    
    m <- cbind(baseline, snapshot[,-1])                   # combine
    m <- data.frame(m[, c(matrix(1:ncol(m), nrow = 2, byrow = T))])   # then reorder
    names(m) <- paste0(i, c("_base_MA", "_opt_MA", "_base_FA", "_opt_FA", "_base_MM", "_opt_MM", "_base_FM", "_opt_FM"))
    consumption <- cbind(consumption, m)
    
    # Baseline and result nutrition
    bsln <- data.frame(result_opt[[i]]$base_nutri$val) %>% spread(key=X2, value = X3, sep="_") 
    rslt <- data.frame(result_opt[[i]]$result_nutri$val) %>% spread(key=X2, value = X3, sep="_") 
    m <- cbind(bsln[,-1], rslt[,-1])                   # combine
    m <- data.frame(m[, c(matrix(1:ncol(m), nrow = 2, byrow = T))])   # then reorder
    names(m) <- paste0(i, c("_base_PRTN", "_opt_PRTN", "_base_Fe", "_opt_Fe", "_base_Zn", "_opt_Zn", "_base_VA", "_opt_VA"))
    nutri <- cbind(nutri, m)
    
    # Total cost and emission
    param <- data.frame(c(result_opt[[i]]$base_tc$val, result_opt[[i]]$result_tc$val, 
                          result_opt[[i]]$base_tem$val, result_opt[[i]]$result_tem$val))
    names(param) <- i
    params_opt <- cbind(params_opt, param)
    
    # Total kg
    n_food <- data.frame(item_no=1:n_fooditem)
    kg_base <- data.frame(result_opt[[i]]$base_kg$val)
    names(kg_base) <- c("item_no", paste0(i, "kg_base"))
    kg_result <- data.frame(result_opt[[i]]$result_kg$val)
    names(kg_result) <- c("item_no", paste0(i, "kg_opt"))
    kg_summary <- n_food %>% left_join(kg_base) %>% left_join(kg_result) %>% select(-item_no)
    kg_summary[is.na(kg_summary)] <- 0
    kg_opt <- cbind(kg_opt, kg_summary)
  }
  
  consumption <- data.frame(item=food_summary$item, consumption)
  kg_opt <- data.frame(item=food_summary$item, kg_opt)
  row.names(params_opt) <- c("Baseline TC", "Result TC", "Baseline TE", "Result TE")
  row.names(nutri) <- c("MA", "FA", "MM", "FM")
  
  return(list(consumption, params_opt, nutri, kg_opt))
}


rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}




### Get total cost and emission (default output from GAMS run)
GetTOtalQuantities <- function(filename, scenariosOpt) {
  
  n_cls <- length((scenariosOpt)[[1]])
  n_scene <- length(scenariosOpt)
  
  TotC <- matrix(, nrow = n_cls, ncol = 0)
  TotE <- matrix(, nrow = n_cls, ncol = 0)
  
  for (j in 1:n_scene) {
    params_opt = read_excel(filename, sheet=names(scenariosOpt)[j])
    if (j==1) {
      TotC <- cbind(TotC, t(params_opt[c(1,2),-1]))
      TotE <- cbind(TotE, t(params_opt[c(3,4),-1]))
    }  else  {
      TotC <- cbind(TotC, t(params_opt[2,-1]))
      TotE <- cbind(TotE, t(params_opt[4,-1]))
    }
  }
  
  TotC <- data.frame(TotC, hh_dem_cts %>% ungroup() %>% select(n_hh))
  TotE <- data.frame(TotE, hh_dem_cts %>% ungroup() %>% select(n_hh))
  
  names(TotC) <- c("base", names(scenariosOpt), "n_hh")
  names(TotE) <- c("base", names(scenariosOpt), "n_hh")
  
  TotC <- TotC %>% mutate_at(1:(n_scene+1), funs(.*n_hh/1e6))  # M.USD/yr
  TotE <- TotE %>% mutate_at(1:(n_scene+1), funs(.*n_hh/1e6))  # kTon CO2e/yr
  
  row.names(TotC) <- names((scenariosOpt)[[1]])
  row.names(TotE) <- names((scenariosOpt)[[1]])
  
  return(list(TotC, TotE))
}


### Get total food kg consumptions
GetTOtalQuantities_kg <- function(filename, scenariosOpt) {
  
  # n_fooditem <- dim(items_to_optimize)[1]
  n_scene <- length(scenariosOpt)
  
  Tot_kg <- matrix(, nrow = n_fooditem, ncol = 0)
  hh_count <- rep(as.matrix(hh_dem_cts %>% ungroup() %>% select(n_hh)), each=2)
  
  for (j in 1:n_scene) {
    consum_opt = read_excel(filename, sheet=names(scenariosOpt)[j])   # kg total per food item of a household
    
    if (j == 5) {  # To deal with those infeasible groups (<- replace them with the counterparts from "te_min_cost" scenario)
      replace_inf <- read_excel(filename, sheet="te_min_cost")   # kg total per food item of a household
      
      vars <- paste0(inf2, "kg_opt")
      consum_opt[,vars] <- replace_inf[,vars]
    }
    
    kg <- consum_opt[,-1]
    sum_kg <- sweep(kg, 2, hh_count, '*')
    base_tot <- rowSums(sum_kg[,seq(1, ncol(sum_kg), by = 2)])        # national total kg at the baseline (survey)
    opt_tot <- rowSums(sum_kg[,seq(2, ncol(sum_kg), by = 2)])         # national total kg optimized 
    # opt_tot_low_inc <- rowSums(sum_kg[,seq(2, ncol(sum_kg), by = 8)])  # total kg for lowest income group
    # opt_tot_hi_inc <- rowSums(sum_kg[,seq(8, ncol(sum_kg), by = 8)])   # total kg for highest income group
    opt_tot_1 <- rowSums(sum_kg[,seq(2, ncol(sum_kg), by = 8)])  # total kg for 1 income group
    opt_tot_2 <- rowSums(sum_kg[,seq(4, ncol(sum_kg), by = 8)])   # total kg for 2 income group
    opt_tot_3 <- rowSums(sum_kg[,seq(6, ncol(sum_kg), by = 8)])  # total kg for 3 income group
    opt_tot_4 <- rowSums(sum_kg[,seq(8, ncol(sum_kg), by = 8)])   # total kg for 4 income group
    
    tot <- data.frame(base_tot, opt_tot, opt_tot_1, opt_tot_2, opt_tot_3,opt_tot_4)
    names(tot) <- paste0(names(scenariosOpt)[j], "_", c("base", "opt_tot", "opt_1", "opt_2", "opt_3", "opt_4"))
    # tot <- data.frame(base_tot, opt_tot, opt_tot_low_inc, opt_tot_hi_inc)
    # names(tot) <- paste0(names(scenariosOpt)[j], "_", c("base", "opt_tot", "opt_1", "opt_4"))
    
    Tot_kg <- cbind(Tot_kg, tot)
  }
  
  return(cbind(food_wgrp,Tot_kg))
}


getcu= function(group) {
  x = cu_eqs %>%
    filter(group==Group) %>%
    select(cu_eq)
  return(as.numeric(x))
}


### Function for grid legend

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
