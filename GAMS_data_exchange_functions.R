igdx("C:/GAMS/win64/24.4")

OrganizeOptOutputs <- function(scenariosOpt) {
  
  l <- length(scenariosOpt)
  
  l_consum <- list()
  l_param <- list()
  l_nutr <- list()
  
  for(i in 1:l) {
    list[l_consum[[i]], l_param[[i]], l_nutr[[i]]] <- OptimizationResultSummary(scenarios[[i]])
    # only for MA now
    xlsx::write.xlsx(ArrConsumption(l_consum[[i]], "MA"), "Consumption_outputs_inc.xlsx", names(scenariosOpt)[i], 
                     row.names = FALSE, col.names = TRUE, append = ifelse(i==1, FALSE, TRUE))
    xlsx::write.xlsx(l_param[[i]], "Parameter_outputs_inc.xlsx", names(scenariosOpt)[i], 
                     row.names = TRUE, col.names = TRUE, append = ifelse(i==1, FALSE, TRUE))
    xlsx::write.xlsx(t(l_nutr[[i]]), "Nutrient_outputs_inc.xlsx", names(scenariosOpt)[i], 
                     row.names = TRUE, col.names = TRUE, append = ifelse(i==1, FALSE, TRUE))
    print(paste0("Writing files... ", names(scenariosOpt)[i]))
    if (sum(l_consum[[i]][,-1] <0)) {
      name_NF <- names(l_consum[[i]][,-1])[which((l_consum[[i]][,-1] <0), arr.ind = TRUE)[,2]]   
      print(paste0("Infeasible at ", name_NF))}
    Sys.sleep(2)
  }
  
  # Only when all entities in scenariosOpt are based on the same clusters.
  clsnames <- names(scenariosOpt[[1]]) 
  names(l_consum) <- names(scenariosOpt) 
  ll <- length(clsnames) # 8 or 16
  
  for(j in 1:ll) {
    conbycls <- CombineOutputByCLS(l_consum, clsnames[j], "MA")
    xlsx::write.xlsx(conbycls, "Consumption_outputs_inc.xlsx", 
                     sheetName=clsnames[j], row.names = FALSE, col.names = TRUE, append=TRUE)
    print(paste0("Writing files... ", clsnames[j]))
    Sys.sleep(3)
  }
}

OptResultsForPlot <- function(scenariosOpt) {
  
  n_scene <- length(scenariosOpt)
  n_cls <- length((scenarios)[[1]])
  
  # Total cost and emission for now (scenarios[[i]][[2]])
  TC <- matrix(, nrow = n_cls, ncol = 0)
  TE <- matrix(, nrow = n_cls, ncol = 0)
  
  for(i in 1:n_scene) {
    Scene <- OptimizationResultSummary(scenarios[[i]])[[2]] 
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
  row.names(TC) <- names((scenarios)[[1]])
  row.names(TE) <- names((scenarios)[[1]])
  names(TC) <- c("base", names(scenarios))
  names(TE) <- c("base", names(scenarios))
  
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
  nutri <- data.frame(matrix(ncol = 0, nrow = 4))
  
  for (i in names(result_opt)) {
    
    # Result consumption
    snapshot <- data.frame(result_opt[[i]]$x$val) %>% spread(key=X2, value = X3, sep="_") %>%
      rename(num=X1) %>% right_join(data.frame(num=1:114))
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
  }
  
  consumption <- data.frame(item=nutrients$item, consumption)
  row.names(params_opt) <- c("Baseline TC", "Result TC", "Baseline TE", "Result TE")
  row.names(nutri) <- c("MA", "FA", "MM", "FM")
  
  return(list(consumption, params_opt, nutri))
}


rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}