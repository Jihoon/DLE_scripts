# Intend to be standalone, based on EXIO3 2015
library(countrycode)
library(tidyverse)

IO.year <- 2015

source("Import_EXIO3.R")


#### EXIO sector catorization ####

exio_food_idx <- 43:53
exio_clth_idx <- 55:57
exio_hlth_idx <- 175
exio_educ_idx <- 174
exio_sect_idx <- list(food=exio_food_idx, clothing=exio_clth_idx, health=exio_hlth_idx, education=exio_educ_idx)

#### EXIO-MESSAGE region mapping ####
reg_map <- read_csv("P:/ene.model/data/regions/message - R12.csv") 
names(reg_map) = c("ISO","name","msg_reg","five_reg")
reg_map <- reg_map %>% mutate(cty.exio=countrycode(ISO, 'iso3c', 'iso2c')) %>% drop_na() %>%
  select(cty.exio, msg_reg)

df.exio_ctys <- data.frame(cty.exio=exio_ctys) %>% left_join(reg_map) %>% 
  mutate(msg_reg=ifelse(cty.exio=="WF", NA, msg_reg))
MSG_reg <- unique(reg_map$msg_reg)

exio_reg_idx <- vector(mode = "list", length = length(MSG_reg))
names(exio_reg_idx) <- MSG_reg

for (r in MSG_reg) {
  exio_reg_idx[[r]] <- which(df.exio_ctys$msg_reg==r)  
}

# Deal with the RoW EXIO regions
exio_reg_idx[["MEA"]] <- which(df.exio_ctys$cty.exio=="WM") 
exio_reg_idx[["RCPA"]] <- which(df.exio_ctys$cty.exio=="WA") 
exio_reg_idx[["AFR"]] <- append(exio_reg_idx[["AFR"]], which(df.exio_ctys$cty.exio=="WF"))
exio_reg_idx[["LAM"]] <- append(exio_reg_idx[["LAM"]], which(df.exio_ctys$cty.exio=="WL"))
exio_reg_idx[["EEU"]] <- append(exio_reg_idx[["EEU"]], which(df.exio_ctys$cty.exio=="WE"))


# Indices of Sectoral grouping
EXIOCategoryIndex <- function(exio.idx) {
  idx <- as.vector(sapply(seq(0,exio.len-200,200), function(x) x+exio.idx, simplify = "array"))
  return(idx)
}

# Indices of MSG region grouping in L mtx
EXIORegionIndex <- function(exio.idx) {
  idx <- as.vector(sapply(1:200, function(x) x+200*(exio.idx-1), simplify = "array"))
  return(sort(idx))
}

# Indices of MSG region grouping in FD mtx
EXIORegionIndex.FD_hh <- function(exio.idx) {
  idx <- as.vector(sapply(1:4, function(x) x+7*(exio.idx-1), simplify = "array"))
  return(sort(idx))
}



# final_demand[intersect(EXIORegionIndex(exio_reg_idx[["WEU"]]), EXIOCategoryIndex(exio_food_idx)), ]

tfe_industry <- tfe_transport <- tfe_feedstock <- 
  tfe_elec_industry <- tfe_elec_transport <- tfe_elec_feedstock <- 
    tfe_n.elec_industry <- tfe_n.elec_transport <- tfe_n.elec_feedstock <- 
      tfe_total_elec <- tfe_total_n.elec <- 
      list()

# Allocation of each region's TFE by each dim
sum_tfe_elec <- list()
sum_tfe_n.elec <- list()

for (r in MSG_reg) {
  for (s in 1:length(exio_sect_idx)) {
    zero_fd <- final_demand
    zero_fd[,] <- 0
    zero_fd[EXIOCategoryIndex(exio_sect_idx[[s]]), EXIORegionIndex.FD_hh(exio_reg_idx[[r]])] <- 
      final_demand[EXIOCategoryIndex(exio_sect_idx[[s]]), EXIORegionIndex.FD_hh(exio_reg_idx[[r]])]
    reg_fd <- zero_fd
    reg_fd <- diag(rowSums(reg_fd))
    print(paste("Region:", r, ", Sector:", names(exio_sect_idx)[s], "- calculating Industry TFE"))
    tfe_industry[[r]]=as.matrix(tfei.sub$NTRA) %*% as.matrix(reg_fd)
    print(paste("Region:", r, ", Sector:", names(exio_sect_idx)[s], "- calculating Transportation TFE"))
    # tfei.exio is the sum of TRAs and NTRA
    tfe_transport[[r]]=as.matrix(tfei.exio - tfei.sub$NTRA) %*% as.matrix(reg_fd)
    print(paste("Region:", r, ", Sector:", names(exio_sect_idx)[s], "- calculating Feedstock TFE"))
    tfe_feedstock[[r]]=as.matrix(tfei.sub$NENE) %*% as.matrix(reg_fd)
    print(paste("Region:", r, ", Sector:", names(exio_sect_idx)[s], "- calculating total elec TFE"))
    tfe_total_elec[[r]]=as.matrix(tfei.elec) %*% as.matrix(reg_fd)
    print(paste("Region:", r, ", Sector:", names(exio_sect_idx)[s], "- calculating total non-elec TFE"))
    tfe_total_n.elec[[r]]=as.matrix(tfei.non.elec) %*% as.matrix(reg_fd)
    
    print(paste("Region:", r, ", Sector:", names(exio_sect_idx)[s], "- subsetting elec/non_elec Industry TFE"))
    tfe_elec_industry[[r]] = tfe_industry[[r]]
    tfe_elec_industry[[r]][-idx.Elec.carrier,] <- 0 
    tfe_n.elec_industry[[r]] = tfe_industry[[r]]
    tfe_n.elec_industry[[r]][idx.Elec.carrier,] <- 0
    
    print(paste("Region:", r, ", Sector:", names(exio_sect_idx)[s], "- subsetting elec/non_elec Transportation TFE"))
    tfe_elec_transport[[r]] = tfe_transport[[r]]
    tfe_elec_transport[[r]][-idx.Elec.carrier,] <- 0 
    tfe_n.elec_transport[[r]] = tfe_transport[[r]]
    tfe_n.elec_transport[[r]][idx.Elec.carrier,] <- 0
    
    print(paste("Region:", r, ", Sector:", names(exio_sect_idx)[s], "- subsetting elec/non_elec Feedstock TFE"))
    tfe_elec_feedstock[[r]] = tfe_feedstock[[r]]
    tfe_elec_feedstock[[r]][-idx.Elec.carrier,] <- 0 
    tfe_n.elec_feedstock[[r]] = tfe_feedstock[[r]]
    tfe_n.elec_feedstock[[r]][idx.Elec.carrier,] <- 0
    
    for (rg in MSG_reg) {
      print(paste("Region:", rg, ", Sector:", names(exio_sect_idx)[s], "- summarizing elec TFE for each demand region"))
      sum_tfe_elec[[names(exio_sect_idx)[s]]][["Ind"]][[r]][[rg]] <- sum(tfe_elec_industry[[r]][,EXIORegionIndex(exio_reg_idx[[rg]])], na.rm = TRUE) # total ind FE demand from rg due to consumption of 'sector' in r 
      sum_tfe_elec[[names(exio_sect_idx)[s]]][["Trp"]][[r]][[rg]] <- sum(tfe_elec_transport[[r]][,EXIORegionIndex(exio_reg_idx[[rg]])], na.rm = TRUE) # total trp FE demand from rg due to consumption of 'sector' in r
      sum_tfe_elec[[names(exio_sect_idx)[s]]][["Fds"]][[r]][[rg]] <- sum(tfe_elec_feedstock[[r]][,EXIORegionIndex(exio_reg_idx[[rg]])], na.rm = TRUE) # total fds FE demand from rg due to consumption of 'sector' in r
      sum_tfe_elec[[names(exio_sect_idx)[s]]][["Tot"]][[r]][[rg]] <- sum(tfe_total_elec[[r]][,EXIORegionIndex(exio_reg_idx[[rg]])], na.rm = TRUE) # total fds FE demand from rg due to consumption of 'sector' in r
      
      print(paste("Region:", rg, ", Sector:", names(exio_sect_idx)[s], "- summarizing non_elec TFE for each demand region"))
      sum_tfe_n.elec[[names(exio_sect_idx)[s]]][["Ind"]][[r]][[rg]] <- sum(tfe_n.elec_industry[[r]][,EXIORegionIndex(exio_reg_idx[[rg]])], na.rm = TRUE) # total ind FE demand from rg due to consumption of 'sector' in r 
      sum_tfe_n.elec[[names(exio_sect_idx)[s]]][["Trp"]][[r]][[rg]] <- sum(tfe_n.elec_transport[[r]][,EXIORegionIndex(exio_reg_idx[[rg]])], na.rm = TRUE) # total trp FE demand from rg due to consumption of 'sector' in r
      sum_tfe_n.elec[[names(exio_sect_idx)[s]]][["Fds"]][[r]][[rg]] <- sum(tfe_n.elec_feedstock[[r]][,EXIORegionIndex(exio_reg_idx[[rg]])], na.rm = TRUE) # total fds FE demand from rg due to consumption of 'sector' in r
      sum_tfe_n.elec[[names(exio_sect_idx)[s]]][["Tot"]][[r]][[rg]] <- sum(tfe_total_n.elec[[r]][,EXIORegionIndex(exio_reg_idx[[rg]])], na.rm = TRUE) # total fds FE demand from rg due to consumption of 'sector' in r
    }
  }
}

# alloc_food_elec = as.data.frame(rbindlist(sum_tfe_elec[["food"]], fill=TRUE, use.names = TRUE, idcol = "sector")) %>% mutate_if(is.numeric, round)

# Need to collapse the nested list into a dataframe
list.elec = do.call(c, unlist(sum_tfe_elec, recursive=FALSE))
list.n.elec = do.call(c, unlist(sum_tfe_n.elec, recursive=FALSE))
df.summary.elec = as.data.frame(do.call(rbind, list.elec)) %>% rownames_to_column("code") %>% 
  separate(code, c("dle_dim", "sector", "src_reg"))  %>% mutate(across(is.list, as.numeric)) %>% 
  mutate(total.share = rowSums(.[4:15])) 
df.summary.n.elec = as.data.frame(do.call(rbind, list.n.elec)) %>% rownames_to_column("code") %>% 
  separate(code, c("dle_dim", "sector", "src_reg"))  %>% mutate(across(is.list, as.numeric)) %>% 
  mutate(total.share = rowSums(.[4:15])) 
 
df.summary.elec = df.summary.elec %>% 
  left_join(
    df.summary.elec %>% filter(sector=='Tot') %>% group_by(dle_dim, src_reg) %>% select(tot.dmd=total.share)) 
df.summary.n.elec = df.summary.n.elec %>% 
  left_join(
    df.summary.n.elec %>% filter(sector=='Tot') %>% group_by(dle_dim, src_reg) %>% select(tot.dmd=total.share)) 

df.share.elec = df.summary.elec %>% group_by(dle_dim, sector, src_reg) %>% 
  mutate(across(LAM:tot.dmd, ~./tot.dmd)) %>% select(-tot.dmd) %>% write.csv(., file = "DLE_elec_share.csv")
df.share.n.elec = df.summary.n.elec %>% group_by(dle_dim, sector, src_reg) %>% 
  mutate(across(LAM:tot.dmd, ~./tot.dmd)) %>% select(-tot.dmd) %>% write.csv(., file = "DLE_non_elec_share.csv")
