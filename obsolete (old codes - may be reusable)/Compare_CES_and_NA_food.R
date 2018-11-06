# All in M.USD 2007 MER
food_survey <- data.frame(item = ICP_catnames, tot2007 = IND_FD_ICP_usd2007[,1], tot2011 = IND_FD_ICP_usd2011[,1])[1:47,]   
food_exio <- data.frame(item = t(EX_catnames), tot2007 = IND_fd_exio)[c(1:14, 19, 43:54),]
View(food_survey)
View(food_exio)

colSums(food_survey[,2:3])
sum(food_exio[,2])
