# Try to understand why some sectors in CES FD are adjusted by large margins
# Run at the breakpoint at line 413 of Bridge_RAS.R

a <- rowSums(result_RAS_fixed)
a[-idx_row_removed] <- a[-idx_row_removed] + rowCon_RAS
compare_a <- cbind(1:151, rowConst_init, a, abs(rowConst_init-a), abs(rowConst_init-a)/rowConst_init*100)
compare_a <- compare_a[order(compare_a[,5], decreasing = TRUE),]

View(cbind(compare_a, ICP_catnames[compare_a[,1]]))

# France row 108 : Other financial services n.e.c.
rownum <- 108
Q_mtx <- Q_UN_EXIO

# France row 138 : Other financial services n.e.c.
rownum <- 138
Q_mtx <- Q_UN_ICP_EXIO
rowConst_init[rownum]
rowConst_init[rownum]
which(Q_mtx[rownum,]==1)
# colConst_init[which(Q_mtx[rownum,]==1)]
colConst_init[Q_mtx[rownum,]==1]
colSums(Q_mtx)[which(Q_mtx[rownum,]==1)]

# check the initial points are well distributed
b <- do.call("rbind", lapply(bridge_draw, '[', 4,1))
hist(b)

# 
int_median <- apply(IND_inten_RAS_all, 2, mean) # Get medians/means for each sector
int_dev <- apply(IND_inten_RAS_all, 1, function(x) {x-int_median})  # Get deviations from the median/mean for each draw 
View(int_dev)
hist(colSums(int_dev), 100)
sum(colSums(int_dev))
a <- apply(int_dev, 2, function(x) {acf(x, plot=FALSE)})
do.call("rbind", lapply(a, '[', 4,1))


# Try to understand 'solid fuel' intensity difference bewteen diff Q mtx.
idx_other_fuel <- 65  # ICP
idx_solid_fuel <- 35  # COICOP
other_fuel_NT <- which(bridge_ICP_EXIO_q[idx_other_fuel,-1]==1)
other_fuel_UN <- which(Q_UN_ICP_EXIO[idx_other_fuel,]==1)
solid_fuel_NT <- which(bridge_COICOP_EXIO_q[idx_solid_fuel,-1]==1)
solid_fuel_UN <- which(Q_UN_EXIO[idx_solid_fuel,]==1)

a <- apply(do.call("rbind", lapply(final_alloc_list_IND_noVal, '[', idx_other_fuel, other_fuel_NT)), 2, mean) 
names(a) <- EX_catnames[other_fuel_NT]
a <- sort(a)
b <- apply(do.call("rbind", lapply(final_alloc_list_IND_all, '[', idx_other_fuel, other_fuel_UN)), 2, mean)
names(b) <- EX_catnames[other_fuel_UN]
b <- sort(b)

a1 <- apply(do.call("rbind", lapply(final_alloc_list_FRA_noVal, '[', idx_solid_fuel,solid_fuel_NT)), 2, mean)
names(a1) <- EX_catnames[solid_fuel_NT]
a1 <- sort(a1)
b1 <- apply(do.call("rbind", lapply(final_alloc_list_FRA_all, '[', idx_solid_fuel,solid_fuel_UN)), 2, mean)
names(b1) <- EX_catnames[solid_fuel_UN]
b1 <- sort(b1)

