require(RJDBC)
require(dplyr)
require(reldist)
require(ineq)
# Create Oracle DB connection via RJDBC (Java)
options(java.parameters = "-Xmx8g")
drv = JDBC("oracle.jdbc.driver.OracleDriver","P:/ene.model/Rcodes/Common_files/ojdbc6.jar", identifier.quote="\"") 
conn = dbConnect(drv, "jdbc:oracle:thin:@gp3.iiasa.ac.at:1521:gp3", "hh_data", "hh_data")

# Function to pull *_HH tables from Oracle DB
getHouseholdData = function(surveys) bind_rows(lapply(paste0(surveys,'_HH'), function(x) dbReadTable(conn, x)))

# Pull data from DB for following surveys
surveys = c('IND1', 'IND4')
hh = getHouseholdData(surveys) %>% 
  group_by(SURVEY, URBAN)
hh$POP=hh$WEIGHT*hh$HH_SIZE

# Compute Gini coefficient for each survey by urban/rural status
gini2 = function(x, weights=rep(1,length(x))) {
  ind = which(!is.na(x))
  gini(x[ind], weights[ind])
}

out = hh %>% 
  summarize_each(
    funs(gini2(./HH_SIZE, weights=WEIGHT*HH_SIZE)),
    INCOME, EXPENDITURE)
        
erf.inv = function(x) qnorm((x + 1)/2)/sqrt(2)

out=out %>%
  na.omit(INCOME) %>%
  mutate(sdlog_inc=2*erf.inv(INCOME)) %>%
#  mutate(sdlog_exp=2*erf.inv(EXPENDITURE))

    # Calc Lorenz curve for data
f= function(x) {data.frame(p_inc=Lc(x$INCOME,x$POP)$p, L_inc=Lc(x$INCOME,x$POP)$L)}  
 
test = hh %>%
   group_by(SURVEY, URBAN) %>% 
   do(f(.))

x= filter(hh, SURVEY=="IND4" & URBAN==1)
lc_ihds_u=Lc(x$INCOME,x$POP, plot=T)
lc_theo_u=lc_ihds_u
Lc.lognorm(lc_theo_u,parameter=)
lines(Lc.lognorm,parameter=sd(log(x$INCOME)),col=4)

# write.table(out3,file="H:/IIASA/SCHEMA/India_state_ginis.csv")