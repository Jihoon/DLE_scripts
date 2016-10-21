require(dplyr)
require(tidyr)

# Function to compute number of individuals/households and average
# income/expenditure for arbitrary set of groups, given specified Gini
# coefficient and group break points (assuming log-Normal distribution)

# Inputs: 
# std dev of lognormal: derived from the Gini coefficient in calling code
# mean_value: Mean value in same units as break points (now using $PPP2010/cap/day)
# break_points: vector of n-1 break points defining group
#   boundaries, where n is the desired number of groups

#--------------------
computeGroups = function(sdlog, mean_value, break_points) {
  
  
  # Compute mean of log-Normal distribution given input
  # std dev and mean income/expenditure
  
  mlog = log(mean_value)-0.5*sdlog^2
  
  # Share of total population (either persons or households depending on input
  # definitions) in each group
  temp = sapply(break_points, plnorm, meanlog=mlog, sdlog=sdlog)
  group_shares = diff(c(0, temp, 1))  
  
  # Sets sufficiently large upper bound to avoid occassional numerical
  # integration error when using 'Inf' instead
  b = c(0, break_points, qlnorm(1-1e-6, meanlog=mlog, sdlog=sdlog))  
  
  # Function to calculate mean value (e.g. mean expenditure) in each group
  func = function(x) x * dlnorm(x, mean=mlog, sd=sdlog)
  intF = function(j) {
    v1 = integrate(func, b[j], b[j+1], rel.tol=1e-5)$value
    v2 = (plnorm(b[j+1], mean=mlog, sd=sdlog) - plnorm(b[j], mean=mlog, sd=sdlog))
    v1 / v2
  }
  
  # Mean expenditure within each group
  group_means = sapply(1:(length(b)-1), intF)
  
  result = data.frame(group_shares, group_means)
  return(result)
  
}

##########################################
# This code takes national data for India (gini, urbanization rate and mean income) and returns
# urban and rural income groups with population shares, given a set of income break points
# It calls the function above (compute_groups). It creates a data frame for SSP1, 2, 3, from 2010-2050,
# and manually adds data for SSP2 for now (Feb 12, 2016).
# INPUTS: specify breakpoints below in code b(). 
#         series of national Ginis, urbanization rate, and mean income for India (same units as above)
# OUTPUTS: two lists, urban and rural, showing group pop shares and mean income for each breakpoint, 
#          each list is for a combination of SSP/year

input <- expand.grid(
   year=c(2010,2015,2020,2025,2030,2035,2040,2045,2050),
   ssp=1:3
  )

# these data are taken from the FEb 10, 2016 spreadsheet sent to SCHEMA, IncomeDistWorksht-Feb2016.xls
input$gini<-c(0,0,0,0,0,0,0,0,0,0.375,0.3827,0.3959,0.4079,0.4173,0.4228,0.4272,0.4312,0.4351,0,0,0,0,0,0,0,0,0)
input$urb_rate<-c(0,0,0,0,0,0,0,0,0,0.3114,0.32,0.34,0.35,0.36,0.37,0.39,0.40,0.41,0,0,0,0,0,0,0,0,0)
input$inc<-c(0,0,0,0,0,0,0,0,0,5.25,7.1,8.8,11.5,14.0,17.3,20.5,24.5,28.3,0,0,0,0,0,0,0,0,0)

erf.inv = function(x) qnorm((x + 1)/2)/sqrt(2)

input=input %>%
  # the income relationship only holds because we assume Urb Inc is twice Rural Inc in India 
  mutate(rur_inc=inc/(1+urb_rate),urb_inc=2*rur_inc, sdlog=2*erf.inv(gini),theil=sdlog^2/2) %>%
  mutate(ur_theil=theil-log(urb_rate*2+(1-urb_rate),10)+urb_rate*log(2,10),ur_sdlog=sqrt(ur_theil*2))

  
# $PPP2010/cap/day breakpoints for income. You can enter any (and any number of) values here.
bpoints = c(2,5,10)

#create output data structures. Each index (row) corresponds to a SSP/year combination. 
# the data are ordered by SSP, year (in 5-yr increments). So, rows 1-9 are SSP1, 2010 to 2050.

urban = lapply(1:nrow(input), 
               FUN=function(i) computeGroups(input$ur_sdlog[i], input$urb_inc[i], break_points=bpoints)
)

rural = lapply(1:nrow(input), 
               FUN=function(i) computeGroups(input$ur_sdlog[i], input$rur_inc[i], break_points=bpoints)
)

# Look at SSP2, 2010 in the list of results
urban[[10]]
rural[[10]]





