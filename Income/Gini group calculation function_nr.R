# Function to compute number of individuals/households and average
# income/expenditure for arbitrary set of groups, given specified Gini
# coefficient and group break points (assuming log-Normal distribution)

# Inputs: 
# gini_value: Gini coefficient 
# mean_value: Mean value in same units as break points
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

#--------------------

# Example inputs:
#gini_value = 0.35
#mean_value = 1000
#break_points = c(250,1000,2500,5000)

# Example usage
test = computeGroups(0.35, 1e3, c(250,1000,2500,5000))

#--------------------
# END
#--------------------