##Plot SSP income distributions, inputs: per capita GDP (PPP2010$/cap), and Ginis for 2030 for SSP1-4
require(plyr)  # Needed by 'caret'; must be loaded before dplyr
require(dplyr)
require(tidyr)
require(reldist)
require(Hmisc)
require(rms)
require(data.table)
require(ineq)

erf.inv = function(x) 2*(qnorm((x + 1)/2)/sqrt(2))

ginis_2010<-c(0.416, 0.416, 0.416, 0.416)
ginis_2030<-c(0.424, 0.478, 0.524, 0.55)
ginis_2050<-c(0.404, 0.512, 0.566, 0.643)
ginis_2100<-c(0.355, 0.673, 0.69, 0.703)

ginis=data.frame("2010"=ginis_2010,"2030"=ginis_2030,"2050"=ginis_2050,"2100"=ginis_2100)
colnames(ginis)<-c("2010","2030","2050","2100")
rownames(ginis)<-c("SSP1", "SSP2", "SSP3", "SSP4")

sdlogs<-apply(ginis,c(1,2),FUN=erf.inv)

grid=seq(0,1,.1)
plot(grid,grid,type="l",xlab="Cumulative Population",ylab="Cumulative Income")
lines(Lc.lognorm,parameter=sdlogs["SSP2","2010"],col="black")

grid=seq(0,1,.1)
plot(grid,grid,type="l",xlab="Cumulative Population",ylab="Cumulative Income")
lines(Lc.lognorm,parameter=sdlogs["SSP2","2030"],col="red")
lines(Lc.lognorm,parameter=sdlogs["SSP4","2030"],col="orange")
lines(Lc.lognorm,parameter=sdlogs["SSP1","2030"],col="blue")
lines(Lc.lognorm,parameter=sdlogs["SSP3","2030"],col="green")

grid=seq(0,1,.1)
plot(grid,grid,type="l",xlab="Cumulative Population",ylab="Cumulative Income")
lines(Lc.lognorm,parameter=sdlogs["SSP2","2050"],col="red")
lines(Lc.lognorm,parameter=sdlogs["SSP4","2050"],col="orange")
lines(Lc.lognorm,parameter=sdlogs["SSP1","2050"],col="blue")
lines(Lc.lognorm,parameter=sdlogs["SSP3","2050"],col="green")

grid=seq(0,1,.1)
plot(grid,grid,type="l",xlab="Cumulative Population",ylab="Cumulative Income")
lines(Lc.lognorm,parameter=sdlogs["SSP2","2100"],col="red")
lines(Lc.lognorm,parameter=sdlogs["SSP4","2100"],col="orange")
lines(Lc.lognorm,parameter=sdlogs["SSP1","2100"],col="blue")
lines(Lc.lognorm,parameter=sdlogs["SSP3","2100"],col="green")

gdp_percap_2030<-c(9.805,8.663,7.578,8.465)
gdp_percap_2010<-c(3.248,3.248,3.248,3.248)
gdp_percap_2030<-c(9.805,8.663,7.578,8.465)
gdp_percap_2050<-c(25.621,17.527,10.322,16.031)
gdp_percap_2100<-c(82.367,55.068,15.244,44.966)

gdps=data.frame("2010"=gdp_percap_2010,"2030"=gdp_percap_2030,"2050"=gdp_percap_2050,"2100"=gdp_percap_2100)
colnames(gdps)<-c("2010","2030","2050","2100")
rownames(gdps)<-c("SSP1", "SSP2", "SSP3", "SSP4")

mlogs<-lapply(1:nrow(gdps), function(x) {
   log(gdps[x,])-0.5*sdlogs[x,]^2})

mlogs<-rbind(mlogs[[1]],mlogs[[2]],mlogs[[3]],mlogs[[4]])


#Plot distributions
grid=seq(0,50,.1)

plot(grid, dlnorm(grid,mlogs["SSP4","2030"],sdlogs["SSP4","2030"]),type="l",xlab="000$/cap-yr (2010PPP) in 2050",ylab="",col="orange")
lines(grid, dlnorm(grid,mlogs["SSP3","2030"],sdlogs["SSP3","2030"]),type="l",col="green")
lines(grid, dlnorm(grid,mlogs["SSP2","2030"],sdlogs["SSP2","2030"]),type="l",col="red")
lines(grid, dlnorm(grid,mlogs["SSP1","2030"],sdlogs["SSP1","2030"]),type="l",col="blue")

# plot(grid, dlnorm(grid,all_data["SSP4","mlog"],all_data["SSP4","sdlog"]),type="l",xlab="000$/cap-yr (2010PPP) in 2030",ylab="",col="orange")
# lines(grid, dlnorm(grid,all_data["SSP3","mlog"],all_data["SSP3","sdlog"]),type="l",col="green")
# lines(grid, dlnorm(grid,all_data["SSP2","mlog"],all_data["SSP2","sdlog"]),type="l",col="red")
# lines(grid, dlnorm(grid,all_data["SSP1","mlog"],all_data["SSP1","sdlog"]),type="l",col="blue")
plot(grid, dlnorm(grid,mlogs["SSP3","2050"],sdlogs["SSP3","2050"]),type="l",xlab="000$/cap-yr (2010PPP) in 2050",ylab="",col="orange")
lines(grid, dlnorm(grid,mlogs["SSP4","2050"],sdlogs["SSP4","2050"]),type="l",col="green")
lines(grid, dlnorm(grid,mlogs["SSP2","2050"],sdlogs["SSP2","2050"]),type="l",col="red")
lines(grid, dlnorm(grid,mlogs["SSP1","2050"],sdlogs["SSP1","2050"]),type="l",col="blue")

grid=seq(0,100,.1)
plot(grid, dlnorm(grid,mlogs["SSP3","2100"],sdlogs["SSP3","2100"]),type="l",xlab="000$/cap-yr (2010PPP) in 2100",ylab="",col="orange")
lines(grid, dlnorm(grid,mlogs["SSP4","2100"],sdlogs["SSP4","2100"]),type="l",col="green")
lines(grid, dlnorm(grid,mlogs["SSP2","2100"],sdlogs["SSP2","2100"]),type="l",col="red")
lines(grid, dlnorm(grid,mlogs["SSP1","2100"],sdlogs["SSP1","2100"]),type="l",col="blue")
