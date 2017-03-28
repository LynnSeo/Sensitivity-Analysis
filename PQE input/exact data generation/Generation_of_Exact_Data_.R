#########exact data######Calibration with defferent default parameters decided by SA
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library('zoo')
library('hydromad')
library('xts')
library('stringr')

####################################
t_year='70s'           #target year for calibration
t_catchment='Gingera'  #target catchment 

####################################
name_tspqe=str_c(t_catchment,'_tsPQE.csv')
# name_tspqe=str_c(t_catchment,'_tsPQE_masked.csv')
tsPQE=read.zoo(name_tspqe,sep=',',header=TRUE)
tsPQE=as.xts(tsPQE)

#sub-decades
ts70s <- tsPQE["1970-01-01::1979-12-31"]
ts80s <- tsPQE["1980-01-01::1989-12-31"]
ts90s <- tsPQE["1990-01-01::1999-12-31"]
ts00s <- tsPQE["2000-01-01::2009-12-31"]


#calibration

#obj functions
NSE=~hmadstat("r.squared")(Q, X)/(2-hmadstat("r.squared")(Q, X))
#objs =[mnse,nse,nse log]
objs=c(NSE,hmadstat('r.squared'),hmadstat('r.sq.log'))
hydromad.options(objective = objs[[1]])

obj_70s <- hydromad(as.zoo(ts70s), sma = "sacramento", routing = NULL)
obj_80s <- hydromad(as.zoo(ts80s), sma = "sacramento", routing = NULL)
obj_90s <- hydromad(as.zoo(ts90s), sma = "sacramento", routing = NULL)
obj_00s <- hydromad(as.zoo(ts00s), sma = "sacramento", routing = NULL)

#Calibration 
set.seed(1221)
mod_cal70s=fitBySCE(obj_70s, control = list(trace = 1, ncomplex = 20) )
mod_cal80s=fitBySCE(obj_80s, control = list(trace = 1, ncomplex = 20) )
mod_cal90s=fitBySCE(obj_90s, control = list(trace = 1, ncomplex = 20) )
save.image('mod_cal70s_80s_90s.RData')

set.seed(1221)
opt_par = fitBySCE(target_cal, control = list(trace = 1, ncomplex = 20))
opt_par$parlist
save(opt_par,file='opt_par.RData')
write.csv(opt_par$parlist,'opt_par.csv')

#update new parameter to original tsPQE
exact_spec_cal80s=update(opt_par, newdata=as.zoo(tsPQE))

#extract simulated flow series (it contains 100 days warm-up period )
exact_sim_cal80s=fitted(exact_spec_cal80s)
exact_obs_cal80s=observed(exact_spec_cal80s)   
head(exact_sim_cal80s)

exact_sim_cal80s=as.xts(exact_sim_cal80s)
exact_sim_cal80s['1964-04-12::2009-12-31']

exact_tsPQE$Q['1964-04-12::2009-12-31']=exact_sim_cal80s['1964-04-12::2009-12-31']


write.zoo(exact_tsPQE,'exact_Gingera_tsPQE_cal80s.csv',sep=',')