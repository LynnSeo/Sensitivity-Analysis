#########exact data######Calibration with defferent default parameters decided by SA
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library('zoo')
library('hydromad')
library('xts')
library('stringr')
load_tsPQE <- function(t_catchment='Gingera', t_year='70s'){
  name_tspqe=str_c('C:/UserData/seol/Sensitivity Analyses/PQE input/',t_catchment,'/',t_catchment,'.csv')
  tsPQE=read.zoo(name_tspqe,sep=',',header=TRUE);  tsPQE=as.xts(tsPQE)
  if(t_year=='70s'){
    assign('ts_t_year', tsPQE["1970-01-01::1979-12-31"],envir = .GlobalEnv)
  }else if(t_year=='80s'){
    assign('ts_t_year', tsPQE["1980-01-01::1989-12-31"],envir = .GlobalEnv)
  }else if(t_year=='90s'){
    assign('ts_t_year', tsPQE["1990-01-01::1999-12-31"],envir = .GlobalEnv)
  }else if(t_year=='00s'){
    assign('ts_t_year', tsPQE["2000-01-01::2009-12-31"],envir = .GlobalEnv)
  }
  return(ts_t_year)
}

#obj functions
NSE=~hmadstat("r.squared")(Q, X)/(2-hmadstat("r.squared")(Q, X))
#objs =[mnse,nse,nse log]
objs=c(NSE,hmadstat('r.squared'),hmadstat('r.sq.log'))
hydromad.options(objective = objs[[1]])

obj_70s <- hydromad(load_tsPQE(t_year = '70s'), sma = "cmd", routing = 'expuh', f=range(0.5,2.5),e=range(0.5,1.2),tau_s=range(5,1000), tau_q=range(0.5,10), v_s=range(0,1))
obj_80s <- hydromad(load_tsPQE(t_year = '80s'), sma = "cmd", routing = 'expuh', f=range(0.5,2.5),e=range(0.5,1.2),tau_s=range(5,1000), tau_q=range(0.5,10), v_s=range(0,1))
obj_90s <- hydromad(load_tsPQE(t_year = '90s'), sma = 'cmd', routing = 'expuh', f=range(0.5,2.5),e=range(0.5,1.2),tau_s=range(5,1000), tau_q=range(0.5,10), v_s=range(0,1))
obj_00s <- hydromad(load_tsPQE(t_year = '00s'), sma = 'cmd', routing = 'expuh', f=range(0.5,2.5),e=range(0.5,1.2),tau_s=range(5,1000), tau_q=range(0.5,10), v_s=range(0,1))

#Calibration 
set.seed(1221)
cal = list()
current.time = proc.time()
cal$ts70s$fitted = fitBySCE(obj_70s, control = list(trace = 1, ncomplex = 20) )
cal$ts70s$time =  proc.time() - current.time

current.time = proc.time()
cal$ts80s$fitted = fitBySCE(obj_80s, control = list(trace = 1, ncomplex = 20) )
cal$ts80s$time =  proc.time() - current.time

current.time = proc.time()
cal$ts90s$fitted = fitBySCE(obj_90s, control = list(trace = 1, ncomplex = 20) )
cal$ts90s$time =  proc.time() - current.time

current.time = proc.time()
cal$ts00s$fitted = fitBySCE(obj_00s, control = list(trace = 1, ncomplex = 20) )
cal$ts00s$time =  proc.time() - current.time
save.image('fitted_decades.Rdata')
########################

#update new parameter to original tsPQE
updated = list()
updated$ts70s = update(cal$ts70s$fitted, newdata = obj_70s$data)
updated$ts80s = update(cal$ts80s$fitted, newdata = obj_80s$data)
updated$ts90s = update(cal$ts90s$fitted, newdata = obj_90s$data)
updated$ts00s = update(cal$ts00s$fitted, newdata = obj_00s$data)

head(fitted(updated$ts70s))
#extract simulated flow series (it contains 100 days warm-up period )
exact.series = list()
exact.series$ts70s = xts(updated$ts70s$data)
exact.series$ts80s = xts(updated$ts80s$data)
exact.series$ts90s = xts(updated$ts90s$data)
exact.series$ts00s = xts(updated$ts00s$data)

name.decades = c('ts70s','ts80s','ts90s','ts00s')
for (y in name.decades){
  exact.series[[y]]$Q[101:(length(fitted(updated[[y]]))+100)] = fitted(updated[[y]])
  write.zoo(exact.series[[y]][101:(length(fitted(updated[[y]])+100))],paste0('exact_Gingera_',y,'.csv'),sep=',')

}


