#summary of statistical features of time series
load_synthetic_tsPQE <- function(t_catchment='Gingera_synthetic', t_year='70s'){
  name_tspqe=str_c('C:/UserData/seol/Sensitivity Analyses/PQE input/',t_catchment,'/',t_catchment,'_ts',t_year,'.csv')
  tsPQE=read.zoo(name_tspqe,sep=',',header=TRUE);  tsPQE=as.xts(tsPQE)
  assign('ts_t_year', tsPQE, envir = .GlobalEnv)
  return(ts_t_year)
}

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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(xts)

years= c('70s','80s','90s','00s')
for (y in years){
  t_year = load_tsPQE(t_catchment = 'Gingera', t_year = y)
  sink(paste0('Gingera_',y,'.txt'))
  print(summary(t_year))
  sink()  
}
