#Sobol & DELSA for Sacramento
library('hydromad');library('sensitivity');library(boot);library('xts');library(parallel)
source('PQE-input/load_pqe_hobj.R');source('PQE-input/functions_morris.R')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

method_morris <- function (t_catchment='Gingera_synthetic', t_year = '70s', Nsamp =50){
  
  name.dir = paste0(t_catchment,'/',t_year,'/')
  dir.create(name.dir,recursive = TRUE, showWarnings = F)
  #load obs and hydromad obj
  ts_t_year = load_tsPQE(t_catchment = t_catchment, t_year=t_year)  
  hobj = load_hobj('ihacres','RMSE')
  incompl <- morris(model = NULL, factors = names(getFreeParsRanges(hobj)) , r = Nsamp,
                    design = list(type = "oat", levels = Nsamp, grid.jump = 1),
                    binf=sapply(getFreeParsRanges(hobj),min),
                    bsup=sapply(getFreeParsRanges(hobj),max))
  incompl$X[,'tau_s'] = exp(incompl$X[,'tau_s'] )
  summary(incompl$X[,'tau_s'])
  
  run_time = proc.time()
  cl <- makeCluster(3, type="SOCK")
  assign('cl', cl, envir = .GlobalEnv)
  results = evalPars(incompl$X, hobj,parallel=list(method="clusterApply", packages=c("hydromad"), async=TRUE))
  stopCluster(cl)
  run_time = proc.time() - run_time
  
  #calculate Morris measures
  morris_obj = tell(incompl,results)
  incompl$scale
  #bootstrap model performance measures
  data=morris_obj$ee
  #estimate CIs by bootsrapping 
  m.boot <- boot(data, estim_morris_mu , R = 2000)
  m.ci <- b.stats.morris(m.boot)
  s.boot <- boot(data, estim_morris_sig , R = 2000)
  s.ci <- b.stats.morris(s.boot)
  rownames(m.ci) =  morris_obj$factors
  rownames(s.ci) =  morris_obj$factors
  #print results
  sink(str_c(name.dir,nrow(incompl$X),'_morris','_mu.txt'))
  print(t_year)
  print(paste0('model runs = ',nrow(incompl$X)))
  print(run_time)
  print(m.ci)     
  sink()
  sink(str_c(name.dir,nrow(incompl$X),'_morris','_sig.txt'))
  print(t_year)
  print(paste0('model runs = ',nrow(incompl$X)))
  print(run_time)
  print(s.ci)    
  sink()
  
  kaka = cbind(incompl$X,results)
  head(kaka)
  write.csv(kaka,paste0(name.dir,'par_set.csv'),row.names = FALSE)
  
  sink(paste0(name.dir,'parameter_range.txt'))
  print(paste('Nsamp =',length(names(hobj$parlist))))
  print(hobj)
  sink()
  
}

catchments = c ('Gingera_synthetic')
years = c('70s', '80s', '90s', '00s')
n.samples = c(2,5,7)

for (c in catchments){
  for (y in years){
    for (nn in n.samples){
      method_morris(t_catchment = c, t_year = y, Nsamp = nn)
    }
  }
}




