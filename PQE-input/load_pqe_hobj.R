load_hobj <- function(model='ihacres', objective='RMSE'){
  if (objective == 'NSE'){
    hydromad.options(objective = ~hmadstat("r.squared")(Q, X)/(2-hmadstat("r.squared")(Q, X)))
  }else if (objective == 'RMSE'){
    hydromad.options(objective = hmadstat('RMSE'))  
  }
  
  if (model=='ihacres'){
    # hobj = hydromad(ts_t_year,sma='cmd',routing='expuh',
    #                 f=range(0.5,1.5),e=range(0.99,1.01),d=range(200,550),
    #                 tau_q=range(3,10), tau_s=range(200,1000),  v_s=range(0.7,1))
    # 
    hobj = hydromad(ts_t_year, sma='cmd',routing='expuh',
                    f=range(0.5,1.5),e=range(0.99,1.01),d=range(200,550),
                    tau_q=range(1,10), tau_s=range(30,300),  v_s=range(0.1, 1))
    hobj$parlist$tau_s = log(hobj$parlist$tau_s)
    
  }else if(model == 'sacramento'){
    hobj = hydromad(ts_t_year, sma='sacramento', routing=NULL)  
  }
  return (hobj)
}


load_tsPQE <- function(t_catchment, t_year){
#load_PQE time series
    library(stringr)
    library(xts)
  if (str_sub(t_catchment, -9)=='synthetic'){
    name_tspqe=str_c('C:/UserData/seol/Sensitivity Analyses/PQE-input/',t_catchment,'/',t_catchment,'_ts',t_year,'.csv')
    tsPQE=read.zoo(name_tspqe,sep=',',header=TRUE);  tsPQE=as.xts(tsPQE)
    assign('ts_t_year', tsPQE, envir = .GlobalEnv)
    return(ts_t_year)
  }else{
    name_tspqe=str_c('C:/UserData/seol/Sensitivity Analyses/PQE-input/',t_catchment,'/',t_catchment,'.csv')
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
}

parallel_evalPars <- function(pset, pobj, pobjective){
  #my customized parallel computing
  #pset : parameter set
  #hobj : hydromad object
  assign('pset',pset, envir = .GlobalEnv)
  assign('pobj',pobj, envir = .GlobalEnv)
  assign('pobjective ',pobjective , envir = .GlobalEnv)
  
  cores <- detectCores(logical = FALSE)
  cl <- makeCluster(cores-1)
  #it may pre-define objective function
  # clusterExport(cl, c('pset', 'pobj','pobjective'))
  clusterExport(cl, 'pset')
  clusterExport(cl, 'pobj')
  clusterCall(cl, function() {library(hydromad)
    pobjective=pobjective})
  system.time(
    parResults <- parSapply(cl, seq(nrow(pset)), function(x) { evalPars(as.matrix(t(pset[x,])), object=pobj, objective=pobjective)
      })
  )
  stopCluster(cl)
  return(parResults)
}


evalFDC = function(pset, hobj){
  #evaluate model by FDC
  slope = apply (pset, 1, function(x){
    mod1 = hobj
    mod1$parlist = x
    predicted1 = predict(mod1, return_components = T)
    # seq.prob = seq(0,1, by=0.001)
    # fdc_series = as.data.frame(quantile(predicted1[,1], seq.prob))
    # plot(seq.prob$plot, fdc.series[,1])
    # seq.slope = c(0.2, 0.8)
    seq.slope = c(0.35, 0.65)
    fdc_slope = quantile(predicted1[,1], seq.slope)
    slope = (fdc_slope[[2]]-fdc_slope[[1]])/0.3
  })
  return(slope)
}

opendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}