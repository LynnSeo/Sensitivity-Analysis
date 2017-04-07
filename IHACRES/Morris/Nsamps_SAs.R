library(stringr)
library(ggplot2)
library(data.table)

#plotting Morris' respect to Nsampples

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
read_sa_results <- function (Nsamp, si.type){
  # si = read.table(paste0(Nsamp,'_sobol_',si.type,'.txt'), skip = 5)
  si = read.table(paste0(Nsamp,'_',si.type,'.txt'), skip = 5, row.names = 1)
  return (si)
}

plot_si_nsamps <- function (t_catchment = 'Gingera_synthetic/', t_year='70s', si.type = 'morris_mu'){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  ndir = paste0(t_catchment,t_year,'/')
  setwd(ndir)
  names.txt = list.files(pattern= paste0('*',si.type,'.txt'))
  samples = as.character(sort(as.numeric(str_sub(names.txt,1,-(nchar(si.type)+6)))))
  tt = read_sa_results(samples[1], si.type)
  Npar = nrow(tt)
  
  #initiate sensitivity indices df
  sindices = data.frame(matrix(nrow=Npar, ncol =0))
  for (i in samples)
    sindices[[i]] = read_sa_results(i, si.type)[,2]  
  
  #need to change 
  sindices$parameter = rownames(tt)
  
  df.plot = melt(sindices, id = 'parameter')
  #adjust level by sensitivity
  factor(df.plot$parameter)
  pars.order = rownames(read_sa_results(i,si.type)[order(read_sa_results(i, si.type)[,1], decreasing = TRUE),])
  df.plot$parameter = factor(df.plot$parameter, pars.order)
  colnames(df.plot)[2:3] = c('Nsamp', 'SI')
  
  size.font = 25
  p1 = ggplot(data=df.plot, aes(x=parameter, y=SI, colour = Nsamp, group = Nsamp))+
    geom_line(size=2)+ scale_colour_manual(values = c('skyblue','grey', 'green','blue','red','black'), 
                                           guide=guide_legend(title='model runs'))+
    ggtitle(paste(t_catchment,',',t_year,',',si.type))+
    theme(axis.line.x=element_line(colour='black') ,axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size=size.font, colour = "black"),axis.text.y = element_text(size=size.font,colour = "black"),
          axis.title.x = element_text(size=size.font),axis.title.y = element_text(size=size.font),
          legend.text = element_text(size = size.font), legend.title = element_text(size = size.font), 
          legend.key.size = unit(20,'mm'), title = element_text(size=size.font, face = 'bold'))
  
  png(paste0(si.type,'.png'), width=800, height=500)
  print(p1)
  dev.off()
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
}

plot_si_nsamps(t_catchment = 'Gingera_synthetic/', t_year='70s', si.type = 'morris_mu')    

years=c('70s','80s','90s','00s')
si.types = c('morris_mu', 'morris_sig')
for (y in years){
  for (s in si.types){
    plot_si_nsamps(t_catchment = 'Gingera_synthetic/', t_year=y, si.type = s)    
  }
}

