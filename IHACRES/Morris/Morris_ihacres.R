#Sobol & DELSA for Sacramento
library('zoo')
library('hydromad')
library('xts')
library('stringr')
library('ggplot2')
library('sensitivity')
library(boot)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('functions_morris.R')

t_year = '70s'
Nsamp =10
  
name.dir = paste0('Gingera_synthetic/',t_year,'/')
dir.create(name.dir,recursive = TRUE, showWarnings = F)

ts_t_year = load_synthetic_tsPQE(t_year = t_year)

## Setup hydromad object  f
# hobj$parlist[['shape']]
# hydromad.options(objective = ~hmadstat("r.squared")(Q, X)/(2-hmadstat("r.squared")(Q, X)))
hydromad.options(objective = hmadstat('RMSE'))
hydromad.getOption('objective')
hobj = hydromad(ts_t_year, sma='cmd',routing='expuh',f=range(0.5,1.5),e=range(0.99,1.01),d=range(200,550),tau_q=range(3,10), tau_s=range(200,1000),  v_s=range(0.7,1))
# hobj = hydromad(ts_t_year,sma='cmd',routing='expuh',f=range(0.5,1.5),e=range(0.99,1.01),d=range(200,550),tau_q=range(3,10), tau_s=range(200,1000),  v_s=range(0.7,1))

incompl <- morris(model = NULL, factors = names(getFreeParsRanges(hobj)) , r = Nsamp,
            design = list(type = "oat", levels = Nsamp, grid.jump = 1),
            binf=sapply(getFreeParsRanges(hobj),min),
            bsup=sapply(getFreeParsRanges(hobj),max))
incompl$X


results=evalPars(incompl$X,object=hobj)

#calculate Morris measures
morris_obj = tell(incompl,results)
incompl$scale
#bootstrap model performance measures
data=morris_obj$ee
#estimate CIs by bootsrapping 
m.boot <- boot(data, estim_morris_mu , R = 2000)
m.ci <- b.stats.morris(m.boot)
# rownames(m.ci) <- col_header
s.boot <- boot(data, estim_morris_sig , R = 2000)
s.ci <- b.stats.morris(s.boot)
# rownames(s.ci) <- col_header
sink(str_c(name.dir,'morris_',nrow(incompl$X),'_mu.txt'))
print(m.ci)     
sink()
sink(str_c(name.dir,'morris_',nrow(incompl$X),'_sig.txt'))
print(s.ci)    
sink()

kaka = cbind(incompl$X,results)
head(kaka)
write.csv(kaka,paste0(name.dir,'par_set.csv'),row.names = FALSE)

sink(paste0(name.dir,'parameter_range.txt'))
print(paste('Nsamp =',length(names(hobj$parlist))))
print(hobj)
sink()
 
# temp_runs=m.ci
# 
# p2=ggplot(data=temp_runs, aes(fill=Parameter, x=model_runs, y=sensitivity))+
#   geom_bar(position="dodge", stat="identity")+
#   geom_errorbar(aes(ymax=max.c.i., ymin=min.c.i.), position=position_dodge(width=0.9), width=0.25)+
#   theme(axis.text.x = element_text(angle=90, colour = "black"),
#         axis.text.y = element_text(colour = "black"),
#         axis.line = element_line(colour = "black"),
#         panel.background = element_blank())+
#   scale_x_discrete(breaks=1:9, labels=runs$alias[1:9], limits=c(1:9))+
#   ylab("TSI")+
#   #   scale_fill_manual( values = c('Rchrg_Dp'='grey80',
#   #                                 'Alpha_Bf'='black',
#   #                                 'Gwqmn'='grey60',
#   #                                 'Esco'='grey20', 
#   #                                 'Sol_Awc'='grey40') )
#   scale_fill_grey()