#########exact data######Calibration of Sensitive Parameters Only
library('zoo')
library('hydromad')
library('xts')
library('stringr')

#set up working directory
wd='C:/UserData/seol/Sensitivity Analyses/Sacramento/Calibration considering SA/Calibration of Sensitive Parameters only/'
setwd(wd)

####################################
t_year='80s'           #target year for calibration
t_catchment='Gingera'  #target catchment 

####################################
name_tspqe=str_c('real_',t_catchment,'_tsPQE_masked','.csv')

name_tspqe='real_Gingera_tsPQE.csv'

name_tspqe='real_masked_Gingera_tsPQE.csv'
tsPQE=read.zoo(name_tspqe,sep=',',header=TRUE)
tsPQE=as.xts(tsPQE)

#sub-decades
ts70s <- tsPQE["1970-01-01::1979-12-31"]
ts80s <- tsPQE["1980-01-01::1989-12-31"]
ts90s <- tsPQE["1990-01-01::1999-12-31"]
ts00s <- tsPQE["2000-01-01::2009-12-31"]

plot.zoo(ts70s)
plot.zoo(ts70s['1974-08-26::1974-09-16'])
plot.zoo(ts70s['1976-10-13::1976-10-25'])

ts70s$Q['1974-08-26::1974-09-16']=0
ts70s$P['1974-08-26::1974-09-16']=0
ts70s$P['1976-10-13::1976-10-25']=0
ts70s$Q['1976-10-13::1976-10-25']=0

plot.zoo(ts70s['1974-08-26::1974-09-16'])
plot.zoo(ts70s['1976-10-13::1976-10-25'])

tsPQE$Q['1974-08-26::1974-09-16']=0
tsPQE$P['1974-08-26::1974-09-16']=0
tsPQE$P['1976-10-13::1976-10-25']=0
tsPQE$Q['1976-10-13::1976-10-25']=0

write.csv(as.data.frame(tsPQE),'real_masked_Gingera.csv',row.names = TRUE)
