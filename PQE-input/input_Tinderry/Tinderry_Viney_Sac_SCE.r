set.seed(2061)
################################################################################
library(hydromad)
library("latticeExtra")
library("xts")
options(chmhelp = FALSE)

################################################################################
## input data
  ## CHANGE
cat(readLines("pq_Tinderry.csv", n = 5), sep = "\n")
pqdat <- read.table("pq_Tinderry.csv", sep = ",", col.names = c("P", "Q", "Date"), as.is = TRUE)
  ## END
str(pqdat)
pqdat$Date <- as.Date(pqdat$Date, "%d/%m/%Y")
pqdat$P[pqdat$P < 0] <- NA
pqdat$Q[pqdat$Q < 0] <- NA
  ## CHANGE
pqdat$Q <- convertFlow(pqdat$Q, from = "ML", area.km2 = 490)
  ## END
tsPQ <- xts(pqdat[, 1:2], pqdat$Date, frequency = 1)

#tdat <- read.table("t.dat", sep = ",", col.names = c("T", "Date"), as.is = TRUE)
tdat<-read.table("ev_070014.csv",sep=",",col.names=c("T","Date"),as.is=TRUE)
str(tdat)
tdat$Date <- as.Date(tdat$Date, "%d/%m/%Y")
tdat <- subset(tdat, !is.na(Date))
tsT <- xts(tdat[, 1], tdat$Date, frequency = 1)

#tsPQE <- merge(tsPQ, E = tsT, all = FALSE)
tsPQE <- merge(tsPQ, E = tsT * 0.8, all = FALSE)
head(tsPQE, 6) 
range(time(tsPQE))
## write table
write.csv(range(time(tsPQE)), file = "RangetsPQE.csv") 
tsPQE <- na.trim(tsPQE)
summary(tsPQE)
## write table
write.csv(summary(tsPQE), file = "SummarytsPQE.csv")
 
## consider warmup period
#ts70s <- tsPQE["1970::1979-12-31"]
ts70s <- tsPQE["1969-09-23::1979-12-31"]
#ts80s <- tsPQE["1980::1989-12-31"]
ts80s <- tsPQE["1979-09-23::1989-12-31"]
#ts90s <- tsPQE["1990::1999-12-31"]
ts90s <- tsPQE["1989-09-23::1999-12-31"]
#ts8090s <- tsPQE["1980::1999-12-31"]
ts8090s <- tsPQE["1979-09-23::1999-12-31"]
#ts7090s <- tsPQE["1970::1999-12-31"]
ts7090s <- tsPQE["1969-09-23::1999-12-31"]
#ts7080s <- tsPQE["1970::1989-12-31"]
ts7080s <- tsPQE["1969-09-23::1989-12-31"]

xyplot(tsPQE)
xyplot(tsPQE, xlim = as.Date(c("1974-01-01","1974-12-31")))
#monthlyPQE<-aggregate(tsPQE,as.yearmon,mean)
#xyplot(monthlyPQE, strip = c("Streamflow (mm/day)", "Areal rain mm/day)", "Temperature (deg. C)"), xlab = NULL)
xyplot(ts90s$Q)
xyplot(log10(ts90s$Q), type = c("l", "g"))
xyplot(list(the70s = ts70s$Q, the80s = ts80s$Q, the90s = ts90s$Q),
       scale = list(y = list(log = TRUE)),
       type = c("l", "g"),
       layout = c(1, 3))

## plot
png(file="tsPQE.png") 
xyplot(tsPQE)
dev.off()
png(file="tsPQE_74.png") 
xyplot(tsPQE, xlim = as.Date(c("1974-01-01","1974-12-31")))
dev.off()
#png(file="MonthlyPQE.png") 
#xyplot(monthlyPQE, strip = c("Streamflow (mm/day)", "Areal rain mm/day)", "Temperature (deg. C)"), xlab = NULL)
#dev.off()
png(file="ts90s_Q.png") 
xyplot(ts90s$Q)
dev.off()
png(file="ts90s_Qlog.png") 
xyplot(log10(ts90s$Q), type = c("l", "g"))
dev.off()
png(file="ts7890s_Qlog.png") 
xyplot(list(the70s = ts70s$Q, the80s = ts80s$Q, the90s = ts90s$Q),
       scale = list(y = list(log = TRUE)),
       type = c("l", "g"),
       layout = c(1, 3))
dev.off()

################################################################################
## cross correlation 
acf(tsPQE[, 2:1], na.action = na.exclude, lag.max = 30)
estimateDelay(tsPQE, plot = TRUE)
delay <- estimateDelay(tsPQE)
## write table
write.csv(delay, file = "DelayTime.csv") 
## plot
png(file="ccf.png") 
acf(tsPQE[, 2:1], na.action = na.exclude, lag.max = 30)
dev.off()

#rolls <- rollccf(tsPQE)
#xyplot(rolls)
## plot
#png(file="rollccf.png") 
#xyplot(rolls)
#dev.off()

################################################################################
## model calibration
#hydromad.options(objective = ~fitStat(Q,X,p=2) -5*abs(log(max(0, 1+sum(X-Q,na.rm=T)/sum(Q,na.rm=T))))^2.5 ) 
#hydromad.options(objective = ~ -fitStat(Q,X,p=2) +5*abs(log(max(0, 1+sum(X-Q,na.rm=T)/sum(Q,na.rm=T))))^2.5 ) 
hydromad.options(objective = ~hmadstat("r.squared")(Q, X) - 5*abs(log(max(0, 1+sum(X-Q,na.rm=T)/sum(Q,na.rm=T))))^2.5 )
  ## CHANGE
hydromad.options(prefilter = makePrefilter(ts70s, order = c(2, 1)))
hydromad.options(prefilter = makePrefilter(ts80s, order = c(2, 1)))
hydromad.options(prefilter = makePrefilter(ts90s, order = c(2, 1)))
  ## END
hydromad.options(polish=F) 
hydromad.options(trace = TRUE)
hydromad.options(normalise = FALSE)
hydromad.options(sacramento = list(pctim = c(0.000001, 0.1)))
hydromad.options(sacramento = list(pfree = c(0, 0.4)))

## cal70s
  ## CHANGE
#modspec70s21 <- hydromad(as.zoo(ts70s), e= 0.166, sma = "cmd", routing = "uh", rfit = list("ls", order = c(3, 2), normalise = FALSE))
#modspec70s21 <- hydromad(as.zoo(ts70s), e= 0.166, sma = "cmd", routing = "uh", rfit = list("sriv", order = c(3, 2), normalise = FALSE))
#modspec70s21 <- hydromad(as.zoo(ts70s), e= 0.166, sma = "cmd", tau_s = range(3, 500), tau_q = range(0, 3), v_s = range(0, 1), v_3 = range(0, 1), normalise = FALSE)
#modspec70s21 <- hydromad(as.zoo(ts70s), e= 0.166, sma = "cmd", routing = "expuh", tau_s = range(3, 500), tau_q = range(0, 3), v_s = range(0, 1), normalise = FALSE)
#modspec70s21 <- hydromad(as.zoo(ts70s), e= 0.166, sma = "cmd", tau_s = range(5, 500), tau_q = range(0, 5), tau_3 = range(0, 500), v_s = range(0, 1), v_3 = range(0, 1), normalise = FALSE)
#modspec70s21 <- hydromad(as.zoo(ts70s), sma = "sacramento", routing = NULL)
modspec70s21 <- hydromad(as.zoo(ts70s), sma = "sacramento", routing = NULL,
uztwm	=	12.39162940142710000000	,
uzfwm	=	56.86780410253400000000	,
uzk	=	0.49999999946519300000	,
pctim	=	0.04234584444475320000	,
adimp	=	0.00000083378550894738	,
zperc	=	189.71714753956900000000	,
rexp	=	3.89943665965706000000	,
lztwm	=	76.23314803645200000000	,
lzfsm	=	1.00000362506106000000	,
lzfpm	=	77.20597507261440000000	,
lzsk	=	0.18376815099833300000	,
lzpk	=	0.23477620588324400000	,
pfree	=	0.31614919176188600000	)

  ## END
modspec70s21

  ## CHANGE
#mod70s21 <- fitByOptim(modspec70s21, method = "BFGS", samples = 64)
mod70s21 <- fitBySCE(modspec70s21, control = list(trace = 1, maxtime = 20000, ncomplex = 20))
  ## END
summary(mod70s21)
coef(mod70s21)
## write table
Qmod70s21 <- (fitted(mod70s21))
Qobs70s21 <- (observed(mod70s21))
write.zoo(merge(Qobs70s21, Qmod70s21), file="Qmod70s21.csv", sep=",")

sim80sPar70s21 <- update(mod70s21, newdata = as.zoo(ts80s))
summary(sim80sPar70s21)
## write table
Qsim80sPar70s21 <- (fitted(sim80sPar70s21))
Qobs80sPar70s21 <- (observed(sim80sPar70s21))
write.zoo(merge(Qobs80sPar70s21, Qsim80sPar70s21), file="Qsim80sPar70s21.csv", sep=",")

sim90sPar70s21 <- update(mod70s21, newdata = as.zoo(ts90s))
summary(sim90sPar70s21)
## write table
Qsim90sPar70s21 <- (fitted(sim90sPar70s21))
Qobs90sPar70s21 <- (observed(sim90sPar70s21))
write.zoo(merge(Qobs90sPar70s21, Qsim90sPar70s21), file="Qsim90sPar70s21.csv", sep=",")

sim8090sPar70s21 <- update(mod70s21, newdata = as.zoo(ts8090s))
summary(sim8090sPar70s21)
## write table
Qsim8090sPar70s21 <- (fitted(sim8090sPar70s21))
Qobs8090sPar70s21 <- (observed(sim8090sPar70s21))
write.zoo(merge(Qobs8090sPar70s21, Qsim8090sPar70s21), file="Qsim8090sPar70s21.csv", sep=",")

allModsPar70s21 <- runlist(mod70s21 = mod70s21, sim80sPar70s21 =sim80sPar70s21, sim90sPar70s21 = sim90sPar70s21, sim8090sPar70s21 = sim8090sPar70s21) 
summary(allModsPar70s21, pars = TRUE) 
## write table
summaryAll70s21 <- summary(allModsPar70s21, pars = TRUE) 
write.csv(summaryAll70s21 ,file="allModsPar70s21.csv")


## cal80s
  ## CHANGE
#modspec80s21 <- hydromad(as.zoo(ts80s), e= 0.166, sma = "cmd", rfit = list("ls", order = c(3, 2), normalise = FALSE))
#modspec80s21 <- hydromad(as.zoo(ts80s), e= 0.166, sma = "cmd", rfit = list("sriv", order = c(3, 2), normalise = FALSE))
#modspec80s21 <- hydromad(as.zoo(ts80s), e= 0.166, sma = "cmd", routing = "expuh", tau_s = range(3, 500), tau_q = range(0, 3), v_s = range(0, 1), normalise = FALSE)
#modspec80s21 <- hydromad(as.zoo(ts80s), e= 0.166, sma = "cmd", tau_s = range(5, 500), tau_q = range(0, 5), tau_3 = range(0, 500), v_s = range(0, 1), v_3 = range(0, 1), normalise = FALSE)
#modspec80s21 <- hydromad(as.zoo(ts80s), sma = "sacramento", routing = NULL)
modspec80s21 <- hydromad(as.zoo(ts80s), sma = "sacramento", routing = NULL,
uztwm	=	37.12366960560600000000	,
uzfwm	=	82.12409069925610000000	,
uzk	=	0.40170951198057200000	,
pctim	=	0.00002327651513607710	,
adimp	=	0.00000001545848501565	,
zperc	=	139.27512936170800000000	,
rexp	=	1.40170660079812000000	,
lztwm	=	39.61258318373390000000	,
lzfsm	=	64.22739360526800000000	,
lzfpm	=	11.73286621440480000000	,
lzsk	=	0.01000521212810110000	,
lzpk	=	0.24798123050257900000	,
pfree	=	0.37707413293591900000	)

  ## END
modspec80s21

  ## CHANGE
#mod80s21 <- fitByOptim(modspec80s21, method = "BFGS", samples = 64)
mod80s21 <- fitBySCE(modspec80s21, control = list(trace = 1, maxtime = 20000, ncomplex = 20))
  ## END
summary(mod80s21)
coef(mod80s21)
## write table
Qmod80s21 <- (fitted(mod80s21))
Qobs80s21 <- (observed(mod80s21))
write.zoo(merge(Qobs80s21, Qmod80s21), file="Qmod80s21.csv", sep=",")

sim70sPar80s21 <- update(mod80s21, newdata = as.zoo(ts70s))
summary(sim70sPar80s21)
## write table
Qsim70sPar80s21 <- (fitted(sim70sPar80s21))
Qobs70sPar80s21 <- (observed(sim70sPar80s21))
write.zoo(merge(Qobs70sPar80s21, Qsim70sPar80s21), file="Qsim70sPar80s21.csv", sep=",")

sim90sPar80s21 <- update(mod80s21, newdata = as.zoo(ts90s))
summary(sim90sPar80s21)
## write table
Qsim90sPar80s21 <- (fitted(sim90sPar80s21))
Qobs90sPar80s21 <- (observed(sim90sPar80s21))
write.zoo(merge(Qobs90sPar80s21, Qsim90sPar80s21), file="Qsim90sPar80s21.csv", sep=",")

tsVerif <- ts7090s 
tsVerif$Q[time(ts80s)] <- NA 
sim7090sPar80s21 <- update(mod80s21, newdata = as.zoo(tsVerif))
summary(sim7090sPar80s21)
## write table
Qsim7090sPar80s21 <- (fitted(sim7090sPar80s21))
Qobs7090sPar80s21 <- (observed(sim7090sPar80s21))
write.zoo(merge(Qobs7090sPar80s21, Qsim7090sPar80s21), file="Qsim7090sPar80s21.csv", sep=",")

allModsPar80s21 <- runlist(mod80s21 = mod80s21, sim70sPar80s21 = sim70sPar80s21, sim90sPar80s21 = sim90sPar80s21, sim7090sPar80s21 = sim7090sPar80s21) 
summary(allModsPar80s21, pars = TRUE) 
## write table
summaryAll80s21 <- summary(allModsPar80s21, pars = TRUE) 
write.csv(summaryAll80s21 ,file="allModsPar80s21.csv")


## cal90s 
  ## CHANGE
#modspec90s21 <- hydromad(as.zoo(ts90s), e= 0.166, sma = "cmd", rfit = list("ls", order = c(3, 2), normalise = FALSE))
#modspec90s21 <- hydromad(as.zoo(ts90s), e= 0.166, sma = "cmd", rfit = list("sriv", order = c(3, 2), normalise = FALSE))
#modspec90s21 <- hydromad(as.zoo(ts90s), e= 0.166, sma = "cmd", routing = "expuh", tau_s = range(3, 500), tau_q = range(0, 3), v_s = range(0, 1), normalise = FALSE)
#modspec90s21 <- hydromad(as.zoo(ts90s), e= 0.166, sma = "cmd", tau_s = range(5, 500), tau_q = range(0, 5), tau_3 = range(0, 500), v_s = range(0, 1), v_3 = range(0, 1), normalise = FALSE)
#modspec90s21 <- hydromad(as.zoo(ts90s), sma = "sacramento", routing = NULL)
modspec90s21 <- hydromad(as.zoo(ts90s), sma = "sacramento", routing = NULL,
uztwm	=	28.87043312863330000000	,
uzfwm	=	62.50057958035100000000	,
uzk	=	0.42470237273014000000	,
pctim	=	0.00670128633980155000	,
adimp	=	0.00000021769354953611	,
zperc	=	249.99999056228800000000	,
rexp	=	4.99218075905379000000	,
lztwm	=	185.37438178134000000000	,
lzfsm	=	21.08295022686750000000	,
lzfpm	=	34.86493243732620000000	,
lzsk	=	0.22123952638197000000	,
lzpk	=	0.01414392022118980000	,
pfree	=	0.37215432483085900000	)

  ## END
modspec90s21

  ## CHANGE
#mod90s21 <- fitByOptim(modspec90s21, method = "BFGS", samples = 64)
mod90s21 <- fitBySCE(modspec90s21, control = list(trace = 1, maxtime = 20000, ncomplex = 20))
  ## END
summary(mod90s21)
coef(mod90s21)
## write table
Qmod90s21 <- (fitted(mod90s21))
Qobs90s21 <- (observed(mod90s21))
write.zoo(merge(Qobs90s21, Qmod90s21), file="Qmod90s21.csv", sep=",")

sim70sPar90s21 <- update(mod90s21, newdata = as.zoo(ts70s))
summary(sim70sPar90s21)
## write table
Qsim70sPar90s21 <- (fitted(sim70sPar90s21))
Qobs70sPar90s21 <- (observed(sim70sPar90s21))
write.zoo(merge(Qobs70sPar90s21, Qsim70sPar90s21), file="Qsim70sPar90s21.csv", sep=",")

sim80sPar90s21 <- update(mod90s21, newdata = as.zoo(ts80s))
summary(sim80sPar90s21)
## write table
Qsim80sPar90s21 <- (fitted(sim80sPar90s21))
Qobs80sPar90s21 <- (observed(sim80sPar90s21))
write.zoo(merge(Qobs80sPar90s21, Qsim80sPar90s21), file="Qsim80sPar90s21.csv", sep=",")

sim7080sPar90s21 <- update(mod90s21, newdata = as.zoo(ts7080s))
summary(sim7080sPar90s21)
## write table
Qsim7080sPar90s21 <- (fitted(sim7080sPar90s21))
Qobs7080sPar90s21 <- (observed(sim7080sPar90s21))
write.zoo(merge(Qobs7080sPar90s21, Qsim7080sPar90s21), file="Qsim7080sPar90s21.csv", sep=",")

allModsPar90s21 <- runlist(mod90s21 = mod90s21, sim70sPar90s21 = sim70sPar90s21, sim80sPar90s21 = sim80sPar90s21, sim7080sPar90s21 = sim7080sPar90s21) 
summary(allModsPar90s21, pars = TRUE) 
## write table
summaryAll90s21 <- summary(allModsPar90s21, pars = TRUE) 
write.csv(summaryAll90s21 ,file="allModsPar90s21.csv")


################################################################################
## All models
tmp <- ~ tsFitStat(Q, X, aggr = list(by = as.yearmon, FUN = sum)) 
tmp1 <- ~ cor(head(X, -1), tail(X - Q, -1), use = "complete")
#tmp1 <- ~cor(Q - X, shiftWindow(X, -1), use = "complete")  
hydromad.options(stats = list(r.sq.monthly = tmp, X1 = tmp1))
allModsStat708090s21 <- runlist(mod70s21 = mod70s21, sim8090sPar70s21 = sim8090sPar70s21,
                               mod80s21 = mod80s21, sim7090sPar80s21 = sim7090sPar80s21,
                               mod90s21 = mod90s21, sim7080sPar90s21 = sim7080sPar90s21)
summary(allModsStat708090s21, stats = c(hydromad.getOption("summary.stats"),"r.sq.monthly", "bias", "X1", "X0"))
## write table
summaryAll708090s21 <- summary(allModsStat708090s21, stats = c(hydromad.getOption("summary.stats"), "r.sq.monthly", "bias", "X1", "X0")) 
write.csv(summaryAll708090s21 ,file="allModsStat708090s21.csv")

# parameter values
coef(mod70s21)
coef(sim8090sPar70s21)
coef(mod80s21)
coef(sim7090sPar80s21)
coef(mod90s21)
coef(sim7080sPar90s21)
## write table
coefmod70s21 <- coef(mod70s21)
coefsim8090sPar70s21 <- coef(sim8090sPar70s21)
coefmod80s21 <- coef(mod80s21)
coefsim7090sPar80s21 <- coef(sim7090sPar80s21)
coefmod90s21 <- coef(mod90s21)
coefsim7080sPar90s21 <- coef(sim7080sPar90s21)
rbind(coefmod70s21, coefsim8090sPar70s21, coefmod80s21, coefsim7090sPar80s21, coefmod90s21, coefsim7080sPar90s21)
allcoefmod708090s21 <- rbind(coefmod70s21, coefsim8090sPar70s21, coefmod80s21, coefsim7090sPar80s21, coefmod90s21, coefsim7080sPar90s21)
write.csv(allcoefmod708090s21 ,file="allcoefMod708090s21.csv")

# objective function values
- objFunVal(mod70s21)
- objFunVal(sim8090sPar70s21)
- objFunVal(mod80s21)
- objFunVal(sim7090sPar80s21)
- objFunVal(mod90s21)
- objFunVal(sim7080sPar90s21)
## write table
objmod70s21 <- - objFunVal(mod70s21)
objsim8090sPar70s21 <- - objFunVal(sim8090sPar70s21)
objmod80s21 <- - objFunVal(mod80s21)
objsim7090sPar80s21 <- - objFunVal(sim7090sPar80s21)
objmod90s21 <- - objFunVal(mod90s21)
objsim7080sPar90s21 <- - objFunVal(sim7080sPar90s21)
rbind(objmod70s21, objsim8090sPar70s21, objmod80s21, objsim7090sPar80s21, objmod90s21, objsim7080sPar90s21)
obj.fn <- rbind(objmod70s21, objsim8090sPar70s21, objmod80s21, objsim7090sPar80s21, objmod90s21, objsim7080sPar90s21)
write.csv(obj.fn ,file="allObjMod708090s21.csv")

# cross correlation values
cor(Qmod70s21, Qobs70s21, use = "complete.obs")
cor(Qsim8090sPar70s21, Qobs8090sPar70s21, use = "complete.obs")
cor(Qmod80s21, Qobs80s21, use = "complete.obs")
cor(Qsim7090sPar80s21, Qobs7090sPar80s21, use = "complete.obs")
cor(Qmod90s21, Qobs90s21, use = "complete.obs")
cor(Qsim7080sPar90s21, Qobs7080sPar90s21, use = "complete.obs")
## write table
cormod70s21 <- cor(Qmod70s21, Qobs70s21, use = "complete.obs")
corsim8090sPar70s21 <- cor(Qsim8090sPar70s21, Qobs8090sPar70s21, use = "complete.obs")
cormod80s21 <- cor(Qmod80s21, Qobs80s21, use = "complete.obs")
corsim7090sPar80s21 <- cor(Qsim7090sPar80s21, Qobs7090sPar80s21, use = "complete.obs")
cormod90s21 <- cor(Qmod90s21, Qobs90s21, use = "complete.obs")
corsim7080sPar90s21 <- cor(Qsim7080sPar90s21, Qobs7080sPar90s21, use = "complete.obs")
rbind(cormod70s21, corsim8090sPar70s21, cormod80s21, corsim7090sPar80s21, cormod90s21, corsim7080sPar90s21)
cross_correl <- rbind(cormod70s21, corsim8090sPar70s21, cormod80s21, corsim7090sPar80s21, cormod90s21, corsim7080sPar90s21)
write.csv(cross_correl ,file="allCorMod708090s21.csv")

# ED calculation
sqrt((cor(Qmod70s21, Qobs70s21, use = "complete.obs") - 1)^2 + 
     (sd(Qmod70s21, na.rm = TRUE)/sd(Qobs70s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qmod70s21, na.rm = TRUE)/mean(Qobs70s21, na.rm = TRUE) - 1)^2)
sqrt((cor(Qsim8090sPar70s21, Qobs8090sPar70s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim8090sPar70s21, na.rm = TRUE)/sd(Qobs8090sPar70s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim8090sPar70s21, na.rm = TRUE)/mean(Qobs8090sPar70s21, na.rm = TRUE) - 1)^2)
sqrt((cor(Qmod80s21, Qobs80s21, use = "complete.obs") - 1)^2 + 
     (sd(Qmod80s21, na.rm = TRUE)/sd(Qobs80s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qmod80s21, na.rm = TRUE)/mean(Qobs80s21, na.rm = TRUE) - 1)^2)
sqrt((cor(Qsim7090sPar80s21, Qobs7090sPar80s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim7090sPar80s21, na.rm = TRUE)/sd(Qobs7090sPar80s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim7090sPar80s21, na.rm = TRUE)/mean(Qobs7090sPar80s21, na.rm = TRUE) - 1)^2)     
sqrt((cor(Qmod90s21, Qobs90s21, use = "complete.obs") - 1)^2 + 
     (sd(Qmod90s21, na.rm = TRUE)/sd(Qobs90s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qmod90s21, na.rm = TRUE)/mean(Qobs90s21, na.rm = TRUE) - 1)^2)
sqrt((cor(Qsim7080sPar90s21, Qobs7080sPar90s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim7080sPar90s21, na.rm = TRUE)/sd(Qobs7080sPar90s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim7080sPar90s21, na.rm = TRUE)/mean(Qobs7080sPar90s21, na.rm = TRUE) - 1)^2)     
## write table
EDmod70s21 <- sqrt((cor(Qmod70s21, Qobs70s21, use = "complete.obs") - 1)^2 + 
                  (sd(Qmod70s21, na.rm = TRUE)/sd(Qobs70s21, na.rm = TRUE) - 1)^2 + 
                  (mean(Qmod70s21, na.rm = TRUE)/mean(Qobs70s21, na.rm = TRUE) - 1)^2)
EDsim8090sPar70s21 <- sqrt((cor(Qsim8090sPar70s21, Qobs8090sPar70s21, use = "complete.obs") - 1)^2 + 
                          (sd(Qsim8090sPar70s21, na.rm = TRUE)/sd(Qobs8090sPar70s21, na.rm = TRUE) - 1)^2 + 
                          (mean(Qsim8090sPar70s21, na.rm = TRUE)/mean(Qobs8090sPar70s21, na.rm = TRUE) - 1)^2)                
EDmod80s21 <- sqrt((cor(Qmod80s21, Qobs80s21, use = "complete.obs") - 1)^2 + 
                  (sd(Qmod80s21, na.rm = TRUE)/sd(Qobs80s21, na.rm = TRUE) - 1)^2 + 
                  (mean(Qmod80s21, na.rm = TRUE)/mean(Qobs80s21, na.rm = TRUE) - 1)^2)
EDsim7090sPar80s21 <- sqrt((cor(Qsim7090sPar80s21, Qobs7090sPar80s21, use = "complete.obs") - 1)^2 + 
                          (sd(Qsim7090sPar80s21, na.rm = TRUE)/sd(Qobs7090sPar80s21, na.rm = TRUE) - 1)^2 + 
                          (mean(Qsim7090sPar80s21, na.rm = TRUE)/mean(Qobs7090sPar80s21, na.rm = TRUE) - 1)^2)                 
EDmod90s21 <- sqrt((cor(Qmod90s21, Qobs90s21, use = "complete.obs") - 1)^2 + 
                  (sd(Qmod90s21, na.rm = TRUE)/sd(Qobs90s21, na.rm = TRUE) - 1)^2 + 
                  (mean(Qmod90s21, na.rm = TRUE)/mean(Qobs90s21, na.rm = TRUE) - 1)^2)
EDsim7080sPar90s21 <- sqrt((cor(Qsim7080sPar90s21, Qobs7080sPar90s21, use = "complete.obs") - 1)^2 + 
                          (sd(Qsim7080sPar90s21, na.rm = TRUE)/sd(Qobs7080sPar90s21, na.rm = TRUE) - 1)^2 + 
                          (mean(Qsim7080sPar90s21, na.rm = TRUE)/mean(Qobs7080sPar90s21, na.rm = TRUE) - 1)^2)  
rbind(EDmod70s21, EDsim8090sPar70s21, EDmod80s21, EDsim7090sPar80s21, EDmod90s21, EDsim7080sPar90s21)
ED <- rbind(EDmod70s21, EDsim8090sPar70s21, EDmod80s21, EDsim7090sPar80s21, EDmod90s21, EDsim7080sPar90s21)
write.csv(ED ,file="allED708090s21.csv") 

# cbind all
cbind(summaryAll708090s21, obj.fn, cross_correl, ED, allcoefmod708090s21)
## write table
allResultMod708090s21 <- cbind(summaryAll708090s21, obj.fn, cross_correl, ED, allcoefmod708090s21)
write.csv(allResultMod708090s21 ,file="allResultMod708090s21.csv")

# Flow Duration curve
xyplot.list(allModsStat708090s21, FUN = qqmath, type = c("l", "g"), scales = list(y = list(log = TRUE)), xlab = "Standard normal variate", ylab = "Flow (mm/day)", panel = panel.qqmath.tails, as.table = TRUE)
## plot 
png(file="FlowDurationCurve.png") 
print(
xyplot.list(allModsStat708090s21, FUN = qqmath, type = c("l", "g"), scales = list(y = list(log = TRUE)), xlab = "Standard normal variate", ylab = "Flow (mm/day)", panel = panel.qqmath.tails, as.table = TRUE)
)
dev.off()

## structure
str(mod70s21)
## write table
sink("mod70s21str.txt")
str(mod70s21)
sink()

str(mod80s21)
## write table
sink("mod80s21str.txt")
str(mod80s21)
sink()

str(mod90s21)
## write table
sink("mod90s21str.txt")
str(mod90s21)
sink()

################################################################################
## Detailed results
allModsStat708090s21_Dt <- runlist(mod70s21 = mod70s21, sim80sPar70s21 = sim80sPar70s21, sim90sPar70s21 = sim90sPar70s21, sim8090sPar70s21 = sim8090sPar70s21,
                               mod80s21 = mod80s21, sim70sPar80s21 = sim70sPar80s21, sim90sPar80s21 = sim90sPar80s21, sim7090sPar80s21 = sim7090sPar80s21,
                               mod90s21 = mod90s21, sim70sPar90s21 = sim70sPar90s21, sim80sPar90s21 = sim80sPar90s21, sim7080sPar90s21 = sim7080sPar90s21)
summary(allModsStat708090s21_Dt, stats = c(hydromad.getOption("summary.stats"),"r.sq.monthly", "bias", "X1", "X0"))
## write table
summaryAll708090s21_Dt <- summary(allModsStat708090s21_Dt, stats = c(hydromad.getOption("summary.stats"), "r.sq.monthly", "bias", "X1", "X0")) 
write.csv(summaryAll708090s21_Dt ,file="allModsStat708090s21_Dt.csv")

# parameter values
coef(mod70s21)
coef(sim80sPar70s21)
coef(sim90sPar70s21)
coef(sim8090sPar70s21)
coef(mod80s21)
coef(sim70sPar80s21)
coef(sim90sPar80s21)
coef(sim7090sPar80s21)
coef(mod90s21)
coef(sim70sPar90s21)
coef(sim80sPar90s21)
coef(sim7080sPar90s21)
coefmod70s21 <- coef(mod70s21)
coefsim80sPar70s21 <- coef(sim80sPar70s21)
coefsim90sPar70s21 <- coef(sim90sPar70s21)
coefsim8090sPar70s21 <- coef(sim8090sPar70s21)
coefmod80s21 <- coef(mod80s21)
coefsim70sPar80s21 <- coef(sim70sPar80s21)
coefsim90sPar80s21 <- coef(sim90sPar80s21)
coefsim7090sPar80s21 <- coef(sim7090sPar80s21)
coefmod90s21 <- coef(mod90s21)
coefsim70sPar90s21 <- coef(sim70sPar90s21)
coefsim80sPar90s21 <- coef(sim80sPar90s21)
coefsim7080sPar90s21 <- coef(sim7080sPar90s21)
rbind(coefmod70s21, coefsim80sPar70s21, coefsim90sPar70s21, coefsim8090sPar70s21, 
      coefmod80s21, coefsim70sPar80s21, coefsim90sPar80s21, coefsim7090sPar80s21, 
      coefmod90s21, coefsim70sPar90s21, coefsim80sPar90s21, coefsim7080sPar90s21)
## write table
allcoefmod708090s21_Dt <- rbind(coefmod70s21, coefsim80sPar70s21, coefsim90sPar70s21, coefsim8090sPar70s21, 
                             coefmod80s21, coefsim70sPar80s21, coefsim90sPar80s21, coefsim7090sPar80s21, 
                             coefmod90s21, coefsim70sPar90s21, coefsim80sPar90s21, coefsim7080sPar90s21)
write.csv(allcoefmod708090s21_Dt ,file="allcoefMod708090s21_Dt.csv")

# objective function values
- objFunVal(mod70s21)
- objFunVal(sim80sPar70s21)
- objFunVal(sim90sPar70s21)
- objFunVal(sim8090sPar70s21)
- objFunVal(mod80s21)
- objFunVal(sim70sPar80s21)
- objFunVal(sim90sPar80s21)
- objFunVal(sim7090sPar80s21)
- objFunVal(mod90s21)
- objFunVal(sim70sPar90s21)
- objFunVal(sim80sPar90s21)
- objFunVal(sim7080sPar90s21)
objmod70s21 <- - objFunVal(mod70s21)
objsim80sPar70s21 <- - objFunVal(sim80sPar70s21)
objsim90sPar70s21 <- - objFunVal(sim90sPar70s21)
objsim8090sPar70s21 <- - objFunVal(sim8090sPar70s21)
objmod80s21 <- - objFunVal(mod80s21)
objsim70sPar80s21 <- - objFunVal(sim70sPar80s21)
objsim90sPar80s21 <- - objFunVal(sim90sPar80s21)
objsim7090sPar80s21 <- - objFunVal(sim7090sPar80s21)
objmod90s21 <- - objFunVal(mod90s21)
objsim70sPar90s21 <- - objFunVal(sim70sPar90s21)
objsim80sPar90s21 <- - objFunVal(sim80sPar90s21)
objsim7080sPar90s21 <- - objFunVal(sim7080sPar90s21)
rbind(objmod70s21, objsim80sPar70s21, objsim90sPar70s21, objsim8090sPar70s21, 
      objmod80s21, objsim70sPar80s21, objsim90sPar80s21, objsim7090sPar80s21, 
      objmod90s21, objsim70sPar90s21, objsim80sPar90s21, objsim7080sPar90s21)
## write table
obj.fn_Dt <- rbind(objmod70s21, objsim80sPar70s21, objsim90sPar70s21, objsim8090sPar70s21, 
                   objmod80s21, objsim70sPar80s21, objsim90sPar80s21, objsim7090sPar80s21, 
                   objmod90s21, objsim70sPar90s21, objsim80sPar90s21, objsim7080sPar90s21)
write.csv(obj.fn_Dt ,file="allObjMod708090s21_Dt.csv")

# cross correlation values
cor(Qmod70s21, Qobs70s21, use = "complete.obs")
cor(Qsim80sPar70s21, Qobs80sPar70s21, use = "complete.obs")
cor(Qsim90sPar70s21, Qobs90sPar70s21, use = "complete.obs")
cor(Qsim8090sPar70s21, Qobs8090sPar70s21, use = "complete.obs")
cor(Qmod80s21, Qobs80s21, use = "complete.obs")
cor(Qsim70sPar80s21, Qobs70sPar80s21, use = "complete.obs")
cor(Qsim90sPar80s21, Qobs90sPar80s21, use = "complete.obs")
cor(Qsim7090sPar80s21, Qobs7090sPar80s21, use = "complete.obs")
cor(Qmod90s21, Qobs90s21, use = "complete.obs")
cor(Qsim70sPar90s21, Qobs70sPar90s21, use = "complete.obs")
cor(Qsim80sPar90s21, Qobs80sPar90s21, use = "complete.obs")
cor(Qsim7080sPar90s21, Qobs7080sPar90s21, use = "complete.obs")
cormod70s21 <- cor(Qmod70s21, Qobs70s21, use = "complete.obs")
corsim80sPar70s21 <- cor(Qsim80sPar70s21, Qobs80sPar70s21, use = "complete.obs")
corsim90sPar70s21 <- cor(Qsim90sPar70s21, Qobs90sPar70s21, use = "complete.obs")
corsim8090sPar70s21 <- cor(Qsim8090sPar70s21, Qobs8090sPar70s21, use = "complete.obs")

cormod80s21 <- cor(Qmod80s21, Qobs80s21, use = "complete.obs")
corsim70sPar80s21 <- cor(Qsim70sPar80s21, Qobs70sPar80s21, use = "complete.obs")
corsim90sPar80s21 <- cor(Qsim90sPar80s21, Qobs90sPar80s21, use = "complete.obs")
corsim7090sPar80s21 <- cor(Qsim7090sPar80s21, Qobs7090sPar80s21, use = "complete.obs")

cormod90s21 <- cor(Qmod90s21, Qobs90s21, use = "complete.obs")
corsim70sPar90s21 <- cor(Qsim70sPar90s21, Qobs70sPar90s21, use = "complete.obs")
corsim80sPar90s21 <- cor(Qsim80sPar90s21, Qobs80sPar90s21, use = "complete.obs")
corsim7080sPar90s21 <- cor(Qsim7080sPar90s21, Qobs7080sPar90s21, use = "complete.obs")
rbind(cormod70s21, corsim80sPar70s21, corsim90sPar70s21, corsim8090sPar70s21, 
      cormod80s21, corsim70sPar80s21, corsim90sPar80s21, corsim7090sPar80s21, 
      cormod90s21, corsim70sPar90s21, corsim80sPar90s21, corsim7080sPar90s21)
## write table
cross_correl_Dt <- rbind(cormod70s21, corsim80sPar70s21, corsim90sPar70s21, corsim8090sPar70s21, 
                         cormod80s21, corsim70sPar80s21, corsim90sPar80s21, corsim7090sPar80s21, 
                         cormod90s21, corsim70sPar90s21, corsim80sPar90s21, corsim7080sPar90s21)
write.csv(cross_correl_Dt ,file="allCorMod708090s21_Dt.csv")

# ED calculation
sqrt((cor(Qmod70s21, Qobs70s21, use = "complete.obs") - 1)^2 + 
     (sd(Qmod70s21, na.rm = TRUE)/sd(Qobs70s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qmod70s21, na.rm = TRUE)/mean(Qobs70s21, na.rm = TRUE) - 1)^2)
sqrt((cor(Qsim80sPar70s21, Qobs80sPar70s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim80sPar70s21, na.rm = TRUE)/sd(Qobs80sPar70s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim80sPar70s21, na.rm = TRUE)/mean(Qobs80sPar70s21, na.rm = TRUE) - 1)^2)
sqrt((cor(Qsim90sPar70s21, Qobs90sPar70s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim90sPar70s21, na.rm = TRUE)/sd(Qobs90sPar70s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim90sPar70s21, na.rm = TRUE)/mean(Qobs90sPar70s21, na.rm = TRUE) - 1)^2)     
sqrt((cor(Qsim8090sPar70s21, Qobs8090sPar70s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim8090sPar70s21, na.rm = TRUE)/sd(Qobs8090sPar70s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim8090sPar70s21, na.rm = TRUE)/mean(Qobs8090sPar70s21, na.rm = TRUE) - 1)^2)
sqrt((cor(Qmod80s21, Qobs80s21, use = "complete.obs") - 1)^2 + 
     (sd(Qmod80s21, na.rm = TRUE)/sd(Qobs80s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qmod80s21, na.rm = TRUE)/mean(Qobs80s21, na.rm = TRUE) - 1)^2)
sqrt((cor(Qsim70sPar80s21, Qobs70sPar80s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim70sPar80s21, na.rm = TRUE)/sd(Qobs70sPar80s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim70sPar80s21, na.rm = TRUE)/mean(Qobs70sPar80s21, na.rm = TRUE) - 1)^2)
sqrt((cor(Qsim90sPar80s21, Qobs90sPar80s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim90sPar80s21, na.rm = TRUE)/sd(Qobs90sPar80s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim90sPar80s21, na.rm = TRUE)/mean(Qobs90sPar80s21, na.rm = TRUE) - 1)^2)  
sqrt((cor(Qsim7090sPar80s21, Qobs7090sPar80s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim7090sPar80s21, na.rm = TRUE)/sd(Qobs7090sPar80s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim7090sPar80s21, na.rm = TRUE)/mean(Qobs7090sPar80s21, na.rm = TRUE) - 1)^2) 
sqrt((cor(Qmod90s21, Qobs90s21, use = "complete.obs") - 1)^2 + 
     (sd(Qmod90s21, na.rm = TRUE)/sd(Qobs90s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qmod90s21, na.rm = TRUE)/mean(Qobs90s21, na.rm = TRUE) - 1)^2)
sqrt((cor(Qsim70sPar90s21, Qobs70sPar90s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim70sPar90s21, na.rm = TRUE)/sd(Qobs70sPar90s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim70sPar90s21, na.rm = TRUE)/mean(Qobs70sPar90s21, na.rm = TRUE) - 1)^2)
sqrt((cor(Qsim80sPar90s21, Qobs80sPar90s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim80sPar90s21, na.rm = TRUE)/sd(Qobs80sPar90s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim80sPar90s21, na.rm = TRUE)/mean(Qobs80sPar90s21, na.rm = TRUE) - 1)^2)  
sqrt((cor(Qsim7080sPar90s21, Qobs7080sPar90s21, use = "complete.obs") - 1)^2 + 
     (sd(Qsim7080sPar90s21, na.rm = TRUE)/sd(Qobs7080sPar90s21, na.rm = TRUE) - 1)^2 + 
     (mean(Qsim7080sPar90s21, na.rm = TRUE)/mean(Qobs7080sPar90s21, na.rm = TRUE) - 1)^2)     
EDmod70s21 <- sqrt((cor(Qmod70s21, Qobs70s21, use = "complete.obs") - 1)^2 + 
                  (sd(Qmod70s21, na.rm = TRUE)/sd(Qobs70s21, na.rm = TRUE) - 1)^2 + 
                  (mean(Qmod70s21, na.rm = TRUE)/mean(Qobs70s21, na.rm = TRUE) - 1)^2)
EDsim80sPar70s21 <- sqrt((cor(Qsim80sPar70s21, Qobs80sPar70s21, use = "complete.obs") - 1)^2 + 
                        (sd(Qsim80sPar70s21, na.rm = TRUE)/sd(Qobs80sPar70s21, na.rm = TRUE) - 1)^2 + 
                        (mean(Qsim80sPar70s21, na.rm = TRUE)/mean(Qobs80sPar70s21, na.rm = TRUE) - 1)^2)
EDsim90sPar70s21 <- sqrt((cor(Qsim90sPar70s21, Qobs90sPar70s21, use = "complete.obs") - 1)^2 + 
                        (sd(Qsim90sPar70s21, na.rm = TRUE)/sd(Qobs90sPar70s21, na.rm = TRUE) - 1)^2 + 
                        (mean(Qsim90sPar70s21, na.rm = TRUE)/mean(Qobs90sPar70s21, na.rm = TRUE) - 1)^2)                   
EDsim8090sPar70s21 <- sqrt((cor(Qsim8090sPar70s21, Qobs8090sPar70s21, use = "complete.obs") - 1)^2 + 
                          (sd(Qsim8090sPar70s21, na.rm = TRUE)/sd(Qobs8090sPar70s21, na.rm = TRUE) - 1)^2 + 
                          (mean(Qsim8090sPar70s21, na.rm = TRUE)/mean(Qobs8090sPar70s21, na.rm = TRUE) - 1)^2)                
EDmod80s21 <- sqrt((cor(Qmod80s21, Qobs80s21, use = "complete.obs") - 1)^2 + 
                  (sd(Qmod80s21, na.rm = TRUE)/sd(Qobs80s21, na.rm = TRUE) - 1)^2 + 
                  (mean(Qmod80s21, na.rm = TRUE)/mean(Qobs80s21, na.rm = TRUE) - 1)^2)
EDsim70sPar80s21 <- sqrt((cor(Qsim70sPar80s21, Qobs70sPar80s21, use = "complete.obs") - 1)^2 + 
                        (sd(Qsim70sPar80s21, na.rm = TRUE)/sd(Qobs70sPar80s21, na.rm = TRUE) - 1)^2 + 
                        (mean(Qsim70sPar80s21, na.rm = TRUE)/mean(Qobs70sPar80s21, na.rm = TRUE) - 1)^2)
EDsim90sPar80s21 <- sqrt((cor(Qsim90sPar80s21, Qobs90sPar80s21, use = "complete.obs") - 1)^2 + 
                        (sd(Qsim90sPar80s21, na.rm = TRUE)/sd(Qobs90sPar80s21, na.rm = TRUE) - 1)^2 + 
                        (mean(Qsim90sPar80s21, na.rm = TRUE)/mean(Qobs90sPar80s21, na.rm = TRUE) - 1)^2)                   
EDsim7090sPar80s21 <- sqrt((cor(Qsim7090sPar80s21, Qobs7090sPar80s21, use = "complete.obs") - 1)^2 + 
                          (sd(Qsim7090sPar80s21, na.rm = TRUE)/sd(Qobs7090sPar80s21, na.rm = TRUE) - 1)^2 + 
                          (mean(Qsim7090sPar80s21, na.rm = TRUE)/mean(Qobs7090sPar80s21, na.rm = TRUE) - 1)^2)                 
EDmod90s21 <- sqrt((cor(Qmod90s21, Qobs90s21, use = "complete.obs") - 1)^2 + 
                  (sd(Qmod90s21, na.rm = TRUE)/sd(Qobs90s21, na.rm = TRUE) - 1)^2 + 
                  (mean(Qmod90s21, na.rm = TRUE)/mean(Qobs90s21, na.rm = TRUE) - 1)^2)
EDsim70sPar90s21 <- sqrt((cor(Qsim70sPar90s21, Qobs70sPar90s21, use = "complete.obs") - 1)^2 + 
                        (sd(Qsim70sPar90s21, na.rm = TRUE)/sd(Qobs70sPar90s21, na.rm = TRUE) - 1)^2 + 
                        (mean(Qsim70sPar90s21, na.rm = TRUE)/mean(Qobs70sPar90s21, na.rm = TRUE) - 1)^2)
EDsim80sPar90s21 <- sqrt((cor(Qsim80sPar90s21, Qobs80sPar90s21, use = "complete.obs") - 1)^2 + 
                        (sd(Qsim80sPar90s21, na.rm = TRUE)/sd(Qobs80sPar90s21, na.rm = TRUE) - 1)^2 + 
                        (mean(Qsim80sPar90s21, na.rm = TRUE)/mean(Qobs80sPar90s21, na.rm = TRUE) - 1)^2)                   
EDsim7080sPar90s21 <- sqrt((cor(Qsim7080sPar90s21, Qobs7080sPar90s21, use = "complete.obs") - 1)^2 + 
                          (sd(Qsim7080sPar90s21, na.rm = TRUE)/sd(Qobs7080sPar90s21, na.rm = TRUE) - 1)^2 + 
                          (mean(Qsim7080sPar90s21, na.rm = TRUE)/mean(Qobs7080sPar90s21, na.rm = TRUE) - 1)^2)  
rbind(EDmod70s21, EDsim80sPar70s21, EDsim90sPar70s21, EDsim8090sPar70s21, 
      EDmod80s21, EDsim70sPar80s21, EDsim90sPar80s21, EDsim7090sPar80s21, 
      EDmod90s21, EDsim70sPar90s21, EDsim80sPar90s21, EDsim7080sPar90s21)
## write table
ED_Dt <- rbind(EDmod70s21, EDsim80sPar70s21, EDsim90sPar70s21, EDsim8090sPar70s21, 
               EDmod80s21, EDsim70sPar80s21, EDsim90sPar80s21, EDsim7090sPar80s21, 
               EDmod90s21, EDsim70sPar90s21, EDsim80sPar90s21, EDsim7080sPar90s21)
write.csv(ED_Dt ,file="allED708090s21_Dt.csv") 

# cbind all
cbind(summaryAll708090s21_Dt, obj.fn_Dt, cross_correl_Dt, ED_Dt, allcoefmod708090s21_Dt)
## write table
allResultMod708090s21_Dt <- cbind(summaryAll708090s21_Dt, obj.fn_Dt, cross_correl_Dt, ED_Dt, allcoefmod708090s21_Dt)
write.csv(allResultMod708090s21_Dt ,file="allResultMod708090s21_Dt.csv")

# save Rdata
save(mod70s21, file = "mod70s21.Rdata")
save(mod80s21, file = "mod80s21.Rdata")
save(mod90s21, file = "mod90s21.Rdata")
save(mod70s21, mod80s21, mod90s21, file = "mods21.Rdata") 
ls()                     

