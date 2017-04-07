#read tsPQE respect to catchment name
read_tsPQE= function(catchment){
  
  fname = paste0('C:/UserData/seol/Sensitivity Analyses/PQE input/',catchment,'/',catchment,'.csv')
  
  #read input
  pqdat <- read.table(fname, sep = ",", col.names = c("P", "Q", "Date"), as.is = TRUE)
  pqdat$Date <- as.Date(pqdat$Date, "%d/%m/%Y")
  pqdat$P[pqdat$P < 0] <- NA
  pqdat$Q[pqdat$Q < 0] <- NA
  pqdat$Q <- convertFlow(pqdat$Q, from = "ML", area.km2 = 148)
  tsPQ <- xts(pqdat[, 1:2], pqdat$Date, frequency = 1)
  tdat <- read.table("input/ev_070014.csv",sep=",",col.names=c("T","Date"),as.is=TRUE)
  tdat$Date <- as.Date(tdat$Date, "%d/%m/%Y")
  tdat <- subset(tdat, !is.na(Date))
  tsT <- xts(tdat[, 1], tdat$Date, frequency = 1)
  tsPQE <- merge(tsPQ, E = tsT * 0.8, all = FALSE)
  tsPQE <- na.trim(tsPQE)
  # change NA in rainfall to 0
  tsPQE$P[is.na(tsPQE$P)]<-0
  # fill E NAs with monthly average
  #getYearMonth<-function(d) as.Date(format(d,"%Y-%m-01"))
  monthly.averages<-aggregate(tsPQE$E,by=as.yearmon,FUN=mean,na.rm=T)
  toreplace<-is.na(tsPQE$E)
  toreplace.yearmonth<-as.yearmon(index(tsPQE)[toreplace])
  tsPQE$E[toreplace]<- coredata(monthly.averages)[match(toreplace.yearmonth,index(monthly.averages))]
  summary(tsPQE)

  return(tsPQE)
}
