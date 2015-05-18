wd <- "C:/Users/gerson64/Desktop/Dropbox Sync/Dropbox/Coursera/reproducibleresearch/project2/"
setwd(wd)
#Loading and preprocessing the data
dat <- read.csv("repdata-data-StormData.csv" , stringsAsFactors = FALSE)

#Create Dates
tmp <- matrix(unlist(strsplit(dat$BGN_DATE, " ")), ncol=2, byrow=TRUE)[,1]
dat$BGN_DATE <- as.Date(tmp, format = "%m/%d/%Y " )
#Event Factors
dat$EVTYPE = as.factor(dat$EVTYPE)
#Create Economic Value
dat$PROPDMG <- dat$PROPDMG * ifelse(dat$PROPDMGEXP == "K" , 1000 , ifelse(dat$PROPDMGEXP == "M" , 1000000 , 1000000000))


#Most Destructive Events Pareto for people
library(qcc)
pop_health <- (dat$FATALITIES * 10) + dat$INJURIES
pop_health <- aggregate( pop_health , 
	by = list( dat$EVTYPE ) , 
	FUN = sum
)
health_measure <- pop_health$x
names(health_measure) <- pop_health[,1]
health_measure <- health_measure[order(-health_measure)]

health_Pareto <- pareto.chart(health_measure[1:10], ylab = "Damage Metric")


#Most Destructive Events Pareto Economicallya 
economic_health <- dat$PROPDMG
economic_health <- aggregate( economic_health , 
	by = list( dat$EVTYPE ) , 
	FUN = sum
)

economic_measure <- economic_health$x
names(economic_measure) <- economic_health[,1]
economic_measure <- economic_measure[order(-economic_measure)]

economic_Pareto <- pareto.chart(economic_measure[1:10], ylab = "Economic Damage")






