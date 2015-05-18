---
title: "The public and economic impact of storms"
author: "Dave Gerson"
date: "Sunday, May 17, 2015"
output: html_document
---
#The public and economic impact of storms
In this paper we aim to analyze and study the impact of storms.  Storms and ofther various weather related incidents lead to a tragic losses, both human and financial.

```{r, cache=TRUE}
wd <- "C:/Users/gerson64/Desktop/Dropbox Sync/Dropbox/Coursera/reproducibleresearch/project2/"
setwd(wd)
#Loading and preprocessing the data
dat <- read.csv("repdata-data-StormData.csv" , stringsAsFactors = FALSE)
```

###Data Prep
In order to make sure that we have the proper information we have to clean up a few fields, specifically dates, events, and property damage.  Dates in this case are currently a string, and they need to be converted to dates.  Events are also held as a string and need to e converted to a factor, finally the property damage is a field that requires both the initial property damage field and and identified for the unit of quantity.  

```{r, echo=FALSE, cache=TRUE}
#Create Dates
tmp <- matrix(unlist(strsplit(dat$BGN_DATE, " ")), ncol=2, byrow=TRUE)[,1]
dat$BGN_DATE <- as.Date(tmp, format = "%m/%d/%Y " )
#Event Factors
dat$EVTYPE = as.factor(dat$EVTYPE)
#Create Economic Value
dat$PROPDMG <- dat$PROPDMG * ifelse(dat$PROPDMGEXP == "K" , 1000 , ifelse(dat$PROPDMGEXP == "M" , 1000000 , 1000000000))
```

###Public Damage reports
In order to assess the damage we have to take two factors into account.  Since both injury and deaths need to be accounted for I defined a measure that accounts for both.  Since deaths and injuries aren't and apples to oranges comparison I decided that all deaths should be at the bare minimum equivalent to 10 injuries.  So I define this as a measure using the aggregate function.  To directly solve which event leads to the greatest impact I decided to use a pareto chart.  Pareto charts are a type of chart developed in 6 sigma that is made to visually demonstrate the 80/20 principle.    

Below it is used as a measure to analyze the health and human impacts of the top 10 events.

```{r, cache=TRUE}
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
health_Pareto
```

###Economic Damage reports
Since I have already processed the data to include the financial quantity modifier.  With the data alreday processed I can immediately aggregate the data and place it into the 

```{r, cache=TRUE}
#Most Destructive Events Pareto Economical
economic_health <- dat$PROPDMG
economic_health <- aggregate( economic_health , 
    by = list( dat$EVTYPE ) , 
	FUN = sum
)

economic_measure <- economic_health$x
names(economic_measure) <- economic_health[,1]
economic_measure <- economic_measure[order(-economic_measure)]

economic_Pareto <- pareto.chart(economic_measure[1:10], ylab = "Economic Damage")
economic_Pareto
```

##Synopsis
After reviewing the data it appears that there are very heavy tails to the data, and that the event attribution s vastly different in the differnt categories.  Tornados cause over 60% of the human damage while under 5% of the economic damage.  An unusual human health impacting event is excessive heat, which does not even register as an economic event.  Events that register on both indicators are Tornadoes, Flash Floods, Lightening, and High Winds.         



