---
title: "Dallas2017"
author: "THAO NGUYEN and GABRIEL TELLEZ"
date: "March 2, 2018"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(rcompanion)
library(corrplot)
library(caret)
library(MASS) # step AIC
library(vars)
library(fanplot)
library(forecast)
library(RColorBrewer)
```


```{r}
# reading in data month by month  from Texas
#jan <-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_01.17.csv",na.string="?",header=T,sep=',')
#feb <-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_02.17.csv",na.string="?",header=T,sep=',')
#mar <-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_03.17.csv",na.string="?",header=T,sep=',')
#apr <-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_04.17.csv",na.string="?",header=T,sep=',')
#may <-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_05.17.csv",na.string="?",header=T,sep=',')
#jun <-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_06.17.csv",na.string="?",header=T,sep=',')
#jul<-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_07.17.csv",na.string="?",header=T,sep=',')
#aug <-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_08.17.csv",na.string="?",header=T,sep=',')
#sep <-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_09.17.csv",na.string="?",header=T,sep=',')
#oct <-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_10.17.csv",na.string="?",header=T,sep=',')
#nov <-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_11.17.csv",na.string="?",header=T,sep=',')
#dec <-  read.table("C:/Users/Gau Tho/Desktop/Data Analytics Application/flight/texas flight/texas_12.17.csv",na.string="?",header=T,sep=',')

# reading in data month by month  from Texas
jan = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_01.17.csv",na.string="?",header=T,sep=',')
feb = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_02.17.csv",na.string="?",header=T,sep=',')
mar = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_03.17.csv",na.string="?",header=T,sep=',')
apr = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_04.17.csv",na.string="?",header=T,sep=',')
may = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_05.17.csv",na.string="?",header=T,sep=',')
jun = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_06.17.csv",na.string="?",header=T,sep=',')
jul = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_07.17.csv",na.string="?",header=T,sep=',')
aug = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_08.17.csv",na.string="?",header=T,sep=',')
sep = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_09.17.csv",na.string="?",header=T,sep=',')
oct = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_10.17.csv",na.string="?",header=T,sep=',')
nov = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_11.17.csv",na.string="?",header=T,sep=',')
dec = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/texas_12.17.csv",na.string="?",header=T,sep=',')



# combine into one single dataset 
mydata <- rbind(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
#dim(dallas2017)  # 984,923 records and 22 variables
str(mydata)
dim(mydata)
# exclude some unnecessary columns
mydata$YEAR<-NULL
mydata$TAIL_NUM<-NULL
```

```{r}
# focus on DALLAS FORTH WORTH airport
# origin
dallas2017 <- subset(mydata, ORIGIN == 'DFW', select = c("MONTH","AIRLINE_ID","DAY_OF_WEEK","ORIGIN","DEST","DEP_DELAY","ARR_DELAY","CANCELLED","AIR_TIME","CANCELLATION_CODE",
                                                       "DISTANCE","CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY",
                                                       "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY","DIVERTED"))
#summary(dallas2017)
#dim(dallas2017)
write.csv(dallas2017, file = "C:/Users/Jonathan/Documents/Classes/DA Applications/Project2/FlightOnTimeAnalysis_DFW2017.csv")
```

# DATA VISUALIZATION
```{r}

# Which airlines has the most flight?
dallas2017$DAY_OF_WEEK[dallas2017$DAY_OF_WEEK == 1] <- 'Mon'
dallas2017$DAY_OF_WEEK[dallas2017$DAY_OF_WEEK == 2] <- 'Tues'
dallas2017$DAY_OF_WEEK[dallas2017$DAY_OF_WEEK == 3] <- 'Wed'
dallas2017$DAY_OF_WEEK[dallas2017$DAY_OF_WEEK == 4] <- 'Thurs'
dallas2017$DAY_OF_WEEK[dallas2017$DAY_OF_WEEK == 5] <- 'Fri'
dallas2017$DAY_OF_WEEK[dallas2017$DAY_OF_WEEK== 6] <- 'Sat'
dallas2017$DAY_OF_WEEK[dallas2017$DAY_OF_WEEK == 7] <- 'Sun'
dallas2017$DAY_OF_WEEK<- as.factor(dallas2017$DAY_OF_WEEK)
dallas2017$AIRLINE_ID <- as.factor(dallas2017$AIRLINE_ID) #19805: american airline, #19977 united airline
#barplot(table(dallas2017$AIRLINE_ID),col='tomato',main="Airline Carries in DFW") 
#summary(dallas2017)

#THIS IS THE BARPLOT IN GGPLOT AND WITH AIRLINE NAMES INSTEAD OF AIRLINE IDS
# looking for major airlines in DFW: American, Express Jet, Spirit, United, Delta
dallas2017$AIRLINE_ID = factor(ifelse(dallas2017$AIRLINE_ID == "19805", "AA",
                                    ifelse(dallas2017$AIRLINE_ID == "19790", "DL",
                                             ifelse(dallas2017$AIRLINE_ID == "20366", "EV",
                                                    ifelse(dallas2017$AIRLINE_ID == "20416", "NK",
                                                            ifelse(dallas2017$AIRLINE_ID == "19977", "UA", "0"))))))
# 19805 American (AA), 20366 Express Jet (EV), 20416 Spirit (NK), 19977 United (UA), 19790 Delta (DL)
ggplot(data=dallas2017, aes(dallas2017$AIRLINE_ID, ..count..))+ 
  theme_bw()+
  #theme(legend.position="none")+
  geom_bar(aes(fill = AIRLINE_ID))+
  geom_text(stat = "count", aes(label = ..count..), vjust = 1.1) +
  labs (x = "Airline", y = "Number of Flights", title = "Top Airlines in DFW")+
  scale_y_continuous(labels = scales::comma) +guides(fill=guide_legend(title="Airlines")) +
  scale_fill_discrete(labels = c("Other", "American", "Delta", "Express Jet", "Spirit", "United")) +
  theme(plot.title = element_text(hjust=.45, face = "bold"))+
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))

# where is popular destination? ORD, ATL, LAX, DEN, IAH. ~7000 flight operated to ORD in 2017 from DFW
popular_dest <- sort(table(dallas2017$DEST), decreasing = TRUE )
barplot(popular_dest[1:10], col = "sky blue") 

#What days of the week are the most popular to fly?
sort(table(dallas2017$DAY_OF_WEEK), decreasing = TRUE )

#Thursdays are the most popualar days to flight! it might be a good idea to fly on Friday (least flights) if you don't like crowded airports!
```

# DELAYS
```{r}
#ARRIVAL DELAY
summary(dallas2017$ARR_DELAY)  #mean =7.046 min
summary(dallas2017$WEATHER_DELAY)
hist(dallas2017$ARR_DELAY,
     breaks = 300, 
     xlim = c(-75,150), 
     col ='sky blue', border='black',
     xlab ='arrival delay (in minute)',
     main=' Histogram of Arrival Delay')
#I add a line indicating the mean of the group.
abline(v=7.046, col = 'red',lwd = 4)

Delay<-dallas2017[dallas2017$ARR_DELAY>0,];dim(Delay) # 73264 flights landed with delay
Ahead <-dallas2017[dallas2017$ARR_DELAY<0,];dim(Ahead) # 106158 flights landed ahead of time

# Highly right skew. It seems that there are more values concentrated in the left side of 0. However, there are still outliers in the right of 0. The implication is that on average the flights have a delay of of 7.046 minutes. However,  more than 58% of the flights arrive in advance to the scheduled arrival time. 


#DEPARTURE DELAY
summary(dallas2017$DEP_DELAY)  # mean =10.23 min

#In reality,the aircraft often departs after the scheduled departure.however, when the plan lands, the plane is often in advance of their arrival schedule. In order to see if this is true, I create a new variable call Diff_Delay, which will help to determine if delays are produced at departure or at the arrival.

mydelay<- mutate(dallas2017, Diff_delay = ARR_DELAY - DEP_DELAY)
summary(mydelay$Diff_delay) #mean = -3.13 minutes
hist(mydelay$Diff_delay, 
     xlim = c(-50,50),
     col ='yellow',
     border ='red',
     xlab ='arrival delay (in minute)',
     main=' Histogram of Diff_delay')

# ggplot for Diff_delay
ggplot(data = mydelay, aes(mydelay$Diff_delay)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_histogram(breaks = seq(-50, 50, by = 5),
                 col = "blue", aes(fill = ..count..)) +
  geom_vline(data = mydelay, aes(xintercept = -3.13), color = "red", linetype = "dashed", size = 1.5) +
  labs(x = "Arrival Delay (minutes)", y = "Number of Flights",
       title = "Difference Between Arrival and Departure Delay") +
  xlim(c(-55,55)) +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(face = "bold"))

# ggplot for ARR_DELAY
ggplot(data = mydelay, aes(mydelay$ARR_DELAY)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_histogram(breaks = seq(-50, 150, by = 5),
                 col = "blue", aes(fill = ..count..)) +
  geom_vline(data = mydelay, aes(xintercept = 7.05), color = "red", linetype = "dashed", size = 1.5) +
  labs(x = "Arrival Delay (minutes)", y = "Number of Flights",
       title = "Arrival Delay for Flights") +
  xlim(c(-55,155)) +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(face = "bold"))

#The negative values of Diff_delay indicates that the flight covered that amount in time in the air. Delays are mainly produced in the departure. Therefore, with most of the values concentrated to the left of zero, fligths tend to have a lower arrival delay than departure delay.

#What proportion of flights that took off late but arrived early/on time ?
 flightArrDepDenom <- subset(dallas2017, DEP_DELAY != "NA" & ARR_DELAY != "NA") # subset flights with known DEP_DELAY & known ARR_DELAY.
flightArrDepLate <- subset(dallas2017, DEP_DELAY > 0 & ARR_DELAY <= 0)# subset flights that both took off and arrived late
nrow(flightArrDepLate) / nrow(flightArrDepDenom) # 8.2% 

#Do flights that take off late fly faster to make up time?
flightDepLate <- subset(dallas2017, DEP_DELAY > 0)
flightDepOnTime <- subset(dallas2017, DEP_DELAY == 0) # subset of on time flights
with(flightDepOnTime, median(DISTANCE / AIR_TIME, na.rm = TRUE)) #7.43 miles/minute
with(flightDepLate , median(DISTANCE / AIR_TIME, na.rm = TRUE)) # 7.46 miles/minute

#The median speed of departed on time flights is 7.43 miles per minute, whereas the median speed of flights that take off late is 7.46 miles per minute. So yes, flights that take off late do fly faster to make up time, but not by much.
```
 
# CANCELLATION
```{r}
cancel_index <- which(dallas2017$CANCELLED == 1) #finding the location of the cancelled flights
cancel <- dallas2017[cancel_index,]
n_cancel <- length(cancel$CANCELLATION_CODE)
table(cancel$CANCELLATION_CODE) / n_cancel * 100  # A: carrier, B:weather, C: National Air System, D: Security. 
# in 2017, 60% of cancelled flight was dued to weather conditions, followed by carrier
cancel$CANCELLATION_CODE <- factor(cancel$CANCELLATION_CODE,
                       levels = c('A','B','C','D'),
                       labels = c("Carrier Delay", "Weather Delay", "National Air System","Security Delay"))
barplot(table(cancel$CANCELLATION_CODE) / n_cancel * 100, col ='darkorange2', main ='Major Reasons for Delays at DFW in 2017')
```

#MISSING VALUE
```{r}
missing_values <- data.frame(column = names(sapply(dallas2017, class)),
                           class = sapply(dallas2017, class),
                           missing.count = colSums(is.na(dallas2017)))
missing_values

#We can divide the missing values in 2 categories:
# [1]Missing values come from "reasons for the delays"
#[2]Missing values come  from "delay time" and air time

#how many flight delay?
x<-dallas2017[dallas2017$ARR_DELAY>0,];dim(x) # 73264 flight


#For [1] reason, There is the high number of NA's(143838 values ~79.37%).But the reason for this is that only ~21% (100%-79%) of the flights were delayed. As a consequence, It not to be worry about this type of missing values as they come from the non-delay flights. We decided to assigned them values of 0. We are going to construct a flight dataframe containg only delayed flights.

#The [2]  we can observed that the NA's match with the observations for cancelled or diverted flights (Once a flight wass cancelled, no recocred data for arrival time)
```

# CREATE A DATASET CONTAINED ONLY DELAY INFORMATION
```{r}
# what is the minimum amount of time for a flight is considered delayed?
index_nodelay <- which(is.na(dallas2017$CARRIER_DELAY)) #locations of records with NA's.
aircraft_delay <- dallas2017[-index_nodelay,]  # this dataset contained only arrival delayed flight
dim(aircraft_delay)

min(aircraft_delay$ARR_DELAY) # the minimum amount of 15 minutes for which a flight is considered delayed.

# check for missing values
missing_values <- data.frame(column = names(sapply(aircraft_delay, class)),
                           class = sapply(aircraft_delay, class),
                           missing.count = colSums(is.na(aircraft_delay)))
missing_values # None missing values were found in this dataset-> this dataset is the one we will work on
#head(aircraft_delay)
```

## WORKING ON AIRCRAFT_DELAY , WHICH IS FINAL DATASET WITH 37370 OBS AND 17 VARIABLES
```{r}
reasons <- cbind(
  apply(aircraft_delay[,12:16],2,min),
  apply(aircraft_delay[,12:16],2,median),
  apply(aircraft_delay[,12:16],2,max),
  apply(aircraft_delay[,12:16],2,mean),
  apply(aircraft_delay[,12:16],2,sd)
)  

colnames(reasons) <- c("Min", "median", "max", "mean", "sd")
reasons
# the most common reason for a delay is the one produced because of the late aircraft arrival
#table representing the percentage of flights delayed by airline air
airline.id = c("DL", "AA", "AS", "UA", "OO", "EV") #, "B6", "NK", "F9")
airline.names = c("Delta", "American", "Alaska", "United", "SkyWest", "ExpressJet") #, "JetBlue", "Spirit", "Frontier")
par(mar = c(6,6,6,6))
barplot(table(aircraft_delay$AIRLINE_ID)/ table(dallas2017$AIRLINE_ID) * 100, 
        col =rev(brewer.pal(8,"Dark2")), 
        main='Percentage of Delayed Flight by Carriers', xlab = "Airline Carriers", ylab = "Percentage", 
        names.arg = airline.id, ylim = c(0,35))
legend("right", legend=airline.names, fill = rev(brewer.pal(9, "Dark2")), bg="white",
       xpd = TRUE, inset = c(-0.24,0)) 

#20409: Jetblue
#19790: delta
#19805: american airline
#19930: alaska
#19977: united airline
#20366: expressject airline
#20416: spirit
#20436: frontier airline
#So, as we see in the barplot and table, almost 30% of the JetBlue flights were delayed, while only 11% of the Delta were.American Airline delayed almost 21% of the times

# What is the best day of week/ month of year to fly to minimize your delay time?


table(aircraft_delay$DAY_OF_WEEK) / table(dallas2017$DAY_OF_WEEK) * 100 
barplot(table(aircraft_delay$DAY_OF_WEEK) / table(dallas2017$DAY_OF_WEEK)*100, main='Percentage of delayed flight by day', col ='coral2',  width = 0.1 )

table(aircraft_delay$MONTH) / table(dallas2017$MONTH) * 100 
barplot(table(aircraft_delay$MONTH) / table(dallas2017$MONTH)*100, main='Percentage of delayed flight by MONTH', col ='cyan2',  width = 0.1)

# NOTE: Friday and Sunday were days suffered more delay-> avoid these days
#  June and April suffered more delays. More people travelled. 

```

#------------- MODELLING-------------------#
```{r}
havedelay <- aircraft_delay
#str(havedelay)
#summary(havedelay)
#exclude some unnecessary variables
havedelay$DEST <- NULL
havedelay$CANCELLATION_CODE <-NULL
havedelay$CANCELLED<- NULL
havedelay$DIVERTED<-NULL
#dim(havedelay) # 37370 records with 13 variables
havedelay$MONTH<- as.factor(havedelay$MONTH)
numeric_var <- sapply(havedelay, is.numeric)
corr_matrix <- cor(havedelay[, numeric_var])
corrplot(corr_matrix,  method='square') 

# Create dependent variable
havedelay$TotalDelay<-(havedelay$ARR_DELAY+havedelay$DEP_DELAY)
havedelay$ARR_DELAY <-NULL
havedelay$DEP_DELAY <-NULL

```

```{r}
# Check for multilinearity
havedelay$MONTH<- as.factor(havedelay$MONTH)
numeric_var <- sapply(havedelay, is.numeric)
corr_matrix <- cor(havedelay[, numeric_var])
corrplot(corr_matrix,  method='square') 
```
# MULTIPLE LINEAR REGRESSION
```{r}
set.seed(8000)
train <- sample(1: nrow(havedelay), nrow(havedelay)*0.7, rep=FALSE)        
test <- -train
training_data <- havedelay[train, ]     
testing_data <- havedelay[test, ]
test_y <- havedelay$TotalDelay[test]
dim(testing_data)
dim(training_data)
LR <- lm(TotalDelay~AIR_TIME+CARRIER_DELAY+WEATHER_DELAY+NAS_DELAY+SECURITY_DELAY+LATE_AIRCRAFT_DELAY, data = training_data)
step <- stepAIC(LR, direction="both")
step$anova # display results data = training_data)
summary(LR)
#Diagnostic Plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(LR)
```

# CART MODEL--------------CLASSIFICATION PROBLEM
```{r}
havedelayLOG <- havedelay
havedelayLOG$DelayClass <- factor(ifelse(havedelayLOG$TotalDelay >= 30, "Major Delay", "Minor Delay"))
havedelayLOG$TotalDelay <- NULL

set.seed(1)
train <- sample(1: nrow(havedelayLOG), nrow(havedelayLOG)*0.7, rep=FALSE)        
test <- -train
training_data <- havedelayLOG[train, ]     
testing_data <- havedelayLOG[test, ]
dim(testing_data)
dim(training_data)
#table(training_data$DelayClass, dnn="# training data") # 22339 major, 3820 Minor
#table(testing_data$DelayClass, dnn="# testing data")  #9563 major, 1648 minor
library(rpart)
library(rpart.plot)
CARTmodel <- rpart(DelayClass ~., data = training_data, method = "class")
prp(CARTmodel)
summary(CARTmodel)

CARTpred_train <- predict(CARTmodel, type = "class")
confusionMatrix(CARTpred_train , training_data$DelayClass)

#test set
CARTpred <- predict(CARTmodel, newdata = testing_data, type = "class")
confusionMatrix(CARTpred , testing_data$DelayClass)

```

## SUPPORT VECTCOR MACHINE------------------
```{r}
library(pastecs)
library(e1071)
set.seed(2)
train <- sample(1: nrow(havedelayLOG), nrow(havedelayLOG)*0.7, rep=FALSE)        
test <- -train
training_data <- havedelayLOG[train, ]     
testing_data <- havedelayLOG[test, ]

# FINDING THE BEST MODEL BASED ON COST & GAMMA PARAMETERS
tune.out <- tune.svm(DelayClass~., data=training_data,gamma=c(.01, 1), cost=c(.01, 1))
summary(tune.out)
# best parameters are gamma 0.01 and cost 1

#Model Fit
delay.y= testing_data$DelayClass
library(rminer)
library(e1071) 
mysvm = svm(DelayClass ~ ., data=training_data, gamma= .01, cost=  1, cross=10)
svmpredict = predict(mysvm, testing_data, type="response")
table(svmpredict, delay.y)
mean(svmpredict != delay.y)
# error rate 0.04968335

model = fit(DelayClass ~ ., data=training_data, model="svm")
VariableImportance=Importance(model,training_data)
VariableImportance
L=list(runs=1,sen=t(VariableImportance$imp),sresponses=VariableImportance$sresponses)
mgraph(L,graph="IMP",leg=names(training_data),col=rev(brewer.pal(9, "Dark2")),Grid=10)
```

## TIME SERIES - VECTOR AUTOREGRESSION (VAR) MODEL
```{r}
# using aircraft_delay data set
# time series model is to show prediction of total number of arrival delayed flights
table(aircraft_delay$DAY_OF_WEEK)
#   Fri   Mon   Sat   Sun Thurs  Tues   Wed 
#  6199  5930  4503  6008  5393  4424  4913
# Total 37370
table(aircraft_delay$MONTH)
#   1    2    3    4    5    6    7    8    9   10   11   12 
# 3608 2156 3224 3629 3304 5080 4058 3974 1718 2097 1753 2769 

# function for flights arriving delayed in DFW
flight.delay = function(day, month){
  day = as.factor(ifelse(aircraft_delay$DAY_OF_WEEK == day & aircraft_delay$MONTH == month, "1", "0"))
  sum.day.month.delay = sum(day == "1")
  return(sum.day.month.delay)
}
# changed month 1-12 to get arrival delayed flights for each day on each month
flight.delay("Mon", 12)
flight.delay("Tues", 12)
flight.delay("Wed", 12)
flight.delay("Thurs", 12)
flight.delay("Fri", 12)
flight.delay("Sat", 12)
flight.delay("Sun", 12)

# time series data frame month by day of week
Monday = c(736,403,426,727,447,801,605,605,223,422,281,254)
Tuesday = c(394,286,397,345,330,559,323,492,212,397,190,499)
Wednesday = c(378,279,482,569,682,489,508,473,265,212,292,284)
Thursday = c(457,304,500,395,567,781,572,770,253,216,286,292)
Friday = c(541,354,613,400,529,1077,619,660,353,277,268,508)
Saturday = c(442,252,320,312,286,759,808,355,214,220,153,382)
Sunday = c(660,278,486,881,463,614,623,619,198,353,283,550)
ts.df = cbind(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)

days = c(736,394,378,457,541,442,660,
         403,286,279,304,354,252,278,
         426,397,482,500,613,320,486,
         727,345,569,395,400,312,881,
         447,330,682,567,529,286,463,
         801,559,489,781,1077,759,614,
         605,323,508,572,619,808,623,
         605,492,473,770,660,355,619,
         223,212,265,253,353,214,198,
         422,397,212,216,277,220,353,
         281,190,292,286,268,153,283,
         254,499,284,292,508,382,550)
days = data.frame(days)

# VARS model
# converting df to timeseries 
ts.df1 = as.ts(ts.df)
# determine optimal lag length
VARselect(ts.df1, lag.max = 5, type = "both")
# estimating VAR, optimal p = 1
fit.var = VAR(ts.df1, p = 1, type = "both")
x11();plot(fit.var)
# checking for stability of system
# since moduli of eigenvalues of companion matrix are > 1 the system is NOT stable
roots(fit.var)
# re-estimate VAR by significance (method = "ser")
fit.var.sig = restrict(fit.var, method = "ser", thresh = 2)
fit.var.sig$restrictions
B(fit.var.sig)
#summary(fit.var)
#plot(fit.var)
# forcasting, 2 months ahead, 95% confidence interval
fit.var.forecast = predict(fit.var, n.ahead = 6, ci = 0.95)
names(fit.var.forecast)
class(fit.var.forecast)
#plot(fit.var.forecast)
#fan(data = fit.var.forecast, data.type = "values", type = "interval", fan.col = colorRampPalette(colors = rev(brewer.pal(9, "Oranges"))))

fanchart(fit.var.forecast, plot.type = "single", colors = rev(brewer.pal(9, "YlOrRd")),
         ylab = "Minutes Delayed", xlab = "Months", main = c("Forecast for Mondays", "Forecast For Tuesdays", "Forecast for Wednesdays", "Forecast for Thursdays", "Forecast for Fridays", "Forecast for Saturdays", "Forecast for Sundays"))

```

