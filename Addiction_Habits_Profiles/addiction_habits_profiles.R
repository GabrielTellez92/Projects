---
title: "Addiction Profiles"
---

### REMINDER: DO NOT CHANGE SECTION STARTS HERE ###
### Global Environment ###
```{r}
library(dplyr)
#Run this chunk to load and transform
#load set
df.addiction = read.csv("D:/School/SpringDA6813/Project1/DrugConsumption.csv")
#df.addiction = read.csv("C:/Users/Jonathan/Documents/Classes/DA Applications/Project1/DrugConsumption.csv")
#Transform/Factorize Demographic Info
df.addiction$Age = ifelse(df.addiction$Age == "-0.95197", "18-24", ifelse(df.addiction$Age == "-0.07854", "25-34",
                                                                         ifelse(df.addiction$Age == "0.49788","35-44","45+")))
                                                                                #ifelse(df.addiction$Age == "1.09449","45-54",
                                                                                      # ifelse(df.addiction$Age == "1.82213","55-64","65+")))))
df.addiction$Age = as.factor(df.addiction$Age)
df.addiction$Gender = ifelse(df.addiction$Gender >= "0", "Female", "Male")
df.addiction$Gender = as.factor(df.addiction$Gender)
df.addiction$Education = ifelse(df.addiction$Education == "-2.43591" | df.addiction$Education == "-1.7379" | df.addiction$Education == "-1.43719" | df.addiction$Education == "-1.22751", "No College",
                                                     ifelse(df.addiction$Education == "-0.61113","Some College",
                                                            ifelse(df.addiction$Education == "-0.05921","Associates",
                                                                   ifelse(df.addiction$Education == "0.45468","Bachelors","Professional"))))
df.addiction$Education = as.factor(df.addiction$Education)
df.addiction$Country = ifelse(df.addiction$Country == "-0.09765", "Australia",
                                ifelse(df.addiction$Country == "0.24923", "Canada",
                                       ifelse(df.addiction$Country == "-0.46841","New Zealand",
                                              ifelse(df.addiction$Country == "-0.28519","Other",
                                                     ifelse(df.addiction$Country == "0.21128","Ireland",
                                                            ifelse(df.addiction$Country == "0.96082","UK",
                                                                   "USA"))))))
df.addiction$Country = as.factor(df.addiction$Country)
df.addiction$Ethnicity = ifelse(df.addiction$Ethnicity == "-0.50212", "Asian",
                                ifelse(df.addiction$Ethnicity == "-1.10702", "Black",
                                       ifelse(df.addiction$Ethnicity == "1.90725","Mixed-Black/Asian",
                                              ifelse(df.addiction$Ethnicity == "0.126","Mixed-White/Asian",
                                                     ifelse(df.addiction$Ethnicity == "-0.22166","Mixed-White/Black",
                                                            ifelse(df.addiction$Ethnicity == "0.1144","Other",
                                                                   "White"))))))
df.addiction$Ethnicity = as.factor(df.addiction$Ethnicity)
# Removing Ethnicity
# keeping data set name the same
df.addiction = df.addiction[,-6]

#summary(df.addiction$Education)

# Three Legal Substances:
# Alcohol
# Caffeine
# Nicotine
# Four Drug classifications
# Depressants: benzadiazepine, cannabis, heroin, methadone
# Stimulants: amphetamines, cocaine, crack, ecstasy
# Hallucinogens: LSD, mushrooms, amyl nitrite, Ketamine
# New Psychoactive Substances: legal highs, VSA

#Reclassifying Legal Substances
df.addiction$Alcohol = ifelse(df.addiction$Alcohol == "CL5" | df.addiction$Alcohol == "CL6", "User","Nonuser")
df.addiction$Nicotine = ifelse(df.addiction$Nicotine == "CL4" | df.addiction$Nicotine == "CL5" | df.addiction$Nicotine == "CL6", "User","Nonuser")
df.addiction$Caff = ifelse(df.addiction$Caff == "CL6", "User","Nonuser")

# Creating new column for Depressants
# almost all have used some depressant (alcohol)
df.addiction$Depressants = ifelse(df.addiction$Benzos == "CL3" | df.addiction$Benzos == "CL4" | df.addiction$Benzos == "CL5" | df.addiction$Benzos == "CL6" | df.addiction$Heroin == "CL3" | df.addiction$Heroin == "CL4" | df.addiction$Heroin == "CL5" | df.addiction$Heroin == "CL6" | df.addiction$Cannabis == "CL3" | df.addiction$Cannabis == "CL4" | df.addiction$Cannabis == "CL5" | df.addiction$Cannabis == "CL6" | df.addiction$Methadone == "CL3" | df.addiction$Methadone == "CL4" | df.addiction$Methadone == "CL5" | df.addiction$Methadone == "CL6" | df.addiction$Amyl == "CL3" | df.addiction$Amyl == "CL4" | df.addiction$Amyl == "CL5" | df.addiction$Amyl == "CL6", "User", "Nonuser")

# Creating new column for Stimulants
df.addiction$Stimulants = ifelse(df.addiction$Amphet == "CL3" | df.addiction$Amphet == "CL4" | df.addiction$Amphet == "CL5" | df.addiction$Amphet == "CL6" | df.addiction$Coke == "CL3" | df.addiction$Coke == "CL4" | df.addiction$Coke == "CL5" | df.addiction$Coke == "CL6" | df.addiction$Crack == "CL3" | df.addiction$Crack == "CL4" | df.addiction$Crack == "CL5" | df.addiction$Crack == "CL6" | df.addiction$Ecstasy == "CL3" | df.addiction$Ecstasy == "CL4" | df.addiction$Ecstasy == "CL5" | df.addiction$Ecstasy == "CL6" , "User", "Nonuser")

# Creating new column for Hallucinogens
df.addiction$Hallucinogens = ifelse(df.addiction$LSD == "CL3" | df.addiction$LSD == "CL4" | df.addiction$LSD == "CL5" | df.addiction$LSD == "CL6" | df.addiction$Mushrooms == "CL3" | df.addiction$Mushrooms == "CL4" | df.addiction$Mushrooms == "CL5" | df.addiction$LSD == "CL6" | df.addiction$Ketamine == "CL3" | df.addiction$Ketamine == "CL4" | df.addiction$Ketamine == "CL5" | df.addiction$Ketamine == "CL6", "User", "Nonuser")

# Creating new column for NPS (Chocolate)
df.addiction$NPS = ifelse(df.addiction$Legalh == "CL3" | df.addiction$Legalh == "CL4" | df.addiction$Legalh == "CL5" | df.addiction$Legalh == "CL6" | df.addiction$VSA == "CL3" |df.addiction$VSA == "CL4" | df.addiction$VSA =="CL5" | df.addiction$VSA == "CL6", "User", "Nonuser")

df.addiction$Semer = ifelse(df.addiction$Semer == "CL0", "Nonuser", "User")
#took chocolate out of NPS
# df.addiction$ Choc == "CL4" | df.addiction$Choc == "CL5" | df.addiction$Choc == "CL6" | 

# convert to factors

df.addiction$Alcohol = as.factor(df.addiction$Alcohol)
df.addiction$Nicotine = as.factor(df.addiction$Nicotine)
df.addiction$Caff = as.factor(df.addiction$Caff)
df.addiction$Depressants = as.factor(df.addiction$Depressants)
df.addiction$Stimulants = as.factor(df.addiction$Stimulants)
df.addiction$Hallucinogens = as.factor(df.addiction$Hallucinogens)
df.addiction$NPS = as.factor(df.addiction$NPS)
df.addiction$Semer = as.factor(df.addiction$Semer)

# for User/Non-user
# USER = CL3, CL4, CL5, CL6
# NONUSER = CL0, CL1, CL2, CL3

#This reduces the df down to size for testing, takes out substances other than nicotine, alcohol, and caffeine, keeps the drug categories
df=df.addiction
df = df %>% filter(df$Semer == "Nonuser")
df= df[,-(30:31)]
df = df[,-(18:28)]
df = df[,-(14:16)]
df = df[,-5]
df = df[,-1]

#######Training and Test Set
set.seed(123)
t= sample(1:nrow(df),.666*nrow(df),rep=F)
train = df[t,]
test= df[-t,]

summary(df)
```
### DO NOT CHANGE SECTION ENDS HERE ###
###################################################
###################################################
# PRELIMINARY ANALYSIS
###################################################
```{r}
#Depressants
library(ggplot2)
#clone dataset twice
dataset=df
dataset1 = df
#Isolate Users and nonusers
dataset = dataset %>% filter(Depressants=="User")
dataset1 = dataset1 %>% filter(Depressants=="Nonuser")
#Create x vector
stimx = c("User Nscore", "Nonuser Nscore", "User Escore","Nonuser Escore","User Oscore","Nonuser Oscore","User Ascore", "Nonuser Ascore", "User Cscore", "Nonuser Cscore", "User Imp","Nonuser Imp","User SS","Nonuser SS")
#Create y vector
stimy = c(mean(dataset$Nscore), mean(dataset1$Nscore),mean(dataset$Escore),mean(dataset1$Escore),mean(dataset$Oscore),mean(dataset1$Oscore),mean(dataset$Ascore),mean(dataset1$Ascore),
          mean(dataset$Cscore),mean(dataset1$Cscore),mean(dataset$Impulsive),mean(dataset1$Impulsive),mean(dataset$SS),mean(dataset1$SS))
#Create dataframe
stim=data.frame(stimx, stimy)
#Plot dataframe
ggplot(data=stim, aes(stimx,stimy, fill=c("blue","blue","red","red","yellow","yellow","green","green","purple","purple","black","black","gray","gray")))+
  theme_bw()+
  geom_bar(stat="identity")+
  ggtitle("Depressants")+
  labs (x = ("Personality Inventory Trait"), y = ("Standardized Means"))+
  theme(plot.title = element_text(hjust=.45))+
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_x_discrete(limits=c("User Nscore", "Nonuser Nscore", "User Escore","Nonuser Escore","User Oscore","Nonuser Oscore","User Ascore", "Nonuser Ascore", "User Cscore", "Nonuser Cscore", "User Imp","Nonuser Imp","User SS","Nonuser SS"))
stim
```


```{r}
#Hallucinogens
library(ggplot2)
#clone dataset twice
dataset=df
dataset1 = df
#Isolate Users and nonusers
dataset = dataset %>% filter(Hallucinogens=="User")
dataset1 = dataset1 %>% filter(Hallucinogens=="Nonuser")
#Create x vector
stimx = c("User Nscore", "Nonuser Nscore", "User Escore","Nonuser Escore","User Oscore","Nonuser Oscore","User Ascore", "Nonuser Ascore", "User Cscore", "Nonuser Cscore", "User Imp","Nonuser Imp","User SS","Nonuser SS")
#Create y vector
stimy = c(mean(dataset$Nscore), mean(dataset1$Nscore),mean(dataset$Escore),mean(dataset1$Escore),mean(dataset$Oscore),mean(dataset1$Oscore),mean(dataset$Ascore),mean(dataset1$Ascore),
          mean(dataset$Cscore),mean(dataset1$Cscore),mean(dataset$Impulsive),mean(dataset1$Impulsive),mean(dataset$SS),mean(dataset1$SS))
#Create dataframe
stim=data.frame(stimx, stimy)
#Plot dataframe
ggplot(data=stim, aes(stimx,stimy, fill=c("blue","blue","red","red","yellow","yellow","green","green","purple","purple","black","black","gray","gray")))+
  theme_bw()+
  geom_bar(stat="identity")+
  ggtitle("Hallucinogens")+
  labs (x = ("Personality Inventory Trait"), y = ("Standardized Means"))+
  theme(plot.title = element_text(hjust=.45))+
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_x_discrete(limits=c("User Nscore", "Nonuser Nscore", "User Escore","Nonuser Escore","User Oscore","Nonuser Oscore","User Ascore", "Nonuser Ascore", "User Cscore", "Nonuser Cscore", "User Imp","Nonuser Imp","User SS","Nonuser SS"))
```


```{r}
#Stimulants
library(ggplot2)
#clone dataset twice
dataset=df
dataset1 = df
#Isolate Users and nonusers
dataset = dataset %>% filter(Stimulants=="User")
dataset1 = dataset1 %>% filter(Stimulants=="Nonuser")
#Create x vector
stimx = c("User Nscore", "Nonuser Nscore", "User Escore","Nonuser Escore","User Oscore","Nonuser Oscore","User Ascore", "Nonuser Ascore", "User Cscore", "Nonuser Cscore", "User Imp","Nonuser Imp","User SS","Nonuser SS")
#Create y vector
stimy = c(mean(dataset$Nscore), mean(dataset1$Nscore),mean(dataset$Escore),mean(dataset1$Escore),mean(dataset$Oscore),mean(dataset1$Oscore),mean(dataset$Ascore),mean(dataset1$Ascore),
          mean(dataset$Cscore),mean(dataset1$Cscore),mean(dataset$Impulsive),mean(dataset1$Impulsive),mean(dataset$SS),mean(dataset1$SS))
#Create dataframe
stim=data.frame(stimx, stimy)
#Plot dataframe
ggplot(data=stim, aes(stimx,stimy, fill=c("blue","blue","red","red","yellow","yellow","green","green","purple","purple","black","black","gray","gray")))+
  theme_bw()+
  geom_bar(stat="identity")+
  ggtitle("Stimulants")+
  labs (x = ("Personality Inventory Trait"), y = ("Standardized Means"))+
  theme(plot.title = element_text(hjust=.45))+
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_x_discrete(limits=c("User Nscore", "Nonuser Nscore", "User Escore","Nonuser Escore","User Oscore","Nonuser Oscore","User Ascore", "Nonuser Ascore", "User Cscore", "Nonuser Cscore", "User Imp","Nonuser Imp","User SS","Nonuser SS"))
stim
```

# HAVE NOT RUN #
```{r}
library(ggplot2)
ggplot(data=df, aes(df$Age, ..count..))+ 
  theme_bw()+
  theme(legend.position="none")+
  geom_bar(aes(fill = Age))+
  ggtitle("Age")+
  labs (x = "Age (Years)", y = "Count")+
  theme(plot.title = element_text(hjust=.45))+
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))

ggplot(data=df, aes(df$Gender, ..count..))+ 
  theme_bw()+
  theme(legend.position="none")+
  geom_bar(aes(fill = Gender))+
  labs (x = "Gender", y = "Count")+
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))

ggplot(data=df, aes(df$Education, ..count..))+ 
  theme_bw()+
  theme(legend.position="none")+
  geom_bar(aes(fill = Education))+
  ggtitle("Education")+
  labs (x = "College Degree", y = "Count")+
  theme(plot.title = element_text(hjust=.45))+
  scale_x_discrete(limits=c("No College", "Some College", "Associates","Bachelors","Professional"))+
  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
```

#Visualizations
```{r}
library(ggplot2)
ggplot(data=df, aes_string(x=df$Stimulants, y=df$Cscore, color="Hallucinogens"))+
      geom_point()+
      geom_jitter()+
      theme_classic()


ggplot(data=df, aes_string(x=df$Depressants, y=df$SS, color="Stimulants"))+
      geom_point()+
      geom_jitter()+
      theme_classic()

ggplot(data=df, aes_string(x=df$Hallucinogens, y=df$Oscore, color="Gender" ))+
      geom_point()+
      geom_jitter()+
      theme_classic()

```
###################################################
# MAIN ANALYSIS # MAIN ANALYSIS # MAIN ANALYSIS 
###################################################
###################################################
# DEPRESSANTS
###################################################
# LOGISTIC REGRESSION - DEPRESSANTS
###################################################
```{r}
Depressants.y = test$Depressants

# Logistic Regression for DEPRESSANTS
# Benzos, Cannabis, Heroin, Methadon (heroin and methadon are opioids)
depressants.logistic = glm(Depressants ~ ., data = train, family = "binomial")
summary(depressants.logistic)

# predicting probability of user/nonuser
#depressants.logistic = glm(Depressants ~ Age+Gender+Escore+Oscore+SS+Nicotine+Stimulants+Hallucinogens+NPS, data = train, family = "binomial")
depressants.logistic.prob = predict(depressants.logistic, newdata = test, type = "response")
# converting probabilities
lab = contrasts(df.addiction$Depressants); length(Depressants.y)
tn = rownames(lab)
# prediction probability
depressants.logistic.pred = ifelse(depressants.logistic.prob<.5,tn[1], tn[2])
# confusion matrix
depressants.confusion = table(depressants.logistic.pred, Depressants.y)
depressants.confusion
# mean misclassification rate
mean(depressants.logistic.pred != Depressants.y)
# error rate 0.1355662; 0.1291866
# Stimulants, Hallucinogens, NPS are significant
# E and O-scores significant, SS is significant
# Nicotine and Alcohol users significant
```
###################################################
# RANDOM FOREST - DEPRESSANTS
###################################################
```{r}
library(randomForest)
set.seed(123)
dep.train.y = train$Depressants
depressants.y= test$Depressants
m=round(sqrt(dim(train)[2]-1))
library(doParallel)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
for (i in c(m-1,m,m+1)){
  dep.rf=randomForest(Depressants~., data=train, mtry = i, ntree=500)
  print(c(i,500,mean(dep.rf$predicted != dep.train.y)))
  dep.rf=randomForest(Depressants~., data=train, mtry = i, ntree=1000)
  print(c(i,1000,mean(dep.rf$predicted != dep.train.y)))
  dep.rf=randomForest(Depressants~., data=train, mtry = i, ntree=2000)
  print(c(i,2000,mean(dep.rf$predicted != dep.train.y)))
}
registerDoSEQ()
dep.rand.forest = randomForest(Depressants ~ ., data=train, mtry=4, ntree=2000)

best=as.data.frame(round(importance(dep.rand.forest), 2))
top=best[order(-best$MeanDecreaseGini), , drop = FALSE]
top20=top[1:20, , drop=FALSE]
top20

#In order of importance Gini index
#Stimulants
#Hallucinogens
#Oscore
#NPS
#Nscore
#SS
#Cscore
#Escore
#Nicotine
#Ascore

#dep.rand.forest = randomForest(Depressants ~ ., data=train, mtry=3, ntree=500)

test.rf = predict(dep.rand.forest, test)
table(test.rf, depressants.y)
mean(test.rf != depressants.y)
# error rate 0.1403509

```
###################################################
# SVM - DEPRESSANTS
###################################################
```{r}
#tune
library(doParallel)
library(e1071)
set.seed(123)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
tuned = tune.svm(Depressants~., data = train, gamma = c(0.1, 1, 10, 100, 1000), cost = c(.001, .01, .1, 1, 10), tunecontrol=tune.control(cross=10))
summary(tuned)
registerDoSEQ()
```

```{r}
#Model Fit
depressants.y= test$Depressants
library(rminer)
library(e1071) 
mysvm = svm(Depressants ~ ., data=train, gamma= .1, cost=  1, cross=10)
svmpredict = predict(mysvm, test, type="response")
table(svmpredict, depressants.y)
mean(svmpredict != depressants.y)
# error rate 0.122807
#11 - 13% error rate
model = fit(Depressants ~ ., data=train, model="svm")
VariableImportance=Importance(model,train)
VariableImportance
L=list(runs=1,sen=t(VariableImportance$imp),sresponses=VariableImportance$sresponses)
mgraph(L,graph="IMP",leg=names(train),col="gray",Grid=10)
```

#parallel SVM for Depressants
```{r}
depressants.y= test$Depressants
library(parallelSVM)
set.seed(123)
model = Depressants ~ .
mySVM = parallelSVM(model, data=train, numberCores=2, gamma= .1  , cost=  1 )
svmpredict=predict(mySVM,test,type="response")
table(svmpredict,depressants.y)
mean(svmpredict != depressants.y)
```
###################################################
# STEP AIC - DEPRESSANTS (To examine interactions)
###################################################
```{r}
library(MASS)
library(doParallel) 
set.seed(123)
dep.test.y= test$Depressants
Depressants.logit.model = glm(Depressants ~., train, family="binomial")
summary(Depressants.logit.model)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
m2 = stepAIC(Depressants.logit.model, ~.^2)
m2$anova
#Significant effects
#Stimulants ~ Age + Nscore + Escore + Oscore + Ascore + Cscore + 
#    Impulsive + SS + Depressants + Hallucinogens + Age:Ascore + 
#    Age:Oscore + Cscore:SS + Age:Impulsive + Cscore:Hallucinogens + 
#    Nscore:Hallucinogens + Escore:Oscore + Age:Hallucinogens + 
#    Ascore:Cscore
AIC.fit = glm(Depressants ~ Age + Gender + Education + Nscore + Escore + Oscore + 
    Ascore + Impulsive + SS + Alcohol + Nicotine + Stimulants + 
    Hallucinogens + NPS + Impulsive:SS + Ascore:NPS + Age:Gender + 
    Age:Stimulants + Gender:Escore + Hallucinogens:NPS + Alcohol:Hallucinogens + 
    Nscore:Ascore + Education:NPS + Oscore:NPS + SS:Hallucinogens + 
    Nscore:Hallucinogens + Nscore:Nicotine + Nscore:Impulsive + 
    Nscore:Oscore + Oscore:SS + Age:Nscore + Escore:SS + Alcohol:Stimulants + 
    Escore:Ascore + Escore:Alcohol + Impulsive:Stimulants + Impulsive:NPS + 
    Gender:Stimulants + Oscore:Nicotine, train, family = "binomial")
Depressants.AIC.test = predict(AIC.fit, test, type="response")
Depressants.AIC.test = ifelse(Depressants.AIC.test<.5,"Nonuser", "User")
# confusion matrix
Depressants.AIC.confusion = table(Depressants.AIC.test, dep.test.y)
Depressants.AIC.confusion
# mean misclassification rate
mean(Depressants.AIC.test != dep.test.y)
# error rate 0.15
registerDoSEQ()
```

```{r}
#Depressants
#Impulsive:Sensation seeking, Ascore:NPS, Age:Gender, Age:Stimulants, and Gender:Escore
library(ggplot2)
ggplot(data=df, aes_string(x=df$Impulsive, y=df$SS, color="Depressants"))+
      theme_classic()+
      labs (x = ("Impulsivity"), y = ("Sensation Seeking"))+
      geom_point()+
      geom_jitter()

ggplot(data=df, aes_string(x=df$Nscore, y=df$Ascore, color="Depressants"))+
      theme_classic()+
      labs (x = ("Neuroticism"), y = ("Agreeableness"))+
      geom_point()+
      geom_jitter()


ggplot(data=df, aes_string(x=df$Nscore, y=df$Impulsive, color="Depressants"))+
      geom_point()+
      geom_jitter()+
      labs (x = ("Neuroticism"), y = ("Impulsivity"))+
      theme_classic()
```


###################################################
###################################################
# HALLUCINOGENS
###################################################
# LOGISTIC REGRESSION - HALLUCINOGENS
###################################################
```{r}
Hallucinogens.y = test$Hallucinogens

# Logistic Regression of HALLUCINOGENS
# LSD, Mushrooms, Amyl Nitrite, Ketamine
hallucinogens.logistic = glm(Hallucinogens ~ ., data = train, family = "binomial")
summary(hallucinogens.logistic)
hallucinogens.logistic = glm(Hallucinogens ~ Age+Gender+Oscore+Caff+Depressants+Stimulants+NPS, data = train, family = "binomial")
# predicting probability of user/nonuser
hallucinogens.logistic.prob = predict(hallucinogens.logistic, newdata = test, type = "response")
# converting probabilities
lab = contrasts(df.addiction$Hallucinogens); length(Hallucinogens.y)
tn = rownames(lab)
# prediction probability
hallucinogens.logistic.pred = ifelse(hallucinogens.logistic.prob<.5,tn[1], tn[2])
# confusion matrix
hallucinogens.confusion = table(hallucinogens.logistic.pred, Hallucinogens.y)
hallucinogens.confusion
# mean misclassification rate
mean(hallucinogens.logistic.pred != Hallucinogens.y)
# error rate 0.1499203
# Depressant, Stimulant, and NPS users significant
# Caffeine user significant; O-scores significant
# all Age groups significant
```
###################################################
# RANDOM FOREST - HALLUCINOGENS
###################################################
```{r}
library(doParallel)
library(randomForest)
set.seed(123)
hal.train.y = train$Hallucinogens
hal.test.y = test$Hallucinogens
m=round(sqrt(dim(train)[2]-1))
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
for (i in c(m-1,m,m+1)){
  hal.rf=randomForest(Hallucinogens~., data=train, mtry = i, ntree=500)
  print(c(i,500,mean(hal.rf$predicted != hal.train.y)))
  hal.rf=randomForest(Hallucinogens~., data=train, mtry = i, ntree=1000)
  print(c(i,1000,mean(hal.rf$predicted != hal.train.y)))
  hal.rf=randomForest(Hallucinogens~., data=train, mtry = i, ntree=2000)
  print(c(i,2000,mean(hal.rf$predicted != hal.train.y)))
}
registerDoSEQ()
#Lowest OOB error rate at mtry = 3, ntree = 500
hal.rand.forest = randomForest(Hallucinogens ~ ., data=train, mtry=3, ntree=500)

best=as.data.frame(round(importance(hal.rand.forest), 2))
top=best[order(-best$MeanDecreaseGini), , drop = FALSE]
top20=top[1:20, , drop=FALSE]
top20

#In order of importance Gini index
#Stimulants
#Age
#Depressants
#Oscore
#SS
#Cscore
#Nscore
#Ascore
#Escore
#Education

test.rf = predict(hal.rand.forest, test)
table(test.rf, hal.test.y)
mean(test.rf != hal.test.y)
#error rate 0.1610845
```
###################################################
# SVM - HALLUCINOGENS
###################################################
```{r}
#tune
library(doParallel)
library(e1071)
set.seed(123)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
tuned = tune.svm(Hallucinogens~., data = train, gamma = c(0.1, 1, 10, 100, 1000), cost = c(.001, .01, .1, 1, 10), tunecontrol=tune.control(cross=10))
summary(tuned)
registerDoSEQ()
```

```{r}
hallucinogens.y= test$Hallucinogens
library(e1071)
library(rminer)
mysvm = svm(Hallucinogens ~ ., data=train, gamma= .1, cost=  1, cross=10)
svmpredict = predict(mysvm, test, type="response")
table(svmpredict, hallucinogens.y)
mean(svmpredict != hallucinogens.y)
# error rate 0.1451356
model = fit(Hallucinogens ~ ., data=train, model="svm")
VariableImportance=Importance(model,train)
L=list(runs=1,sen=t(VariableImportance$imp),sresponses=VariableImportance$sresponses)
mgraph(L,graph="IMP",leg=names(train),col="gray",Grid=10)
```

# parallel SVM for Hallucinogens
```{r}
hal.test.y= test$Hallucinogens
set.seed(123)
library(parallelSVM)
model = Hallucinogens ~ .
mySVM = parallelSVM(model, data=train, numberCores=2, gamma= .1  , cost=  1 )
svmpredict=predict(mySVM,test,type="response")
table(svmpredict,hal.test.y)
mean(svmpredict != hal.test.y)
# error rate ~ 0.17 - 0.22
```
###################################################
#STEP AIC - HALLUCINOGENS (To examine interactions)
###################################################
```{r}
library(MASS)
library(doParallel)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
hal.test.y= test$Hallucinogens
Hallucinogens.logit.model = glm(Hallucinogens ~., train, family="binomial")
summary(Hallucinogens.logit.model)
m2 = stepAIC(Hallucinogens.logit.model, ~.^2)
registerDoSEQ()
m2$anova
#Significant effects
#Stimulants ~ Age + Nscore + Escore + Oscore + Ascore + Cscore + 
#    Impulsive + SS + Depressants + Hallucinogens + Age:Ascore + 
#    Age:Oscore + Cscore:SS + Age:Impulsive + Cscore:Hallucinogens + 
#    Nscore:Hallucinogens + Escore:Oscore + Age:Hallucinogens + 
#    Ascore:Cscore
AIC.fit = glm(Hallucinogens ~ Age + Gender + Nscore + Escore + Oscore + Ascore + 
    Cscore + Impulsive + SS + Alcohol + Caff + Nicotine + Depressants + 
    Stimulants + NPS + Cscore:Stimulants + Escore:Alcohol + Alcohol:Depressants + 
    Depressants:NPS + SS:NPS + Impulsive:Nicotine + Oscore:Caff + 
    Alcohol:Caff + Escore:Depressants + SS:Depressants + SS:Stimulants + 
    Oscore:Stimulants + Ascore:Impulsive + Cscore:SS + Cscore:Depressants + 
    Oscore:Cscore + Gender:Caff + Gender:Nicotine + Nscore:Impulsive + 
    Nscore:Stimulants, train, family = "binomial")
Hallucinogens.AIC.test = predict(AIC.fit, test, type="response")
Hallucinogens.AIC.test = ifelse(Hallucinogens.AIC.test<.5,"Nonuser", "User")
# confusion matrix
Hallucinogens.AIC.confusion = table(Hallucinogens.AIC.test, hal.test.y)
Hallucinogens.AIC.confusion
# mean misclassification rate
mean(Hallucinogens.AIC.test != hal.test.y)
```

```{r}
#Cscore:Stimulants, Escore:Alcohol, and Alcohol:Depressants
library(ggplot2)
ggplot(data=df, aes_string(x=df$SS, y=df$Cscore, color="Hallucinogens"))+
      theme_classic()+
      labs (x = ("Sensation Seeking"), y = ("Conscientiousness"))+
      geom_point()+
      geom_jitter()

ggplot(data=df, aes_string(x=df$Oscore, y=df$Cscore, color="Hallucinogens"))+
      theme_classic()+
      labs (x = ("Openness to Experience"), y = ("Conscientiousness"))+
      geom_point()+
      geom_jitter()


ggplot(data=df, aes_string(x=df$Nscore, y=df$Impulsive, color="Hallucinogens"))+
      geom_point()+
      geom_jitter()+
      labs (x = ("Neuroticism"), y = ("Impulsivity"))+
      theme_classic()
```

###################################################
###################################################
# STIMULANTS
###################################################
# LOGISTIC REGRESSION - STIMULANTS
###################################################
```{r}
Stimulants.y = test$Stimulants

# Logistic Regression of STIMULANTS
# Amphetamines, Coke, Crack, Ecstasy
stimulants.logistic = glm(Stimulants ~ ., data = train, family = "binomial")
summary(stimulants.logistic)
stimulants.logistic = glm(Stimulants ~ Age+Escore+Oscore+Nicotine+Depressants+Hallucinogens+NPS, data = train, family = "binomial")
# predicting probability of user/nonuser
stimulants.logistic.prob = predict(stimulants.logistic, newdata = test, type = "response")
# converting probabilities
lab = contrasts(df.addiction$Stimulants); length(Stimulants.y)
tn = rownames(lab)
# prediction probability
stimulants.logistic.pred = ifelse(stimulants.logistic.prob<.5,tn[1], tn[2])
# confusion matrix
stimulants.confusion = table(stimulants.logistic.pred, Stimulants.y)
stimulants.confusion
# mean misclassification rate
mean(stimulants.logistic.pred != Stimulants.y)
# error rate 0.1483254
# Depressant, Hallucinogen, and NPS user significant
# Nicotine user significant; C-score is significant
```

###################################################
# RANDOM FOREST - STIMULANTS # WILL USE #
###################################################
```{r}
set.seed(1)
library(randomForest)
stim.y = test$Stimulants
stim.train.y = train$Stimulants
m=round(sqrt(dim(train)[2]-1))
library(doParallel)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
for (i in c(m-1,m,m+1)){
  stim.rf=randomForest(Stimulants~., data=train, mtry = i, ntree=500)
  print(c(i,500,mean(stim.rf$predicted != stim.train.y)))
  stim.rf=randomForest(Stimulants~., data=train, mtry = i, ntree=1000)
  print(c(i,1000,mean(stim.rf$predicted != stim.train.y)))
  stim.rf=randomForest(Stimulants~., data=train, mtry = i, ntree=2000)
  print(c(i,2000,mean(stim.rf$predicted != stim.train.y)))
}
registerDoSEQ()
#Lowest OOB error rate at mtry = 3, ntree = 500
stim.rand.forest = randomForest(Stimulants ~ ., data=train, mtry=4, ntree=1000)

best=as.data.frame(round(importance(stim.rand.forest), 2))
top=best[order(-best$MeanDecreaseGini), , drop = FALSE]
top20=top[1:20, , drop=FALSE]
top20

#In order of importance Gini index
#Depressants
#Hallucinogens
#NPS
#Cscore
#Ascore
#Nscore
#Oscore
#Escore
#SS
#Age


test.rf = predict(stim.rand.forest, test)
table(test.rf, stim.y)
mean(test.rf != stim.y)
# error rate 0.1467305

```
###################################################
# SUPPORT VECTOR MACHINE (SVM) - STIMULANTS
###################################################
```{r}
library(doParallel) 
library(e1071)
#set.seed(1)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
tuned = tune.svm(Stimulants~., data = train, gamma = c(0.1, 1, 10, 100, 1000), cost = c(.001, .01, .1, 1, 10), tunecontrol=tune.control(cross=10))
summary(tuned)
registerDoSEQ()
#the best parameters are g = .1, c = 1
```

```{r}
library(e1071)
library(rminer)
stim.y = test$Stimulants
mysvm = svm(Stimulants ~ ., data=train, gamma= .1, cost=  1, cross=10)
svmpredict = predict(mysvm, test, type="response")
table(svmpredict, stim.y)
mean(svmpredict != stim.y)
#16 - 18% error rate
model = fit(Stimulants ~ ., data=train, model="svm")
VariableImportance=Importance(model,train)
VariableImportance
L=list(runs=1,sen=t(VariableImportance$imp),sresponses=VariableImportance$sresponses)
mgraph(L,graph="IMP",leg=names(train),col="gray",Grid=10)
```

#parallel SVM for Stimulants
```{r}
set.seed(123)
library(parallelSVM)
stim.y = test$Stimulants
model = Stimulants ~ .
parallelsvm=parallelSVM(model,data=train, numberCores=2, gamma=.1, cost=1, cross=10)
parallel.svmpredict = predict(parallelsvm, test, type="response")
table(parallel.svmpredict, stim.y)
mean(parallel.svmpredict != stim.y)
```
###################################################
# STEP AIC - STIMULANTS
###################################################
```{r}
library(MASS)
library(doParallel) 
set.seed(1)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
Stimulants.y = test$Stimulants
Stimulants.logit.model = glm(Stimulants ~ Age+Gender+Education+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS+Depressants+Hallucinogens, train, family="binomial")
summary(Stimulants.logit.model)
m2 = stepAIC(Stimulants.logit.model, ~.^2)
registerDoSEQ()
m2$anova
#Significant effects
AIC.fit = glm(Stimulants ~ Age + Gender + Escore + Oscore + Ascore + Cscore + 
    Impulsive + SS + Depressants + Hallucinogens + Cscore:Hallucinogens + 
    Age:Depressants + Ascore:Depressants + Age:Escore + Age:Ascore + 
    Ascore:Impulsive + Impulsive:Depressants + Cscore:SS + Gender:Oscore + 
    Escore:Ascore + Depressants:Hallucinogens, train, family = "binomial")
Stimulants.AIC.test = predict(AIC.fit, test, type="response")
#lab = contrasts(df.addiction$Stimulants);
#lab
#Stimulants.AIC.test
#length(Stimulants.y)
# prediction probability
Stimulants.AIC.test = ifelse(Stimulants.AIC.test<.5,"Nonuser", "User")
# confusion matrix
Stimulants.AIC.confusion = table(Stimulants.AIC.test, Stimulants.y)
Stimulants.AIC.confusion
# mean misclassification rate
mean(Stimulants.AIC.test != Stimulants.y)
# error rate 0.1674641

```


```{r}
library(ggplot2)
#Cscore:Hallucinogens, Age:Depressants, and Ascore:Depressants
ggplot(data=df, aes_string(x=df$Ascore, y=df$Impulsive, color="Stimulants"))+
      theme_classic()+
      labs (x = ("Agreeableness"), y = ("Impulsivity"))+
      geom_point()+
      geom_jitter()
ggplot(data=df, aes_string(x=df$Cscore, y=df$SS, color="Stimulants"))+
      theme_classic()+
      labs (x = ("Conscientiousness"), y = ("Sensation Seeking"))+
      geom_point()+
      geom_jitter()


ggplot(data=df, aes_string(x=df$Escore, y=df$Ascore, color="Stimulants"))+
      geom_point()+
      geom_jitter()+
      labs (x = ("Extraversion"), y = ("Agreeableness"))+
      theme_classic()

```


#Appendix Plots - Substance Abuse Affects Us All
```{r}
library(ggplot2)
ggplot(data=df, aes_string(x=df$Age, y=df$Ascore, color="Depressants"))+
      theme_classic()+
      ggtitle("Depressants")+
      labs (x = ("Age"), y = ("Agreeableness"))+
      geom_point()+
      geom_jitter()

ggplot(data=df, aes_string(x=df$Gender, y=df$Oscore, color="Stimulants"))+
      theme_classic()+
      ggtitle("Stimulants")+
      labs (x = ("Gender"), y = ("Openness to Experience"))+
      geom_point()+
      geom_jitter()

ggplot(data=df, aes_string(x=df$Education, y=df$Escore, color="Hallucinogens"))+
      theme_classic()+
      ggtitle("Hallucinogens")+
      labs (x = ("Education"), y = ("Extraversion"))+
      geom_point()+
      geom_jitter()+
      scale_x_discrete(limits=c("No College", "Some College", "Associates","Bachelors","Professional"))

```





```{r}
#Write dataframe to CSV
write.csv(df, file = "Irregular_Expressions.csv", row.names = FALSE)
```







