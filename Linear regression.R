setwd("D:\\DS\\predictive analysis")
data <- read.csv("DirectMarketing-2.csv")
library(dplyr)
library(ggplot2)
install.packages('car')
library(car)
dim(data)
str(data)
plot(data$Age)
plot(data$Age,data$AmountSpent,col="red")
plot(data$Salary)
#combine middle and old level together
data$Age1<-ifelse(data$Age!="Young","Middle-old",as.character(data$Age))
data$Age1<-as.factor(data$Age1)
summary(data$Age1)
summary(data$Age)
#bivariate analysis independent vs dependent
plot(data$Age1,data$AmountSpent)
plot(data$Gender,data$AmountSpent,col="red")
data$Children<-as.factor(data$Children)
plot(data$Children,data$AmountSpent,col="red")
data$Children1<-ifelse(data$Children==3|data$Children==2,"3-2",as.character(data$Children))
data$Children1<-as.factor(data$Children1)
summary(data$Children1)
summary(data$Children)
#missing value treatment
data$History1<-ifelse(is.na(data$History),"Missing",as.character(data$History))
data$History1<-as.factor((data$History1))
summary(data$History1)
summary(data$History)
names(data)
#remove age children history
# adds data1<-data[,c(1,7,8)]
data2<-data[,-c(1,7,8)]
mod1<-lm(AmountSpent~.,data=data2)#amtspent independent ~. for all dependent
summary(mod1)
#y=b0+b1*x1+b2*x2....
#y=-200+0.0185*salary+...
#estimates are beta values
#prob <0.05 then significant consider only those impactful
#when location changes from near to far the amtspent increases by 432.8 that means far customers spend more..categotrical value
#missing values will be the base for comparision like children0 or historyhigh..
#when salary inc by 1 amtspnt increases by 0.0185..continous value
#when move from children0 base value to children 3-2 amtspent is 413 less
#when move from history high to low amtspent decreases by 373

mod2<-lm(formula = AmountSpent ~ Gender + Location + Salary + Catalogs + Children1 + History1, data=data2)
summary(mod2)
#historymissing insignificant so remove it..it is part of history 1 so cannot remove missing directly..**so create dummy variable for history1**
#creating dummy vars
data2$med_d<-ifelse(data$History1=="Medium",1,0)
data2$high_d<-ifelse(data$History1=="High",1,0)
data2$low_d<-ifelse(data$History1=="Low",1,0)

mod3<-lm(formula=AmountSpent ~ Location + Salary + Catalogs + Children1 + low_d + high_d + med_d, data = data2)
summary(mod3)
#higher R square better the fit..it also depends on the domain we are working on

mod4<-lm(formula=AmountSpent ~ Location + Salary + Catalogs + Children1 + low_d + med_d, data = data2)
summary(mod4)

#**************Assumption checkS************************
#Normality
dim(data)
hist(mod4$residuals)
qqPlot(mod4$residuals)

#multicollinearity check
#correltion btwn independent variables
vif(mod4)  # checks for that..if >2 den multicollinearity here no multicollinearity so we can keep all indepen vars if greater than 2 remove them and build model 5
#1/(1-R^2) formula for vif

#constatnt variance check
plot(mod4$fitted.values,mod4$residuals)
#funnel shape so heteroskedasticity if a var varies 1 in range 1 10 and 2 in range  11 100 means it is itself acting as two vars so variation i not uniform
#throughout
#can be solved using data transformations removes heteroskedasticity
mod5<-lm(formula=log(AmountSpent) ~ Location + Salary + Catalogs + Children1 + low_d + med_d, data = data2)
summary(mod5)
#log transformation removes heteroskedasticity
#r square changes to 84 from 74
qqPlot(mod5$residuals)
plot(mod5$fitted.values, mod5$residuals) #*no funnel 
