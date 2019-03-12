#Loading required packages
rm(list=ls())
library("mice")
library("stargazer")
library(data.table)
library(RCurl)

#Loadingthe data set from github
wages <-  read.csv("https://raw.githubusercontent.com/tyleransom/DScourseS19/master/ModelingOptimization/wages.csv")


#Dropping observations where either hgc or tenure are missing.
wages <-wages[!is.na(wages$hgc),]
wages <-wages[!is.na(wages$tenure),]

#producing a summary table of this data frame
stargazer(wages)

#Finding the rate at which log wages are missing
sum(is.na(wages$logwage))/length(wages$logwage)

#Estimating the regression using only complete cases
wages$tenure2 <-(wages$tenure)^2
wages2 <- wages[!is.na(wages$logwage),]
lm1 <-lm(logwage~hgc+college+tenure+tenure2+age+married, wages2)


#Perform mean imputation to fill in missing log wages
mean <-sum(wages2$logwage)/length(wages2$logwage)
wagemean <- wages
wagemean$logwage[is.na(wagemean$logwage)]<-mean
lm2 <-lm(logwage ~ hgc + tenure+ college  + tenure2 + age + married ,wagemean)


#Imputing missing log wages as their predicted values from the complete cases
wagepr <- wages
wagepr$logwage[is.na(wagepr$logwage)] <- predict(lm1, wagepr[is.na(wagepr$logwage),])
prreg <- lm(logwage ~ hgc + tenure + college + tenure2 + age + married ,wagepr)

#Performing amultiple imputation regressionmodel using mice

wages.imp = mice(wages, seed = 12345)

fit = with(wages.imp, lm(wages$logwage ~ wages$hgc + wages$tenure + wages$college + wages$tenure2 + wages$age + wages$married))
round(summary(pool(fit)),2)

#Creating the regression table
stargazer(lm1, lm2, prreg, column.labels=c("Complete","Mean", "Prediction"))






