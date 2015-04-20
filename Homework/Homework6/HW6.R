setwd("I:/OSU/2015 Spring/STAT 5302/Homeworks/HW6")
load("I:/OSU/2015 Spring/STAT 5302/Homeworks/HW6/.RData")

## question 10
list.model <- data.frame(model=c("None","A","B","C","AB","AC","BC","ABC"),
                         RSS=c(8100,6240,5980,6760,5500,5250,5750,5160),
                         df=c(27,26,26,26,25,25,25,24))
# add a variable "p" (number of regression parameters) to the data frame
# p = n - (df of RSS)
list.model$p <- 28 - list.model$df
list.model

## a
sigma.sq = list.model$RSS/list.model$df
sigma.sq

## b
adj.r.sq = 1 - sigma.sq/(8100/27)
adj.r.sq

## c
cp = list.model$p + list.model$df*(sigma.sq-215)/215
cp

## d
BIC = 28 * log(list.model$RSS/28)+log(28)*(list.model$p+1)
BIC


## question 11
f1 = ((8100-5980)/1)/(5980/26)
f1
f2 = ((5980-5500)/1)/(5500/25)
f2
fa = ((8100-6240)/1)/(6240/26)
fa
fac = ((6240-5250)/1)/(5250/25)
fac
fabc = ((5250-5160)/1)/(5160/24)
fabc


## question 17
## a
# Model selection
q17 = read.csv("ex1217.csv")
Mortality.lm = lm(Mortality ~ . - CITY, data = q17)
summary(Mortality.lm)

library(leaps)
M = regsubsets(Mortality ~ . - CITY, data = q17, method = "exhaustive")
M.sum = summary(M)

plot(M.sum$cp, xlab = "No. of Para", ylab = "CP", pch = 19)
plot(M.sum$bic, xlab = "No. of Para", ylab = "BIC", pch = 19)
M.sum

# reduced model and full model
Mortality.reduced.lm = lm(Mortality ~ Precip + JanTemp + JulyTemp + Educ + NonWhite + SO2,
                          data = q17)
summary(Mortality.reduced.lm)

Mortality.full.lm = lm(Mortality ~ Precip + JanTemp + JulyTemp + Educ + NonWhite + HC + NOX + SO2 ,
                          data = q17)
summary(Mortality.full.lm)

anova(Mortality.reduced.lm, Mortality.full.lm)

## b
step(Mortality.lm, direction = "backward", test = "F")
