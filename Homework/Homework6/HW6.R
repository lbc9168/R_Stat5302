setwd("I:/OSU/2015 Spring/STAT 5302/Homeworks/HW6")

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
