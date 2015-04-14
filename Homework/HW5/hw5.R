setwd("I:/OSU/2015 Spring/STAT 5302/Homeworks/HW5")

##question 20 
q20 = read.csv("ex1120.csv")
head(q20)
fit = with(q20, lm(Calcite ~ Carbonate))
summary(fit)
