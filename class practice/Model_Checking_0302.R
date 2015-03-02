## Model Checking March 2

setwd("I:/OSU/2015 Spring/STAT 5302/Homeworks/HW2")
election = read.csv("ex0820.csv")
head(election)

with(election, plot(DemPctOfMachineVotes, DemPctOfAbsenteeVotes, pch=16))

library(ggplot2)
qplot(DemPctOfMachineVotes, DemPctOfAbsenteeVotes, data = election, geom = "point")
qplot(DemPctOfMachineVotes, DemPctOfAbsenteeVotes, data = election, geom = c("density2d","point"))


alcohol = read.csv("case1101.csv")
head(alcohol)
summary(alcohol)
boxplot(alcohol)
summary(alcohol$Gastric)

with(alcohol,plot(Metabol ~ Gastric))
qplot(Gastric,Metabol, data = alcohol) + facet_wrap(~ Sex + Alcohol)

qplot(Gastric,Metabol, data = alcohol) + facet_grid(~ Sex + Alcohol)

ggplot(alcohol, aes(Gastric, Metabol)) + geom_point(size = 5, aes(colour = as.factor(Alcohol),shape = as.factor(Sex)))
ggplot(alcohol, aes(Gastric, Metabol)) + geom_point(size = 5, aes(colour = as.factor(Alcohol),shape = as.factor(Sex))) + geom_density2d()
## order matters(aes and 2d)

qplot(Gastric,Metabol, data = alcohol) + facet_grid(~ Sex + Alcohol) + geom_density2d()


## hat matrix
fit.alcohol = lm(Metabol ~ Gastric + as.factor(Sex) + as.factor(Alcohol), data = alcohol)
summary(fit.alcohol)
X <- data.frame(one = rep(1, nrow(alcohol)), 
                gastric = alcohol$Gastric,
                sex = as.numeric(as.factor(alcohol$Sex)), 
                alcohol = as.numeric(as.factor(alcohol$Alcohol)))

library(MASS)
hatvalues(fit.alcohol)
p = sum(hatvalues(fit.alcohol))
h = p/length(hatvalues(fit.alcohol))
rule = 2*h
hatvalues(fit.alcohol)[hatvalues(fit.alcohol)>rule]
alcohol$leverage <- hatvalues(fit.alcohol)
alcohol$outlier <- alcohol$leverage > rule
head(alcohol)
