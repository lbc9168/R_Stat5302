setwd("I:/OSU/2015 Spring/STAT 5302/Homeworks/HW5")
load("I:/OSU/2015 Spring/STAT 5302/Homeworks/HW5/.RData")

## question 20 
## a
q20 = read.csv("ex1120.csv")
head(q20)
fit = with(q20, lm(Calcite ~ Carbonate))
summary(fit)
plot(q20)

fit2 = with(q20,lm(Calcite ~ Carbonate, subset = (Carbonate > 22)))
summary(fit2)

fit3 = with(q20,lm(Calcite ~ Carbonate, subset = (Carbonate > 24)))
summary(fit3)

## b
plot(q20)
with(q20, abline(h = mean(Calcite)))
abline(fit)
with(q20[q20$Carbonate > 24,], abline(h = mean(Calcite), lty = 6, col = "red"))
abline(fit3, col = "red", lty = 6)

legend('bottomright', c("with outlier","without outlier") , 
       col=c('black','red'), lty = c(6,1), bty='n')

anova(fit)
anova(fit2)
anova(fit3)

## c
## leverage
se.fit1 = predict(fit, se.fit = T)$se.fit
sigma1 = summary(fit)$sigma
leverage1 = (se.fit1/sigma1)^2
leverage1
plot(leverage1)

## studres
library(MASS)
studres(fit)
q20[q20$studres.outlier ==T,]
plot(studres(fit), main = "student residual")

## cook's distance
cooks.distance(fit)
plot(1:18, cooks.distance(fit), main = "Cook's distance")

## d
## leverage
se.fit2 = predict(fit2, se.fit = T)$se.fit
sigma2 = summary(fit)$sigma
leverage2 = (se.fit2/sigma2)^2
leverage2
plot(leverage2)

## studres
library(MASS)
studres(fit2)
plot(studres(fit2), main = "student residual")

## cook's distance
cooks.distance(fit2)
plot(1:17, cooks.distance(fit2), main = "Cook's distance")


## question 22
q22 = read.csv("ex1122.csv")
head(q22)
pairs(q22)

with(q22, cor(Deforest, Debt))
with(q22, cor(Deforest, Pop))
with(q22, cor(Debt, Pop))

with(q22, boxcox(Deforest ~ Debt)) 
with(q22[q22$Pop < 70000,], boxcox(Deforest ~ Debt + Pop)) 

fit_Pop = with(q22, lm(Deforest ~ Pop))
summary(fit_Pop)

fit_Debt = with(q22, lm(Deforest ~ Debt))
summary(fit_Debt)

fit_DP = with(q22, lm(Deforest ~ Debt + Pop))
summary(fit_DP)

hatvalues(fit_DP)
plot(hatvalues(fit_DP),main = "leverage")
studres(fit_DP)
plot(studres(fit_DP), main = "student residual")
cooks.distance(fit_DP)
plot(1:11, cooks.distance(fit_DP), main = "Cook's distance")

fit_DP_without1 = with(q22, lm(Deforest ~ Debt + Pop, subset = (Deforest < 3000)))
plot(hatvalues(fit_DP_without1),main = "leverage")
plot(studres(fit_DP_without1), main = "student residual")
plot(1:10, cooks.distance(fit_DP_without1), main = "Cook's distance")

fit_DP2 = with(q22, lm(log(Deforest) ~ log(Pop) + log(Debt), subset = (Deforest<3000)))
summary(fit_DP2)

fit_Pop2 = with(q22, lm(log(Deforest) ~ log(Pop), subset = (Deforest<3000)))
summary(fit_Pop2)

fit_Debt2 = with(q22, lm(log(Deforest) ~ log(Debt), subset = (Deforest<3000)))
summary(fit_Debt2)

anova(fit_DP2)
anova(fit_Pop2)

step(fit_DP2, direction = "backward", test = "F")


## 23
q23 = read.csv("ex1123.csv")
head(q23)
pairs(q23)
fit.death = with(q23, lm(Mort ~ Precip + Educ + NonWhite + NOX + SO2))
summary(fit.death)

plot(hatvalues(fit.death),main = "leverage")
plot(studres(fit.death), main = "student residual")
abline(h = -2)+abline(h = 2)
plot(1:60, cooks.distance(fit.death), main = "Cook's distance")

fit.death.2 = with(q23, lm(Mort ~ Precip + Educ + NonWhite + NOX + SO2, subset = (City != 'Miami, FL')))
summary(fit.death.2)

plot(hatvalues(fit.death.2),main = "leverage")
plot(studres(fit.death.2), main = "student residual")
abline(h = -2, lty = 2)+abline(h = 2, lty = 2)
plot(1:59, cooks.distance(fit.death.2), main = "Cook's distance")

fit.death.3 = with(q23[q23$City != 'Miami, FL' & q23$City != 'Los Angeles, CA',], 
                   lm(Mort ~ Precip + Educ + NonWhite + NOX + SO2))
summary(fit.death.3)
plot(hatvalues(fit.death.3),main = "leverage")
plot(studres(fit.death.3), main = "student residual")
abline(h = -2, lty = 2)+abline(h = 2, lty = 2)
plot(1:58, cooks.distance(fit.death.3), main = "Cook's distance")

## partial regression 
step(fit.death.3, direction = "backward", test = "F")
fit.part = with(q23[q23$City != 'Miami, FL' & q23$City != 'Los Angeles, CA',], 
                lm(Mort ~ Precip + Educ + NonWhite + NOX))
anova(fit.part)
anova(fit.death.3)