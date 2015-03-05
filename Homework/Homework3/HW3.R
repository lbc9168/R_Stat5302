setwd("I:/OSU/2015 Spring/STAT 5302/Homeworks/HW3")

## question 16
bee = read.csv("ex0327.csv")

## a
head(bee)
plot(PollenRemoved ~ DurationOfVisit, type = "n", data = bee)
points(PollenRemoved ~ DurationOfVisit, subset=(BeeType == "Queen"), pch =1, data=bee)
points(PollenRemoved ~ DurationOfVisit, subset=(BeeType == "Worker"), pch =15, data=bee)

## b
plot(log(PollenRemoved/(1-PollenRemoved)) ~ DurationOfVisit, type="n", data = bee)
points(log(PollenRemoved/(1-PollenRemoved)) ~ DurationOfVisit, subset=(BeeType == "Queen"), pch =1, data=bee)
points(log(PollenRemoved/(1-PollenRemoved)) ~ DurationOfVisit, subset=(BeeType == "Worker"), pch =15, data=bee)

## c
plot(log(PollenRemoved/(1-PollenRemoved)) ~ log(DurationOfVisit), type="n", data = bee)
points(log(PollenRemoved/(1-PollenRemoved)) ~ log(DurationOfVisit), subset=(BeeType == "Queen"), pch =1, data=bee)
points(log(PollenRemoved/(1-PollenRemoved)) ~ log(DurationOfVisit), subset=(BeeType == "Worker"), pch =15, data=bee)

## d
summary(lm(log(PollenRemoved/(1-PollenRemoved)) ~ log(DurationOfVisit), data = bee))
summary(lm(log(PollenRemoved/(1-PollenRemoved)) ~ log(DurationOfVisit) + BeeType, data = bee))
summary(lm(log(PollenRemoved/(1-PollenRemoved)) ~ log(DurationOfVisit) + BeeType + log(DurationOfVisit):BeeType, data = bee))

## e
## scripts same as d


## question 22
animal = read.csv("ex0826 (1).csv")
# a matrix of pairwise scatterplots
pairs(~ Metab+Mass+Life, data = animal)
pairs(~ log(Metab) + log(Mass) + log(Life), data = animal)

summary(lm(log(Life) ~ log(Mass)+log(Metab), data = animal))
attach(animal)
cor(Metab,Mass)

## subquestion1
fit.noMetab = lm(Life ~ Mass, data = animal)
fit.Metab = lm(Life ~ Mass + Metab, data = animal)
anova(fit.noMetab,fit.Metab)

## subquestion2,3
summary(lm(Life ~ Mass + Metab, data = animal))
summary(lm(log(Life) ~ log(Mass), data = animal))
summary(lm(log(Life) ~ log(Mass) + log(Metab), data = animal))
summary(lm(log(Life) ~ log(Mass) + log(Metab):CommonName, data = animal))
summary(lm(Life ~ Mass + Metab, data = animal))
summary(lm(Life ~ Metab, data = animal))
summary(lm(log(Life) ~ log(Metab), data = animal))


## question 23
income = read.csv("ex0923.csv")
head(income)
pairs(~ AFQT + Educ + log(Income2005) + Gender, data = income)
summary(lm(log(Income2005) ~ AFQT + Educ + as.factor(Gender), data = income))
summary(lm(log(Income2005) ~ AFQT + Educ + Gender + AFQT:Gender + Educ:Gender, data = income))
