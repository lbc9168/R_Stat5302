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
