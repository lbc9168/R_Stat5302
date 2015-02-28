setwd("I:/OSU/2015 Spring/STAT 5302/Homeworks/HW3")

bee = read.csv("ex0327.csv")
head(bee)
plot(PollenRemoved ~ DurationOfVisit, type = "n", data = bee)
points(PollenRemoved ~ DurationOfVisit, subset=(BeeType == "Queen"), pch =1, data=bee)
points(PollenRemoved ~ DurationOfVisit, subset=(BeeType == "Worker"), pch =15, data=bee)