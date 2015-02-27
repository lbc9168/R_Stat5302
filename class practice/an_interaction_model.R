data = read.csv("BBang.csv")

fit = lm(Distance ~ Velocity, data = data)
summary(fit)

fit.intercept = lm(Distance ~ 0 + Velocity, data = data)
summary(fit.intercept)