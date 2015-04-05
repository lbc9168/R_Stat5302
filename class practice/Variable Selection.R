data(state)
state.x77
statedata = data.frame(state.x77, row.names = state.abb)
head(statedata)
head(state.x77)

g = lm(Life.Exp ~ . , data = statedata)
summary(g)
g1 = update(g, .~ .- Area)
summary(g1)

?step
step(g, direction = "backward", test = "F")
step(lm(Life.Exp ~ 1,data = statedata), scope = formula(g), direction = "forward", test = "F")
step(lm(Life.Exp ~ 1,data = statedata), scope = formula(g), direction = "both", test = "F")

library(leaps)
?regsubsets
h = regsubsets(Life.Exp ~ . , data = statedata, method = "exhaustive")
h = regsubsets(Life.Exp ~ . , data = statedata, method = "backward")
h.sum = summary(h)
plot(2:8, h.sum$adjr2)
