setwd("F:/Bingcai Liu/GitHub/R_Stat5302/Homework/HW4")

## 10.9

qt(0.975,22)

1.6601-2.0739*0.789
((8.38155-5.99713)/2)/(5.99713/32)

1-pf(6.3615,32,34)


## 10.32
ex0726 = read.csv("ex0726.csv")
head(ex0726)
height.lm=with(ex0726, lm(Height ~ Father + Mother + Gender))

16.43221+0.39339*72+0.31840*64
newdata = with(ex0726,data.frame(Gender = "female", Father = 72, Mother = 64))

predict(height.lm,newdata, interval = "confidence")


## 10.33
ex1033 = read.csv("ex1033.csv")
head(ex1033)

cor(ex1033)

AFQT.lm = with(ex1033,lm(log10(Income2005) ~ AFQT))
summary(AFQT.lm)
plot(resid(AFQT.lm))
AWPM.lm = with(ex1033,lm(log10(Income2005) ~ Arith + Word + Parag + Math))
summary(AWPM.lm)

attach(ex1033)
detach(ex1033)

M.lm = lm(log10(Income2005) ~ Math) 
summary(M.lm)
plot(Math,log10(Income2005))

MA.lm = lm(log10(Income2005) ~ Math + Arith)
summary(MA.lm)
plot(resid(MA.lm))

MW.lm = lm(log10(Income2005) ~ Math + Word)
summary(MW.lm)

MP.lm = lm(log10(Income2005) ~ Math + Parag)
summary(MP.lm)
plot(resid(MP.lm))

MAW.lm = lm(log10(Income2005) ~ Math + Arith + Word)
summary(MAW.lm)
plot(resid(MAW.lm))

MAP.lm = lm(log10(Income2005) ~ Math + Arith + Parag)
summary(MAP.lm)
plot(resid(MAP.lm))

ALL.lm = lm(log10(Income2005) ~ Arith + Word + Parag + Math + AFQT)
summary(ALL.lm)
plot(resid(ALL.lm))

anova(MAP.lm,MA.lm)
anova(MA.lm,MAP.lm)
anova(MA.lm,MAW.lm)
anova(AWPM.lm,MA.lm)

step(AWPM.lm, direction = "backward", test = "F")

three.lm = lm(log10(Income2005) ~ Math + Arith + AFQT)
summary(three.lm)
