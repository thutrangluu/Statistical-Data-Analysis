source("functions_Ch8.txt")

# Exercise 7.1 
# a.
geese = read.table("geese.txt",header = T)
attach(geese)

par(mfrow=c(1,2))
plot(observer1,photo,
     main = "Photo vs res observer 1")
abline(0,1)
plot(observer.2,photo,
     main = "Photo vs res observer 2")
abline(0,1)

# the data points in the 2 graphs align quite well on a straight line with some 
# variation around x=200, so a simple linear regression model might be appropriate

#b.
ob1_lm = lm(formula = photo ~ observer1, data = geese)
ob1_lm
ob2_lm = lm(formula = photo ~ observer.2, data = geese)
ob2_lm

summary(ob1_lm) # p-value = 1.54e-14 < 0.05
summary(ob2_lm) # p-value = 2e-16 < 0.05

# c. 
res_ob1 = residuals(ob1_lm)
res_ob2 = residuals(ob2_lm)

par(mfrow=c(1,2),pty='s')
plot(photo,res_ob1,
     main = "Photo vs res observer 1")
abline(0,0)

plot(photo,res_ob2,
     main = "Photo vs res observer 2")
abline(0,0)

# d.
par(mfrow=c(1,2))
qqnorm(res_ob1,
       main = "Normal Q-Q Plot observer 1")
qqline(res_ob1, col = "red")
qqnorm(res_ob2,
       main = "Normal Q-Q Plot observer 2")
qqline(res_ob2, col = "red")

Dn <- function (x) {
  f <- ecdf(x)
  y <- numeric(length(x))
  for (i in 1:length(x)) {
    y [i] = abs(f(x[i]) - pnorm((x[i]-mean(x))/ sd(x)))
  }
  return (max(y))
}

set.seed(2695303+46)

d1 = lm.norm.test(res_ob1,photo)
d2 = lm.norm.test(res_ob2,photo)

# null hypothesis of truly normal distributed errors in a simple linear regression model

p_ob1 = sum(d1>Dn(res_ob1))/length(d1) # 0< 0.05 reject 
p_ob2 = sum(d2>Dn(res_ob2))/length(d2) # 0<0.05 reject

# e.

#a
par(mfrow=c(3,2),pty='s')
plot(log(observer1),log(photo),
     main = "Photo vs res observer 1 (log)")
abline(0,1)
plot(log(observer.2),log(photo),
     main = "Photo vs res observer 2 (log)")
abline(0,1)

#b
ob1_lm_log = lm(formula = log(photo) ~ log(observer1), data = geese)
ob1_lm_log
ob2_lm_log = lm(formula = log(photo) ~ log(observer.2), data = geese)
ob2_lm_log

summary(ob1_lm_log) # p-value = 1.54e-14 < 0.05
summary(ob2_lm_log) # p-value = 2e-16 < 0.05

#c
res_ob1_log = residuals(ob1_lm_log)
res_ob2_log = residuals(ob2_lm_log)

#par(mfrow=c(1,2))
plot(log(photo),res_ob1_log,
     main = "Photo vs res observer 1 (log)")
abline(0,0)

plot(log(photo),res_ob2_log,
     main = "Photo vs res observer 2 (log)")
abline(0,0)

#d
#par(mfrow=c(1,2))
qqnorm(res_ob1_log,
       main = "Normal Q-Q Plot observer 1 (log)")
qqline(res_ob1_log, col = "red")
qqnorm(res_ob2_log,
       main = "Normal Q-Q Plot observer 2 (log)")
qqline(res_ob2_log, col = "red")

set.seed(2695303+46)

d1_log = lm.norm.test(res_ob1_log,log(photo))
d2_log = lm.norm.test(res_ob2_log,log(photo))

# null hypothesis of truly normal distributed errors in a simple linear regression model

p_ob1_log = sum(d1_log>Dn(res_ob1_log))/length(d1_log) # >0.05 not reject 
p_ob2_log = sum(d2_log>Dn(res_ob2_log))/length(d2_log) # >0.05 not reject

# Exercise 7.2 
# a. (
air = read.table("airpollution.txt",header = T)
pairs(air[c(6,2,3,4,5)])

attach(air)

#   b.

### Step up

summary(lm(oxidant~wind, data = air))
summary(lm(oxidant~temperature, data = air))
summary(lm(oxidant~humidity, data = air))
summary(lm(oxidant~insolation, data = air))

# add wind bc highest R^2

summary(lm(oxidant~wind + temperature, data = air))
summary(lm(oxidant~wind + humidity, data = air))
summary(lm(oxidant~wind + insolation, data = air))

# add temp

summary(lm(oxidant~wind + temperature + humidity, data = air))
summary(lm(oxidant~wind + temperature + insolation, data = air))

# doesnt improve much 

# final model: intercept + wind + temp

# c. 
summary(lm(oxidant~wind + temperature + humidity + insolation, data = air))

#   d. 
# remove insolation
summary(lm(oxidant~wind + temperature + humidity, data = air))
#remove humidity
summary(lm(oxidant~wind + temperature, data = air))

# final

final_model72 = lm(oxidant~wind + temperature, data = air)

# f. 
## added value plot
par(mfrow = c(1,2))
plot(lm(oxidant~wind + temperature, data = air)$residuals,
     lm(humidity~wind + temperature, data = air)$residuals,
     main = "Added variable plot for humidity",
     ylab = "RYXK",
     xlab = "RXXK")

cor(lm(oxidant~wind + temperature, data = air)$residuals,
    lm(humidity~wind + temperature, data = air)$residuals)

plot(lm(oxidant~wind + temperature, data = air)$residuals,
     lm(insolation~wind + temperature, data = air)$residuals,
     main = "Added variable plot for insolation",
     ylab = "RYXK",
     xlab = "RXXK")

cor(lm(oxidant~wind + temperature, data = air)$residuals,
    lm(insolation~wind + temperature, data = air)$residuals)

## scatter plot of residuals against each Xk in the model separately
par(mfrow = c(2,2))
plot(wind, final_model72$residuals,
     main = "Res vs wind",
     ylab = "residuals")
plot(temperature, final_model72$residuals,
     main = "Res vs temperature",
     ylab = "residuals")

## scatter plot of residuals against each Xk in the model separately
plot(insolation, final_model72$residuals,
     main = "Res vs insolation",
     ylab = "residuals")
plot(humidity, final_model72$residuals,
     main = "Res vs humidity",
     ylab = "residuals")

## scatter plot of residuals against Y
par(mfrow = c(1,2),pty = 's')
plot(oxidant, final_model72$residuals,
     main = "Res vs oxidant",
     ylab = "residuals")
abline(0,0)

## scatter plot of residuals against Y^
plot(final_model72$fitted.values, final_model72$residuals,
     main = "Res vs fitted",
     ylab = "residuals",
     xlab = "fitted values")
abline(0,0)

# g. 
u=c(rep(0,3),1,rep(0,26))
summary(lm(oxidant~wind + temperature + u, data = air))

#2p = 0.05297

# h.
# leverage
round(hatvalues(final_model72),3) > 2*final_model72$rank/30
# 1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16 
# 0.036 0.037 0.084 0.236 0.083 0.084 0.053 0.277 0.083 0.061 0.051 0.079 0.059 0.096 0.054 0.039 
# 17    18    19    20    21    22    23    24    25    26    27    28    29    30 
# 0.078 0.070 0.039 0.043 0.059 0.170 0.242 0.083 0.212 0.044 0.072 0.067 0.165 0.242 

# 1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16 
# FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
# 17    18    19    20    21    22    23    24    25    26    27    28    29    30 
# FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE 

# 4,8,23,25,30

#influence
round(cooks.distance(final_model72),2) >= 1
# 1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20 
# 0.00 0.02 0.05 0.38 0.02 0.04 0.00 0.00 0.04 0.00 0.09 0.02 0.00 0.01 0.00 0.00 0.01 0.00 0.01 0.02 
# 21   22   23   24   25   26   27   28   29   30 
# 0.08 0.21 0.07 0.02 0.00 0.00 0.03 0.03 0.01 0.01 

# 1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16 
# FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
# 17    18    19    20    21    22    23    24    25    26    27    28    29    30 
# FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 

plot(1:30,cooks.distance(final_model72),
     ylim = 0:1)
lines(cooks.distance(final_model72))

#colinear
round(cor(air[,2:5]),2)
# wind temperature humidity insolation
# wind         1.00       -0.50     0.37      -0.32
# temperature -0.50        1.00    -0.54       0.57
# humidity     0.37       -0.54     1.00      -0.18
# insolation  -0.32        0.57    -0.18       1.00

varianceinflation(air[,2:5])
# [1] 1.363044 2.259899 1.503797 1.533546

# i. 
## normal QQ-plot of the residuals
par(mfrow=c(1,1))
qqnorm(final_model72$residuals,
       main = "Normal Q-Q Plot res")
qqline(final_model72$residuals,
       col = 'red')

set.seed(2695303+46)

d72 = lm.norm.test(final_model72$residuals,oxidant)
p72 = sum(d72>Dn(final_model72$residuals))/length(d72)# > 0.05 not reject

# Exercise 7.3 
# a.

steam = read.table ("steamtable.txt", header = T)
pairs(steam)

t(round(cor(steam[,1],steam[,-1]),3))
#                 [,1]
# Fatty.Acid      0.383
# Glycerine       0.306
# Wind.Mph        0.474
# Calendar.Days   0.137
# Operating.Days  0.536
# Freezing.Days   0.641
# Temperature    -0.845
# Wind2           0.395
# Startups        0.382

# the correlation with Temperature is biggest in absolute value

# step up
attach(steam)

summary(lm(Steam~Fatty.Acid))$r.squared
# 0.1468252
summary(lm(Steam~Glycerine))$r.squared
# 0.09335798
summary(lm(Steam~Wind.Mph))$r.squared
# 0.2249694
summary(lm(Steam~Calendar.Days))$r.squared
# 0.01869725
summary(lm(Steam~Operating.Days))$r.squared
# 0.2874273
summary(lm(Steam~Freezing.Days))$r.squared
# 0.4104264
summary(lm(Steam~Temperature))$r.squared
# 0.7144375 -> highest
summary(lm(Steam~Wind2))$r.squared
# 0.1556595
summary(lm(Steam~Startups))$r.squared
# 0.146019

# add temperature
summary(lm(Steam~Temperature))$r.squared

#   b. 
summary(lm(Steam~Temperature + Fatty.Acid))$r.squared # highest R^2
summary(lm(Steam~Temperature + Glycerine))$r.squared
summary(lm(Steam~Temperature + Wind.Mph))$r.squared
summary(lm(Steam~Temperature + Calendar.Days))$r.squared
summary(lm(Steam~Temperature + Operating.Days))$r.squared
summary(lm(Steam~Temperature + Freezing.Days))$r.squared
summary(lm(Steam~Temperature + Wind2))$r.squared
summary(lm(Steam~Temperature + Startups))$r.squared

# add Fatty.Acid

summary(lm(Steam~Temperature + Fatty.Acid + Glycerine))$r.squared
summary(lm(Steam~Temperature + Fatty.Acid + Wind.Mph))$r.squared
summary(lm(Steam~Temperature + Fatty.Acid + Calendar.Days))$r.squared
summary(lm(Steam~Temperature + Fatty.Acid + Operating.Days))$r.squared
summary(lm(Steam~Temperature + Fatty.Acid + Freezing.Days))$r.squared
summary(lm(Steam~Temperature + Fatty.Acid + Wind2))$r.squared
summary(lm(Steam~Temperature + Fatty.Acid + Startups))$r.squared

## doesnt changes much

## final: intercept + temperature + fatty.acid

final_model73 = lm(Steam~Temperature+Fatty.Acid)

##full
summary(lm(Steam~Fatty.Acid+Glycerine+Wind.Mph+Calendar.Days+Operating.Days
           +Freezing.Days+Temperature+Wind2+Startups))

### diagnostics:
## added variable plot
par(mfrow=c(4,2))
plot(lm(Steam~Temperature + Fatty.Acid)$residuals,
     lm(Glycerine~Temperature + Fatty.Acid)$residuals,
     main = "Added variable plot for Glycerine",
     ylab = "RYXK",
     xlab = "RXXK")

plot(lm(Steam~Temperature + Fatty.Acid)$residuals,
     lm(Wind.Mph~Temperature + Fatty.Acid)$residuals,
     main = "Added variable plot for Wind.Mph",
     ylab = "RYXK",
     xlab = "RXXK")

plot(lm(Steam~Temperature + Fatty.Acid)$residuals,
     lm(Calendar.Days~Temperature + Fatty.Acid)$residuals,
     main = "Added variable plot for Calendar.Days",
     ylab = "RYXK",
     xlab = "RXXK")

plot(lm(Steam~Temperature + Fatty.Acid)$residuals,
     lm(Operating.Days~Temperature + Fatty.Acid)$residuals,
     main = "Added variable plot for Operating.Days",
     ylab = "RYXK",
     xlab = "RXXK")

plot(lm(Steam~Temperature + Fatty.Acid)$residuals,
     lm(Freezing.Days~Temperature + Fatty.Acid)$residuals,
     main = "Added variable plot for Freezing.Days",
     ylab = "RYXK",
     xlab = "RXXK")

plot(lm(Steam~Temperature + Fatty.Acid)$residuals,
     lm(Wind2~Temperature + Fatty.Acid)$residuals,
     main = "Added variable plot for Wind2",
     ylab = "RYXK",
     xlab = "RXXK")

plot(lm(Steam~Temperature + Fatty.Acid)$residuals,
     lm(Startups~Temperature + Fatty.Acid)$residuals,
     main = "Added variable plot for Startups",
     ylab = "RYXK",
     xlab = "RXXK")

## no clear linear relation

## scatter plot of residuals against each Xk in the model separately
par(mfrow=c(1,2),pty = 's')
plot(Temperature, final_model73$residuals,
     main = "Res vs Temp",
     ylab = "residuals")
plot(Fatty.Acid, final_model73$residuals,
     main = "Res vs fatty acid",
     ylab = "residuals")

## scatter plot of residuals against each Xk not in the model separately
par(mfrow=c(3,2))
plot(Glycerine, final_model73$residuals,
     main = "Res vs Glycerine",
     ylab = "residuals")
plot(Wind.Mph, final_model73$residuals,
     main = "Res vs Wind.Mph",
     ylab = "residuals")
plot(Calendar.Days, final_model73$residuals,
     main = "Res vs Calendar.Days",
     ylab = "residuals")
plot(Freezing.Days, final_model73$residuals,
     main = "Res vs Freezing.Days",
     ylab = "residuals")
plot(Operating.Days, final_model73$residuals,
     main = "Res vs Operating.Days",
     ylab = "residuals")
plot(Startups, final_model73$residuals,
     main = "Res vs Startups",
     ylab = "residuals")

## scatter plot of residuals against Y
par(mfrow=c(1,2),pty = 's')
plot(Steam, final_model73$residuals,
     main = "Res vs steam",
     ylab = "residuals")
abline(0,0)
plot(final_model73$fitted.values, final_model73$residuals,
     main = "Res vs fitted values",
     ylab = "residuals",
     xlab = "fitted values")
abline(0,0)

# c. 
#influence
round(cooks.distance(final_model73),2) >= 1

# 1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20 
# 0.00 0.02 0.05 0.38 0.02 0.04 0.00 0.00 0.04 0.00 0.09 0.02 0.00 0.01 0.00 0.00 0.01 0.00 0.01 0.02 
# 21   22   23   24   25   26   27   28   29   30 
# 0.08 0.21 0.07 0.02 0.00 0.00 0.03 0.03 0.01 0.01 

# 1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16 
# FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
# 17    18    19    20    21    22    23    24    25 
# FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 

plot(1:25,cooks.distance(final_model73),
     ylim = 0:1)
lines(cooks.distance(final_model73))

#no influence point

# colinear

round(cor(steam[,2:10]),2)
#                   Fatty.Acid Glycerine Wind.Mph Calendar.Days Operating.Days Freezing.Days Temperature Wind2 Startups
# Fatty.Acid           1.00      0.94    -0.13          0.38           0.69         -0.19        0.00 -0.13     0.62
# Glycerine            0.94      1.00    -0.14          0.25           0.76         -0.23        0.07 -0.13     0.60
# Wind.Mph            -0.13     -0.14     1.00         -0.32           0.23          0.56       -0.62  0.99     0.07
# Calendar.Days        0.38      0.25    -0.32          1.00           0.02         -0.20        0.08 -0.32    -0.05
# Operating.Days       0.69      0.76     0.23          0.02           1.00          0.12       -0.21  0.21     0.60
# Freezing.Days       -0.19     -0.23     0.56         -0.20           0.12          1.00       -0.86  0.49     0.12
# Temperature          0.00      0.07    -0.62          0.08          -0.21         -0.86        1.00 -0.54    -0.24
# Wind2               -0.13     -0.13     0.99         -0.32           0.21          0.49       -0.54  1.00     0.03
# Startups             0.62      0.60     0.07         -0.05           0.60          0.12       -0.24  0.03     1.00

## Fatty.Acid + Glycerine: 0.94
## Wind + Wind2: 0.99
## Glycerine + Operating.Days: 0.76
## Fatty.Acid + Operating.Days: 0.69
## Temperatutre + Freezing.Days: -0.86

varianceinflation(steam[,2:10])
# [1]  15.746595  20.137114 126.625618   1.836626   4.411920   4.695013   6.067426 107.590891   2.385046
# 126.625618, 107.590891

conditionindices(steam[,2:10])
# [1]    1.000000    3.038816    8.491469   19.740447   36.909878  119.495637  187.363419  456.151140
# [9] 2640.849747 5026.859410
# condition number = 5026.859410 >>30

round(vardecomposition(steam[,2:10]),3)

# d.
## normal QQ-plot of the residuals
par(mfrow=c(1,1))
qqnorm(final_model73$residuals,
       main = "Normal Q-Q Plot res")
qqline(final_model73$residuals,
       col = 'red')

set.seed(2695303+46)

d73 = lm.norm.test(final_model73$residuals,Steam)
p73 = sum(d73>Dn(final_model73$residuals))/length(d73)# > 0.05 not reject

## normal
