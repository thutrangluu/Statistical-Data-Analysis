source("Functions_Ch3.txt")
source("sample2022.txt")
#2.1
set.seed(12345)
par(mfrow = c(1, 2), pty = "s")

n = 10
x <- rnorm(n, 0, 1)

qqnorm(x, main = paste("Normal QQ Plot, n=", n))
qqline(x, col = 2, lwd = 3)



#Exercise 2.2
#2.2a
# I. standard normal against exponential with rate 3.

norm <- qnorm(seq(0, 1, by = 0.025))
exp <- qexp(seq(0, 1, by = 0.025), rate = 3)

# II. normal with mean -1 and variance 9 against t4.

norm2 <- qnorm(seq(0, 1, by = 0.025), mean = -1, sd = 3)
t4 <- qt(seq(0, 1, by = 0.025), df = 4)

# III. t6 against chi-squared with 3 degrees of freedom.

chi2 <- qchisq(seq(0, 1, by = 0.025), df = 3)
t6 <- qt(seq(0, 1, by = 0.025), df = 6)

par(pty = "s", mfrow = c(1, 3))
plot(norm,
     exp,
     type = "l",
     main = paste("True QQ Plot
      N(0,1) vs. Exp(3)"))
plot(
  norm2,
  t4,
  type = "l",
  col = "red",
  main = paste("True QQ Plot
      N(-1,9) vs. t_4")
)
plot(
  t6,
  chi2,
  type = "l",
  col = "blue",
  main = paste("True QQ Plot
        t_6 vs. Chi-squared")
)

#2.2b
par(pty = "s", mfrow = c(1, 3))

## fit to exp(1)
qqnorm(sample2022$sample2022a)
qqline(
  sample2022$sample2022a,
  distribution = function(p)
    qexp(p, rate = 1),
  col = "red",
)

## fit to exp(0.5) shifted by -1 
qqnorm(sample2022$sample2022a)
qqline(
  sample2022$sample2022a,
  distribution = function(p)
    qexp(p, rate = 0.5) - 1,
  col = "red"
)

## fit to exp(0.75) shifted by -1 
qqnorm(sample2022$sample2022a)
qqline(
  sample2022$sample2022a,
  distribution = function(p)
    qexp(p, rate = 0.75) - 1,
  col = "red"
)

## qq chisq
qqchisq(sample2022$sample2022a, df = 4)
qqline(
  sample2022$sample2022a,
  distribution = function(p)
    qchisq(p, df = 4),
  col = "red"
)

## qqt
qqt(sample2022$sample2022a, df = 4)
qqline(
  sample2022$sample2022a,
  distribution = function(p)
    qt(p, df = 4),
  col = "red"
)

## qq exp(1)
qqexp(sample2022$sample2022a)
qqline(
  sample2022$sample2022a,
  distribution = function(p)
    qexp(p, rate = 1),
  col = "red"
)

## qq exp(1), transform: sqrt
sqrtData <- sqrt(sample2022$sample2022a)
qqexp(sqrtData)
qqline(
  sqrtData,
  distribution = function(p)
    qexp(p, rate = 1),
  col = "red"
)

## qq exp(1), transform: log 
logData <- log(sample2022$sample2022a)
qqexp(logData)
qqline(
  logData,
  distribution = function(p)
    qexp(p, rate = 1),
  col = "red"
)

## qq exp(1), transform: log10
log10Data <- log10(sample2022$sample2022a)
qqexp(log10Data)
qqline(
  log10Data,
  distribution = function(p)
    qexp(p, rate = 1),
  col = "red"
)

##Exercise 2.3
#2.3a.

par(pty = "s", mfrow = c(1, 3))

qqnorm(sample2022$sample2022b)
qqline(
  sample2022$sample2022b,
  distribution = function(p)
    qnorm(p),
  col = "red"
)

##transform: sqrt
qqnorm(sqrt(sample2022$sample2022b))
qqline(
  sqrt(sample2022$sample2022b),
  distribution = function(p)
    qnorm(p),
  col = "red"
)

##transform: log
qqnorm(log(sample2022$sample2022b))
qqline(
  log(sample2022$sample2022b),
  distribution = function(p)
    qnorm(p),
  col = "red"
)

##scale to N(0,1.2^2)
qqnorm(sqrt(sample2022$sample2022b))
qqline(
  sqrt(sample2022$sample2022b),
  distribution = function(p) 
    qnorm(p, mean = 0, sd = 1.2),
  col = "red"
)

qqexp(sqrt(sample2022$sample2022b))
qqline(
  sqrt(sample2022$sample2022b),
  distribution = function(p)
    qexp(p, rate =1),
  col = "red"
)

#2.3b.

alpha = 0.05

KS_test <- ks.test(sample2022$sample2022b, pgompertz(x, shape = 1, scale = 0.2))
KS_test

KS_score <- KS_test$statistic
KS_score

KS_pvalue <- KS_test$p.value
KS_pvalue

KS_reject <- if(KS_pvalue < alpha) TRUE else FALSE
KS_reject

#2.3c.

range(sample2022$sample2022b)
length(sample2022$sample2022b)
quantiles = length(sample2022$sample2022b)/5

breaks <- qgompertz(c(1/quantiles,2*1/quantiles,3*1/quantiles,
                      4*1/quantiles,5*1/quantiles,6*1/quantiles,
                      7*1/quantiles,8*1/quantiles,9*1/quantiles,
                      10*1/quantiles,11*1/quantiles,12*1/quantiles,
                      13*1/quantiles,14*1/quantiles,15*1/quantiles), 
                   shape = 1, 
                   scale = 0.2)

Chisq_test <- chisquare(sample2022$sample2022b, 
                        pgompertz,
                        k = 10,
                        lb = 0,
                        ub = 10,
                        breaks,
                        shape = 1,
                        scale = 0.2)
Chisq_test 

Chisq_breaks <- breaks
Chisq_breaks

Chisq_score <- Chisq_test$chisquare
Chisq_score

Chisq_pvalue <- Chisq_test$pr
Chisq_pvalue

Chisq_reject <- if(Chisq_pvalue < alpha) TRUE else FALSE
Chisq_reject

mylist <- list(KS_score, KS_pvalue, KS_reject,
              Chisq_breaks, Chisq_score, Chisq_pvalue, Chisq_reject)
save(mylist, file="Assignment2/myfile2_46.RData")

#Exercise 2.4
body_data <- scan("body.dat.txt")
body_data <- matrix(body_data, ncol = 25, byrow = TRUE)
body_data <- data.frame(body_data)

colnames(body_data) <- c("Biacromial diameter",
                         "Biiliac diameter",
                         "Bitrochanteric diameter",
                         "Chest depth between spine and sternum at nipple level",
                         "Chest diameter at nipple level",
                         "Elbow diameter",
                         "Wrist diameter",
                         "Knee diameter",
                         "Ankle diameter",
                         "Shoulder girth",
                         "Chest girth",
                         "Waist girth",
                         "Navel girth",
                         "Hip girth",
                         "Thigh girth",
                         "Bicep girth",
                         "Forearm girth",
                         "Knee girth",
                         "Calf girth",
                         "Ankle girth",
                         "Wrist girth",
                         "Age",
                         "Weight",
                         "Height",
                         "Gender")

body_data_male <- subset(body_data, body_data$Gender == 1)

#2.4a
BMI <- body_data_male$Weight / (body_data_male$Height/100)^2

hist(
  BMI, 
  breaks = seq(min(BMI) - 5, max(BMI) + 5, by = 2),
  main = paste("Histogram for BMI in male"),
  prob = F
)

boxplot(
  BMI,
  main = "Boxplot BMI in male",
  xlab = "BMI"
)

hist(
  body_data_male$`Ankle girth`, 
  breaks = seq(min(BMI) - 5, max(BMI) + 5, by = 2),
  main = paste("Histogram for ankle grith in male"),
  prob = F,
  xlab = "Ankle grith"
)

boxplot(
  body_data_male$`Ankle girth`,
  main = "Boxplot Ankle grith in male",
  xlab = "Ankle grith"
)

#2.4b
qqplot(BMI,body_data_male$`Ankle girth`,
       ylab="Ankle grith")

#2.4c

#2.4d

differences <- (BMI - body_data_male$`Ankle girth`)

qqnorm(differences)

qqline(
  differences,
  distribution = function(p)
    qnorm(p, mean = 0, sd = 0.9),
  col = "red"
)

##transform: sqrt

qqnorm(sqrt(abs(differences)))

qqline(
  sqrt(abs(differences)),
  distribution = function(p)
    qnorm(p, mean = 0, sd = 1),
  col = "red"
)


#2.4e

#2.4f

