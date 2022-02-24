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

#Exercise 2.3
#a.

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


#b.

alpha = 0.05

KS_test <- ks.test(sample2022$sample2022b, pgompertz(x, shape = 1, scale = 0.2))
KS_test

KS_score <- KS_test$statistic
KS_score

KS_pvalue <- KS_test$p.value
KS_pvalue

KS_reject <- if(KS_pvalue < alpha) TRUE else FALSE
KS_reject

#c.

range(sample2022$sample2022b)
length(sample2022$sample2022b)

breaks <- qgompertz(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), 
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