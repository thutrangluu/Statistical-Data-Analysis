source("Functions_Ch3.txt")
source("sample2022.txt")

p = seq(0,1, 0.1)
par(pty="s", mfrow=c(1,3))


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
     main = paste("True QQ Plot N(0,1) vs. Exp(3)"))
plot(
  norm2,
  t4,
  type = "l",
  col = "red",
  main = paste("True QQ Plot N(-1,9) vs. t_4")
)
plot(
  t6,
  chi2,
  type = "l",
  col = "blue",
  main = paste("True QQ Plot t_6 vs. Chi-squared")
)

#2.2b
par(pty = "s", mfrow = c(1, 3))

hist(sample2022$sample2022a,
     prob = T)

## fit to exp(1)
qqnorm(sample2022$sample2022a,
       main = "Normal Q-Q Plot fitted to Exp(1)")
qqline(
  sample2022$sample2022a,
  distribution = function(p)
    qexp(p, rate = 1),
  col = "red",
)


## fit to exp(0.75) shifted by -1 
qqnorm(sample2022$sample2022a,
       main = "Normal Q-Q Plot fitted to Exp(0.75) shifted by -1")
qqline(
  sample2022$sample2022a,
  distribution = function(p)
    qexp(p, rate = 0.75) - 1,
  col = "red"
)

## fit to lognorm 
qqlnorm(sample2022$sample2022a)
qqline(sample2022$sample2022a, 
       distribution = function(p) 
         qlnorm(p),
       col = "red"
)

## qq chisq
qqchisq(sample2022$sample2022a, df = 4)
qqline(sample2022$sample2022a,
       distribution = function(p)
         qchisq(p, df = 4),
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
qqexp(sqrtData,
      main = "Exp Q-Q Plot transformed by sqrt")
qqline(
  sqrtData,
  distribution = function(p)
    qexp(p, rate = 1),
  col = "red"
)

## fitting with a and b
qqexp(sample2022$sample2022a,
      main = "Exp Q-Q Plot fitted")
b <- sqrt(var(sample2022$sample2022a))/1/1^2
b
a <- mean(sample2022$sample2022a) - b*1/1
a
abline(a=a, b=b,
       col = "red")

## qq exp(1), transform: log 
logData <- log(sample2022$sample2022a)
qqexp(logData,
      main = "Exp Q-Q Plot transformed by log")
qqline(
  logData,
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
qqnorm(sqrt(sample2022$sample2022b),
       main = "Normal Q-Q Plot transformed by sqrt")
qqline(
  sqrt(sample2022$sample2022b),
  distribution = function(p)
    qnorm(p),
  col = "red"
)

##transform: log
qqnorm(log(sample2022$sample2022b),
       main = "Normal Q-Q Plot transformed by log")
qqline(
  log(sample2022$sample2022b),
  distribution = function(p)
    qnorm(p),
  col = "red"
)

##scale to N(0,1.2^2)
qqnorm(sqrt(sample2022$sample2022b),
       main = "Normal Q-Q Plot sqrt scaled to N(0,1.2^2")
qqline(
  sqrt(sample2022$sample2022b),
  distribution = function(p) 
    qnorm(p, mean = 0, sd = 1.2),
  col = "red"
)

#2.3b.

alpha = 0.05

KS_test <- ks.test(sample2022$sample2022b, 
                   pgompertz(seq(0,1,by=0.1), shape = 1, scale = 0.2))
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

breaks <- qgompertz(seq(0,1,by=1/quantiles), 
                    shape = 1, 
                    scale = 0.2)

Chisq_test <- chisquare(sample2022$sample2022b, 
                        pgompertz,
                        k = 16,
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
BMI_male = body_data_male$Weight / (body_data_male$Height/100)^2

BMI_hist <- hist(
  BMI_male, 
  breaks = seq(min(BMI_male) - 5, max(BMI_male) + 5, by = 2),
  main = paste("Histogram for BMI in male"),
  prob = T
)

boxplot(
  BMI_male,
  main = "Boxplot BMI in male",
  xlab = "BMI"
)

ankle_male = body_data_male$`Ankle girth`

ankle_hist <- hist(
  ankle_male, 
  breaks = seq(min(ankle_male) - 5, 
               max(ankle_male) + 5, by = 2),
  main = paste("Histogram for ankle girth in male"),
  prob = T,
  xlab = "Ankle grith"
)

boxplot(
  ankle_male,
  main = "Boxplot Ankle girth in male",
  xlab = "Ankle girth"
)

#2.4b
qqplot(BMI_male,ankle_male,
       ylab="Ankle girth",
       main = "QQ-plot BMI vs. Ankle girth"
)

#2.4c

# Without the use of hypothesis tests, find for each of the two datasets of BMIs and ankle
# measurements appropriate distributions, as members of certain location-scale families

##BMI

qqnorm(BMI_male,
       main = "Normal Q-Q Plot BMI male")
qqline(BMI_male,
       distribution = function(p)
         qnorm(p),
       col = "red"
)

qqnorm(BMI_male,
       main = "Normal Q-Q Plot BMI male fitted")
b <- sqrt(var(BMI_male))/1
b
a <- mean(BMI_male) - b*0
a
abline(a=a, b=b,
       col = "red")

qqt(log(BMI_male), df = 8,
    main = "t_8 Q-Q Plot BMI male"
)
qqline(log(BMI_male),
       distribution = function(p)
         qt(p, df = 8),
       col = "red"
)

##Ankle

qqnorm(ankle_male,
       main = "Normal Q-Q Plot ankle girth male")
qqline(ankle_male,
       distribution = function(p)
         qnorm(p),
       col = "red"
)

qqnorm(sqrt(ankle_male),
       main = "Normal Q-Q Plot sqrt ankle girth male")
qqline(sqrt(ankle_male),
       distribution = function(p)
         qnorm(p),
       col = "red"
)

qqnorm(ankle_male,
       main = "Normal Q-Q Plot ankle girth male fitted")
b <- sqrt(var(ankle_male))/1
b
a <- mean(ankle_male) - b*0
a
abline(a=a, b=b,
       col = "red")

#2.4d

differences <- (BMI_male - ankle_male)

qqnorm(differences,
       main = "Q-Q Plot N(0,0.9^2)")

qqline(
  differences,
  distribution = function(p)
    qnorm(p, mean = 0, sd = 0.9),
  col = "red"
)

##transform: sqrt

qqnorm(sqrt(abs(differences)),
       main = "Q-Q Plot for squared root of differences")

qqline(
  sqrt(abs(differences)),
  distribution = function(p)
    qnorm(p, mean = 0, sd = 1.05),
  col = "red"
)

#2.4e

shapiro.test(differences)

#2.4f

#full sample

BMI_full <- body_data$Weight / (body_data$Height/100)^2

shapiro.test(BMI_full)

hist(BMI_full,
     breaks = seq(min(BMI_full) - 5, 
                  max(BMI_full) + 5, by = 2),
     main = paste("Histogram for BMI full sample"),
     prob = F
)

#first 50

BMI_50 <- head(body_data$Weight,50) / (head(body_data$Height,50)/100)^2

shapiro.test(BMI_50)

hist(BMI_50,
     breaks = seq(min(BMI_50) - 5, 
                  max(BMI_50) + 5, by = 2),
     main = paste("Histogram for BMI first 50 obs"),
     prob = F
)
