#Exercise 2.2
#2.2a
# I. standard normal against exponential with rate 3.

norm <- qnorm(p)
exp <- qexp(p, rate = 3)

# II. normal with mean -1 and variance 9 against t4.

norm2 <- qnorm(p, mean = -1, sd = 3)
t4 <- qt(p, df = 4)

# III. t6 against chi-squared with 3 degrees of freedom.

chi2 <- qchisq(p, df = 3)
t6 <- qt(p, df = 6)

par(pty="s", mfrow= c(1,3))
plot(norm, exp)
plot(norm2, t4)
plot(t6, chi2)

#2.2b
source("sample2022.txt")

par(pty="s",mfrow=c(1,3))
qqnorm(sample2022$sample2022a)

qqline(sample2022$sample2022a, 
       distribution = function(p) qexp(p,rate = 1), col="red")

qqnorm(sample2022$sample2022a)
qqline(sample2022$sample2022a, 
       distribution = function(p) qexp(p,rate = 0.5) - 1, col="red")

qqnorm(sample2022$sample2022a)
qqline(sample2022$sample2022a, 
       distribution = function(p) qexp(p,rate = 0.75) - 1, col="red")

#Exercise 2.3

#Exercise 2.4