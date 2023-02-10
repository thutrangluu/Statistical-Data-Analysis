                                                                                                                                                       # setwd("C:/Users/Dennis/surfdrive/Documents/VU Amsterdam/Lectures/2023 SDA/Assignments/2023/Assignment 1")

# How to grade:
# see separate explanations.


### Solution Assignment 1 SDA 2023

## 1.4

norm <- function(n, mu, sigma){
  set.seed(2023210)
  x <- norm(n, mu, sigma)
  mylist <- list(quants = quantile(x, probs=c(0.05, 0.5,0.95)),
                 loc = mean(x),
                 spread = sd(x),
                 stud_no = c(2999999,3000000))
  save(mylist, file="myfile1_0.RData")
}

norm(100, 1, 1)

## 1.5

par(mfrow=c(2,2))

## make sure the figure looks nice
## e.g. both density and hist are visible,
## i.e. nothing is cut off,
## and proper axis labels and title

CLT_unif<-function(n,m)
{
  x <- replicate(n,runif(m, min=0, max=1))
  means <- apply(x, 2, mean)
  h <- hist(means, prob=T, xlim=c(0.35,0.65))
  h <- hist(means, prob=T, xlim=c(0.35,0.65), ylim=c(0,max(sqrt(12*m/2/pi), h$density)))
  h
  u=seq(0,1,0.001)
  lines(u,dnorm(u, mean=0.5, sd=1/sqrt(12*m)), col=2)
}

# not required:
set.seed(2023210)

# effect of increasing sample size m
CLT_unif(n=50,m=30)
CLT_unif(50,200)
# more concentrated around 0.5
# generally a better fit to the normal density


# effect of increasing replications n 
CLT_unif(50,30)
CLT_unif(300,30)
# same spread but generally much better fit to the normal density
# in particular, more symmetric with bigger n

CLT_unif(300,200)
# in general, very good fit to the normal density




## 1.6

setwd("C:/Users/Dennis/surfdrive/Documents/VU Amsterdam/Lectures/2023 SDA/Assignments/2023/Assignment 1")
military_per_cap <- read.csv("military-spending-per-capita.csv")
head(military_per_cap)

attach(military_per_cap)

military_per_cap_1988 <- military_per_cap[Year==1988,]
military_per_cap_2020 <- military_per_cap[Year==2020,]

head(military_per_cap_2020)

# in USD
ME1988 <- military_per_cap_1988$military_expenditure_per_capita
ME2020 <- military_per_cap_2020$military_expenditure_per_capita

par(mfrow=c(1,1))
hist(ME2020, prob=T, main="Histogram of Military expenditures\nper capita 2020 (in US$)", xlab="US$", ylim=c(0,0.0035), xlim=c(0,2500))

summary(ME2020)
# median less than mean -> data distribution seemingly right-skewed.
# big difference between 3rd quartile and median 
#   (much bigger than between 1st quartile and median)-> right-skewed.

sd(ME2020)
# large sd compared to interquartile range -> rather heavy tails or unsymmetric data
var(ME2020)


c(min(ME2020), max(ME2020))


par(mfrow=c(1,1))
boxplot(ME2020, main="Boxplot of military expenses\nper capita (in US$)")


# not very important:
par(mfrow=c(1,1))
plot(ecdf(ME2020), xlim=c(0,2500), main="", col="red", ylim=c(0,1), xlab="")
legend("bottomright", legend=c("1988","2020"), col=1:2, lty=c(1,1))


index_bi_1988 <- which(military_per_cap_1988$Code %in% military_per_cap_2020$Code)
index_bi_2020 <- which(military_per_cap_2020$Code %in% military_per_cap_1988$Code)

# double-check
# military_per_cap_1985[index_bi_1985,]
# military_per_cap_2020[index_bi_2020,]

military_bivariate <- cbind(military_per_cap_1988[index_bi_1988,c(1,2,4)], military_per_cap_2020[index_bi_2020,4])
names(military_bivariate)[3:4] <- c("military_expenditure_1988","military_expenditure_2020")

head(military_bivariate)

round(colMeans(military_bivariate[,3:4]), 2)
# 2020 has a much higher mean

round(cov(military_bivariate[,3:4]), 2)
round(cor(military_bivariate[,3:4]), 2)
# very strong correlation

round(cor(military_bivariate[,3:4], method="spearman"), 2)
round(cor(military_bivariate[,3:4], method="kendall"), 2)


plot(military_bivariate[,3:4], xlab="1988", ylab="2020", main="Military expenditures in % of GDP")
# one can still see the strong positive correlation in the scatterplot
