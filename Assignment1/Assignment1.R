#Exercise 1.4

lognorm <- function(n, mu, sigma) {
  set.seed(20220211 + 46)
  randomSample = rlnorm(n, meanlog = mu, sdlog = sigma)
  quants = quantile(randomSample, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
  loc = mean(randomSample)
  spread = sd(randomSample)
  stud_no = c(2695303, 2665825)
  mylist = list(quants, loc, spread, stud_no)
  save(mylist, file = "Assignment1/myfile1_46.RData")
}

lognorm(n = 100, mu = 1, sigma = 1)

# Exercise 1.5
# 1.5a
bin <- function(n, size, prob) {
  m = size
  p = prob
  x = rbinom(n, m, p)
  pdfPoi = dpois(0:n, lambda = m * p)
  
  hist(
    x,
    breaks=0:(max(x)+1)-0.001,
    main = paste("Histogram for X~Bin(", m ,",", p ,") vs. Poisson(", m*p, ")"),
    prob = T,
    xlim = c(0,max(x)+3)
  )
  
  lines(
    pdfPoi,
    type = "s",
    col = "red",
    xlab = "x",
    ylab = "Density"
  )
  legend("topright",legend=c("Binomial", "Poisson"), 
          fill=c("grey", "red"))
}
# 1.5b
lambda = 10
bin (n = 100, size = 1000, prob = lambda/1000)
bin (n = 200, size = 2000, prob = lambda/2000)
bin (n = 500, size = 5000, prob = lambda/5000)
bin (n = 800, size = 8000, prob = lambda/8000)

#Exercise 1.6
# Read data
covid_data_select <-
  read.csv(file = "owid_covid_data_selection.csv", header = TRUE)
head(covid_data_select)
covid_data_asia <-
  subset(covid_data_select, covid_data_select$continent == "Asia")
head(covid_data_asia)

# 1.6a
mean(covid_data_asia$partly_vacc, na.rm = T)
sd(covid_data_asia$partly_vacc, na.rm = T)
var(covid_data_asia$partly_vacc, na.rm = T)
quants <- unname(quantile(covid_data_asia$partly_vacc,
                  na.rm = T,
                  probs = c(0.25, 0.5, 0.75)))
range <- c(
  min(covid_data_asia$partly_vacc, na.rm = T),
  max(covid_data_asia$partly_vacc, na.rm = T)
)
IQR = quants[3] - quants[1]
IQR

summary(covid_data_asia$partly_vacc)

par(mfrow=c(1, 3))

# Box plot
boxplot(
  covid_data_asia$partly_vacc,
  main = "Boxplot of partly vaccinated people (in %)",
  xlab = "Asia",
  col = "blue"
)

# Histogram
hist(
  covid_data_asia$partly_vacc,
  main = "Histogram for Asia",
  xlab = "partly vaccinated people (in %)",
  prob = T,
  xlim = c(0, 100),
  ylim = c(0, 0.03),
  breaks = 10
)

# Empirical cumulative distribution function
plot(ecdf(covid_data_asia$partly_vacc), col="red", 
     main="Empirical cum. distribution function of Asia",
     xlab="partly vaccinated people (in %)", xlim=c(0,100))

# 1.6b
# Scatter plot
plot(
  covid_data_asia$partly_vacc,
  covid_data_asia$human_development_index,
  main = "Scatter plot partly vaccinated % vs. HDI in Asia",
  xlab = "% of partly vaccinated people",
  ylab = "Human development index"
)

# Covariance matrix and correlation coefficient
bivariate_hdi_vacc <-
  cbind(covid_data_asia$partly_vacc, covid_data_asia$human_development_index)
bivariate_hdi_vacc_noNA <-
  bivariate_hdi_vacc[-which(is.na(covid_data_asia$human_development_index) |
                              is.na(covid_data_asia$partly_vacc)),]

cov(bivariate_hdi_vacc_noNA)

cor(bivariate_hdi_vacc_noNA, method = "spearman")
