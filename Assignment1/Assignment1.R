#Exercise 1.4

lognorm <- function(n, mu, sigma) {
  set.seed(20220211 + 46)
  randomSample = rlnorm(n, meanlog = mu, sdlog = sigma)
  
  quants = quantile(randomSample, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
  loc = mean(randomSample)
  spread = sd(randomSample)
  # print(quants)
  # print(loc)
  # print(spread)
  stud_no = c(2695303, 2665825)
  
  mylist = list(quants, loc, spread, stud_no)
  save(mylist, file = "G:/My Drive/Programming R/Statistical-Data-Analysis/Assignment1/myfile1_46.RData")
}

lognorm(n = 100, mu = 1, sigma = 1)

#Exercise 1.5
#a.
bin <- function(n, size, prob) {
  m = size
  p = prob
  x = rbinom(n, m, p)
  pdfPoi = dpois(0:n, lambda = m * p)
  
  hist = hist(
    x,
    breaks = 0:max((7 * size * prob), max(x) + 1) - 0.001,
    main = paste("Histogram for X~Bin(", m ,",", p ,")"),
    prob = T,
    xlim = c(min(x), max(x))
  )
  
  lines(
    pdfPoi,
    xlim = c(min(x), max(x)),
    type = "s",
    col = "red",
    xlab = "x",
    ylab = "Density"
  )
}
#b.
bin (n = 100, size = 1000, prob = 0.01)
bin (n = 200, size = 2000, prob = 0.01)
bin (n = 500, size = 5000, prob = 0.01)
bin (n = 800, size = 8000, prob = 0.01)

#Exercise 1.6
covid_data_select <-
  read.csv(file = "owid_covid_data_selection.csv", header = TRUE)
head(covid_data_select)
covid_data_asia <-
  subset(covid_data_select, covid_data_select$continent == "Asia")
head(covid_data_asia)

#a.
#numerical

mean(covid_data_asia$partly_vacc, na.rm = T)
sd(covid_data_asia$partly_vacc, na.rm = T)
var(covid_data_asia$partly_vacc, na.rm = T)
quants = quantile(covid_data_asia$partly_vacc,
                  na.rm = T,
                  probs = c(0.25, 0.5, 0.75))
range = c(
  min(covid_data_asia$partly_vacc, na.rm = T),
  max(covid_data_asia$partly_vacc, na.rm = T)
)

summary(covid_data_asia$partly_vacc)

boxplot(
  covid_data_asia$partly_vacc,
  main = "Boxplot of partly vaccinated people (in %)",
  xlab = "Asia",
  col = "blue"
)

#graphical

#histogram

hist(
  covid_data_asia$partly_vacc,
  main = "Histogram for Asia",
  xlab = "partly vaccinated people (in %)",
  prob = T,
  xlim = c(0, 100),
  ylim = c(0, 0.05)
)

#ecdf

plot(ecdf(covid_data_asia$partly_vacc), col="red", 
     main="Empirical cum. distribution function of Asia",
     xlab="partly vaccinated people (in %)", xlim=c(0,100))

#b.
# scatterplot
par(mfrow=c(1, 2))
plot(
  covid_data_asia$partly_vacc,
  covid_data_asia$gdp_per_capita,
  xlab = "Partly vaccinated people (Percent)",
  ylab = "GDP per capita (Dollar)"
)
par(new=F)
plot(
  covid_data_asia$partly_vacc,
  log10(covid_data_asia$gdp_per_capita),
  xlab = "Partly vaccinated people (Percent)",
  ylab = "Log-normed GDP per capita (Dollar)"
)

plot(
  covid_data_asia$partly_vacc,
  covid_data_asia$human_development_index,
  xlab = "Partly vaccinated people (Percent)",
  ylab = "Human development index"
)

# gpd_per_capita vs. partly_vacc
bivariate_gpd_vacc <-
  cbind(covid_data_asia$partly_vacc, covid_data_asia$gdp_per_capita)
bivariate_gpd_vacc_noNA <-
  bivariate_gpd_vacc[-which(is.na(covid_data_asia$gdp_per_capita) |
                     is.na(covid_data_asia$partly_vacc)),]

cov(bivariate_gpd_vacc_noNA)

cor(bivariate_gpd_vacc_noNA, method = "spearman")

# human_development_index vs. partly_vacc
bivariate_hdi_vacc <-
  cbind(covid_data_asia$partly_vacc, covid_data_asia$human_development_index)
bivariate_hdi_vacc_noNA <-
  bivariate_hdi_vacc[-which(is.na(covid_data_asia$human_development_index) |
                              is.na(covid_data_asia$partly_vacc)),]

cov(bivariate_hdi_vacc_noNA)

cor(bivariate_hdi_vacc_noNA, method = "spearman")

#ecdf
