#Exercise 1.4

lognorm <- function(n, mu, sigma) {
  set.seed(20220211+46)
  randomSample = rlnorm(n, meanlog = mu, sdlog = sigma)
  
  quants = quantile(randomSample)
  loc = mean(randomSample)
  spread = sd(randomSample)
  stud_no = c(2695303,0)
  
  mylist = list(quants, loc, spread, stud_no)
  save(mylist, file ="G:/My Drive/Programming R/myfile1_46.RData")
}

lognorm(n=100,mu=1,sigma=1)

#Exercise 1.5
#a.
bin = function(n, size, prob) {
  m = size
  p = prob
  x = rbinom(n, m, p)
  pdfPoi = dpois(x, lambda=m*p)
  
  hist = hist(x, prob=T, ylim=c(0,0.20))

  # break??
  
  lines(dpois(x, lambda=m*p),
        ylim=c(0,0.20),
        type = "s", col = "red",
        xlab = "x", ylab = "Density")
}
#b.
bin (n=100, size = 1000, prob = 0.01)
bin (n=200, size = 1000, prob = 0.01)
bin (n=500, size = 1000, prob = 0.01)
bin (n=800, size = 1000, prob = 0.01)

#Exercise 1.6
covid_data_select <- read.csv(file = "C:/Users/PC/Downloads/owid_covid_data_selection.csv", header=TRUE)
head(covid_data_select)
covid_data_Asia 







