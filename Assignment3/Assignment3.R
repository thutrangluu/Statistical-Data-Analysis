source("functions_Ch4.txt")
source("functions_Ch5.txt")

#3.2

sample32 <- scan("sample32.txt")
h_opt_norm32 = h_opt(sample32)
h_opt_logis32 = (4 * pi) ^ (-1 / 10) * (pi ^ 5 * 13 / (3 ^ (7 / 2) * 35)) ^
  (-1 / 5) * sd(sample32) * length(sample32) ^ (-1 / 5)

par(mfrow = c(1, 3))

x32 <-
  seq(min(sample32) - 3 * h_opt_norm32,
      max(sample32) + 3 * h_opt_norm32,
      length.out = 512)
dense32_norm <-
  density(
    sample32,
    kernel = "gaussian",
    bw = h_opt_norm32,
    from = min(x32),
    to = max(x32)
  )

hist(sample32,
     prob = T,
     ylim = c(0, max(dense32_norm$y)),
     main = "Histogram of sample32, h_norm KDE")
lines(x32, dense32_norm$y, col = "red")


x32_logis <-
  seq(min(sample32) - 3 * h_opt_logis32,
      max(sample32) + 3 * h_opt_logis32,
      length.out = 512)
dense32_logis <-
  density(
    sample32,
    kernel = "gaussian",
    bw = h_opt_logis32,
    from = min(x32_logis),
    to = max(x32_logis)
  )

hist(sample32,
     prob = T,
     ylim = c(0, max(dense32_logis$y)),
     main = "Histogram of sample32, h_logistics KDE")
lines(x32_logis, dense32_logis$y, col = "red")

plot(dense32_logis, col = "red",
     main = "Normal bandwidth vs. logistics bandwidth")
lines(dense32_norm, col = "blue")


#3.3

sample33 <- scan("sample33.txt")

par(mfrow = c(1, 2))

y <- log(sample33)

h_opt_norm33 = h_opt(y)
h_opt_exp33 = (4 * pi) ^ (-1 / 10) * (0.5) ^ (-1 / 5) *
  sd(y) * length(y) ^ (-1 / 5)

yrange <- seq(min(y), max(y), length.out = 512)

hist(sample33, prob = T,
     main = "Histogram of sample33, h_norm KDE")
lines(
  exp(yrange),
  density(
    y,
    kernel = "gaussian",
    bw = h_opt_norm33,
    from = min(yrange),
    to = max(yrange)
  )$y / exp(yrange),
  col = "red"
)

hist(sample33, prob = T,
     main = "Histogram of sample33, h_exp KDE")
lines(
  exp(yrange),
  density(
    y,
    kernel = "gaussian",
    bw = h_opt_exp33,
    from = min(yrange),
    to = max(yrange)
  )$y / exp(yrange),
  col = "red"
)

#3.4

sample34 <- scan("sample34.txt")

h_opt34 <- h_opt(sample34)

h_opt_exp34 = (4 * pi) ^ (-1 / 10) * (0.5) ^ (-1 / 5) *
  sd(sample34) * length(sample34) ^ (-1 / 5)

h_vec <- seq(0.001, 10.0, by = 0.005)

cv_crit <- sapply(h_vec, CV, sample = sample34, kernel = "gauss")
h_opt34_cv <- h_vec[which(cv_crit == min(cv_crit))]

x34 <-
  seq(min(sample34) - 4 * h_opt34,
      max(sample34) + 4 * h_opt34,
      length.out = 512)
dense34 <- density(
  sample34,
  kernel = "gauss",
  bw = h_opt34,
  from = min(x34),
  to = max(x34)
)

x34_exp <-
  seq(min(sample34) - 4 * h_opt_exp34,
      max(sample34) + 4 * h_opt_exp34,
      length.out = 512)
dense34_exp <- density(
  sample34,
  kernel = "gauss",
  bw = h_opt_exp34,
  from = min(x34_exp),
  to = max(x34_exp),
)

x34_cv <-
  seq(min(sample34) - 4 * h_opt34_cv,
      max(sample34) + 4 * h_opt34_cv,
      length.out = 512)
dense34_cv <- density(
  sample34,
  kernel = "gauss",
  bw = h_opt34_cv,
  from = min(x34_cv),
  to = max(x34_cv),
)

par(mfrow = c(1, 3))

hist(sample34,
     prob = T,
     ylim = c(0, max(density(sample34)$y)),
     main = "Histogram of sample34, h_norm KDE")
lines(x34, dense34$y, col = "red")

hist(sample34,
     prob = T,
     ylim = c(0, max(density(sample34)$y)),
     main = "Histogram of sample34, h_dexp KDE")
lines(x34_exp, dense34_exp$y, col = "red")

hist(sample34,
     prob = T,
     ylim = c(0, max(density(sample34)$y)),
     main = "Histogram of sample34, CV KDE")
lines(x34_cv, dense34_cv$y, col = "red")

ddexp <- function(x, loc, scale) {
  de <- numeric(length(x))
  for (i in 1:length(x)) {
    de[i] <- (2 * scale) ^ -1 * exp(-abs(x[i] - loc) / scale)
  }
  de
}

par(mfrow = c(1, 1))

plot(
  sort(sample34),
  ddexp(
    x = sort(sample34),
    loc = 0,
    scale = 1
  ),
  type = "l",
  ylab = "True double exponential density",
  xlab = "x",
  main = "KDE vs. true doubleexponential density"
)
lines(x34, dense34$y, col = "red", lwd = 2)
lines(x34_exp, dense34_exp$y, col = "blue", lwd = 3)
lines(x34_cv, dense34_cv$y, col = "orange", lwd = 2)
legend(
  "topright",
  legend = c("true dexp", "h_norm", "h_dexp", "h_cv"),
  fill = c("black", "red", "blue", "orange")
)

#3.5

tsample <- scan("t-sample.txt")
par(mfrow = c(1, 3))

# a. Compute the median absolute deviation based on the given t-sample.

mad_sample = mad(tsample)

# b.
set.seed(20220302 + 1)

B = 2000
mad_empBS <- bootstrap(tsample, mad, B = 2000)

set.seed(20220302 + 1)

hist(mad_empBS, prob = T)
abline(v = mean(mad_empBS),
       col = "blue",
       lwd = 2)
sd(mad_empBS)

# c.
set.seed(20220302 + 1)

mad_parBS <- numeric(B)
k <- 2 * var(tsample) / (var(tsample) - 1)
for (i in 1:B) {
  xstar_par <- rt(length(tsample), df = k)
  mad_parBS[i] = mad(xstar_par)
}

hist(mad_parBS, prob = T)
abline(v = mean(mad_parBS),
       col = "blue",
       lwd = 2)
sd(mad_parBS)

# d.

set.seed(20220302 + 1)

mad_realizations = numeric(B)
for (i in 1:B) {
  truet <- rt(100, df = 10)
  mad_realizations[i] = mad(truet)
}

hist(mad_realizations, prob = T)
abline(v = mean(mad_realizations),
       col = "blue",
       lwd = 2)
sd(mad_realizations)

# e.
#empirical
var_empBS = var(mad_empBS)

# parametric
var_parBS = var(mad_parBS)

var_realizations = var(mad_realizations)

mylist <- list(
  mad_sample = mad_sample,
  mad_empBS = mad_empBS,
  mad_parBS = mad_parBS,
  mad_realizations = mad_realizations,
  var_empBS = var_empBS,
  var_parBS = var_parBS,
  var_realizations = var_realizations,
  stud_no = c(2695303)
)
save(mylist, file = "Assignment3/myfile3_46.RData")
