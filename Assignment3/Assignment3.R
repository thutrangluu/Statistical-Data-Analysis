source("functions_Ch4.txt")
source("functions_Ch5.txt")



#3.2
# The file sample32.txt contains a sample of n = 100 observations. Pick a
# kernel function and a bandwidth, and use a kernel density estimator to estimate the density
# based on the sample.
# Note: don’t just use trial and error but proceed systematically, i.e. motivate your choices.

sample32 <- scan("sample32.txt")
h_opt_norm32 = h_opt(sample32)
h_opt_logis32 = (4 * pi) ^ (-1 / 10) * (pi ^ 5 * 13 / (3 ^ (7 / 2) * 35)) ^
  (-1 / 5) * sd(sample32) * length(sample32) ^ (-1 / 5)

par(mfrow = c(1, 2))

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

hist(sample32, prob = T, ylim = c(0, max(dense32_norm$y)))
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

hist(sample32, prob = T, ylim = c(0, max(dense32_logis$y)))
lines(x32_logis, dense32_logis$y, col = "red")

plot(dense32_logis, col = "red",
     main = "Normal bandwidth vs. logistics bandwidth")
lines(dense32_norm, col = "blue")


#3.3

# The file sample33.txt contains a sample of n = 80 positive observations.
# Find a suitable kernel density estimate based on this sample.
# Hint: it seems appropriate to assign no mass to ˆf(x) for x < 0. Take a look at Lecture
# 4 to find out how this can be achieved in a reasonable way. Use just one of the available
# approaches.
# Hint: if you would like to use the log-transformation and first find a suitable kernel density
# estimate ˆfy based on the log-transformed sample y, i.e. y1 = log(x1),...,yn = log(xn), you
# can obtain the density estimate ˆfx for the original sample based on
# yrange <- seq(min(y), max(y), length.out=512)
# lines(exp(yrange), density(y, ..., from=min(yrange) , to=max(yrange))$y/exp(yrange))
# This is due to Fy(t) = Fx(exp(t)) for the cumulative distribution functions of the y- and
# x-samples, respectively. Thus, fy(t) = fx(exp(t)) ·exp(t) for their densities.

sample33 <- scan("sample33.txt")

hist33 <- hist(sample33, prob = T)
h_opt_norm33 = h_opt(sample33)
y <- log(sample33)

yrange <- seq(min(y), max(y), length.out = 512)
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

#3.4

# The file sample34.txt contains a sample of n = 90 observations. Find
# two kernel density estimates based on this sample: for the first, use the bandwidth obtained
# from the function h opt, for the second, use the bandwidth obtained from the cross-validation
# criterion. Compare these estimates to the true density function, which is a doubly exponential
# density with location 0 and scale 1. Compare the density estimate with the true density and
# argue which kernel density estimate seems preferable.
# Tip: first explore the functions h opt & CV.

sample34 <- scan("sample34.txt")

hist34 <- hist(sample34, prob = T)

h_opt34 <- h_opt(sample34)

h_vec <- seq(0.001, 3.0, by = 0.005)

cv_crit <- sapply(h_vec, CV, sample = sample34, kernel = "gauss")
h_opt34_cv <- h_vec[which(cv_crit == min(cv_crit))]

par(mfrow = c(1, 3))

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

hist(sample34, prob = T, ylim = c(0, max(density(sample34)$y)))
lines(x34, dense34$y, col = "red")

hist(sample34, prob = T, ylim = c(0, max(density(sample34)$y)))
lines(density(sample34), col = "red")

hist(sample34, prob = T, ylim = c(0, max(density(sample34)$y)))
lines(x34_cv, dense34_cv$y, col = "red")


#3.5

# The functions bootstrap (in functions Ch5.txt), rt, mad could be useful for the following
# exercise.
# Exercise 3.5 (partially .RData file hand-in) One sample drawn from a t-distribution
# with unknown degrees of freedom k > 0 is stored in the file t-sample.txt. With the help
# of this sample, we would like to estimate the distribution of the median absolute deviation
# statistic (see e.g. syllabus).

tsample <- scan("t-sample.txt")
par(mfrow = c(1,3))

# a. Compute the median absolute deviation based on the given t-sample.

mad_sample = mad(tsample)

# b. Set the seed to 20220302 + [GROUP NO]. Then use the empirical bootstrap method ap-
#   plied to the t-sample to generate B = 2000 bootstrap estimates of the median absolute
# deviation statistic. Store these in a vector mad empBS in your R environment.

set.seed(20220302 + 1)

B = 2000
mad_empBS = numeric(B)
for (i in 1:B) {
  xstar_emp <- sample(tsample, replace = TRUE)
  mad_empBS[i] = mad(xstar_emp)
}

hist(mad_empBS, prob = T)
abline(v = mean(mad_empBS),
       col = "blue",
       lwd = 2)
sd(mad_empBS)


# c. Repeat the steps of part b. (including setting back the seed) but with the parametric
# bootstrap instead of the empirical bootstrap. Use ˆk = 2s2/(s2 −1) as an estimator of
# the degrees of freedom k, where s2 denotes the sample variance. Call the vector which
# contains the obtained parametrically bootstrapped statistics mad parBS.

set.seed(20220302 + 1)

mad_parBS = numeric(B)
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

# d. Plot two separate histograms of the bootstrap samples obtained in b. and c.
# Compare them to another histogram for the true distribution of the median absolute
# deviation. One can obtain this in the following way:
#   Set the seed to 20220302 + [GROUP NO]. Then generate 2000 independent samples of
# size 100 from the t-distribution with 10 degrees of freedom (which is the true un-
# derlying distribution by the way!), and compute the median absolute deviation for
# each sample; then store those 2000 realizations of the median absolute deviations in
# the vector mad realizations and plot their histogram. Next to this, also plot the
# histograms of mad empBS and mad parBS, and compare them with the histogram of
# mad realizations.
# Based on these comparisons, which bootstrap method seems preferable in the present
# context? Motivate your answer.

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

# e. Use the empirical and the parametric bootstrap samples to find estimates of the vari-
#   ance of the median absolute deviation statistic. Compare these two estimates with an
# approximation of the true variance of that statistic, which could be obtained as the
# sample variance of the realizations from part d.

#empirical

# var_empBS = numeric(B)
# for(i in 1:B){
#   mad_xstar_emp <- sample(xstar_emp, replace = T)
#   var_empBS[i] = var(mad_xstar_emp)
# }
# 
# hist(var_empBS, prob = T)
# abline(v = mean(var_empBS),
#        col = "blue",
#        lwd = 2)
# sd(var_empBS)

var_empBS = var(mad_empBS)

# parametric

# var_parBS = numeric(B)
# for(i in 1:B){
#   mad_xstar_par <- sample(xstar_par, replace = T)
#   var_parBS[i] = var(mad_xstar_par)
# }
# 
# hist(var_parBS, prob = T)
# abline(v = mean(var_parBS),
#        col = "blue",
#        lwd = 2)
# sd(var_parBS) = 0

var_parBS = var(mad_parBS)
  
var_realizations = var(mad_realizations)

# Hand in:
#   For the the main report: from part d.: relevant plots, descriptions, and motivated answers.
# Stored in your .RData file: from parts a.-e. the following entries of your list mylist in R:
#   a.: mad sample: the value of the median absolute deviation statistic based on the t-sample,
# b.: mad empBS: the vector of 2000 empirically bootstrapped median absolution deviation
# statistics,
# c.: mad parBS: the vector of 2000 parametrically bootstrapped median absolution deviation
# statistics,
# d.: mad realizations: the vector of 2000 realized median absolution deviation statistics,
# e.: var empBS: the variance estimate based on mad empBS,
# var parBS: the variance estimate based on mad empBS,
# var realizations: the variance estimate based on mad realizations.


mylist <- list(
  mad_sample = mad_sample,
  mad_empBS = mad_empBS,
  mad_parBS = mad_parBS,
  mad_realizations = mad_realizations,
  var_empBS = var_empBS,
  var_parBS = var_parBS,
  var_realizations = var_realizations,
  stud_no = c(2695303, 2665825)
)
save(mylist, file = "Assignment3/myfile3_46.RData")
