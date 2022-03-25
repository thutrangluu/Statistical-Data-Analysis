source("functions_Ch3.txt")
source("functions_Ch5.txt")

p = seq(0, 1, 0.05)

# Exercise 4.1 The file birthweight.txt contains data on birth weights (in grams).
birthweight <- scan("birthweight.txt")

# a. Explore the distribution of the birth weight data graphically and find an appropriate distribution
# where the data could have originated from.

hist(birthweight, prob = T)

boxplot(birthweight)

qqnorm(birthweight)
qqline(
  birthweight,
  distribution = function(p)
    qnorm(p),
  col = "red",
)

qqnorm(birthweight)
qqline(
  birthweight,
  distribution = function(p)
    qnorm(p, sd = 1.05),
  col = "red",
)


# b. Estimate the 10%-quantile of birth weights and find a bootstrap estimate of the median absolute
# deviation (“mad”; see syllabus) of the 10% quantile estimator. Motivate and explain your choice
# of bootstrap method.

quantile(birthweight, 0.1)

quant_empBS <-
  bootstrap(birthweight,
            statistic = quantile,
            B = 2000,
            probs = 0.1)

mad_quant <- mad(quant_empBS)

# c. Now, repeat part b but use as a bootstrap method a parametric bootstrap based on an exponential distribution with suitably estimated rate parameter.1 Compare the resulting estimate
# of the median absolute deviation of the 10%-quantile estimator with the one found in part b.
# Refer to the theory of Lecture 5, to explain what went wrong.



# d. Reprogram part c so that everything, i.e. the calculation of the median absolute deviations,
# the realizations of (e.g. B = 1000) bootstrap samples, and the parameter estimation is done in
# exactly one line in R code. Avoid the use of loops.


# NB. You could, for example, use a clever combination of the R functions mad, var, replicate,
# quantile, rexp, mean.
# Hand in: relevant plots, descriptions, results, answers to the questions, and your comments.

# Exercise 4.2 Read Examples 3.4 and 5.4 in the syllabus about data on β-thromboglobulin levels
# which can be loaded by the R-code in thromboglobulin.txt2
# . You can select e.g. the PRRP data using
# R-command thromboglobulin$PRRP or thromboglobulin[[1]]. Or use attach(thromboglobulin)3
# so that the variables PRRP, SDRP and CTRP are defined.

# a. Determine a two-sided 90%-bootstrap confidence interval for the mean of the underlying distribution of PRRP. Take B sufficiently large.


# b. Repeat part a with the median instead of the mean.


# c. Compare the answers of a and b. Which estimator of location do you prefer and why?


#   d. Determine a 90%-bootstrap confidence interval for the difference in mean between the two groups
# SDRP and PRRP. What can you conclude from this interval about the difference in mean of the
# two underlying distributions?
#   Note: this is a two sample problem, like in Example 5.4.
# Hand in: the computed intervals and your answers to parts c and d.
