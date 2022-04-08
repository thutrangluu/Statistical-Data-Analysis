source ("functions_Ch3.txt")
source ("functions_Ch5.txt")

# Exercise 5.1 (.RData file hand-in) This exercise is about the grades for one of the resit
# exams for a statistics course for the Computer Science students. The data can be found in the
# file statgrades.txt. We assume that these data are a random sample that is representative
# for the grades from resit exams in any other year the statistic course took place. Denote by
# m the unknown median of the unknown true grades distribution.

statgrades <- scan("statgrades.txt")

# a. Test H0 : m ≥6.2 against Ha: m < 6.2 at level α = 1%.

m = median(statgrades)

par(mfrow=c(1,4))
hist(statgrades)
symplot(statgrades)
qqnorm(statgrades)
boxplot(statgrades)

n51 <- length(statgrades)

alpha_a = 0.01

x51a <- sum(statgrades>6.2)
x51a1 <- sum(statgrades==6.2)

binom.test(x51a,n51-x51,alt = "less",conf.level = 1-alpha_a, p=0.5)

"1_a_pval" = binom.test(x51a,n51-x51,alt = "less",conf.level = 1-alpha_a, p=0.5)$p.value
"1_a_reject" = binom.test(x51a,n51-x51,alt = "less",conf.level = 1-alpha_a, p=0.5)$p.value < 0.01

# b. Test H0 : m = 6 against Ha: m 6= 6 at level α = 5%.

alpha_b = 0.05

x51b <- sum(statgrades>6)
x51b1 <- sum(statgrades==6)

binom.test(x51b,n51-x51b1,alt = "two.sided",conf.level = 1-alpha_b, p=0.5)

"1_b_pval" = binom.test(x51b,n51-x51b1,alt = "two.sided",conf.level = 1-alpha_b, p=0.5)$p.value
"1_b_reject" = binom.test(x51b,n51-x51b1,alt = "two.sided",conf.level = 1-alpha_b, p=0.5)$p.value < 0.05

# c. Denote by p the probability to get a grade of at least 5.5.
# Test H0 : p ≤45% against Ha: p > 45% at level α = 10% .

alpha_c = 0.1

x51c <- sum(statgrades>5.5)
x51c1 <- sum(statgrades==5.5)

binom.test(x51c,n51-x51c1,alt="greater",conf.level = 1-alphac, p=0.45)

"1_c_pval" = binom.test(x51c,n51-x51c1,alt="greater",conf.level = 1-alpha_c, p=0.45)$p.value 
"1_c_reject" = binom.test(x51c,n51-x51c1,alt="greater",conf.level = 1-alpha_c, p=0.45)$p.value < alpha_c

# Hand in (all stored in your .RData file): the following entries of your list mylist in R:
# a.: "1 a pval": p-value of the test in a. i.e. a single number between 0 and 1,
# "1 a reject": boolean indicating rejection of H0 i.e. TRUE or FALSE (not as a string),
# b.: "1 b pval": p-value of the test in b. i.e. a single number between 0 and 1,
# "1 b reject": boolean indicating rejection of H0 i.e. TRUE or FALSE (not as a string),
# c.: "1 c pval": p-value of the test in c. i.e. a single number between 0 and 1,
# "1 c reject": boolean indicating rejection of H0 i.e. TRUE or FALSE (not as a string).

# Exercise 5.2 (partially .RData file hand-in) With cloud seeding a small airplane is
# used to add a particular substance to clouds in order to change the precipitation properties2.
# In a cloud seeding experiment in 1975, precipitation values of two groups of clouds, seeded
# and unseeded, were compared. The precipitation data of this experiment are contained in the
# file clouds.txt. In this exercise, we only focus on the data set unseeded.

# a. Investigate the data graphically and numerically.3

# b. Determine the sample standard deviation of the data set; this is often used as a measure
# for the accuracy of the measurements.

# c. Determine a bootstrap estimate of the standard deviation of the estimator of accuracy
# used in part b.4

# d. Repeat parts b and c, but now use as a measure for the accuracy of the measurements
# the MAD which is a more robust estimator for spread.4

# e. Which estimator for the accuracy do you prefer for these data, the sample standard
# deviation or the MAD? Explain how you reached your conclusion.

# f. Which test do you prefer for testing the location of the precipitation values of the
# unseeded clouds: the t-test, the sign test, or the signed rank test? Motivate your
# answer.

# g. Test whether the location of the precipitation value of the unseeded clouds is less than
# 40 by using the test your preferred in part f. Take the significance level α = 0.05.
# h. Make two-sided 95% confidence intervals for the location of the precipitation value of
# unseeded clouds based on the sign test, the signed rank test, and the t-test.
# Note: Warning messages from the wilcox.test command may be ignored.


# i. Which confidence interval from h. do you value most, and why?


#   Hand in:
#   In your main report: Results of parts a, c, d, and g, and your answers to parts e, f, and i.
# Stored in your .RData file:
#   b.: "2 b sd": the sample standard deviation based on the unseeded dataset i.e. a single
# positive number,
# d.: "2 d mad": the median absoulte deviation based on the unseeded dataset i.e. a single
# positive number,
# h.: "2 h CI sign": confidence interval for the location parameter based on the sign test
# i.e. an interval specified by a vector consisting of the lower and upper boundary,
# h.: "2 h CI wilcox": confidence interval for the location parameter based on the signed
# rank test i.e. an interval specified by a vector consisting of the lower and upper boundary,
# h.: "2 h CI t": confidence interval for the location parameter based on the t-test i.e. an
# interval specified by a vector consisting of the lower and upper boundary.
