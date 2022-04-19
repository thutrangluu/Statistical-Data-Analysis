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

par(mfrow = c(2, 2))
hist(statgrades)
symplot(statgrades)
qqnorm(statgrades)
qqline(statgrades, distribution = function(p)
  qnorm(p), col="red")
boxplot(statgrades)

n51 <- length(statgrades)

alpha_1a = 0.01

x51a <- sum(statgrades > 6.2)
x51a1 <- sum(statgrades == 6.2)

binom.test(
  x51a,
  n51 - x51a1,
  alt = "less",
  conf.level = 1 - alpha_1a,
  p = 0.5
)

pval_1a = binom.test(
  x51a,
  n51 - x51a1,
  alt = "less",
  conf.level = 1 - alpha_1a,
  p = 0.5
)$p.value
reject_1a = pval_1a < alpha_1a

# b. Test H0 : m = 6 against Ha: m 6= 6 at level α = 5%.

alpha_1b = 0.05

x51b <- sum(statgrades > 6)
x51b1 <- sum(statgrades == 6)

binom.test(
  x51b,
  n51 - x51b1,
  alt = "two.sided",
  conf.level = 1 - alpha_1b,
  p = 0.5
)

pval_1b = binom.test(
  x51b,
  n51 - x51b1,
  alt = "two.sided",
  conf.level = 1 - alpha_1b,
  p = 0.5
)$p.value
reject_1b = pval_1b < alpha_1b

# c. Denote by p the probability to get a grade of at least 5.5.
# Test H0 : p ≤45% against Ha: p > 45% at level α = 10% .

alpha_1c = 0.1

x51c <- sum(statgrades > 5.5)
x51c1 <- sum(statgrades == 5.5)

binom.test(
  x51c,
  n51 - x51c1,
  alt = "greater",
  conf.level = 1 - alpha_1c,
  p = 0.45
)

pval_1c = binom.test(
  x51c,
  n51 - x51c1,
  alt = "greater",
  conf.level = 1 - alpha_1c,
  p = 0.45
)$p.value
reject_1c = pval_1c < alpha_1c

# Exercise 5.2 (partially .RData file hand-in) With cloud seeding a small airplane is
# used to add a particular substance to clouds in order to change the precipitation properties2.
# In a cloud seeding experiment in 1975, precipitation values of two groups of clouds, seeded
# and unseeded, were compared. The precipitation data of this experiment are contained in the
# file clouds.txt. In this exercise, we only focus on the data set unseeded.

clouds <- read.table("clouds.txt", header = T)
unseeded_clouds <- clouds$unseeded.clouds

# a. Investigate the data graphically and numerically.3

summary(unseeded_clouds)

par(mfrow = c(2, 2), pty = 's')
hist(unseeded_clouds, prob = T)
symplot(unseeded_clouds)
qqnorm(unseeded_clouds)
qqline(unseeded_clouds, distribution = function(p)
  qnorm(p), col="red")
qqexp(unseeded_clouds)
qqline(unseeded_clouds, distribution = function(p)
  qexp(p), col="red")

boxplot(unseeded_clouds)

## the data was investigated numerically ang graphically.
## The numerical summary is shown in the following table:

## we explore the data graphically with the 4 graphs: histogram, symplot, qq plot and boxplot

## from the graphical and numerical summaries, we can clearly see that the data is right skewed  

# b. Determine the sample standard deviation of the data set; this is often used as a measure
# for the accuracy of the measurements.

sd_sample <- sd(unseeded_clouds)

# c. Determine a bootstrap estimate of the standard deviation of the estimator of accuracy
# used in part b.4

B = 2000

set.seed(2695303 + 46)

sd_empBS <- bootstrap(unseeded_clouds, statistic = sd, B = B)

sd_b_empBS <- sd(sd_empBS)

set.seed(2695303 + 46)

sd_parBS <-
  replicate(B, sd(rexp(
    length(unseeded_clouds), rate = 1 / mean(unseeded_clouds)
  )))

sd_b_parBS <- sd(sd_parBS)

# d. Repeat parts b and c, but now use as a measure for the accuracy of the measurements
# the MAD which is a more robust estimator for spread.4

mad_sample <- mad(unseeded_clouds)

set.seed(2695303 + 46)

mad_empBS <- bootstrap(unseeded_clouds, statistic = mad, B = B)

sd_d_empBS <- sd(mad_empBS)

set.seed(2695303 + 46)

mad_parBS <-
  replicate(B, mad(rexp(
    length(unseeded_clouds), rate = 1 / mean(unseeded_clouds)
  )))

sd_d_parBS <- sd(mad_parBS)

# e. Which estimator for the accuracy do you prefer for these data, the sample standard
# deviation or the MAD? Explain how you reached your conclusion.

# mad bc smaller sd and mad is not effected by outliers

################## confidence intervals? bs ci

# f. Which test do you prefer for testing the location of the precipitation values of the
# unseeded clouds: the t-test, the sign test, or the signed rank test? Motivate your
# answer.

# use sign test because it required weaker assumption about the underlying distribution
# t-test assume normality while signed rank test assume symmetry. But clearly from (a)
# the distribution is neither normal nor symmetric.

# g. Test whether the location of the precipitation value of the unseeded clouds is less than
# 40 by using the test your preferred in part f. Take the significance level α = 0.05.

# test median < 40

alpha_2g = 0.05

x52g <- sum(unseeded_clouds > 40)
x52g1 <- sum(unseeded_clouds == 40)

binom.test(
  x52g,
  length(unseeded_clouds) - x52g1,
  alt = "greater",
  conf.level = 1 - alpha_2g,
  p = 0.5
)

# h. Make two-sided 95% confidence intervals for the location of the precipitation value of
# unseeded clouds based on the sign test, the signed rank test, and the t-test.
# Note: Warning messages from the wilcox.test command may be ignored.

median(unseeded_clouds)

t.test(
  unseeded_clouds,
  mu = 40,
  alternative = "two.sided",
  conf.level = 0.95
)

wilcox.test(
  unseeded_clouds,
  mu = 40,
  alternative = "two.sided",
  conf.level = 0.95,
  conf.int = T
)

rbind(0:length(unseeded_clouds), round(pbinom(
  0:length(unseeded_clouds),
  size = length(unseeded_clouds),
  p = 0.5
), 3))

rbind(0:length(unseeded_clouds), round(1 - pbinom((0:length(unseeded_clouds)) -
                                                    1,
                                                  size = length(unseeded_clouds),
                                                  p = 0.5
), 3))

# T= 9,...,18 not reject

sort(unseeded_clouds)

# 0.01    4.90    4.90   11.50   17.30   21.70   24.40   26.10   26.30   28.60   29.00   36.60   41.10
# 47.30   68.50   81.50   87.00   95.00  147.80  163.00  244.30  321.20  345.50  372.40  830.10 1202.60

#m<87
#m>26.1

# conditional tests???

1-pbinom(18-1,length(unseeded_clouds)-1,0.5)
1-pbinom(10-1,length(unseeded_clouds)-1,0.5)

#(26.1,87]

# i. Which confidence interval from h. do you value most, and why?

# from sign test, bc is the smallest


# Exercise 5.3 In 1882 S. Newcomb performed a series of experiments to determine the speed
# of light with the method of Foucault. Light bounced from a fast rotating mirror in Fort Meyer
# on the west bank of the Potomac in Washington to a fixed mirror at the foot of Washington
# monument, and back to the rotating mirror. The speed of the light was calculated from the
# measured distance between the mirrors (3721 meter) and the deflection angle of the emitted
# and received light on the rotating mirror. The file newcomb.txt contains Newcomb’s data;
# the given values times 10−3 plus 24.8 are the original observations of the time (in sec−6) the
# light needed for traveling twice the 3721 meters.

newcomb <- scan("newcomb.txt")
hist(newcomb)

# a. Investigate whether there is a difference between the first 20 and the last 46 observations.
# Do this by making suitable graphical summaries, by determining an estimate of the
# difference, and by performing one or more suitable tests at level α = 5%.

par(mfrow = c(2,2))
p = seq(0,1,0.1)

first <- newcomb[1:20]
last <- newcomb[21:66]

hist(first, prob=T)
hist(last,prob=T)
boxplot(first)
boxplot(last)

qqnorm(first)
qqline(first,  distribution = function(p)
  qnorm(p), col="red")
qqnorm(last)
qqline(last,  distribution = function(p)
  qnorm(p), col="red")

# Explanation: 1. The graphical summaries should visualize the datasets and make them
# comparable. Possibly choose the same scales for the axes.
# 2. Choose a suitable estimator and a suitable test. Motivate your choice in one or two
# sentences.

mean(first)
mean(last)

median(first)
median(last)

# 2 sample ks test for difference shape 
ks.test(first,last,alternative="greater")

# 2 sample wilcoxon test for discovering difference in location

wilcox.test(first,last, alternative = "less")

# b. Determine a 95% confidence interval for the traveling time of the light in Newcomb’s
# experiment. According to present day physics the true value of the traveling time is
# equal to 24.8332 (in sec−6). Is this value in the computed confidence interval? What
# is your conclusion? Explanation: choose a suitable location estimator. Motivate your
# choice in one or two sentences.
# Note: to compare the given true travling time to the confidence intervals based on the
# data, you should subtract 24.8 from the true time and then multiply the result with 1000.
# Hand in: relevant plots and numbers and your answers to the questions




mylist <- list(stud_no = c(2695303),
  "1_a_pval" = pval_1a,
  "1_a_reject" = reject_1a,
  "1_b_pval" = pval_1b,
  "1_b_reject" = reject_1b,
  "1_c_pval" = pval_1c,
  "1_c_reject" = reject_1c,
  "2_b_sd" = sd_sample,
  "2_d_mad" = mad_sample,
  "2_h_CI_sign" = c(26.1,87),
  "2_h_CI_wilcox" = c(36.69997, 187.24999),
  "2_h_CI_t" = c(52.09509, 277.02876)
)
save(mylist, file = "Assignment5/myfile5_46.RData")
