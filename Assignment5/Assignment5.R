source ("functions_Ch3.txt")
source ("functions_Ch5.txt")

# Exercise 5.1

statgrades <- scan("statgrades.txt")

# a.

m = median(statgrades)

par(mfrow = c(2, 2), pty = 's')
hist(statgrades)
symplot(statgrades)
qqnorm(statgrades)
qqline(
  statgrades,
  distribution = function(p)
    qnorm(p),
  col = "red"
)
boxplot(statgrades, main = "Boxplot for statgrades")

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

# b.

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

# c.

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

# Exercise 5.2
clouds <- read.table("clouds.txt", header = T)
unseeded_clouds <- clouds$unseeded.clouds

# a.
summary(unseeded_clouds)
var(unseeded_clouds)

par(mfrow = c(3, 2), pty = 's')
hist(unseeded_clouds, prob = T)
symplot(unseeded_clouds)
qqnorm(unseeded_clouds)
qqline(
  unseeded_clouds,
  distribution = function(p)
    qnorm(p),
  col = "red"
)
qqexp(unseeded_clouds)
qqline(
  unseeded_clouds,
  distribution = function(p)
    qexp(p),
  col = "red"
)
boxplot(unseeded_clouds, main = "Boxplot unseeded clouds")


# b.

sd_sample <- sd(unseeded_clouds)

# c.

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

# d.

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

# e.

sd_zstar = sd_empBS - sd_sample

c(sd_sample - quantile(sd_zstar, 0.975),
  sd_sample - quantile(sd_zstar, 0.025))

mad_zstar = mad_empBS - mad_sample

c(mad_sample - quantile(mad_zstar, 0.975),
  mad_sample - quantile(mad_zstar, 0.025))

# g.

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

# h.
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

1 - pbinom(18 - 1, length(unseeded_clouds) - 1, 0.5)
1 - pbinom(10 - 1, length(unseeded_clouds) - 1, 0.5)

#(26.1,87]

# Exercise 5.3

newcomb <- scan("newcomb.txt")
hist(newcomb)

# a.

par(mfrow = c(3, 2), pty = 's')
p = seq(0, 1, 0.1)

first <- newcomb[1:20]
last <- newcomb[21:66]

hist(first, prob = T)
hist(last, prob = T)
boxplot(first, last, main = "Boxplot for first 20 and last 46 obs")
boxplot(first, last, main = "Boxplot for first 20 and last 46 obs", ylim = c(10, max(newcomb)))


qqnorm(first)
qqline(
  first,
  distribution = function(p)
    qnorm(p),
  col = "red"
)
qqnorm(last)
qqline(
  last,
  distribution = function(p)
    qnorm(p),
  col = "red"
)

#tests

mean(first)
mean(last)

median(first)
median(last)

# 2 sample ks test for difference shape
ks.test(first, last, alternative = "greater")

# 2 sample wilcoxon test for discovering difference in location

wilcox.test(first, last, alternative = "less")

# b.
par(mfrow = c(1, 1))
hist(newcomb, probability = T)

newcomb_median <- median(newcomb)

set.seed(2695303 + 46)
newcomb_median_BS <- bootstrap(newcomb, statistic = median, B = B)
newcomb_zstar = newcomb_median_BS - newcomb_median

CI <- c(
  newcomb_median - quantile(newcomb_zstar, 0.975),
  newcomb_median - quantile(newcomb_zstar, 0.025)
)
CI

CI[1] <= (24.8332 - 24.8) * 1000 && (24.8332 - 24.8) * 1000 <= CI[2]


mylist <- list(
  stud_no = c(2695303),
  "1_a_pval" = pval_1a,
  "1_a_reject" = reject_1a,
  "1_b_pval" = pval_1b,
  "1_b_reject" = reject_1b,
  "1_c_pval" = pval_1c,
  "1_c_reject" = reject_1c,
  "2_b_sd" = sd_sample,
  "2_d_mad" = mad_sample,
  "2_h_CI_sign" = c(26.1, 87),
  "2_h_CI_wilcox" = c(36.69997, 187.24999),
  "2_h_CI_t" = c(52.09509, 277.02876)
)
save(mylist, file = "Assignment5/myfile5_46.RData")
