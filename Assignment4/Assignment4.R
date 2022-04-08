source("functions_Ch3.txt")
source("functions_Ch5.txt")

p = seq(0, 1, by = 0.05)
B = 5000

# Exercise 4.1 
birthweight <- scan("birthweight.txt")

# a.

par(mfrow = c(2,2), pty = 's')

hist(birthweight, prob = T)
abline(v = mean(birthweight),
       col = "blue",
       lwd = 2)
abline(v = median(birthweight),
       col = "red",
       lwd = 2)

boxplot(birthweight)

qqnorm(birthweight)
qqline(
  birthweight,
  distribution = function(p)
    qnorm(p),
  col = "red",
)

qqnorm(birthweight, main = "Normal QQ-Plot N(0,1.05)")
qqline(
  birthweight,
  distribution = function(p)
    qnorm(p, sd = 1.05),
  col = "red",
)

# b.

quantile(birthweight, 0.1)

## Empirical BS
set.seed(2695303 + 46)
quant_empBS <-
  bootstrap(birthweight,
            statistic = quantile,
            B = B,
            probs = 0.1)

mad_quant_emp <- mad(quant_empBS)

## Parametric BS

set.seed(2695303 + 46)
quant_parBS <- numeric(B)
for (i in 1:B) {
  xstar_par <- rnorm(length(birthweight), mean = mean(birthweight), sd = sd(birthweight))
  quant_parBS[i] = quantile(xstar_par, probs = 0.1)
}

mad_quant_par <- mad(quant_parBS)

# c. 
set.seed(2695303 + 46)
quant_parBSc <- numeric(B)
for (i in 1:B) {
  xstar_par <- rexp(length(birthweight), rate = 1 / mean(birthweight))
  quant_parBSc[i] = quantile(xstar_par, probs = 0.1)
}

mad_quant_parc <- mad(quant_parBSc)

# d. 

set.seed(2695303 + 46)
quant_parBSd <-
  replicate(B, quantile(rexp(length(birthweight), rate = 1 / mean(birthweight)),
                     probs = 0.1))
mad_quant_pard <- mad(quant_parBSd)

# Exercise 4.2 
source("thromboglobulin.txt")
PRRP = thromboglobulin$PRRP
SDRP = thromboglobulin$SDRP

# a. 

PRRP_mean <- mean(PRRP)

set.seed(2695303 + 46)
PRRP_BSmean <- bootstrap(PRRP, statistic = mean, B = B)
PRRP_zstar_a = PRRP_BSmean - PRRP_mean

c(PRRP_mean - quantile(PRRP_zstar_a, 0.95),
  PRRP_mean - quantile(PRRP_zstar_a, 0.05))

# b. Repeat part a with the median instead of the mean.

PRRP_median <- median(PRRP)

set.seed(2695303 + 46)
PRRP_BSmedian <- bootstrap(PRRP, statistic = median, B = B)
PRRP_zstar_b = PRRP_BSmedian - PRRP_median

c(
  PRRP_median - quantile(PRRP_zstar_b, 0.95),
  PRRP_median - quantile(PRRP_zstar_b, 0.05)
)

# d.

SDRP_mean = mean(SDRP)

diff_mean = SDRP_mean - PRRP_mean

set.seed(2695303 + 46)
PRRP_BS <- bootstrap(PRRP, statistic = mean, B = B)

set.seed(2695303 + 46)
SDRP_BS <- bootstrap(SDRP, statistic = mean, B = B)

zstar_d = (SDRP_BS - PRRP_BS) - diff_mean

c(diff_mean - quantile(zstar_d, 0.95),
  diff_mean - quantile(zstar_d, 0.05))

par(mfrow = c(1,1), pty = 's')
hist(zstar_d, prob = T)
abline(v = mean(zstar_d),
       col = "blue",
       lwd = 2)

# Exercise 4.3
source("light.txt")

light_1879 <- light$`1879`
light_1882 <- light$`1882`

hist(light_1879)
hist(light_1882)

Dn <- function(x) {
  f <- ecdf(x)
  y <- numeric(length(x))
  for (i in 1:length(x)) {
    y [i] = abs(f(x[i]) - pnorm((x[i]-mean(x))/ sd(x)))
  }
  return (max(y))
}

#b.

## 1879

D_1879 <-
  ks.test(light_1879, "pnorm", mean(light_1879), sd(light_1879))$statistic

set.seed(2695303 + 46)

D_1879BS <- numeric(B)
for (i in 1:B) {
  xstar_1879 = rnorm(length(light_1879))
  D_1879BS[i] = Dn(xstar_1879)
}

p_1879= sum(D_1879BS>=D_1879)/B
p_1879

ks.test(light_1879, "pnorm", mean(light_1879), sd(light_1879))

## 1882

D_1882 <-
  ks.test(light_1882, "pnorm", mean(light_1882), sd(light_1882))$statistic

set.seed(2695303 + 46)

D_1882BS <- numeric(B)
for (i in 1:B) {
  xstar_1882 = rnorm(length(light_1882))
  D_1882BS[i] = Dn(xstar_1882)
}

p_1882=sum(D_1882BS>=D_1882)/B
p_1882

ks.test(light_1882, "pnorm", mean(light_1882), sd(light_1882))
