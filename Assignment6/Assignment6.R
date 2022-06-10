source("functions_Ch7.txt")
library ("mvtnorm")


# Exercise 6.1 

expensescrime <- read.table("expensescrime.txt", header = T)

expensescrime = expensescrime[,-1]
head(expensescrime)

#a
par(pty = "s", mfrow=c(1,1))
pairs(expensescrime)

# b.
attach(expensescrime)
par(pty="m")
sum(pop>4000)
expend_rate = expend/pop
plot(expend_rate[pop>4000],crime[pop>4000])

crime_4000 = crime[pop>4000]
expend_rate_4000 = expend_rate[pop>4000]

# c.

cor.test(crime_4000,expend_rate_4000,method="k",conf.level = 0.95)

cor.test(crime_4000,expend_rate_4000,method="s",conf.level = 0.95)

# d. 

B=5000
t_61d=cor.test(crime_4000,expend_rate_4000,method="s", conf.level = 0.95)[[1]]

set.seed(2695303+46)
t_perm = numeric(B)
for(i in 1:B) {
  y = sample(expend_rate_4000, replace = F)
  t_perm[i] = cor.test(crime_4000,y, method="s",conf.level = 0.95)[[1]]
}

pl_61d = sum(t_perm >= t_61d)/B
pr_61d = sum(t_perm <= t_61d)/B
p_61d =2*min(pl_61d, pr_61d)

p_61d 

#p<0.05 so reject

# e.  

set.seed(2695303+46)

aresimulation = function (B, n, df) {
  p_Kendall = p_Spearman = numeric(B)
  for (i in 1:B) {
    x = rmvt(n, sigma=matrix(c(1,0.3,0.3,1), 2,2), df=df)
    x1 = x[,1]
    x2 = x[,2]
    p_Kendall [i] = cor.test(x1, x2, method = "k")[[3]]
    p_Spearman [i] = cor.test(x1, x2, method ="s")[[3]]
  }
  power_Kendall = sum(p_Kendall < 0.05)/B
  power_Spearman = sum(p_Spearman < 0.05)/B
  rbind(c("kendall","spearman"), c(power_Kendall, power_Spearman))
}

B=8000
sim48 = aresimulation(B,n=48,df=6)
sim50 = aresimulation(B,n=50,df=6)
sim52 = aresimulation(B,n=52,df=6)

as.numeric(sim48[2,1])/as.numeric(sim50[2,2])
as.numeric(sim52[2,1])/as.numeric(sim50[2,2])

###s
#k1.04 k is more powerfull than s


# Exercise 6.2 
# a. 

m62 = matrix(c(30, 17, 1067, 1120),nrow=2,ncol=2)
m62

fisher.test(m62, alternative = "t", conf.level = 0.95)

# b. 
fisher.test(m62, alternative = "g", conf.level = 0.95)

# c.

n11_62 = 30
n_62 = 30+1067+17+1120
n1_62 = 30+1067
n2_62 = n_62 - n1_62
n3_62 = 30+17

pl_62b = phyper(n11_62,n1_62,n2_62,n3_62)
pr_62b = 1-phyper(n11_62-1,n1_62,n2_62,n3_62)
c(pl_62b, pr_62b)
2*min(pl_62b, pr_62b)

# Exercise 6.3 
nausea = read.table("nausea.txt")
nausea[,1] = nausea[,1] - nausea[,2]
nausea

#   b.

nausea_chisq = chisq.test(nausea)
nausea_chisq
nausea_chisq$expected

# c.

chisq.test(nausea,simulate.p.value = T)

# d. 

# contribution

nausea_chisq$residuals

# stdres
round(nausea_chisq$stdres,2)

# e. 

t_63 = maxcontributionscat(nausea)

nausea_bs = bootstrapcat(nausea,B=5000,maxcontributionscat)
mean(nausea_bs>=t_63)

# f.

Chlorpromazine = nausea[c(1,2),]

fisher.test(Chlorpromazine,alternative = "l")
