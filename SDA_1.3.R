x = c(23,0.1,-5.15)
x = c(x,-28)
y = sort(x, decreasing = F)
x = x*4
x = round(x, digits = 1)
x[x>0]
x = replace(x, 3, 7)
z = seq(2,4,0.1)
m = matrix(z, nrow = 7, ncol = 3)
m[2,1]
m[,3]
mean(m)
apply(m, 2, mean)
