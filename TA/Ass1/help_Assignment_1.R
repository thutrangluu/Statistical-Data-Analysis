setwd("C:/Users/Dennis/surfdrive/Documents/VU Amsterdam/Lectures/2023 SDA/Assignments/2023/Assignment 1")
military_per_cap <- read.csv("military-spending-per-capita.csv")
head(military_per_cap)

attach(military_per_cap)

military_per_cap_1988 <- military_per_cap[Year==1988,]
military_per_cap_2020 <- military_per_cap[Year==2020,]

head(military_per_cap_2020)

# in USD
ME1988 <- military_per_cap_1988$military_expenditure_per_capita
ME2020 <- military_per_cap_2020$military_expenditure_per_capita

#############################################################

# a)

# insert your own code here


#############################################################

# b)

index_bi_1988 <- which(military_per_cap_1988$Code %in% military_per_cap_2020$Code)
index_bi_2020 <- which(military_per_cap_2020$Code %in% military_per_cap_1988$Code)
   
military_bivariate <- cbind(military_per_cap_1988[index_bi_1988,c(1,2,4)], military_per_cap_2020[index_bi_2020,4])
names(military_bivariate)[3:4] <- c("military_expenditure_1988","military_expenditure_2020")

head(military_bivariate)

# insert your own code here