##GET CHANGED. 
## Again.

## Set wd
setwd("~/Documents/QMSS/Fall '22/Lab 5")

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stargazer)
gss <- read.csv(file.choose()) ## use GSS.2006.csv

# Question 1.

#Code new variable -- grad_school
gss$grad_school = NA
gss$grad_school[gss$educ>16 ] <- 1
gss$grad_school[gss$educ<=16 ] <- 0

#Recode sex
gss$women = ifelse((gss$sex == 2), 1, 0) ## binary recode

#Recode income
gss$inc_belowavg = NA
gss$inc_belowavg[gss$incom16 < 3] <- 1
gss$inc_belowavg[gss$incom16 >= 3] <- 0

lm1 = lm(grad_school ~ inc_belowavg + women, data=gss)
summary(lm1)

# Question 2.

logit1 = glm(grad_school ~ inc_belowavg + women, data=gss, family=binomial)
summary(logit1)

# Question 3

exp(coef(logit1))




