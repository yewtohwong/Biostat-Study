# Author: Yew Toh Wong
# Date 15 June 2026
# Purpose: Worked examples and activities 

# library used 
library(skimr)
library(jmv)
library(ggplot2)
install.packages("devtools")
library(devtools)
install_github("raviselker/surveymv")
library("surveymv")
library(dplyr)

# Probability that a person drawn from the population will have a DBP greater than 80
# Population mean = 70
# SD = 12mmhg
# p(X>80)

# Probability that a normally distributed variable is greater and equal to 80, assuming the distribution has mean 70 and standard deviation 12.
p80 <- pnorm(80, mean=70, sd=12, lower.tail = FALSE)
p80

# Probability that a person will have BP less than 65
p65 <-pnorm(65, mean=70, sd=12, lower.tail = TRUE)
p65

# Read diastolic blood pressure file in example module 3
list.files()
dbp <- read.csv("mod03_blood_pressure.csv")
descriptives(data=dbp, vars=dbp, ci=TRUE)

# Finding CI from summarised data
# width is the CI level, e.g. 0.95.
# 1 - width is the total alpha (e.g. 0.05).
# (1 - width)/2 is alpha/2 (e.g. 0.025 in each tail).
# 1 - (1 - width)/2 gives the upper tail probability (e.g. 0.975).
# qt() is the t‑quantile function in R: it returns the t value such that 𝑃(𝑇≤𝑡crit)= 1−(1−width)/2
# df = n - 1 sets the degrees of freedom for the t‑distribution.
# lcl - lower confidence limit
# ucl - upper confidence limit

ci_mean <- function(n, mean, sd, width = 0.95, digits = 3){ 
  tcrit <- qt(1 - (1 - width)/2, df = n - 1) 
  lcl <- mean - tcrit * sd/sqrt(n) 
  ucl <- mean + tcrit * sd/sqrt(n) 
  return( c(lower = round(lcl, digits), 
            upper = round(ucl, digits)) )
}
ci_mean(n = 242, mean = 128.4, sd = 19.56, width = 0.95)


