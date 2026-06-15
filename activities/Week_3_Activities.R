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
