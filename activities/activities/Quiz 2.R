# Author: Yew Toh Wong 
# Date: 28 June 2026
# Purpose: Week_4_Activities

# library used 
library(skimr)
library(jmv)
library(ggplot2)
install.packages("devtools")
library(devtools)
install_github("raviselker/surveymv")
library("surveymv")
library(dplyr)
library(readxl)

# Read mod04_blood_pressure.csv
list.files()
data <- read.csv("Quiz2.csv")
t_test <- t.test(data$weight, conf.level = 0.95)
t_test

# alternative use descriptive
descriptives(data=data, vars=weight, ci=TRUE, se=TRUE)

