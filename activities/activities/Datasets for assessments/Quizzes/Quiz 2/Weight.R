# Author: Yew Toh Wong 
# Date: 23 June 2026
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
descriptives(data=data, vars=weight, se=TRUE, ci=TRUE)
t.test(data$weight, conf.level = 0.95)

descriptives(data=data, vars=height, se=TRUE, ci=TRUE)
t.test(data$height, conf.level = 0.95)
