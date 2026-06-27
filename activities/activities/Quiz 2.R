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
bp <- read.csv("mod04_blood_pressure.csv")
t_test <- t.test(bp$dbp, mu = 71, conf.level = 0.99)
t_test

# default (2-sided test)
t_std <- t.test(bp$dbp, mu = 71)
t_std

# Activities 4.1 ---------------------------------------------------------
# Average Australian 8700KJ/d
# Average male aged 30-50 9461KJ (ci:8879-10043)
# The average male consumed more than the average Australian but ci cannot quantify

# Activity 4.2 -----------------------------------------------------------
# read Activity_4.2.csv
list.files()
health <- read.csv("Activity_4.2.csv")

# Australian female waist circumference 87.9cm
# Compare the GP clinic sample 
t.health <- t.test(health$waist, mu=87.9)
t.health
