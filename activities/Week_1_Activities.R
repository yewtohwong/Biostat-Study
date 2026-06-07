# Author: Yew Toh Wong
# Date: 5 June 2026
# Purpose: PHCM 9795 Biostat Week 1 Activites

# Activity 1.1 ----
activity1.1 <- readRDS("Activity_1.1.rds")

# Density plot
# important to put the xlab and main on separate lines of codes
plot(density(activity1.1$weightloss),
     xlab="Weight loss (g)",
     main="Figure 1: Weight loss for 25 participants")

# Need to install and load jmv package
library(jmv)
descriptives(data=activity1.1, vars=weightloss)

# Activity 1.3 ----
DBP <- read.csv("Activity_1.3.csv")
descriptives(DBP, vars=dbp, pc=TRUE)

# Activity 1.4 ----
# Box plot can be done within descriptives with box
age <- readRDS("Activity_1.4.rds")
descriptives(age, vars = age, pc=TRUE, dens=TRUE, box=TRUE)

# Load clean data 
age.clean <- readRDS("Activity_1.4_clean.rds")
descriptives(age.clean, vars = age, pc=TRUE, dens=TRUE, box=TRUE)
