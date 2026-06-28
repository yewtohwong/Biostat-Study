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
summary(health)

# Australian female waist circumference 87.9cm
# Compare the GP clinic sample
# Look at data distribution first 
plot(density(health$waist, na.rm=TRUE),
     main= " Density Plot of Waist Circumference",
     xlab= "Waist Circumference (cm)",
     ylab= "Density")

ggplot(data=health, mapping=aes(x="", y=waist))+
         geom_boxplot()+
         theme_classic()+
         labs(
           title= "Boxplot of Waist Circumference (cm)",
           y="Waist Circumference in cm",
           x=""
         )
       
       
t.health <- t.test(health$waist, mu=87.9, ci=TRUE)
t.health

# alternative use descriptive from jmv
descriptives(data=health, vars=waist, ci=TRUE)
