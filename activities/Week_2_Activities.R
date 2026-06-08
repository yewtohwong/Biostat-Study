# Author: Yew Toh Wong
# Date: 7 June 2026
# Purpose: Week 2 Actvities 

# library used 
library(jmv)
library(ggplot2)
install.packages("devtools")
library(devtools)
install_github("raviselker/surveymv")
library(dplyr)

# Activity 2.1 -----------------------------------------------------------
# Read in data 
list.files()
lbw <- readRDS("Activity_2.1.rds")

# Create 2x2 contingency table for lbw and smoking status
contTables(data=lbw, rows="SMOKE", cols="LOW", pcRow = TRUE)
contTables(data=lbw, rows="SMOKE", cols="LOW", pcCol = TRUE)

# Create stacked bar chart using contTables (not able to have title)
contTables(
  data=lbw, 
  rows="AgeGrp", 
  cols="LOW", 
  barplot=TRUE, 
  bartype="stack", 
  xaxis="xrows"
  )

# Create stacked bar chart using ggplot
# use ggtitles() to create title
# geom_bar(position="fill") will show proportion
ggplot(data = lbw, aes(x = AgeGrp, fill = LOW)) +
  geom_bar(position = "stack") +
  ggtitle("Birth Weight According to Age Group (Years)") +
  xlab("Age Group (Years)") +
  ylab("Counts")

# Smoking is associated with lbw 
# Younger mother <30 yo are associated with lbw

# Activity 2.2 -------------------------------------------------------------
# 90 participants in RCT
# Preference (probability = 0.5)

# 60 or more people prefer the new drug
p60 <- pbinom(59, 90, 0.5, lower.tail = FALSE)
p60

# Activity 2.3 --------------------------------------------------------------
# field technique of detecting schistosomiasis ova probability = 0.035
# 5 samples per patient
# patient with low level of infection will not be identified
p0 <- pbinom(0,5,0.035, lower.tail=TRUE, log.p=FALSE)
p0

d0 <- dbinom(0, 5, 0.035)
d0

# identified in 2 of the samples
d2 <- dbinom(2, 5, 0.035, log=FALSE)
d2

# identified in all the samples
d5 <- dbinom(5, 5, 0.035)
d5

# identified in at most 3 of the samples
p3 <- pbinom(3, 5, 0.035, lower.tail = TRUE)
p3

# Activity 2.4 --------------------------------------------------------------
list.files()
hs <- read.csv("Activity_2.4-health-survey.csv") 
hs

# find range of height 

# bc$agegroup <- cut(pbc$age, breaks = c(0, 30, 50, 70, 100))