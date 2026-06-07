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
