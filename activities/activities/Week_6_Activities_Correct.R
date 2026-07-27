# Author: Yew Toh Wong 
# Date: 26 July 2026
# Purpose: Report_1_assessment

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
library(tidyr)
library(labelled)

# Activity 6.1 -------------------------------------
list.files()
alcohol <- readRDS("Activity_6.1.rds")

# proportion of drinkers in sample with 95% CI
library(DescTools)
BinomCI(x=90, n=150, method='wilson') # confidence intervals for a single proportion

# is the sample representative of the population with 70% drinker?
# evaluate whether an observed proportion differs from a hypothesised proportion
# binom.test report "Clopper-Pearson" CI (should not use, use "Wilson")
binom.test(x=90, n=150, p=0.7)

# perform hypothesis test using individual data 
table(alcohol$Drinking_Status)

# or 
library(jmv)
descriptives(data=alcohol, vars=Drinking_Status, freq=TRUE)

# Activity 6.2 ------------------------------------------
asthma <- readRDS("Activity_6.2.rds")

# before analysing binary data 
# need to ensure that variables are coded as factors 
# positive exposure and outcomes ordered first 
summary(asthma$Asthma)
summary(asthma) # Asthma and Male are coded as first level

# prevalence ratio of asthma
# proportion of asthma within each sex 
contTables(asthma, Gender, Asthma, relRisk = TRUE, pcRow = TRUE)

# Activity 6.4 --------------------------------
# relative risk when presented with summarized data 
# need to construct the table first 
library(jmv)
summarydata <- data.frame(
  heart_attack = c("Yes", "Yes", "No", "No"),
  death = c("Yes", "No", "Yes", "No"),
  n = c(10, 35, 5, 39)
)

# interpret using this
tab <- xtabs(n ~ heart_attack + death, data = summarydata)
tab

# define heart attack and death as a factor with Yes as first level 
summarydata$heart_attack <- factor(summarydata$heart_attack,
                                   levels=c("Yes", "No"))
summarydata$death <- factor(summarydata$death,
                              levels=c("Yes", "No"))
summarydata

# calculate the relative risk 
contTables(data = summarydata,
           rows = heart_attack, 
           cols = death,
           count=n, 
           # treat each row as a number without count, use when creating own table or summarized data
           pcRow = TRUE, 
           relRisk = TRUE)
