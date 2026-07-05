# Author: Yew Toh Wong 
# Date: 5 July 2026
# Purpose: Week_5_Activities

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

# Activity 5.2 ----------------------------------------------------------------------
# Read Activity_5.2.csv file 
list.files()
hb <- read.csv("Activity_5.2.csv")

# Make status of cf as factor
hb$cf.status <- factor(hb$cf, level=c(0,1), labels=c("No CF", 'CF'))

# distribution of hb by cf status 
descriptives(data=hb, vars=haem, dens=TRUE, box=TRUE, splitBy=cf.status, 
             iqr=TRUE, pc=TRUE, pcValues=c(25,75),
             ci=TRUE, ciWidth=95)

# independent samples t-test 
t_testIS <- ttestIS(data=hb, vars=haem, group=cf.status, meanDiff=TRUE, ci=TRUE, welch=TRUE)
t_testIS

# Activity 5.3 ------------------------------------------------------------------------
# Read Activity_5.3.rds
hct <- readRDS("Activity_5.3.rds")

# distribution of hematocrit by group status
descriptives(data=hct, vars=hematocrit, dens=TRUE, box=TRUE, splitBy=group, 
             iqr=TRUE, pc=TRUE, pcValues=c(25,75),
             ci=TRUE, ciWidth=95)

# independent samples t-test 
hct_t_testIS <- ttestIS(data=hct, vars=hematocrit, group=group, meanDiff=TRUE, ci=TRUE, welch=TRUE)
hct_t_testIS

# Activity 5.4 -----------------------------------------
# Read Activity_5.4.rds
hv <- readRDS("Activity_5.4.rds")

# distribution of hemangioma volume pre and post topical medication
# 3 negative volume at weak 12 that is impossible 
# set them to NA
# Replace negative values with NA
hv$week_12 <- ifelse(hv$week_12 < 0, NA, hv$week_12)
hv$baseline <- ifelse(hv$baseline < 0, NA, hv$baseline)

# Remove rows where id is NA
hv <- hv[!is.na(hv$id), ]

# calculate the difference in hemangioma volume between baseline and week 12
# Can still use mean and paired t-test because reduction is normally distributed 
# Even though the hemangioma volume is not normally distributed at baseline and week 12

hv$reduction <- hv$baseline - hv$week_12

# check the distribution of the reduction using density plot 
descriptives(
  data = hv,
  vars = reduction,
  dens = TRUE,
  box = TRUE,
  iqr = TRUE,
  pc = TRUE,
  pcValues = c(25, 75),
  ci = TRUE,
  ciWidth = 95)

# paired t-test 
hv_t_test <- t.test(hv$baseline, hv$week_12, paired=TRUE)
hv_t_test

# alternative to subset rows where data is not NA at week 12
descriptives(data=subset(hv, is.na(hv$week_12) == FALSE), # select the rows
             vars=c(baseline, week_12)) 

# Activities 5.5 ---------------------------------------------------------
# read Activity_5.5_heartrate.csv
hr <- read.csv("Activity_5.5_heartrate.csv")

# Rename agegroup 
hr$age <- factor(hr$agegroup, levels=c(1,2), labels=c("20-24 years", "25-30 years"))

# distribution of hb by cf status 
descriptives(data=hr, vars=heartrate, dens=TRUE, box=TRUE, splitBy=age, 
             iqr=TRUE, pc=TRUE, pcValues=c(25,75),
             ci=TRUE, ciWidth=95)

# independent samples t-test 
hr_t_testIS <- ttestIS(data=hr, vars=heartrate, group=age, meanDiff=TRUE, ci=TRUE, welch=TRUE)
hr_t_testIS

# Activity 5.6 -----------------------------------------------------------
# Read Activity_5.6_fitness.csv
fitdis <- read.csv("Activity_5.6_fitness.csv")
descriptives(
  data = fitdis,
  vars = c(before, after),
  dens = TRUE,
  box = TRUE,
  iqr = TRUE,
  pc = TRUE,
  pcValues = c(25, 75),
  ci = TRUE,
  ciWidth = 95)

# calculate the difference in distance before and after health promotion program
fitdis$improve <- fitdis$after - fitdis$before

# check the distribution of the improvement using density plot 
descriptives(
  data = fitdis,
  vars = improve,
  dens = TRUE,
  box = TRUE,
  iqr = TRUE,
  pc = TRUE,
  pcValues = c(25, 75),
  ci = TRUE,
  ciWidth = 95)

# paired t-test
# order of variables are important (after first then initial)
fitdis_t_test <- t.test(fitdis$after, fitdis$before, paired=TRUE) # default is 2 tailed test
fitdis_t_test