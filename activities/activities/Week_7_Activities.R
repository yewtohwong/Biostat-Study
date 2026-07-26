# Author: Yew Toh Wong 
# Date: 26 July 2026
# Purpose: Activity_7

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

# Activity 7.1 ------------------------------------------
children <- readRDS("Activity_6.2.rds")

# before analysing binary data 
# need to ensure that variables are coded as factors 
# positive exposure and outcomes ordered first 
summary(children$Asthma)


# prevalence ratio of asthma
# proportion of asthma within each sex 
# exp = TRUE means “show the effect measure on the natural scale (risk ratio), not the log scale”
# If you leave it out, jmv will show log(RR) instead of RR
contTables(data=children, rows=Gender, cols=Asthma, pcRow=TRUE, exp=TRUE, diffProp=TRUE)

# Activity 7.2 ------------------------------
mi <- readRDS("Activity_7.2.rds")

# ensure heart attack and death are factors with positive = level 1
mi$heart_attack <- factor(mi$heart_attack,
                          levels = c(2, 1),
                          labels = c("Yes", "No"))

mi$mortality <- factor(mi$mortality,
                   levels = c(2, 1),
                   labels = c("Yes", "No"))
mi

# calculate the relative risk 
contTables(data = mi,
           rows = heart_attack, 
           cols = mortality,
           pcRow = TRUE,
           exp=TRUE,
           relRisk=TRUE)

# Activity 7.3 -----------------------------------
hh <- read.csv("Activity_7.3.csv")

# ensure handwash_baseline and handwash_followup are factors with adequate being first level
hh$handwash_baseline <- factor(hh$handwash_baseline,
                          levels = c(1, 0),
                          labels = c("adequate technique", "inadequate technique"))

hh$handwash_followup <- factor(hh$handwash_followup,
                               levels = c(1, 0),
                               labels = c("adequate technique", "inadequate technique"))

summary(hh)

# use McNemar test (basic) to check the effect of campaign
contTablesPaired(
  data = hh,
  rows = "handwash_baseline",
  cols = "handwash_followup")

# estimate the proportions, the difference in proportions and the 95% CI of the difference 
# (https://timothydobbins.github.io/mcnemarcalculator/) 

# Activity 7.4 -------------------------------------------------------
# construct the 2x2 table
# region is the row; term is the column
birth <- data.frame(
  region=c("Urban", "Urban", "Rural", "Rural"),
  term=c("Premature", "Full term", "Premature", "Full term"),
  n=c(2,198,5,75)
)

# use contTables to examine the expected counts in each cell
contTables(data=birth, rows=region, cols=term, counts=n, pcRow=TRUE, exp=TRUE)

# expected counts in b is 2 (<5)
# should use Fischer exact test 
# when the table is prepared, need to add count=n
contTables(data = birth,
           rows = region, 
           cols = term,
           counts=n,
           pcRow = TRUE,
           exp=TRUE,
           fisher=TRUE)
