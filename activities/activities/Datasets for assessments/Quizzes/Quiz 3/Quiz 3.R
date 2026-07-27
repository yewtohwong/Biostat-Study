# Author: Yew Toh Wong 
# Date: 28 July 2026
# Purpose: Quiz.3

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

# Quiz 3 -----------------------------------------------

# Q2 proportion of children aged 2-5 years who are not fully vaccinated
library(DescTools)
BinomCI(x=63, n=400, method='wilson') # confidence intervals for a single proportion


# Q4
list.files()
cp <- readRDS("chickenpox.RDS")

# convert vaccinated to chickenpox status to factors
cp$vaccinated <- factor(cp$vaccinated,
                          levels = c(1, 0),
                          labels = c("Vaccinated", "Not Vaccinated"))

cp$chickenpox <- factor(cp$chickenpox,
                       levels = c(1, 0),
                       labels = c("Developed Chickenpox", "Did Not Developed Chickenpox"))
summary(cp)

# expected count in each cells (use original data therefore count=n not required)
contTables(data=cp, rows=vaccinated, cols=chickenpox, pcRow=TRUE, exp=TRUE) 
# no need to use Fisher's Exact when the expected cell count is greater than or equal 5 




