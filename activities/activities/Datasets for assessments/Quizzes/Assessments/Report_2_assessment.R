# Author: Yew Toh Wong 
# Date: 6 August 2026
# Purpose: Report 2 Assessment

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

# Q2 -----------------------------------------------
list.files()
pa <- readRDS("physical-activity.rds")

# Data exploration
descriptives(data=pa, vars=c(age, sex, work_met, rec_met), pc = TRUE, dens = TRUE, box = TRUE)

# Approximately 10% are outliers

# Find out the difference between MET-min/week at work vs recreational activities 
# whether the difference is Normally distributed 
pa$diff_met <- pa$work_met - pa$rec_met
diff_clean <- pa$diff_met[!is.na(pa$diff_met)]
plot(
  density(diff_clean),
  main = "Distribution of Difference in MET",
  xlab = "Difference (Work MET – Rec MET)",
  ylab = "Density")

polygon(
  density(diff_clean),
  col = rgb(0.2, 0.4, 0.8, 0.4),  # ocean blue with transparency
  border = "steelblue"
)

# Testing for the difference using Wilcoxon matched paired signed ranked test 
wilcox.test(pa$work_met, pa$rec_met,
            paired=TRUE)


# Q3
ei <- readRDS("ear-infection.rds")
ei1 <- ei

# before analysing binary data 
# need to ensure that variables are coded as factors 
# positive exposure and outcomes ordered first 
ei1$treatment <- factor(
  ei1$treatment,
  levels = c(2, 1),
  labels = c("New antibiotic", "Standard treatment")
)

ei1$clinical_cure <- factor(
  ei1$clinical_cure,
  levels = c(2, 1),
  labels = c("Cured", "Not Cured")
)

contTables(ei1, treatment, clinical_cure, relRisk = TRUE, pcRow = TRUE)

# check expected cell count >5
tab <- table(ei1$treatment, ei1$clinical_cure)
tab

chisq.test(tab)$expected

# Q4
he <- readRDS("healthy-eating.rds")

# create a scatterplot of age vs hei
ggplot(data = he, aes(x = age, y = hei)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Age (Years)") +
  ylab("Healthy Eating Index") +
  theme_classic()

# or use plot 
plot(x=he$age, y=he$hei,
     xlab = "Age (Years)",
     ylab = "Healthy Eating Index")

# obtain the correlation coefficient using jmv package
# correlation matrix, the order of the variable doesn't matter 
corrMatrix(data=he, vars = c(age, hei)) 

# use the lm function to estimate the regression equation 
model.he <- lm(hei~age, data=he)
summary(model.he)

# use confint() to estimate the 95CI for regression slope
confint(model.he)

# must check residual has Normal distribution without significant outliers
residual.he <- resid(model.he)
plot(density(residual.he), main="", xlab="Residual")
