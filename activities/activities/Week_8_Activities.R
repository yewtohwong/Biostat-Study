# Author: Yew Toh Wong 
# Date: 27 July 2026
# Purpose: Activity_8

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

# Activity 8.2 -----------------------------------------------
list.files()
iq <- readRDS("Activity_8.2.rds")

# create a scatterplot of age vs iq
ggplot(data = iq, aes(x = age, y = iq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Age (Years)") +
  ylab("IQ") +
  theme_classic()

# or use plot 
plot(x=iq$age, y=iq$iq,
     xlab = "Age (Years)",
     ylab = "IQ")

# obtain the correlation coefficient using jmv package
# correlation matrix the order of the variable doesn't matter 
corrMatrix(data=iq, vars = c(age, iq)) 

# use the lm function to estimate the regression equation 
model.iq <- lm(iq~age, data=iq)
summary(model.iq)

# use confint() to estimate the 95CI for regression slope
confint(model.iq)

# must check residual has Normal distribution without significant outliers
residual.iq <- resid(model.iq)
plot(density(residual.iq), main="", xlab="Residual")
