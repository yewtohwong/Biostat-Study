# Author: Yew Toh Wong
# Date: 7 June 2026
# Purpose: Week 2 Actvities 

# library used 
library(skimr)
library(jmv)
library(ggplot2)
install.packages("devtools")
library(devtools)
install_github("raviselker/surveymv")
library("surveymv")
library(dplyr)
install.packages("epiDisplay")
library(epiDisplay)

# Cold in winter ----------------------------------
# probability is 0.05
# 10 people

p1 <- pbinom(0, 10, 0.05, lower.tail = FALSE)
p1

# BP ------------------------------------------------------------------------
list.files()
bp <- readRDS("bloodpressure.rds")
descriptives(data=bp, vars="systolic_bp", pc=TRUE)

















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
































# Activity 2.1 -----------------------------------------------------------
# Read in data 
list.files()
lbw <- readRDS("Activity_2.1.rds")

skim(lbw)

# Create 2x2 contingency table for lbw and smoking status
contTables(data=lbw, rows="SMOKE", cols="LOW", pcRow = TRUE)
contTables(data=lbw, rows="SMOKE", cols="LOW", pcCol = TRUE)

# Create stacked bar chart using contTables (not able to have title)

# Distribution of age group
descriptives(data=lbw, vars=AgeGrp, freq=TRUE)

# Low birth weight by age group 
# When it is an object with variables and data within no need for quotation mark
# Description of how you want the program to work then use quotation mark
library(surveymv)

surveyPlot(
  data = lbw,
  vars = "LOW",
  group = "AgeGrp",
  type = "stacked",
  freq = "perc"
)


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
# Launch the applet in epiDisplay
# applet() doesn't work anymore 

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

# Summary statistics for height
descriptives(data=hs, vars = height, dens = TRUE)

# find range of height 
height.range <- range(hs$height)
height.range
# Categorize height into 20cm groups
# right=FALSE means the higher value is not included in the group (default)  
hs$height.cat <- cut(hs$height,
                     breaks = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2),
                     right = FALSE,# Checking at this point using summary(hs$height.cat) can tell us what is included in which group 
                     labels = c("1.2m to less than 1.4m",
                                "1.4m to less than 1.6m",
                                "1.6m to less than 1.8m",
                                "1.8m to less than 2.0m",
                                "2.0m to less than 2.2m"))

summary(hs$height.cat)
table(hs$height.cat)
plot(hs$height.cat, hs$weight)

# Barchart or frequency table 
descriptives(data=hs, vars=height.cat, fre=TRUE)


# Activities 2.5 -------------------------------------------------------------
list.files()
los <- readRDS("Activity_2.5-LengthOfStay.rds")

# Looking at distribution of data
hist(los$BirthWt)
plot(density(los$BirthWt, na.rm=TRUE))
boxplot(los$BirthWt)
# Birth weight is reasonably symmetric
# Report mean and SD

hist(los$LengthStay)
plot(density(los$LengthStay, na.rm=TRUE))
boxplot(los$LengthStay)
# Length of stay is not symmetric
# Report median and IQR

descriptives(data=los, vars=c(BirthWt, LengthStay),
             pc=TRUE)



End of activities ------------------------------------------------------
  
  
  
  # find out if na is present in each of the variable
  any(is.na(los$Sex))
any(is.na(los$BirthWt))
any(is.na(los$GestAge))
any(is.na(los$LengthStay))

# Plot each variable for birth weight and los
# plot sex vs birth weight 
# check sex is a factor and make sex a factor if not (categorical variable) 
is.factor(los$Sex)
# Sex is already a factor

# Plot sex vs birth weight 
subset(los, is.na(BirthWt)==FALSE) |>
  ggplot(mapping = aes(x=Sex))+
  geom_bar(position = "dodge")+
  theme_classic()+
  labs(x="Sex", y="Birth Weight (Grams)")

# Plot sex vs los
ggplot(data=los, mapping = aes(x=Sex, y=LengthStay))+
  geom_boxplot(position = "dodge")+
  theme_classic()+
  labs(x="Sex", y="LOS (days)")

# Plot birth weight vs los
subset(los, is.na(BirthWt)==FALSE) |>
  ggplot(mapping = aes(x=BirthWt, y=LengthStay))+
  geom_point()+
  geom_smooth(method="lm")
theme_classic()+
  labs(x="Birth Weight (Grams)", y="LOS (Days)")

# Plotting scatter plot 
library(dplyr)
library(ggplot2)

los.clean.all <- los |>
  filter(!is.na(BirthWt),
         !is.na(GestAge))

ggplot(los.clean.all, aes(x = GestAge, y = BirthWt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, colour = "blue") +
  labs(
    x = "Gestational Age (weeks)",
    y = "Birth Weight (grams)",
    title = "Birth Weight vs Gestational Age"
  )

ggplot(los.clean.all, aes(x = BirthWt, y = LengthStay)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, colour = "red") +
  labs(
    x = "Birth Weight (grams)",
    y = "Length of Stay (days)",
    title = "Birth Weight vs Length of Stay"
  )

ggplot(los.clean.all, aes(x = GestAge, y = LengthStay)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, colour = "darkgreen") +
  labs(
    x = "Gestational Age (weeks)",
    y = "Length of Stay (days)",
    title = "Gestational Age vs Length of Stay"
  )


# Categorize birth weight and gestational age into categories
# have to use right = FALSE to not include the higher bound value and include the lower bound value 
# right = TRUE is the default value (include the higher bound but exclude the lower bound)

los.clean.all <- los.clean.all |>
  mutate(
    bw.cat = cut(BirthWt,
                 breaks = c(1500, 2000, 2500, 3000, 3500, 4000),
                 labels = c("1500–<2000g", "2000–<2500g", "2500–<3000g",
                            "3000–<3500g", "3500–<4000g"), right=FALSE),
    ga.cat = cut(GestAge,
                 breaks = c(31, 33, 35, 37, 39, 42),
                 labels = c("31–<33w", "33–<35w", "35–<37w",
                            "37–<39w", "39–<42w"), right=FALSE),
    
    los.cat = cut(LengthStay,
                  breaks = c(0, 3, 7, 14, 90, 365),
                  labels = c("0–<3d", "3d–<7d", "7d–<14d",
                             "14d–<90d", "90d–<365d"), right=FALSE),
  )

# Can still use box plot for numerical data that has been categorized
# Birth weight categories vs LOS
los.clean.all |>
  ggplot(aes(x = bw.cat, y = LengthStay)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Birth Weight Category", y = "LOS (Days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gestational age categories vs LOS
los.clean.all |>
  ggplot(aes(x = ga.cat, y = LengthStay)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Gestational Age Category", y = "LOS (Days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Summary statistics for each variables
mean(los.clean.all$BirthWt)
median(los.clean.all$BirthWt)
median(los.clean.all$LengthStay)