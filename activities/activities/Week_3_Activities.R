# Author: Yew Toh Wong
# Date 15 June 2026
# Purpose: Worked examples and activities 

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

# Probability that a person drawn from the population will have a DBP greater than 80
# Population mean = 70
# SD = 12mmhg
# p(X>80)

# Probability that a normally distributed variable is greater and equal to 80, assuming the distribution has mean 70 and standard deviation 12.
p80 <- pnorm(80, mean=70, sd=12, lower.tail = FALSE)
p80

# Probability that a person will have BP less than 65
p65 <-pnorm(65, mean=70, sd=12, lower.tail = TRUE)
p65

# Read diastolic blood pressure file in example module 3
list.files()
dbp <- read.csv("mod03_blood_pressure.csv")
descriptives(data=dbp, vars=dbp, ci=TRUE)

# Finding CI from summarised data
# width is the CI level, e.g. 0.95.
# 1 - width is the total alpha (e.g. 0.05).
# (1 - width)/2 is alpha/2 (e.g. 0.025 in each tail).
# 1 - (1 - width)/2 gives the upper tail probability (e.g. 0.975).
# qt() is the t‑quantile function in R: it returns the t value such that 𝑃(𝑇≤𝑡crit)= 1−(1−width)/2
# df = n - 1 sets the degrees of freedom for the t‑distribution.
# lcl - lower confidence limit
# ucl - upper confidence limit

ci_mean <- function(n, mean, sd, width = 0.95, digits = 3){ 
  tcrit <- qt(1 - (1 - width)/2, df = n - 1) 
  lcl <- mean - tcrit * sd/sqrt(n) 
  ucl <- mean + tcrit * sd/sqrt(n) 
  return( c(lower = round(lcl, digits), 
            upper = round(ucl, digits)) )
}
ci_mean(n = 242, mean = 128.4, sd = 19.56, width = 0.95)

# Course material p 139 ----------------------------------------------
# read EXCEL file
list.files()
survey <- read_excel("mod03_health_survey.xlsx")
survey1 <- survey
summary(survey)

# Transform sex into factor 
survey$sex <- factor(survey$sex, levels = c(1,2), labels = c("Male", "Female"))
summary(survey$sex)

# create a new column for BMI
survey$bmi <- survey$weight/(survey$height^2)
head(survey)
tail(survey)

# Checking with plots
library(ggplot2)
# density plot
ggplot(data=survey, mapping=aes(x=bmi))+
  geom_density()+
  theme_classic()+
  labs(x="BMI(Kg/m2)")

# boxplot, need to label both x and y
ggplot(data=survey, mapping=aes(y=bmi))+
  geom_boxplot()+
  theme_classic()+
  labs(y="BMI(Kg/m2)")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

# # dplyr can be used to manipulate data -----------------------------------------
# this section codes is correct but doesn't work because sex is already a factor
# mutate () function can create multiple new variables or change existing variables
library(dplyr)

survey1<- mutate(survey1, 
                 sex=factor(sex, level=c(1,2), labels=c("Male", 'Female')),
                 bmi= weight/(height^2))
# summarizing data by another variable
# summarizing bmi by sex
descriptives(data=survey1, vars=bmi, splitBy = sex) # code here didn't work because the earlier codes has already converted sex into factor 
### --------------------------------------------------------------------------

# use facet_wrap() to plot graph for each value within the categorical variable 
# bmi density plot for sex (male & female) separately

ggplot(data=survey1, mapping=aes(x=bmi))+
         geom_density()+
         facet_wrap(vars(sex))+
         theme_classic()+
           labs(x="BMI (Kg/m2")

# boxplot of bmi
ggplot(data=survey1, mapping=aes(x=sex, y=bmi))+
  geom_boxplot()+
  theme_classic()+
  labs(x="Sex", y="BMI (Kg/m2)")

# descriptives() needs a dataframe
# a single column of data is a vector. dataframe is multi-columns data 
# use as.data.frame() to convert vector into dataframe
age <- c(20, 25, 23, 29, 21, 27)
age.df <- as.data.frame(age)
descriptives(data=age) # data needs to be in a dataframe therefore not working
descriptives(data=age.df)

# calculate probability that dbp is greater than 90
# mean 77.9 sd 11
pnorm(90, 77.9, 11, lower.tail=FALSE)

# calculate 95% CI of a mean
# read blood pressure file 
list.files()
bp <- read.csv("mod03_blood_pressure.csv")
summary(bp)

# finding standard error and confidence interval 
# std. error mean = standard error (standard deviation of multiple samples means)
descriptives(data=bp, vars=dbp, se=TRUE, ci=TRUE )

# finding CI from summarised data -----------------------------------------
ci_mean <- function(n, mean, sd, width=0.95, digits=3){
  lcl <- mean- qt(p=(1- (1-width)/2), df=n-1) * sd/sqrt(n)
  ucl <- mean + qt(p=(1- (1-width)/2), df=n-1) * sd/sqrt(n)
  print(paste0(width*100, "%", " CI: ", format(round(lcl, digits=digits), nsmall = digits),
               " to ", format(round(ucl, digits=digits), nsmall = digits) ))
}

# examples
ci_mean(n=242, mean=128.4, sd=19.56, width=0.95)
ci_mean(n=242, mean=128.4, sd=19.56, width=0.99)

# Activity 3.2 daily sugar consumption ---------------------------------------------------------
# read file Activity_3.2.rds
list.files() # session need to set to activities folder
# desc mean descriptive and can be use to present as rows
dsc <- readRDS("Activity_3.2.rds")
descriptives(data=dsc, vars=TotalSugar, splitBy = Preschool, se=TRUE, dens=TRUE, desc='rows')

# for categorical variable Preschool, this need to be a factor before separate boxplots will appear
ggplot(data=dsc, mapping=aes(x=factor(Preschool), y=TotalSugar))+
  geom_boxplot()+
  theme_minimal()+
  labs(x="Preschool", y="Daily Sugar Consumption (grams)", title="Daily Sugar Consumption for Preschools (Grams)")

ggplot(data=dsc, mapping=aes(x=TotalSugar, colour=factor(Preschool)))+
  geom_density()+
  theme_minimal()+
  labs(x="Daily Sugar Consumption (grams)", y="Density", color="Preschool", title="Daily Sugar Consumption by Preschools (Grams)")

# standard error by preschool
descriptives(data=dsc, vars=TotalSugar, splitBy=Preschool, se=TRUE, ci=TRUE)

# Activity 3.3 -------------------------------------------------------------
# read Activity_1.3.csv
dbp <- read.csv("Activity_1.3.csv")
ggplot(data=dbp, aes(x=dbp))+
  geom_density()+
  theme_bw()+
  labs(x="Diastolic Blood Pressure (mmHg)", y="Density", title="Diastolic Blood Pressure of NSW Students (mmHg)")

# estimate mean, standard error of the mean, 95% ci for dbp
descriptives(data=dbp, vars=dbp, se=TRUE, ci=TRUE)

# Activity 3.4 -------------------------------------------------------------
# read Activity-3.4.xlsx
bmi <- read_excel("Activity_3.4.xlsx" )

# skim in R provide overall summary of dataframe
skim(bmi)

# create new variable BMI 
bmi$BMI <- bmi$weight/(bmi$height^2)

# categorize BMI and sex into categories 
bmi <- bmi|>
  mutate(
    BMI.cat =cut(BMI, 
                 breaks=c(0, 18.5, 25, 30, 100),
                 labels=c("Underweight", "Normal Weight", "Overweight", "Obese"), 
                 right=FALSE
    ))
bmi$sex=factor(bmi$sex, level=c(1,2), labels=c("Male", 'Female'))
# check the distribution of bmi
descriptives(data=bmi, vars=c(height, weight, BMI), dens=TRUE, box=TRUE) 

# use the subset function to examine the outlier 
subset(bmi, BMI<15)
subset(bmi, BMI>50)


# create a two way table for BMI categorize by sex
contTables(data=bmi, rows=sex, cols=BMI.cat, pcRow=TRUE)

# Activity 3.5 -----------------------------------------------------------
# read file Activity_3.5.csv
los <- read.csv("Activity_3.5.csv")

# female=1; male=0
# factor female into gender
# los$gender will create a new column in the dataframe "gender"
los$gender=factor(los$female, level=c(0,1), labels=c("Male", 'Female'))

# distribution of los by gender 
descriptives(data=los, vars=los, dens=TRUE, box=TRUE, splitBy=gender, 
             iqr=TRUE, pc=TRUE, pcValues=c(25,75),
             ci=TRUE, ciWidth=95)

# Activity 3.6 -------------------------------------------------------------
# men weights mean =87
# sd= 8kg
# probability that a man will weigh 95 Kg and above
# the setting in the R suggestion are default settings
pnorm(95, mean=87, sd=8, lower.tail=FALSE, log.p=FALSE)
# probability that a man will weight more than 75 Kg but less than 95 Kg
1-pnorm(95, mean=87, sd=8, lower.tail=FALSE, log.p=FALSE)-
  pnorm(75, mean=87, sd=8, lower.tail=TRUE, log.p=FALSE) 
# alternative
p_between <- pnorm(95, 87, 8) - pnorm(75, 87, 8)
p_between
