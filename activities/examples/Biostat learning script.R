# Author: Yew Toh Wong
# Date: 6 June 2026
# Purpose: Biostat study notes

# binomial distribution calculation -----------------------------------
# 19.8% of Oz population are smokers
# random sample of 6 people, probablity that 3 will be smoker
# log=FALSE return actual probability, not the log probability
dbinom(3, 6, 0.198, log = FALSE)

# at least 4 of the 6 will be smokers, p(x>=4)
p4 <- dbinom(4, 6, 0.198, log = FALSE) #pmf at 4
p5 <- dbinom(5, 6, 0.198, log = FALSE) #pmf at 5
p6 <- dbinom(6, 6, 0.198, log = FALSE) #pmf at 6

pg4 <- p4+p5+p6
pg4

# alternatively use pbinom(x)
# pbinom(x) gives p(x<=k), this is a CDF
# lower tail = FALSE flips to upper tail p(x>k)
pbinom(4, 6, 0.198, lower.tail = FALSE) # more than 4 of 6 are smokers
pbinom(3, 6, 0.198, lower.tail = FALSE) # need to change k to 3 for 4 or more of 6 are smokers

# at most 2 are smokers of the 6
pbinom(2, 6, 0.198, lower.tail = TRUE)

# or 
p0 <- dbinom(0, 6, 0.198, log = FALSE) # probability mass function of no smoker
p1 <- dbinom(1, 6, 0.198, log = FALSE) # probability mass function of 1 in 6 smokers
p2 <- dbinom(2, 6, 0.198, log = FALSE) # probability mass function of 2 in 6 smokers 

pg2 <- p0 + p1 + p2 # probability up to 2 in 6 are smokers 
pg2

# Charting categorical variables -----------------------------------------
# one way frequency table 
# load pbc data 
pbc <- readRDS("mod01_pbc.rds" )

# summarise sex, stage and vital status 
# define categorical variables as factor 
# descriptives will use the old data set and cannot create new objects sex, stage and status
# have to use the pbc$sex <- factor(pbc$sex, ----)
pbc$sex <- factor(pbc$sex, 
               levels=c(1,2), 
               labels=c("Male", "Female"))

pbc$stage <- factor(pbc$stage, 
                       levels=c(1,2,3,4), 
                       labels=c("Stage 1", "Stage 2", "Stage 3", "Stage 4"))

pbc$status <- factor(pbc$status, 
              levels=c(0,1,2), 
              labels=c("Alive, No Transplant", "Alive, Liver Transplant", "Dead"))

library(jmv)
# freq=TRUE include freq table for categorical variables
descriptives(data=pbc, vars=c(sex, stage, status), freq = TRUE) 
              
# two-way frequency table (contingency table) ----------------------------
# use contTables()
contTables(data=pbc, rows = sex, cols = stage)

# add pcCol=TRUE or pcRow=TRUE for percentages 
contTables(data=pbc, rows = sex, cols = stage, pcCol = TRUE)
contTables(data=pbc, rows = sex, cols = stage, pcRow = TRUE)

# creating barchart -------------------------------------------------------
# categorical data must be defined as factor before plotting
plot(pbc$stage,
     main="Bar Graph of Stage of Disease",
     ylab="Number of Participant")

# or using contTables
# xaxis = "xrows" - row variable on x-axis
# xaxis = "xcols" - column variable on x-axis
# bartype = "stack" for stacked bar chart
contTables(data=pbc, rows = sex, cols = stage, barplot = TRUE, xaxis = "xcols")
contTables(data=pbc, 
           rows = sex, 
           cols = stage, 
           barplot = TRUE, 
           xaxis = "xcols",
           bartype = "stack"
           )

# stacked relative bar chart 
contTables(data=pbc, 
           rows = sex, 
           cols = stage, 
           barplot = TRUE, 
           xaxis = "xcols",
           bartype = "stack",
           yaxis = "ypc",
           yaxisPc = "column_pc"
           )

# using surveyPlot function for categorical data -------------------------
# surveymv is not on standrad package 
# install from github.com
# install devtools first (allows packages to be installed from alternative locations)
install.packages("devtools")
library(devtools)
install_github("raviselker/surveymv")
library(surveymv)
# surveyPlot(data = , vars = "", group = "", type = "stacked", freq = "perc")
surveyPlot(
  data = pbc,
  vars = "sex",
  group = "stage",
  type = "stacked",
  freq = "perc")

# recode variables ----------------------------------------------------
# use cut() to regroup continuous variable into bins
# need to define minimum and maximum 
# exclude the lower limit but include the upper limit
# (30,50], 30 not included but 50 included in the group 
pbc$agegroup <- cut(pbc$age, breaks = c(0, 30, 50, 70, 100))
summary(pbc$agegroup)

# to include the lower limit but not the upper use right=FALSE
pbc$agegroup <- cut(pbc$age, 
                    breaks = c(0, 30, 50, 70, 100), 
                    right = FALSE,
                    labels = c(
                      "less than 30",
                      "30 to less than 50",
                      "50 to less than 70",
                      "70 or over"
                    ))
summary(pbc$agegroup)

# computing binomial probabilities -----------------------------------------
# dbinom & pbinom

# ggplots2 ------------------------------------------------------------------
library(ggplot2)
# ggplot() can produce a blank canvas, aes is x, type of graph(fill="colour"), theme, labels
ggplot()
# constructing the graph
# sex
ggplot(data=pbc, mapping=aes(x=sex)) +
  geom_bar(fill="steelblue")+
  theme_classic()+
  labs(x="Participant Sex", y="Frequency")

# stage of disease
# fill with "colour", alpha for transparent version (0-1 or 00-FF)
ggplot(data=pbc, mapping=aes(x=stage)) +
  geom_bar(fill="gold3")+
  theme_classic()+
  labs(x="Participant Stage", y="Frequency")

ggplot(data=pbc, mapping=aes(x=stage)) +
  geom_bar(fill="gold", alpha=0.5)+
  theme_classic()+
  labs(x="Participant Stage", y="Frequency")

# excluding the NA in the dataset for plotting using subset()
# R runs what is inside bracket (nested) first 
ggplot(data=subset(pbc, is.na(stage)==FALSE), mapping=aes(x=stage)) +
  geom_bar(fill="green", alpha=0.6)+
  theme_classic()+
  labs(x="Participant Stage", y="Frequency")

# another way is to subset before the ggplot runs using |>
# |> is built in R4.1 similar to tidyverse %>% but stricter rule
subset(pbc, is.na(stage)==FALSE) |>
  ggplot(mapping=aes(x=stage)) +
  geom_bar(fill="green", alpha=0.6)+
  theme_classic()+
  labs(x="Participant Stage", y="Frequency")

# using ggplot to plot barchart with 2 variables
# inside aes(), first specify x, next fill with second variable
# once fill with second variable then geom_bar(cannot have fill but position OK)
subset(pbc, is.na(stage)==FALSE) |>
  ggplot(mapping=aes(x=stage, fill=sex)) +
  geom_bar(position="dodge")+
  theme_classic()+
  labs(x="Stage of Disease", y="Frequency")

# changing the colour of the bar when using a second variable as fill
subset(pbc, is.na(stage)==FALSE) |>
  ggplot(aes(x = stage, fill = sex)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Male" = "gold",
                               "Female" = "pink")) +
  theme_classic() +
  labs(x = "Stage of Disease", y = "Frequency")

# density plot using ggplot ------------------------------------------------
ggplot(data = pbc, aes(x=age))+
  geom_density()+
  theme_classic()+
  labs(x="Age", y="Density", title = "Density Distribution of Age of Participants in PBC Study")

# Box plot using ggplot ------------------------------------------------------
ggplot(data = pbc, aes(x=sex, y=age))+
  geom_boxplot()+
  theme_classic()+
  labs(x="Sex", y="Age", title = "Boxplot of Age of Participants in PBC Study")

# removing x-axis label using theme(axis.title/text/ticks = element_blank())
# ggplot can have 2 themes 
ggplot(data = pbc, aes(y=age))+
  geom_boxplot()+
  theme_classic()+
  labs(y="Age (years)", title = "Boxplot of Age of Participants in PBC Study")+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        )

# ---------------------------------------------------------------------------
