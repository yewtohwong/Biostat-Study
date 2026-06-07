# Author: Yew Toh Wong 
# Date 6 June 2026
# Purpose: PBC Examples from Week 1 Course

# library
library(skimr)
library(jmv)
library(dplyr)

# Read in mod01_pbc.rds
pbc.1 <- readRDS("mod01_pbc.rds")

# Summary 
summary(pbc.1)

# or use skim()
skim(pbc.1)

# 0r use descriptives()
descriptives(data=pbc.1, vars=c(age,ast,bili), pc=TRUE, dens=TRUE)

# more flexible to use plot
plot(density(pbc.1$age),
     xlab="Age in Years",
     main="Density Plot of Participant Age"
)

# density will have error message if there are missing values 
plot(density(pbc.1$ast, na.rm=TRUE),
     xlab="AST in mmol/L",
     main="Density Plot of AST")

# box plot 
boxplot(pbc.1$age,
     xlab="Age (Years)",
     main="Box Plot of Participant Age"
)

# count gender and percentages
tab <- table(pbc.1$sex)
percent <- prop.table(tab)*100

tab
percent

# alternative 1=male, 2=female
library(dplyr)

pbc.1%>%
  mutate(sex=ifelse(sex==1, "Male", "Female")) %>%
  count(sex) %>%
  mutate(percent= n/sum(n)*100)

# stages 
tab.stage <- table(pbc.1$stage)
percent.stage <- prop.table(tab.stage)*100

tab.stage
percent.stage

# alternative for stages
pbc.1 %>%
  mutate(stage = factor(stage,
                        levels = c(1, 2, 3, 4),
                        labels = c("Stage 1", "Stage 2", "Stage 3", "Stage 4"))) %>%
  count(stage) %>%
  mutate(percent = n / sum(n) * 100)

# the outputs for stage for both methods doesn't match due to na (n=6)
sum(is.na(pbc.1$stage))

# vital status removing the na
pbc.1 %>%
  filter(!is.na(status)) %>%
  mutate(status = factor(status,
                        levels = c(0, 1, 2),
                        labels = c("Alive, No Transplant", "Alive, Liver Transplant", "Dead"))) %>%
  count(status) %>%
  mutate(percent = n / sum(n) * 100)

# relative frequency of sex by stage 
df.stage.sex <- pbc.1 %>%
  mutate(stage = factor(stage,
                        levels = c(1, 2, 3, 4),
                        labels = c("Stage 1", "Stage 2", "Stage 3", "Stage 4"))) %>%
  mutate(sex = factor(sex,
                      levels = c(1, 2),
                      labels = c("Male", "Female"))) %>%
  filter(!is.na(stage), !is.na(sex)) %>%
  count(stage, sex) %>%
  group_by(stage) %>%
  mutate(percent = n / sum(n) * 100)

df.stage.sex

# plot the relative frequency of sex by stage (stacked bar chart)
# position "fill" forces the bar to reach the 100%
# position "stack" for stacked bars 
# position "dodge" for grouped bars 
# geom_bar counts the data for you and therefore need raw data
# geom_col y values need to be supplied 

library(ggplot2)
# using pre-calculated percentages 

ggplot(df.stage.sex,aes(x=stage, y= percent, fill=sex)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Relative Frequency of Sex by Stage",
    x = "Stage",
    y = "Relative Frequency",
    fill = "Sex"
  ) +
  theme_minimal()

# alternative use geom_bar
ggplot(pbc.1 %>%
         mutate(
           stage = factor(stage, levels = 1:4,
                          labels = paste("Stage", 1:4)),
           sex = factor(sex, levels = 1:2,
                        labels = c("Male", "Female"))
         ) %>%
         filter(!is.na(stage), !is.na(sex)),
       aes(x = stage, fill = sex)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Relative Frequency of Sex by Stage",
    x = "Stage",
    y = "Relative Frequency",
    fill = "Sex"
  ) +
  theme_minimal()



# alternative for bar to be side by side 
ggplot(df.stage.sex, aes(x = stage, y = percent, fill = sex)) +
  geom_col(position = position_dodge()) +
  labs(
    title = "Relative Frequency of Sex by Stage",
    x = "Stage",
    y = "Percent",
    fill = "Sex"
  ) +
  theme_minimal()

# read dataset mod01_weight_1000.rds
weight <- readRDS("mod01_weight_1000.rds")

# plot density graph
# density() must receive numeric vector 
w <- weight$weight
plot(
  density(w, na.rm=TRUE),
  xlab="Weight (Kg)",
  main="Density Plot of 1000 Weights")

boxplot(weight$weight,
        xlab="Weight (Kg)",
        main="Boxplot of 1000 Weights")

# outlier at 700Kg
# view outlier using subset()
# first weight is the dataframe
# the second weight is the column inside the dataframe
subset(weight, weight>200)

# using ifelse to replace the errorneous data
# ifelse(test, value_if_true, value_if_false)
# create a new column called weight_clean
weight$weight_clean = ifelse(weight$weight<200, weight$weight, NA)

# if the correct weight is 70.2kg instead of 700.2kg
weight$weight_clean1 = ifelse(weight$weight==700.2, 70.2, weight$weight)
