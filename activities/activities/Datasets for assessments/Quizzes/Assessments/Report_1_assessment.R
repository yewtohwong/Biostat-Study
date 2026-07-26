# Author: Yew Toh Wong 
# Date: 6 July 2026
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

# Q1 part 2 -----------------------------------
list.files()
survey <- read.csv("surveydata.csv")

# Labelling the data 
survey <- survey |>
  mutate(
    # the factor using levels
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
    
    education = factor(education, levels = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("Did not complete Year 10", "Year 10 or 11", "Year 12 or equivalent",
                                  "Certificate, diploma, or some tertiary study", "Bachelor degree or higher", 
                                  "Don't know", "Refuse to answer")),
    
    mod_pa = factor(mod_pa, levels = c(1, 2), 
                    labels = c("Moderate-intensity activity for 10 minutes or more", 
                               "Moderate-intensity activity for less than 10 minutes"))
  ) |>
  # assign descriptive labels
  set_variable_labels(
    sedentary_min = "Time spent sitting in a usual day (minutes)",
    height        = "Height (cm)",
    waist         = "Waist circumference (cm)",
    kcal          = "Estimated total daily energy intake (kcal)",
    fibre         = "Estimated total daily fibre intake (grams)"
  )

# Check for missing values 
colSums(is.na(survey))

# Check for distribution of variables 
skim(survey)
descriptives(
  data = survey,
  vars = c("age", "sedentary_min", "height", "waist", "kcal", "fibre"),
  pc=TRUE,
  dens=TRUE,
  box=TRUE
)

# Summarising the categorical variables
descriptives(data=survey, vars = c(sex, education, mod_pa), freq = TRUE)

# Two-way frequency table for sex and mod_pa
contTables(
  data = survey,
  rows = "sex",
  cols = "mod_pa",
  pcRow = TRUE,
  pcCol = TRUE,
  pcTot = TRUE
)

# Two-way frequency table for education and mod_pa
contTables(
  data = survey,
  rows = "education",
  cols = "mod_pa",
  pcRow = TRUE,
  pcCol = TRUE,
  pcTot = TRUE
)

# Two-way frequency table for sex and education
contTables(
  data = survey,
  rows = "education",
  cols = "sex",
  pcRow = TRUE,
  pcCol = TRUE,
  pcTot = TRUE
)

# Difference between male and female in energy and fibre intake
descriptives(
  data = survey,
  vars = c("kcal", "fibre"),
  splitBy = "sex",
  pc = TRUE,    
  dens=TRUE
)

# Check for significant difference between male and female 
ttestIS(
  data = survey,
  vars = c("kcal", "fibre"),
  group = "sex",
  mann = TRUE,
  desc = TRUE
)

# Q2 -----------------------
# Read bp.csv file
bp <- read.csv("bp.csv")

# part a
descriptives(data=bp, 
             vars=c(sysbp, diasbp), 
             pc=TRUE,
             dens=TRUE,
             box=TRUE )

ggplot(bp, aes(x = sysbp)) +
  geom_density() +
  labs(
    x = "Systolic BP (mmHg)",
    y = "Density"
  )

ggplot(bp, aes(y = sysbp)) +
  geom_boxplot() +
  labs(
    y = "Systolic BP (mmHg)"
  )

# part b
# Categorize systolic bp optimal, normal, high-normal and high
range(bp$sysbp, na.rm=TRUE)
bp$systolic_bp <- cut(bp$sysbp, 
                breaks = c(60, 120, 130, 140, 200), 
                labels = c("optimal", "normal", "high-normal", "high"),
                right=FALSE)
summary(bp$systolic_bp)

# Group by new categories and calculate relative frequency and percentages
# Remove NA
sbp.table <- bp |>
 count(systolic_bp, name = "Frequency") |>
  filter(!is.na(systolic_bp)) |>
  mutate(
    Relative_Frequency = Frequency / sum(Frequency),
    Percentage = round(Relative_Frequency*100, 2)
  )

print(sbp.table)

# part c
# mean sbp 122mmHg
# One sample t test 
t.test(bp$sysbp, mu=122)

# part f
# mean sbp 122mmHg
# sd 15mmHg
# expected probability of an Australian adult having high systolic blood pressure 
mu <- 122
sigma <- 15

# Calculate the probability of being ABOVE 140
prob_high.sbp <- pnorm(140, mean = mu, sd = sigma, lower.tail = FALSE)
prob_high.sbp

# Plot the expected probability of high sbp
# Create a sequence of blood pressure values for the x-axis
x_vals <- seq(mu - 4*sigma, mu + 4*sigma, length = 200)
y_vals <- dnorm(x_vals, mean = mu, sd = sigma)

# Plot the main normal distribution curve
plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue",
     xlab = "Systolic Blood Pressure (mmHg)", ylab = "Density",
     main = "Distribution of Systolic BP among Australian Adults")

# Add a vertical line at the cutoff point (140)
abline(v = 140, col = "darkgray", lty = 2, lwd = 2)

# Shade the area representing "high" blood pressure (above 140)
shade_x <- seq(140, max(x_vals), length = 100)
shade_y <- dnorm(shade_x, mean = mu, sd = sigma)
polygon(c(140, shade_x, max(x_vals)), c(0, shade_y, 0), col = rgb(1, 0, 0, 0.3), border = NA)

# Add text showing the calculated percentage
text(155, 0.005, labels = paste0("Prob = ", round(prob_high * 100, 2), "%"), col = "red", font = 2)

# alternative --------------------------------------------
# Probability of high SBP (>140)
prob_high <- 1 - pnorm(140, mean = mu, sd = sigma)

# Data frame for full curve
df <- data.frame(
  x = seq(mu - 4*sigma, mu + 4*sigma, length.out = 400)
)
df$y <- dnorm(df$x, mean = mu, sd = sigma)

# Data frame for shaded region
shade_df <- subset(df, x >= 140)

ggplot(df, aes(x, y)) +
  geom_line(colour = "blue", linewidth = 1.2) +
  geom_area(data = shade_df, aes(x, y),
            fill = "red", alpha = 0.3) +
  geom_vline(xintercept = 140, linetype = "dashed", colour = "darkgray") +
  annotate("text",
           x = 150,
           y = max(df$y) * 0.15,
           label = paste0("Prob = ", round(prob_high * 100, 2), "%"),
           colour = "red",
           fontface = "bold",
           size = 5) +
  labs(
    title = "Distribution of Systolic BP among Australian Adults",
    x = "Systolic Blood Pressure (mmHg)",
    y = "Density"
  ) +
  theme_classic(base_size = 14)

# Q3 --------------------------------------------------
# Open heart surgery RCT, intervention n=35 and control n=35
# 7 sessions
# First dose 30min before extubation
# Numerical Pain Rating Scale 0-10
# read peppermint.csv file
list.files()
nprs <- read.csv("peppermint.csv")
# Characteristics of participants
nprs <- nprs |>
  mutate(Treatment_group = factor(arm, levels = c(0, 1), labels = c("Control Aromatherapy", "Peppermint Oil Aromatherapy")),
         Sex = factor(female, levels = c(0, 1), labels = c("Male", "Female")),
         CABG = factor(CABG, levels = c(0, 1), labels = c("No CABG", "CABG"))
  )
# Check for missing values
colSums(is.na(nprs))
# Check for distribution of variables
skim(nprs)
descriptives(
  data = nprs,
  vars = c("age", "weight", "time", "pain"),
  pc=TRUE,
  ci=TRUE,
  dens=TRUE,
  box=TRUE
)

library(ggplot2)
library(patchwork)

p1 <- ggplot(nprs, aes(x = age)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(title = "Age")

p2 <- ggplot(nprs, aes(x = weight)) +
  geom_density(fill = "darkgreen", alpha = 0.6) +
  labs(title = "Weight")

p3 <- ggplot(nprs, aes(x = time)) +
  geom_density(fill = "purple", alpha = 0.6) +
  labs(title = "Duration of Surgery")

p4 <- ggplot(nprs, aes(x = pain)) +
  geom_density(fill = "red", alpha = 0.6) +
  labs(title = "Pain Score")

# Combine into one figure (2×2 layout)
(p1 | p2) /
  (p3 | p4)+
  plot_annotation(
    title = "Figure 1: Density Plots for Age, Weight, Time and Pain"
  )


descriptives(
  data = nprs,
  vars = c("age", "weight", "time", "pain"),
  splitBy = "Treatment_group",
  pc=TRUE,
  ci=TRUE,
  dens=TRUE,
  box=TRUE
)
# Summarising the categorical variables
descriptives(data=nprs, vars = c(Sex, CABG), splitBy="Treatment_group", freq = TRUE)
# Two-way frequency table for Sex and CABG
contTables(
  data = nprs,
  rows = "Sex",
  cols = "CABG",
  pcRow = TRUE,
  pcCol = TRUE,
  pcTot = TRUE
)
# Comparing the 2 groups of aromatherapy
# age and pain have normal distribution
age.pain.wt.t <- ttestIS(
  data = nprs,
  vars = c("age", "pain", "weight", "time"),
  group = "Treatment_group",
  meanDiff=TRUE,
  ci = TRUE,
  welchs=TRUE
)
age.pain.wt.t


# Compare time and weight using Wilcoxon rank sum test
wilcox.test(time ~ Treatment_group, data=nprs)
wilcox.test(weight ~ Treatment_group, data=nprs)
# Stratify pain by CABG
cabg.treat <- nprs |>
  group_by(CABG, Treatment_group) |>
  summarise(
    n = n(),
    mean_pain = mean(pain),
    sd_pain = sd(pain),
    median_pain = median(pain),
    IQR_pain = IQR(pain)
  )
cabg.treat
# For the group that had CABG
cabg_yes.t <- ttestIS(
  data = subset(nprs, CABG == "CABG"),
  vars = "pain",
  group = "Treatment_group",
  meanDiff = TRUE,
  ci = TRUE,
  welchs = TRUE
)
cabg_yes.t
# For the group that did not have CABG
cabg_no.t <- ttestIS(
  data = subset(nprs, CABG == "No CABG"),
  vars = "pain",
  group = "Treatment_group",
  meanDiff = TRUE,
  ci = TRUE,
  welchs = TRUE
)
cabg_no.t
# Stratify by duration of surgery 4h
nprs <- nprs |>
  mutate(
    time_group = ifelse(time > 4, "Long surgery", "Short surgery")
  )
descriptives(
  data = nprs,
  vars = "pain",
  splitBy = c("time_group"),
  sd = TRUE,
  ci=TRUE,
  dens=TRUE,
  box=TRUE
)
# Independent samples t-test for Long Surgery (> 4 hours)
time_long.t <- ttestIS(
  data = subset(nprs, time_group == "Long surgery"),
  vars = "pain",
  group = "Treatment_group",
  meanDiff = TRUE,
  ci = TRUE,
  welchs = TRUE
)
time_long.t
  
  

# Q3 --------------------------------------------------
# Open heart surgery RCT, intervention n=35 and control n=35
# 7 sessions
# First dose 30min before extubation
# Numerical Pain Rating Scale 0-10
# read peppermint.csv file
list.files()
nprs <- read.csv("peppermint.csv")
# Characteristics of participants
nprs <- nprs |>
mutate(Treatment_group = factor(arm, levels = c(0, 1), labels = c("Control Aromatherapy", "Peppermint Oil Aromatherapy")),
Sex = factor(female, levels = c(0, 1), labels = c("Male", "Female")),
CABG = factor(CABG, levels = c(0, 1), labels = c("No CABG", "CABG"))
)
# Check for missing values
colSums(is.na(nprs))
# Check for distribution of variables
skim(nprs)
descriptives(
data = nprs,
vars = c("age", "weight", "time", "pain"),
pc=TRUE,
ci=TRUE,
dens=TRUE,
box=TRUE
)
# Summarising the categorical variables
descriptives(data=nprs, vars = c(Sex, CABG), freq = TRUE)
# Two-way frequency table for Sex and CABG
contTables(
data = nprs,
rows = "Sex",
cols = "CABG",
pcRow = TRUE,
pcCol = TRUE,
pcTot = TRUE
)
# Comparing the 2 groups of aromatherapy
# age and pain have normal distribution
age.pain.t <- ttestIS(
data = nprs,
vars = c("age", "pain"),
group = "Treatment_group",
meanDiff=TRUE,
ci = TRUE,
welchs=TRUE
)
age.pain.t
# Compare time and weight using Wilcoxon rank sum test
wilcox.test(time ~ Treatment_group, data=nprs)
wilcox.test(weight ~ Treatment_group, data=nprs)
# Stratify pain by CABG
cabg.treat <- nprs |>
group_by(CABG, Treatment_group) |>
summarise(
n = n(),
mean_pain = mean(pain),
sd_pain = sd(pain),
median_pain = median(pain),
IQR_pain = IQR(pain)
)
cabg.treat
# For the group that had CABG
cabg_yes.t <- ttestIS(
data = subset(nprs, CABG == "CABG"),
vars = "pain",
group = "Treatment_group",
meanDiff = TRUE,
ci = TRUE,
welchs = TRUE
)
cabg_yes.t
# For the group that did not have CABG
cabg_no.t <- ttestIS(
data = subset(nprs, CABG == "No CABG"),
vars = "pain",
group = "Treatment_group",
meanDiff = TRUE,
ci = TRUE,
welchs = TRUE
)
cabg_no.t
# Stratify by duration of surgery 4h
nprs <- nprs |>
mutate(
time_group = ifelse(time > 4, "Long surgery", "Short surgery")
)
descriptives(
data = nprs,
vars = "pain",
splitBy = c("time_group", "Treatment_group"),
sd = TRUE,
ci=TRUE,
dens=TRUE,
box=TRUE
)
# Independent samples t-test for Long Surgery (> 4 hours)
time_long.t <- ttestIS(
data = subset(nprs, time_group == "Long surgery"),
vars = "pain",
group = "Treatment_group",
meanDiff = TRUE,
ci = TRUE,
welchs = TRUE
)
time_long.t

  