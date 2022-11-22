## Author: Stephen E. Porter
## Title: Churn Data Analysis
## Course: WGU D207: Exploratory Data Analysis
## Instructor: Dr. William Sewell


################################################################################


# Libraries
library(tidyverse)
library(ggplot2)
library(cowplot)

# Importing cleaned data file & getting basic overview

df <- read.csv(file = 'C:/WGU/D207 Exploratory Data Analysis/churn_clean.csv')
colnames(df)


# Renaming unclear columns named Item1 through Item8 for improved readability &
# confirming they have been renamed correctly

df <- df %>%
  rename(
    Response = Item1,
    Fix = Item2,
    Replacement = Item3,
    Reliability = Item4,
    Options = Item5,
    Respectful = Item6,
    Courteous = Item7,
    Listening = Item8
  )


colnames(df)

# Summary statistics for each column

summary(df)


################################################################################


# Analysis Question: Do any of the variables identified in the Principal
# Component Analysis have an effect on customer churn?


################################################################################


# Analysis of variables in PC1: Response, Fix, Replacement, Respectful,
# Courteous, Listening

plot_grid(
  
  ggplot(df, aes(x=Churn, y=Response)) +
    geom_boxplot(),
  
  ggplot(df, aes(x=Churn, y=Fix)) +
    geom_boxplot(),
  
  ggplot(df, aes(x=Churn, y=Replacement)) +
    geom_boxplot(),
  
  ggplot(df, aes(x=Churn, y=Respectful)) +
    geom_boxplot(),
  
  ggplot(df, aes(x=Churn, y=Courteous)) +
    geom_boxplot(),
  
  ggplot(df, aes(x=Churn, y=Listening)) +
    geom_boxplot(),
  
  ncol = 3, nrow = 2)

response_churn <- table(df$Churn, df$Response)
summary(response_churn)

fix_churn <- table(df$Churn, df$Fix)
summary(fix_churn)

replacement_churn <- table(df$Churn, df$Replacement)
summary(replacement_churn)

respectful_churn <- table(df$Churn, df$Respectful)
summary(respectful_churn)

courteous_churn <- table(df$Churn, df$Courteous)
summary(courteous_churn)

listening_churn <- table(df$Churn, df$Listening)
summary(listening_churn)

# All p-values lie outside of the standard 0.05 alpha value. We cannot reject
# the null hypothesis.

################################################################################


# Analysis of variables from PC4: Reliability, Options

plot_grid(
  
  ggplot(df, aes(x=Churn, y=Reliability)) +
    geom_boxplot(),

  ggplot(df, aes(x=Churn, y=Options)) +
    geom_boxplot(),

  ncol = 2, nrow = 1)

reliability_churn <- table(df$Churn, df$Reliability)
summary(reliability_churn)

options_churn <- table(df$Churn, df$Options)
summary(options_churn)

# All p-values lie outside of the standard 0.05 alpha value. We cannot reject
# the null hypothesis.


################################################################################


# Analysis of variables in PC6: Children, Age, Contacts

plot_grid(
  
  ggplot(df, aes(x=Churn, y=Children)) +
    geom_boxplot(),

  ggplot(df, aes(x=Churn, y=Age)) +
    geom_boxplot(),

  ggplot(df, aes(x=Churn, y=Contacts)) +
    geom_boxplot(),

  ncol = 3, nrow = 1)

children_churn <- table(df$Churn, df$Children)
summary(children_churn)

age_churn <- table(df$Churn, df$Age)
summary(age_churn)

contacts_churn <- table(df$Churn, df$Contacts)
summary(contacts_churn)

# All p-values lie outside of the standard 0.05 alpha value. We cannot reject
# the null hypothesis.


################################################################################


# Analysis of variables in PC7: Email, Yearly_equip_failure

plot_grid(
  
  ggplot(df, aes(x=Churn, y=Email)) +
    geom_boxplot(),

  ggplot(df, aes(x=Churn, y=Yearly_equip_failure)) +
    geom_boxplot(),

  ncol = 2, nrow = 1)

email_churn <- table(df$Churn, df$Email)
summary(email_churn)

yef_churn <- table(df$Churn, df$Yearly_equip_failure)
summary(yef_churn)

# All p-values lie outside of the standard 0.05 alpha value. We cannot reject
# the null hypothesis.


################################################################################


# Graphing various relationships in the data frame

# Q-Q Plots of Tenure, Bandwidth_GB_Year and MonthlyCharge
plot_grid(
  qplot(sample = Tenure, data = df) +
    geom_qq_line() +
    labs(title = "Q-Q Tenure", y="Months"),
  
  qplot(sample = Bandwidth_GB_Year, data = df) +
    geom_qq_line() +
    labs(title = "Q-Q Bandwidth", y="GB (Year)"),
  
  qplot(sample = MonthlyCharge, data = df) +
    geom_qq_line() +
    labs(title = "Q-Q Monthly Charge", y="US Dollars"),
  
  ncol = 3, nrow = 1)


# Univariate graphs of continuous & categorical variables

plot_grid(
  ggplot(df, aes(x=Tenure)) +
    geom_histogram(binwidth = 3) +
    labs(title = "Tenure"),
  
  ggplot(df, aes(x=Bandwidth_GB_Year)) +
    geom_histogram(binwidth = 100) +
    labs(title = "Bandwidth"),
  
  ggplot(df, aes(x=MonthlyCharge)) +
    geom_histogram(binwidth = 10) +
    labs(title = "Monthly Charge"),
  
  ncol = 3, nrow = 1)

plot_grid(
  ggplot(df, aes(y=Contract)) +
    geom_bar() +
    labs(title = "Contract Type"),
  
  ggplot(df, aes(y=Marital)) +
    geom_bar() +
    labs(title = "Marital Status"),
  
  ncol = 2, nrow = 1)

# Bivariate graphs of continuous & categorical variables vs Churn

plot_grid(
  ggplot(df, aes(x=Tenure, color = Churn, fill = Churn)) +
    geom_histogram(binwidth = 3) +
    labs(title = "Tenure") +
    theme(legend.position = 'top'),
  
  ggplot(df, aes(x=Bandwidth_GB_Year, color = Churn, fill = Churn)) +
    geom_histogram(binwidth = 100) +
    labs(title = "Bandwidth") +
    theme(legend.position = 'top'),
  
  ggplot(df, aes(x=MonthlyCharge, color = Churn, fill = Churn)) +
    geom_histogram(binwidth = 10) +
    labs(title = "Monthly Charge") +
    theme(legend.position = 'top'),

  ncol = 3, nrow = 1)

plot_grid(
  
  ggplot(df, aes(y=Contract, color = Churn, fill = Churn)) +
    geom_bar() +
    labs(title = "Contract by Churn") +
    theme(legend.position = 'top'),
  
  ggplot(df, aes(y=Marital,  color = Churn, fill = Churn)) +
    geom_bar() +
    labs(title = "Marital by Churn") +
    theme(legend.position = 'top'),
  
  ncol = 2, nrow = 1)

# Bivariate graphs of categorical variables
plot_grid(
  ggplot(df, aes(x=Contract, color = Marital, fill = Marital)) +
    labs(title = "Contract Type by Marital Status") +
    geom_bar(),
  
  ggplot(df, aes(x=Contract, color = Area, fill = Area)) +
    labs(title = "Contract Type by Area") +
    geom_bar(),
  
  ncol = 1, nrow = 2)


#Scatterplots of MonthlyCharge, Bandwidth_GB_Year, and Tenure
plot_grid(

ggplot(df, aes(x=MonthlyCharge, y=Bandwidth_GB_Year, color = Churn, alpha = 0.5)) +
  geom_point() +
  geom_smooth(method = "auto") +
  labs(title = "Monthly Charge vs Bandwidth"),

ggplot(df, aes(x=MonthlyCharge, y=Tenure, color = Churn, alpha = 0.5)) +
  geom_point() +
  geom_smooth(method = "auto") +
  labs(title = "Monthly Charge vs Tenure"),

ncol = 2, nrow = 1)
