# Replacing categorical values of Churn (Yes/No) with numerical values (1/0),
# respectively
count(df, Churn)
churn_numeric <- mutate(df, Churn = ifelse(Churn == "Yes", 1, 0))
count(df, Churn)


# Perform chi-squared to confirm this result with p-value

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

# All p-values are greater than the standard alpha of 0.05
# No apparent relationship between Churn & the values from PC1


plot_grid(
  
  ggplot(df, aes(x=Tenure, fill = Churn, color = Churn)) +
    geom_histogram(binwidth = 1),
  
  ggplot(df, aes(x=Bandwidth_GB_Year, fill = Churn, color = Churn)) +
    geom_histogram(binwidth = 100),
  
  ncol = 2, nrow = 1)

################################################################################


# Analysis of variables in PC2: Zip, Lng

plot_grid(
  
  ggplot(df, aes(x=Churn, y=Zip)) +
    geom_boxplot(),
  
  ggplot(df, aes(x=Churn, y=Lng)) +
    geom_boxplot(),
  
  ncol = 2, nrow = 1)


# Based on boxplots, the distribution appears highly similar between Churn-Yes
# and Churn-No populations. There is no statistically significant relationship
# from the variables in PC2.

################################################################################


# Analysis of variables in PC3: Tenure, Bandwidth_GB_Year

plot_grid(
  
  ggplot(df, aes(x=Churn, y=Tenure)) +
    geom_boxplot(),
  
  ggplot(df, aes(x=Churn, y=Bandwidth_GB_Year)) +
    geom_boxplot(),
  
  ncol = 2, nrow = 1)


# There are notable differences in the box plots which suggests there is need
# for further analysis.

tenure_churn <- table(df$Churn, df$Tenure)
summary(tenure_churn)

bandwidth_churn <- table(df$Churn, df$Bandwidth_GB_Year)
summary(bandwidth_churn)

# The p-value for the chi-squared analysis of both the Tenure and Bandwidth
# variables are above the standard alpha level of 0.05. We cannot reject the
# null hypothesis.

################################################################################


# Analysis of variables in PC5: Lat, Population

plot_grid(
  
  ggplot(df, aes(x=Churn, y=Lat)) +
    geom_boxplot(),
  
  ggplot(df, aes(x=Churn, y=Population)) +
    geom_boxplot(),
  
  ncol = 2, nrow = 1)


# Based on boxplots, the distribution appears highly similar between Churn-Yes
# and Churn-No populations. There is no statistically significant relationship
# from the variables in PC5.

################################################################################


# Analysis of variables in PC8: Income, MonthlyCharge

plot_grid(
  
  ggplot(df, aes(x=Churn, y=Income)) +
    geom_boxplot(),
  
  ggplot(df, aes(x=Churn, y=MonthlyCharge)) +
    geom_boxplot(),
  
  ncol = 2, nrow = 1)


# There are notable differences in the box plots for MonthlyCharge which 
# suggests there is need for further analysis.

monthly_churn <- table(df$Churn, df$MonthlyCharge)
summary(monthly_churn)

# The p-value is well below the 0.05 standard threshold. We cannot reject the
# null hypothesis.

