library(car)
library(dplyr)
library(ggplot2)
library(GGally)
library(readxl)

# Load data:
gas <- read_excel("gas.xlsx") 

View(gas)


gas <- gas %>%
  mutate(POP=lag(POP), GASEXP=lag(GASEXP), INCOME=lag(INCOME), PNC=lag(PNC), PUC=lag(PUC), YEAR=lag(YEAR), GASP=lag(GASP))

# Data Pre-processing
gas <- gas %>%
  mutate(POP=lag(POP), GASEXP=lag(GASEXP), INCOME=lag(INCOME), PNC=lag(PNC), PUC=lag(PUC), YEAR=lag(YEAR), GASP=lag(GASP)) %>%
  na.omit()

# Examine data
str(gas)
head(gas)

# split into training and test sets 
gas.train <- filter(gas, YEAR <= 1993) 
head(gas.train)
tail(gas.train)
gas.test <- filter(gas, YEAR > 1993)

# multiple linear regression model
#lm(y~x1+x2+...,data)
mod1 <- lm(GASEXP ~ ., 
           data = gas.train)
summary(mod1)

# compute OSR^2
gasPredictions <- predict(mod1, newdata=gas.test)
# this builds a vector of predicted values on the test set
SSE = sum((gas.test$GASEXP - gasPredictions)^2)
OSR2 = 1 - SSE/SST

### Feature Selection for Interpretation ###
############################################
vif(mod1)

# A better model...
# Remove YEAR
mod2 <- lm(GASEXP ~ .-YEAR, 
           data = gas.train)
summary(mod2)
vif(mod2)

# Remove YEAR, INCOME
mod3 <- lm(GASEXP ~ .-YEAR -INCOME, 
           data = gas.train)
summary(mod3)
vif(mod3)

# Remove YEAR
mod4 <- lm(GASEXP ~ .-YEAR -INCOME -PNC, 
           data = gas.train)
summary(mod4)
vif(mod4)

gasPredictions.final <- predict(mod4, newdata=gas.test)
SSE.final = sum((gas.test$GASEXP - gasPredictions.final)^2)
OSR2.final = 1 - SSE.final/SST
summary(gasPredictions.final)


# Report the R2 and OSR2. What can you imply from the R2 and OSR2?
# R2 = 0.987 (The coefficient of determination for the training data.)
# OSR2 = 0.871 (The performance of the model on the test data.)

# Report the estimated coefficient associated with POP. Does the sign of the coefficient make sense to you? What can you imply from it?
# Estimated coefficient = -1.404e-03
# A negative sign on the coefficient of POP would suggest that as the population increases, gasoline expenditure decreases.
# Implication: A negative coefficient could imply that the rise in population has been accompanied by shifts in behavior or technology that reduce gasoline dependence.

# Report the VIFs for all the variables. What can you imply from the VIFs?
# YEAR        POP       GASP     INCOME        PNC        PUC 
# 3838.48138 1887.96693   13.19133  335.44178   68.99352   55.49170 

# Variance Inflation Factor (VIF) is a measure used in regression analysis to detect multicollinearity, which occurs when independent variables are highly correlated with each other. 
# High multicollinearity can make it difficult to determine the individual effect of each variable on the dependent variable, leading to unreliable estimates of the coefficients.

# Build a better model using feature selection. Show, step by step, which variable you removed and why you chose to remove it.
# --Included in code--

# Report your final model and the final R2 and OSR2.
# R2 = 0.973
# OSR2 = 0.948

