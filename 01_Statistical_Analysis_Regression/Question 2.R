library(readxl)
library(car)
library(dplyr)
library(ggplot2)
library(GGally)
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

# linear trend model/simple linear regression
mod.trend <- lm(GASEXP ~ YEAR, 
                data = gas.train)
summary(mod.trend)


# compute OSR^2
gasPredictions.trend <- predict(mod.trend, newdata=gas.test)
# this builds a vector of predicted values on the test set
SSE.trend = sum((gas.test$GASEXP - gasPredictions.trend)^2)
SST = sum((gas.test$GASEXP - mean(gas.train$GASEXP))^2)
OSR2.trend = 1 - SSE.trend/SST

# Forecasting gasoline expenditure for 2005
new_data <- data_frame(YEAR = 2005)
forecast_2005 <- predict(mod.trend, newdata = new_data)
cat("Forecasted gasoline expenditure in 2005:", forecast_2005, "\n")

# Report the estimated values for 𝛽0 and 𝛽1. How do you interpret these two values?
# B0 = 6098.0385
# 𝛽0 represents the intercept of the regression line. It is the value of the dependent variable 
# (gasoline expenditure) when the independent variable (Year) is zero. 
# B1 = 3.1147 
# (Slope of regression line)

# Report the R2 and OSR2. How do you interpret their meaning?
# R2 = 0.8898 (The coefficient of determination for the training data.)
# OSR2 = 0.896 (The performance of the model on the test data.)

# Report the p-value associated with 𝛽1. How do you interpret this p-value?
# 𝛽1 P-Value = <2e-16. P-value is the level of statistical significance. 
# Smaller p-values are better(p<0.05 to be significant).


