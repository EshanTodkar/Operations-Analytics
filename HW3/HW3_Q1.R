# Load necessary libraries
library(quantreg)
library(ggplot2)

# Read the datasets
train_data <- read.csv("quantile_train.csv")
test_data <- read.csv("quantile_test.csv")

# Part (a): Scatterplot and Quantile Regression Lines
# Plot the scatterplot of quality vs demand
ggplot(train_data, aes(x = quality, y = demand)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatterplot of Quality vs Demand",
       x = "Quality",
       y = "Demand") +
  theme_minimal()

# Fit quantile regression models for different quantiles
taus <- c(0.25, 0.5, 0.75)
quantile_models <- lapply(taus, function(tau) {
  rq(demand ~ quality, data = train_data, tau = tau)
})

# Create a new data frame for predictions
quality_values <- seq(min(train_data$quality), max(train_data$quality), length.out = 100)
predicted_quantiles <- sapply(quantile_models, function(model) {
  predict(model, newdata = data.frame(quality = quality_values))
})
predicted_data <- data.frame(
  quality = rep(quality_values, times = length(taus)),
  demand = as.vector(predicted_quantiles),
  quantile = factor(rep(taus, each = length(quality_values)))
)

# Plot the scatterplot of quality vs demand
ggplot(train_data, aes(x = quality, y = demand)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_line(data = predicted_data, aes(x = quality, y = demand, color = quantile, linetype = quantile), size = 1) +
  labs(title = "Scatterplot of Quality vs Demand with Quantile Regression Lines",
       x = "Quality",
       y = "Demand",
       color = "Quantile",
       linetype = "Quantile") +
  theme_minimal()

# Part (b): Report estimated quantile function for each tau
quantile_functions <- lapply(quantile_models, function(model) {
  coef(model)
})

names(quantile_functions) <- paste("Quantile", taus)
print("Estimated Quantile Functions:")
print(quantile_functions)

# Part (c): Optimal ordering quantity if quality score is 5
quality_value <- 5
ordering_quantities <- sapply(quantile_models, function(model) {
  predict(model, newdata = data.frame(quality = quality_value))
})
names(ordering_quantities) <- paste("Quantile", taus)
print("Optimal Ordering Quantities for Quality Score 5:")
print(ordering_quantities)

# Cost calculation
cost_price <- 10
sale_price <- 30

# Part (d): Out-of-sample cost calculation for test data
test_quality <- test_data$quality
test_demand <- test_data$demand

# Predict demand for each quantile
predicted_demand <- sapply(quantile_models, function(model) {
  predict(model, newdata = data.frame(quality = test_quality))
})

# Calculate the cost for each quantile prediction
calculate_cost <- function(actual, predicted) {
  overstock_cost <- (predicted > actual) * (predicted - actual) * cost_price
  understock_cost <- (predicted < actual) * (actual - predicted) * (sale_price - cost_price)
  total_cost <- overstock_cost + understock_cost
  return(total_cost)
}

# Compute costs for each prediction on the test set
out_of_sample_costs <- apply(predicted_demand, 2, function(pred) {
  sum(calculate_cost(test_demand, pred))
})
names(out_of_sample_costs) <- paste("Quantile", taus)
print("Out-of-Sample Costs:")
print(out_of_sample_costs)