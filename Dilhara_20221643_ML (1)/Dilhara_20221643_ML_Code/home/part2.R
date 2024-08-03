# Load necessary libraries
library(readxl)
library(nnet)
library(caret)
library(dplyr)
library(ggplot2) 

# Load the dataset
exchange_data <- read_excel("ExchangeUSD.xlsx")

# Extract the "USD/EUR" column from exchange_data
exchange_rate <- exchange_data %>% pull(`USD/EUR`)

# Split dataset into training and testing sets
train_data <- exchange_rate[1:400]
test_data <- exchange_rate[401:length(exchange_rate)]

# Define Input Variables for MLP Models (Autoregressive Approach)
create_input <- function(data, lag){
  if (!is.vector(data)) {
    stop("Input data must be a vector.")
  }
  lagged_data <- embed(data, lag + 1)
  input <- lagged_data[, -1]
  output <- lagged_data[, 1]
  return(list(input = input, output = output))
}

# Experiment with four input vectors
lag_values <- c(1, 4, 7, 10)  # Choose lag values
input_vectors <- lapply(lag_values, function(lag) create_input(as.vector(train_data), lag))

# Construct Input/Output Matrices for Training and Testing
train_input <- lapply(input_vectors, function(input) input$input)
train_output <- lapply(input_vectors, function(input) input$output)

# Train MLP Models
models <- lapply(input_vectors, function(input) {
  lapply(c(5, 10, 15), function(size) {
    nnet(train_input[[1]], train_output[[1]], size = size, decay = 1e-5, maxit = 1000, linout = TRUE)
  })
})

# Flatten the list of models
models <- unlist(models, recursive = FALSE)

# Evaluate MLP Models
model_evaluation <- lapply(models, function(model) evaluate_model(model, train_input[[1]], train_output[[1]]))

# Performance table
performance_table <- data.frame(
  Model = paste("Model", 1:length(model_evaluation)),
  Neurons = rep(c(5, 10, 15), each = 4),
  RMSE = sapply(model_evaluation, function(x) x$rmse),
  MAE = sapply(model_evaluation, function(x) x$mae),
  MAPE = sapply(model_evaluation, function(x) x$mape),
  sMAPE = sapply(model_evaluation, function(x) x$smape)
)

# Print performance table
print(performance_table)

# Plot Actual vs. Predicted Exchange Rates
predicted_values <- predict(models[[1]], as.matrix(train_input[[1]]))
actual_vs_predicted <- data.frame(Actual = train_output[[1]], Predicted = predicted_values)

ggplot(actual_vs_predicted, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Exchange Rate", y = "Predicted Exchange Rate") +
  ggtitle("Actual vs. Predicted Exchange Rates")

# Plot RMSE vs. Number of Neurons
rmse_vs_neurons <- data.frame(Neurons = rep(c(5, 10, 15), each = 4), RMSE = performance_table$RMSE)

ggplot(rmse_vs_neurons, aes(x = factor(Neurons), y = RMSE)) +
  geom_boxplot() +
  labs(x = "Number of Neurons", y = "RMSE") +
  ggtitle("RMSE vs. Number of Neurons")

# Density Plot of Residuals
residuals <- actual_vs_predicted$Actual - actual_vs_predicted$Predicted
ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_density(fill = "skyblue", color = "darkblue") +
  labs(x = "Residuals", y = "Density") +
  ggtitle("Density Plot of Residuals")

# Time Series Plot of Actual vs. Predicted Exchange Rates
exchange_ts <- data.frame(
  Date = seq(as.Date("2020-01-01"), by = "day", length.out = length(train_output[[1]])),
  Actual = train_output[[1]],
  Predicted = predicted_values
)

ggplot(exchange_ts, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed") +
  labs(x = "Date", y = "Exchange Rate", color = "Series") +
  ggtitle("Time Series Plot of Actual vs. Predicted Exchange Rates")

# Print test performance metrics
test_rmse <- sapply(models, function(model) {
  predicted_values <- predict(model, as.matrix(train_input[[1]]))
  sqrt(mean((predicted_values - train_output[[1]])^2))
})
test_mae <- sapply(models, function(model) {
  predicted_values <- predict(model, as.matrix(train_input[[1]]))
  mean(abs(predicted_values - train_output[[1]]))
})
test_mape <- sapply(models, function(model) {
  predicted_values <- predict(model, as.matrix(train_input[[1]]))
  mean(abs((predicted_values - train_output[[1]]) / train_output[[1]])) * 100
})
test_smape <- sapply(models, function(model) {
  predicted_values <- predict(model, as.matrix(train_input[[1]]))
  mean(2 * abs(predicted_values - train_output[[1]]) / (abs(predicted_values) + abs(train_output[[1]]))) * 100
})

# Print test performance metrics
cat("Test RMSE:", test_rmse, "\n")
cat("Test MAE:", test_mae, "\n")
cat("Test MAPE:", test_mape, "\n")
cat("Test sMAPE:", test_smape, "\n")

# Find the index of the best model based on minimum RMSE
best_model_index <- which.min(test_rmse)

# Check if models list is not empty
if (length(models) > 0) {
  # Check if best_model_index is within bounds
  if (best_model_index > 0 && best_model_index <= length(models)) {
    # Access the best model
    best_model <- models[[best_model_index]]
    
    # Print the best model's details
    print(best_model)
  } else {
    cat("best_model_index is out of bounds.\n")
  }
} else {
  cat("No models found.\n")
}
