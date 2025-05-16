################################################################################
#                                                                              #
#                             Gradient boosting                                #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

#load libraries
library(car)
library(ggplot2)
library(tidymodels)
library(tidyverse)
library(glmtoolbox)
library(plotly)

#gradient descent

#when we have a lot of parameters, gradient descent is a computationally efficient method to find local minimum of a loss function
#requiring just the existence of first derivative
#Let consider a simple function with just one minimum

set.seed(123)
f <- function(x, y) {
  (x - 1)^2 + (y - 2)^2 + 2 * sin(2*x) * cos(3*y)  
}

grad_f <- function(x, y) {
  dx <- 2*(x - 1) + cos(x)*cos(y)   # Partial derivative w.r.t x
  dy <- 2*(y - 2) - sin(x)*sin(y)   # Partial derivative w.r.t y
  c(dx, dy)
}
gradient_descent <- function(start, learning_rate = 0.1, n_iter = 100) {
  path <- matrix(0, nrow = n_iter + 1, ncol = 2)
  path[1, ] <- start
  
  for (i in 1:n_iter) {
    grad <- grad_f(path[i, 1], path[i, 2])
    path[i + 1, ] <- path[i, ] - learning_rate * grad
  }
  
  return(path)
}

x <- seq(-5, 5, length.out = 100)
y <- seq(-5, 5, length.out = 100)
z <- outer(x, y, Vectorize(f))

# Run gradient descent 
path <- gradient_descent(start = c(4, 5), learning_rate = 0.1, n_iter = 50)

# Plot gradient descent
plot_ly() %>%
  add_surface(x = ~x, y = ~y, z = ~z, opacity = 0.8, colorscale = "Earth") %>%
  add_trace(
    x = path[, 1], 
    y = path[, 2], 
    z = apply(path, 1, function(p) f(p[1], p[2])) + 0.1,  # <-- Add small offset
    type = "scatter3d", mode = "lines+markers", 
    line = list(color = "red", width = 4),
    marker = list(color = "red", size = 4)
  ) %>%
  layout(title = "Gradient Descent Path")

# multiple minima

# Himmelblau's function
f <- function(x, y) (x^2 + y - 11)^2 + (x + y^2 - 7)^2

# Gradient of Himmelblau's function
grad_f <- function(x, y) {
  dx <- 4 * x * (x^2 + y - 11) + 2 * (x + y^2 - 7)
  dy <- 2 * (x^2 + y - 11) + 4 * y * (x + y^2 - 7)
  c(dx, dy)
}

# Run gradient descent from different starting points
path1 <- gradient_descent(start = c(-3, 3), learning_rate = 0.01, n_iter = 100)
path2 <- gradient_descent(start = c(3, -2), learning_rate = 0.01, n_iter = 100)

# Create the plot
plot_ly() %>%
  add_surface(x = ~x, y = ~y, z = ~z, opacity = 0.7, colorscale = "Viridis") %>%
  add_trace(
    x = path1[, 1], y = path1[, 2], z = apply(path1, 1, function(p) f(p[1], p[2])),
    type = "scatter3d", mode = "lines+markers",
    line = list(color = "red", width = 4),
    marker = list(color = "red", size = 4),
    name = "Path 1"
  ) %>%
  add_trace(
    x = path2[, 1], y = path2[, 2], z = apply(path2, 1, function(p) f(p[1], p[2])),
    type = "scatter3d", mode = "lines+markers",
    line = list(color = "blue", width = 4),
    marker = list(color = "blue", size = 4),
    name = "Path 2"
  ) %>%
  layout(
    title = "Gradient Descent on Himmelblau's Function",
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "f(X, Y)")
    )
  )

# Himmelblau's function
himmelblau <- function(x, y) {
  (x^2 + y - 11)^2 + (x + y^2 - 7)^2
}

# Gradient of Himmelblau's function
grad_himmelblau <- function(x, y) {
  dx <- 4 * x * (x^2 + y - 11) + 2 * (x + y^2 - 7)
  dy <- 2 * (x^2 + y - 11) + 4 * y * (x + y^2 - 7)
  c(dx, dy)
}

x <- seq(-5, 5, length.out = 300)
y <- seq(-5, 5, length.out = 300)
z <- outer(x, y, Vectorize(f))

path1 <- gradient_descent(start = c(-3, 3), learning_rate = 0.01, n_iter = 200)  # Converges to (-3.78, -3.28)
path2 <- gradient_descent(start = c(3, -2), learning_rate = 0.01, n_iter = 200)   # Converges to (3.58, -1.85)
path3 <- gradient_descent(start = c(-2, 2), learning_rate = 0.01, n_iter = 200) # Converges to (-2.81, 3.13)
path4 <- gradient_descent(start = c(2, 2), learning_rate = 0.01, n_iter = 200)    # Converges to (3.0, 2.0)

plot_ly() %>%
  add_surface(
    x = ~x, y = ~y, z = ~z,
    opacity = 0.7,
    colorscale = list(c(0, 0.3, 0.6, 1), c("blue", "cyan", "yellow", "red")),
    contours = list(
      z = list(show = TRUE, usecolormap = TRUE, highlightcolor = "#ff0000")
    )
  ) %>%
  add_trace(
    x = path1[, 1], y = path1[, 2], z = apply(path1, 1, function(p) himmelblau(p[1], p[2])),
    type = "scatter3d", mode = "lines+markers",
    line = list(color = "red", width = 4),
    marker = list(color = "red", size = 4),
    name = "Path 1"
  ) %>%
  add_trace(
    x = path2[, 1], y = path2[, 2], z = apply(path2, 1, function(p) himmelblau(p[1], p[2])),
    type = "scatter3d", mode = "lines+markers",
    line = list(color = "blue", width = 4),
    marker = list(color = "blue", size = 4),
    name = "Path 2"
  ) %>%
  add_trace(
    x = path3[, 1], y = path3[, 2], z = apply(path3, 1, function(p) himmelblau(p[1], p[2])),
    type = "scatter3d", mode = "lines+markers",
    line = list(color = "green", width = 4),
    marker = list(color = "green", size = 4),
    name = "Path 3"
  ) %>%
  add_trace(
    x = path4[, 1], y = path4[, 2], z = apply(path4, 1, function(p) himmelblau(p[1], p[2])),
    type = "scatter3d", mode = "lines+markers",
    line = list(color = "purple", width = 4),
    marker = list(color = "purple", size = 4),
    name = "Path 4"
  ) %>%
  layout(
    title = "Gradient Descent on Himmelblau's Function (4 Minima)",
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "f(X, Y)", range = c(0, 2000))  # Adjust z-axis range
    )
  )
# Gradient descent for linear regression

cost <- function(beta, x, y) {
  sum((y - (beta[1] + beta[2] * x))^2) / length(x)
}

gradient <- function(beta, x, y) {
  y_pred <- beta[1] + beta[2] * x
  d_b0 <- -2 * sum(y - y_pred) / length(x)  # Partial derivative w.r.t. beta0
  d_b1 <- -2 * sum((y - y_pred) * x) / length(x)  # Partial derivative w.r.t. beta1
  c(d_b0, d_b1)
}

gradient_descent <- function(x, y, beta_init, lr = 0.01, n_iter = 100) {
  beta <- matrix(0, nrow = n_iter + 1, ncol = 2)
  beta[1, ] <- beta_init
  cost_history <- numeric(n_iter + 1)
  cost_history[1] <- cost(beta_init, x, y)
  
  for (i in 1:n_iter) {
    grad <- gradient(beta[i, ], x, y)
    beta[i + 1, ] <- beta[i, ] - lr * grad
    cost_history[i + 1] <- cost(beta[i + 1, ], x, y)
  }
  
  list(beta = beta, cost_history = cost_history)
}

data(advertising)

# Initial guess (intercept = 0, slope = 0)
beta_init <- c(3, 0)
result <- gradient_descent(advertising$TV, advertising$sales, beta_init, lr = 0.00001, n_iter = 10000)

# Final coefficients
cat("Final coefficients (intercept, slope):", round(result$beta[51, ], 2)) # compare with estimated betas in simple linear regression class

# exercise: try to change the initial betas and learning rate

rm(list = ls())

#gradient boosting

# gradient descent can help to obtain efficiently the estimates while allowing more complex trees 

#load data
esg_data <- read.csv("esg_financial_dataset.csv")
head(esg_data)

#split train-val

set.seed(123)  # For reproducibility
split <- initial_split(esg_data, prop = 0.8)

train_data <- training(split)
val_data <- testing(split)

# gradient boosting 

grad_boost <- boost_tree(
  trees = 1000,               # Number of trees
  tree_depth = 6,             # Maximum tree depth
  learn_rate = 0.01,          # Learning rate (eta)
  min_n = 5,                  # Minimum number of observations in terminal nodes
  loss_reduction = 0,         # Gamma (minimum loss reduction)
  sample_size = 1,            # Subsample ratio (1 = no subsampling)
  mtry = NULL                 # Feature fraction (NULL = all features)
) %>%
  set_engine("xgboost") %>%   # Use xgboost engine
  set_mode("regression") 

#recipe

esg_recipe <- recipe(MarketCap~., train_data) |> 
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

grad_work <- 
  workflow() |>
  add_model(grad_boost) |> 
  add_recipe(esg_recipe)

grad_fit <- fit(grad_work, train_data)

# Get predictions on training data
train_preds <- predict(grad_fit, new_data = train_data) %>% 
  bind_cols(train_data)  # Attach true values
# Get predictions on validation data
val_preds <- predict(grad_fit, new_data = val_data) %>% 
  bind_cols(val_data)  # Attach true values

# Compute metrics (e.g., RMSE, R-squared for regression)
metrics <- metric_set(rmse, rsq)
train_metrics <- metrics(train_preds, truth = MarketCap, estimate = .pred)
val_metrics <- metrics(val_preds, truth = MarketCap, estimate = .pred)

round(train_metrics$.estimate, 4)
round(val_metrics$.estimate,4) #overfitting

#let's try to reduce learning rate

grad_boost_2 <- boost_tree(
  trees = 1000,               # Number of trees
  tree_depth = 6,             # Maximum tree depth
  learn_rate = 0.0001,          # Learning rate (eta)
  min_n = 5,                  # Minimum number of observations in terminal nodes
  loss_reduction = 0,         # Gamma (minimum loss reduction)
  sample_size = 1,            # Subsample ratio (1 = no subsampling)
  mtry = NULL                 # Feature fraction (NULL = all features)
) %>%
  set_engine("xgboost") %>%   # Use xgboost engine
  set_mode("regression") 

grad_work_2 <- 
  workflow() |>
  add_model(grad_boost_2) |> 
  add_recipe(esg_recipe)

grad_fit_2 <- fit(grad_work_2, train_data)

# Get predictions on training data
train_preds <- predict(grad_fit_2, new_data = train_data) %>% 
  bind_cols(train_data)  # Attach true values
# Get predictions on validation data
val_preds <- predict(grad_fit_2, new_data = val_data) %>% 
  bind_cols(val_data)  # Attach true values

# Compute metrics (e.g., RMSE, R-squared for regression)
metrics <- metric_set(rmse, rsq)
train_metrics <- metrics(train_preds, truth = MarketCap, estimate = .pred)
val_metrics <- metrics(val_preds, truth = MarketCap, estimate = .pred)

round(train_metrics$.estimate, 4)
round(val_metrics$.estimate,4) #reduced overfitting but worst performances

#let's try to reduce number of trees

grad_boost_2 <- boost_tree(
  trees = 1200,               # Number of trees
  tree_depth = 6,             # Maximum tree depth
  learn_rate = 0.01,          # Learning rate (eta)
  min_n = 5,                  # Minimum number of observations in terminal nodes
  loss_reduction = 0,         # Gamma (minimum loss reduction)
  sample_size = 1,            # Subsample ratio (1 = no subsampling)
  mtry = NULL                 # Feature fraction (NULL = all features)
) %>%
  set_engine("xgboost") %>%   # Use xgboost engine
  set_mode("regression") 

grad_work_2 <- 
  workflow() |>
  add_model(grad_boost_2) |> 
  add_recipe(esg_recipe)

grad_fit_2 <- fit(grad_work_2, train_data)

# Get predictions on training data
train_preds <- predict(grad_fit_2, new_data = train_data) %>% 
  bind_cols(train_data)  # Attach true values
# Get predictions on validation data
val_preds <- predict(grad_fit_2, new_data = val_data) %>% 
  bind_cols(val_data)  # Attach true values

# Compute metrics (e.g., RMSE, R-squared for regression)
metrics <- metric_set(rmse, rsq)
train_metrics <- metrics(train_preds, truth = MarketCap, estimate = .pred)
val_metrics <- metrics(val_preds, truth = MarketCap, estimate = .pred)

round(train_metrics$.estimate, 4)
round(val_metrics$.estimate,4) #overfitting

#try other hyperparameters
