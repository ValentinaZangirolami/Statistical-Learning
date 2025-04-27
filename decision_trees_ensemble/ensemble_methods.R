################################################################################
#                                                                              #
#                             Ensemble methods                                 #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

# you can find the notes of this lecture in my github: https://github.com/ValentinaZangirolami/Statistical-Learning

#load libraries
library(car)
library(vip)
library(ggplot2)
library(tidymodels)
library(baguette)
library(tidyverse)

esg_data <- read.csv("esg_financial_dataset.csv")
head(esg_data)

#split train-val

set.seed(123)  # For reproducibility
split <- initial_split(esg_data, prop = 0.8)

train_data <- training(split)
val_data <- testing(split)

#bagging

# Create recipe
bag_recipe <- recipe(MarketCap ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Create bagging model
bag_spec <- bag_tree(cost_complexity = 0.01, tree_depth = 30, min_n = 20) %>%
  set_engine("rpart", times = 50) %>%
  set_mode("regression")

# Create workflow
bag_wf <- workflow() %>%
  add_recipe(bag_recipe) %>%
  add_model(bag_spec)

# Fit model
bag_fit <- fit(bag_wf, data = train_data)

# Get predictions on training data
train_preds <- predict(bag_fit, new_data = train_data) %>% 
  bind_cols(train_data)  # Attach true values
# Get predictions on validation data
val_preds <- predict(bag_fit, new_data = val_data) %>% 
  bind_cols(val_data)  # Attach true values

# Compute metrics (e.g., RMSE, R-squared for regression)
metrics <- metric_set(rmse, rsq)
train_metrics <- metrics(train_preds, truth = MarketCap, estimate = .pred)
val_metrics <- metrics(val_preds, truth = MarketCap, estimate = .pred)

round(train_metrics$.estimate, 4)
round(val_metrics$.estimate,4)

#random forest

rf_spec <- rand_forest(mtry = 8, trees = 10, min_n = 8) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Create workflow
rf_wf <- workflow() %>%
  add_recipe(esg_recipe) %>%
  add_model(rf_spec)

# Fit model
rf_fit <- fit(rf_wf, data = train_data)

# Get predictions on training data
train_preds <- predict(rf_fit, new_data = train_data) %>% 
  bind_cols(train_data)  # Attach true values
# Get predictions on validation data
val_preds <- predict(rf_fit, new_data = val_data) %>% 
  bind_cols(val_data)  # Attach true values

# Compute metrics (e.g., RMSE, R-squared for regression)
metrics <- metric_set(rmse, rsq)
train_metrics <- metrics(train_preds, truth = MarketCap, estimate = .pred)
val_metrics <- metrics(val_preds, truth = MarketCap, estimate = .pred)

round(train_metrics$.estimate, 4)
round(val_metrics$.estimate,4)

# reduce complexity

rf_spec <- rand_forest(mtry = 8, trees = 6, min_n = 8) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Create workflow
rf_wf <- workflow() %>%
  add_recipe(esg_recipe) %>%
  add_model(rf_spec)

# Fit model
rf_fit <- fit(rf_wf, data = train_data)

# Get predictions on training data
train_preds <- predict(rf_fit, new_data = train_data) %>% 
  bind_cols(train_data)  # Attach true values
# Get predictions on validation data
val_preds <- predict(rf_fit, new_data = val_data) %>% 
  bind_cols(val_data)  # Attach true values

# Compute metrics (e.g., RMSE, R-squared for regression)
metrics <- metric_set(rmse, rsq)
train_metrics <- metrics(train_preds, truth = MarketCap, estimate = .pred)
val_metrics <- metrics(val_preds, truth = MarketCap, estimate = .pred)

round(train_metrics$.estimate, 4)
round(val_metrics$.estimate,4)

# feature importance

final_model <- extract_fit_engine(rf_fit)

vip(final_model, geom = "point", horizontal = FALSE) +
  ggtitle("Decision Tree Feature Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#exercise

# Analyze the dataset london_bike_sharing from e-learning and apply 
# appropriate statistical learning while finding good hyperparameters whether it is possible.

london_data <- read.csv("london_bike_sharing.csv")
head(london_data)

