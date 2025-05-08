################################################################################
#                                                                              #
#                              Decision Trees                                  #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################


#load libraries
library(car)
library(ggplot2)
library(tidymodels)
library(tidyverse)
library(rpart.plot)
library(vip)

esg_data <- read.csv("esg_financial_dataset.csv")
head(esg_data)

#from this class, we introduce tidymodels!

#cross-validation

set.seed(123)  # For reproducibility
split <- initial_split(esg_data, prop = 0.8)

train_data <- training(split)
val_data <- testing(split)

folds <- vfold_cv(train_data, v = 5)
folds

# 4 step: build a model, create a recipe, define a workflow and fit your model!

#With tidymodels, we need to define the functional form of our model. Thus, in this case, we need to call decision_tree 
#specifyng the main hyperparameters (you can look them up on the help page) and, if you want, 
#you can also call the engine (i.e. what kind of decision tree implementation you want). 
#The most popular library on R is rpart. After that, we should also specify if we are considering regression or classification.

tree_spec <- decision_tree(cost_complexity = 0.01, tree_depth=30, min_n=20) |>
  set_engine("rpart") |> set_mode("regression")

# Another component of tidymodels is the recipe. In general, we can use it to preprocess
# the data and also create new predictors. In our case, we already add new features
# but the recipe can be helpful to perform most classical operation of data preprocessing
# or feature engineering
# For instance, in this case we will perform one-hot encoding for categorical predictors
# and we will remove columns from the data when the training set data have a single value
# (hence, it is better add it after step_dummy)

esg_recipe <- recipe(MarketCap~., train_data) |> 
  step_dummy(all_nominal_predictors()) |>   
  step_zv(all_predictors())

# The third step requires to create a workflow: it is used to merge the model and the recipe

rt_work <- 
  workflow() |>
  add_model(tree_spec) |> 
  add_recipe(esg_recipe)

# Finally, we can fit our model

trees_cv_results <- fit_resamples(rt_work, folds)
trees_fit_1 <- fit(rt_work, train_data)

# Get predictions on training data
train_preds <- predict(trees_fit_1, new_data = train_data) %>% 
  bind_cols(train_data)  # Attach true values
# Get predictions on validation data
val_preds <- predict(trees_fit_1, new_data = val_data) %>% 
  bind_cols(val_data)  # Attach true values

# Compute metrics (e.g., RMSE, R-squared for regression)
metrics <- metric_set(rmse, rsq)
train_metrics <- metrics(train_preds, truth = MarketCap, estimate = .pred)
val_metrics <- metrics(val_preds, truth = MarketCap, estimate = .pred)

round(train_metrics$.estimate, 4)
round(val_metrics$.estimate,4)

#new hyperparameter for cost complexity

tree_spec <- decision_tree(cost_complexity = 0.1, tree_depth=30, min_n=20) |>
  set_engine("rpart") |> set_mode("regression")

rt_work <- 
  workflow() |>
  add_model(tree_spec) |> 
  add_recipe(esg_recipe)

tree_spec <- decision_tree(cost_complexity = 0.1, tree_depth=30, min_n=20) |>
  set_engine("rpart") |> set_mode("regression")

rt_work <- 
  workflow() |>
  add_model(tree_spec) |> 
  add_recipe(esg_recipe)

trees_fit_2 <- fit(rt_work, train_data)

# Get predictions on training data
train_preds <- predict(trees_fit_2, new_data = train_data) %>% 
  bind_cols(train_data)  # Attach true values
# Get predictions on validation data
val_preds <- predict(trees_fit_2, new_data = val_data) %>% 
  bind_cols(val_data)  # Attach true values

# Compute metrics (e.g., RMSE, R-squared for regression)
metrics <- metric_set(rmse, rsq)
train_metrics <- metrics(train_preds, truth = MarketCap, estimate = .pred)
val_metrics <- metrics(val_preds, truth = MarketCap, estimate = .pred)

round(train_metrics$.estimate, 4)
round(val_metrics$.estimate,4)

#grid_search --> don't run!

tree_spec <- decision_tree(cost_complexity = 0.01, tree_depth=tune(), min_n=tune()) |>
  set_engine("rpart") |> set_mode("regression")

tree_grid <- grid_regular(
  tree_depth(),
  min_n(),
  levels = 20  # Number of values for each parameter
)

rt_work <- 
  workflow() |>
  add_model(tree_spec) |> 
  add_recipe(esg_recipe)


tree_res <- tune_grid(
  rt_work,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(rmse, rsq)  # Choose appropriate metrics
)

best_tree <- tree_res |> select_best("rmse")

final_wf <- tree_wf |>  
  finalize_workflow(best_tree)

final_fit <- final_wf |> 
  fit(train_data)

# Get predictions on training data
train_preds <- predict(final_fit, new_data = train_data) %>% 
  bind_cols(train_data)  # Attach true values
# Get predictions on validation data
val_preds <- predict(final_fit, new_data = val_data) %>% 
  bind_cols(val_data)  # Attach true values

# Compute metrics (e.g., RMSE, R-squared for regression)
metrics <- metric_set(rmse, rsq)
train_metrics <- metrics(train_preds, truth = MarketCap, estimate = .pred)
val_metrics <- metrics(val_preds, truth = MarketCap, estimate = .pred)

round(train_metrics$.estimate, 4)
round(val_metrics$.estimate,4)

#plot first tree

final_model <- extract_fit_engine(trees_fit_1)

rpart.plot(
  final_model,
  type = 4,
  extra = 101,
  box.palette = "Blues",
  nn = TRUE,
  fallen.leaves = FALSE,
  shadow.col = "gray"
)

#feature importance

vip(final_model, geom = "point", horizontal = FALSE) +
  ggtitle("Decision Tree Feature Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))