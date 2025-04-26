################################################################################
#                                                                              #
#                              Decision Trees                                  #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

# you can find the notes of this lecture in my github: https://github.com/ValentinaZangirolami/Statistical-Learning

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
collect_metrics(trees_cv_results) 
# Get predictions on training data
train_preds <- predict(trees_fit_1, new_data = train_data) %>% 
  bind_cols(train_data)  # Attach true values

# Compute metrics (e.g., RMSE, R-squared for regression)
metrics <- metric_set(rmse, rsq)
train_metrics <- metrics(train_preds, truth = MarketCap, estimate = .pred)

train_metrics
