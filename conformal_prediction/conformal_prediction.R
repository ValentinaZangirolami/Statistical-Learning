################################################################################
#                                                                              #
#                           Conformal Prediction                               #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

#load libraries
library(ISLR2)
library(tidyverse)
library(car)
library(ggplot2)
library(randomForest)
library(tidymodels)

# Regression task

data_house <- Boston
data_house <- data_house |> select(c(nox, dis))

head(data_house) 

# we already saw this dataset in polynomial regression class
#Variables: (1) Response variable: nox (nitrogen oxides concentration in parts per 10 million), 
# (2) Covariate: dis (the weighted mean of distances to five Boston employment centers)

# If you remember, the relationship between nox and dis is non-linear and we proposed to use polynomial regression

# Then, let consider the same models of the last time

set.seed(123)  # For reproducibility
split <- initial_split(data_house, prop = 0.9)

train_data <- training(split)
test_data <- testing(split)

poly_lm <- lm(nox~poly(dis, 2), data=train_data)
poly_lm_2 <- lm(nox~poly(dis, 3), data=train_data)

# and we consider also simple linear regression

mod_lm <- lm(nox~dis, data=train_data)

#standard prediction intervals

alpha = 0.1

pi_poly = predict(poly_lm, newdata=data.frame(dis = test_data[,2]), interval = "prediction", level = 1-alpha)
pi_poly_2 = predict(poly_lm_2, newdata=data.frame(dis = test_data[,2]), interval = "prediction", level = 1-alpha)
pi_lm = predict(mod_lm, newdata=data.frame(dis = test_data[,2]), interval = "prediction", level = 1-alpha)

# prediction intervals for first poly

ggplot() + geom_point(data = test_data, aes(x = dis, y = nox)) + geom_errorbar(
    data = data.frame(x_new = test_data$dis, lower = pi_poly[,2], upper = pi_poly[,3]),
    aes(x = x_new, ymin = lower, ymax = upper),
    color = "red", width = 0, linewidth = 1
  ) + theme_minimal() + labs(x = "dis", y = "nox")

# prediction intervals for 2nd poly

ggplot() + geom_point(data = test_data, aes(x = dis, y = nox)) +  geom_errorbar(
  data = data.frame(x_new = test_data$dis, lower = pi_poly_2[,2], upper = pi_poly_2[,3]),
  aes(x = x_new, ymin = lower, ymax = upper),
  color = "violet", width = 0, linewidth = 1
) + theme_minimal() + labs(x = "dis", y = "nox")

# prediction intervals for lm

ggplot() + geom_point(data = test_data, aes(x = dis, y = nox)) + geom_errorbar(
  data = data.frame(x_new = test_data$dis, lower = pi_lm[,2], upper = pi_lm[,3]),
  aes(x = x_new, ymin = lower, ymax = upper),
  color = "blue", width = 0, linewidth = 1
) + theme_minimal() + labs(x = "dis", y = "nox")

# Especially in the last case, our prediction intervals are affected by model misspecification.
# It can be the case also in the firsts two cases since we don't know in advance the exact relationship 
# between the response and the covariate. For this reason, we can use conformal prediction
# to obtain prediction intervals with marginal coverage guarantees

#let's check the coverage in this case

coverage <- function(true_param, lower, upper) {
  
  covered <- (true_param >= lower) & (true_param <= upper)
  
  # Calculate coverage probability
  mean(covered)
}

lenght_int <- function(lower, upper) {
  
  mean(upper -lower)
}

cat("First poly regression has coverage equal to: ", coverage(test_data$nox, pi_poly[,2], pi_poly[,3]), ", and interval length equal to: ", lenght_int(pi_poly[,2], pi_poly[,3]))
cat("Second poly regression has coverage equal to: ", coverage(test_data$nox,pi_poly_2[,2], pi_poly_2[,3]), ", and interval length equal to: ", lenght_int(pi_poly_2[,2], pi_poly_2[,3]))
cat("Linear regression has coverage equal to: ", coverage(test_data$nox,pi_lm[,2], pi_lm[,3]),", and interval length equal to: ", lenght_int(pi_lm[,2], pi_lm[,3]))

# Full conformal prediction
# case: conformal score corresponds to |residuals|
# less efficient method, but more robust
full_conformal_regression <- function(X, y, X_new, alpha = 0.1, poly_degree = 2, 
                                      grid_size = 100, verbose = FALSE) {
  
  # Convert inputs to appropriate formats
  X <- as.numeric(X)
  y <- as.numeric(y)
  X_new <- as.numeric(X_new)
  
  # Create proper polynomial data frame
  create_poly_data <- function(x, y, degree) {
    df <- data.frame(y = y)
    for (d in 1:degree) {
      df[paste0("X", d)] <- x^d
    }
    df
  }
  
  # Model fitting function
  fit_model <- function(X, y, degree) {
    df <- create_poly_data(X, y, degree)
    lm(y ~ ., data = df)
  }
  
  # Prediction function
  predict_from_model <- function(model, x, degree) {
    newdata <- data.frame(X1 = x)
    if (degree > 1) {
      for (d in 2:degree) {
        newdata[paste0("X", d)] <- x^d
      }
    }
    predict(model, newdata = newdata)
  }
  
  # Prepare output
  n_new <- length(X_new)
  intervals <- matrix(NA, nrow = n_new, ncol = 2)
  colnames(intervals) <- c("lower", "upper")
  
  # For each new point
  for (i in 1:n_new) {
    if (verbose) message("Processing point ", i, " of ", n_new)
    
    x0 <- X_new[i]
    
    # Determine search range for y0
    model <- fit_model(X, y, poly_degree)
    y0_hat <- predict_from_model(model, x0, poly_degree)
    search_range <- range(y) + c(-1, 1) * diff(range(y))
    y0_grid <- seq(search_range[1], search_range[2], length.out = grid_size)
    
    # Calculate scores for each candidate y0
    scores <- sapply(y0_grid, function(y0) {
      # Augment the dataset
      X_aug <- c(X, x0)
      y_aug <- c(y, y0)
      
      # Fit model on augmented data
      model_aug <- fit_model(X_aug, y_aug, poly_degree)
      
      # Get prediction for new point
      y_hat <- predict_from_model(model_aug, x0, poly_degree)
      
      # Calculate residual
      abs(y0 - y_hat)
    })
    
    # Find cutoff and valid y0's
    cutoff <- quantile(scores, 1 - alpha, type = 1)
    valid_y0 <- y0_grid[scores <= cutoff]
    
    # Store interval
    intervals[i, ] <- c(min(valid_y0), max(valid_y0))
  }
  
  return(list(prediction_intervals = intervals))
}

full_pred_interval_poly <- full_conformal_regression(train_data$dis, train_data$nox, test_data$dis, 0.1, poly_degree = 2)
full_pred_interval_poly_2 <- full_conformal_regression(train_data$dis, train_data$nox, test_data$dis, 0.1, poly_degree=3)
full_pred_interval_lm <- full_conformal_regression(train_data$dis, train_data$nox, test_data$dis, 0.1, poly_degree=1)


cat("First poly regression has coverage equal to: ", coverage(test_data$nox, full_pred_interval_poly$prediction_intervals[,1], full_pred_interval_poly$prediction_intervals[,2]), ", and interval length equal to: ", lenght_int(full_pred_interval_poly$prediction_intervals[,1], full_pred_interval_poly$prediction_intervals[,2]))
cat("Second poly regression has coverage equal to: ", coverage(test_data$nox,full_pred_interval_poly_2$prediction_intervals[,1], full_pred_interval_poly_2$prediction_intervals[,2]), ", and interval length equal to: ", lenght_int(full_pred_interval_poly_2$prediction_intervals[,1], full_pred_interval_poly_2$prediction_intervals[,2]))
cat("Linear regression has coverage equal to: ", coverage(test_data$nox,full_pred_interval_lm$prediction_intervals[,1], full_pred_interval_lm$prediction_intervals[,2]),", and interval length equal to: ", lenght_int(full_pred_interval_lm$prediction_intervals[,1], full_pred_interval_lm$prediction_intervals[,2]))


# prediction intervals for first poly

ggplot() + geom_point(data = test_data, aes(x = dis, y = nox)) + geom_errorbar(
  data = data.frame(x_new = test_data$dis, lower = full_pred_interval_poly$prediction_intervals[,1], upper = full_pred_interval_poly$prediction_intervals[,2]),
  aes(x = x_new, ymin = lower, ymax = upper),
  color = "red", width = 0, linewidth = 1
) + theme_minimal() + labs(x = "dis", y = "nox")

# prediction intervals for 2nd poly

ggplot() + geom_point(data = test_data, aes(x = dis, y = nox)) +  geom_errorbar(
  data = data.frame(x_new = test_data$dis, lower = full_pred_interval_poly_2$prediction_intervals[,1], upper = full_pred_interval_poly_2$prediction_intervals[,2]),
  aes(x = x_new, ymin = lower, ymax = upper),
  color = "violet", width = 0, linewidth = 1
) + theme_minimal() + labs(x = "dis", y = "nox")

# prediction intervals for lm

ggplot() + geom_point(data = test_data, aes(x = dis, y = nox)) + geom_errorbar(
  data = data.frame(x_new = test_data$dis, lower = full_pred_interval_lm$prediction_intervals[,1], upper = full_pred_interval_lm$prediction_intervals[,2]),
  aes(x = x_new, ymin = lower, ymax = upper),
  color = "blue", width = 0, linewidth = 1
) + theme_minimal() + labs(x = "dis", y = "nox")


# Split conformal prediction

# slightly less robust method, but computationally efficient

split_conformal_regression <- function(X, y, X_new, alpha = 0.1, train_ratio = 0.6,
                                       poly_degree = 1, grid_size = 100, verbose = FALSE) {
  
  # Convert inputs to appropriate formats
  X <- as.numeric(X)
  y <- as.numeric(y)
  X_new <- as.numeric(X_new)
  
  # Split data into proper training and calibration sets
  set.seed(123)  # For reproducibility
  n <- length(X)
  idx <- sample(1:n, size = floor(train_ratio * n))
  X_train <- X[idx]
  y_train <- y[idx]
  X_cal <- X[-idx]
  y_cal <- y[-idx]
  
  # Create polynomial features function
  create_poly_features <- function(x, degree) {
    if (degree == 1) return(matrix(x, ncol = 1))
    sapply(1:degree, function(d) x^d)
  }
  
  # Train model on proper training set
  X_poly_train <- create_poly_features(X_train, poly_degree)
  train_df <- data.frame(X_poly_train, y = y_train)
  colnames(train_df) <- c(paste0("X", 1:poly_degree), "y")
  model <- lm(y ~ ., data = train_df)
  
  # Compute residuals on calibration set
  X_poly_cal <- create_poly_features(X_cal, poly_degree)
  cal_df <- data.frame(X_poly_cal)
  colnames(cal_df) <- paste0("X", 1:poly_degree)
  y_hat_cal <- predict(model, newdata = cal_df)
  residuals <- abs(y_cal - y_hat_cal)
  
  # Compute quantile of residuals
  n_cal <- length(residuals)
  quantile_level <- min((n_cal + 1) * (1 - alpha) / n_cal, 1)
  q_hat <- quantile(residuals, probs = quantile_level, type = 1)
  
  # Create prediction intervals for new points
  X_poly_new <- create_poly_features(X_new, poly_degree)
  new_df <- data.frame(X_poly_new)
  colnames(new_df) <- paste0("X", 1:poly_degree)
  y_hat_new <- predict(model, newdata = new_df)
  
  intervals <- cbind(y_hat_new - q_hat, y_hat_new + q_hat)
  colnames(intervals) <- c("lower", "upper")
  
  return(list(prediction_intervals = intervals))
}

split_pred_interval_poly <- split_conformal_regression(train_data$dis, train_data$nox, test_data$dis, 0.1, poly_degree = 2)
split_pred_interval_poly_2 <- split_conformal_regression(train_data$dis, train_data$nox, test_data$dis, 0.1, poly_degree=3)
split_pred_interval_lm <- split_conformal_regression(train_data$dis, train_data$nox, test_data$dis, 0.1, poly_degree=1)


cat("First poly regression has coverage equal to: ", coverage(test_data$nox, split_pred_interval_poly$prediction_intervals[,1], split_pred_interval_poly$prediction_intervals[,2]), ", and interval length equal to: ", lenght_int(split_pred_interval_poly$prediction_intervals[,1], split_pred_interval_poly$prediction_intervals[,2]))
cat("Second poly regression has coverage equal to: ", coverage(test_data$nox,split_pred_interval_poly_2$prediction_intervals[,1], split_pred_interval_poly_2$prediction_intervals[,2]), ", and interval length equal to: ", lenght_int(split_pred_interval_poly_2$prediction_intervals[,1], split_pred_interval_poly_2$prediction_intervals[,2]))
cat("Linear regression has coverage equal to: ", coverage(test_data$nox,split_pred_interval_lm$prediction_intervals[,1], split_pred_interval_lm$prediction_intervals[,2]),", and interval length equal to: ", lenght_int(split_pred_interval_lm$prediction_intervals[,1], split_pred_interval_lm$prediction_intervals[,2]))

# prediction intervals for first poly

ggplot() + geom_point(data = test_data, aes(x = dis, y = nox)) + geom_errorbar(
  data = data.frame(x_new = test_data$dis, lower = split_pred_interval_poly$prediction_intervals[,1], upper = split_pred_interval_poly$prediction_intervals[,2]),
  aes(x = x_new, ymin = lower, ymax = upper),
  color = "red", width = 0, linewidth = 1
) + theme_minimal() + labs(x = "dis", y = "nox")

# prediction intervals for 2nd poly

ggplot() + geom_point(data = test_data, aes(x = dis, y = nox)) +  geom_errorbar(
  data = data.frame(x_new = test_data$dis, lower = split_pred_interval_poly_2$prediction_intervals[,1], upper = split_pred_interval_poly_2$prediction_intervals[,2]),
  aes(x = x_new, ymin = lower, ymax = upper),
  color = "violet", width = 0, linewidth = 1
) + theme_minimal() + labs(x = "dis", y = "nox")

# prediction intervals for lm

ggplot() + geom_point(data = test_data, aes(x = dis, y = nox)) + geom_errorbar(
  data = data.frame(x_new = test_data$dis, lower = split_pred_interval_lm$prediction_intervals[,1], upper = split_pred_interval_lm$prediction_intervals[,2]),
  aes(x = x_new, ymin = lower, ymax = upper),
  color = "blue", width = 0, linewidth = 1
) + theme_minimal() + labs(x = "dis", y = "nox")

# Classification task

# download data from https://www.kaggle.com/datasets/olcaybolat1/dermatology-dataset-classification/data

derm_data <- read.csv("dermatology_database_1.csv")
head(derm_data)

derm_data$class <- as.factor(derm_data$class)

set.seed(123)  # For reproducibility
split <- initial_split(derm_data, prop = 0.9)

train_data <- training(split)
test_data <- testing(split)

# Full conformal prediction

full_conformal_classification <- function(X, y, X_new, y_new = NULL, alpha = 0.1, 
                                             ntree = 500, verbose = TRUE) {
  
  # Input validation
  classes <- levels(y)
  n_classes <- length(classes)
  
  # Convert new data
  if (is.data.frame(X)) {
    X_new <- as.data.frame(X_new)
    colnames(X_new) <- colnames(X)
  } else {
    X_new <- as.matrix(X_new)
  }
  
  n_new <- nrow(X_new)
  prediction_sets <- vector("list", n_new)
  
  # Calculate prediction sets
  for (i in 1:n_new) {
    if (verbose) cat("Processing point", i, "of", n_new, "\n")
    
    x0 <- X_new[i, , drop = FALSE]
    class_scores <- numeric(n_classes)
    names(class_scores) <- classes
    
    for (class in classes) {
      # Augment dataset with test point and candidate class
      X_aug <- rbind(X, x0)
      y_aug <- factor(c(as.character(y), class), levels = classes)
      
      # Train RF on augmented data
      rf_model <- randomForest(X_aug, y_aug, ntree = ntree)
      
      # Get predicted probability for candidate class
      prob <- predict(rf_model, x0, type = "prob")[1, class]
      class_scores[class] <- 1 - prob  # Conformity score
    }
    
    # Determine prediction set
    cutoff <- quantile(class_scores, probs = 1 - alpha, type = 1)
    prediction_sets[[i]] <- names(class_scores)[class_scores <= cutoff]
  }
  
  # Calculate empirical coverage if true labels are provided
  empirical_coverage <- NULL
  if (!is.null(y_new)) {
    y_new <- as.character(y_new)
    correct <- sapply(1:n_new, function(i) y_new[i] %in% prediction_sets[[i]])
    empirical_coverage <- mean(correct)
  }
  
  return(list(
    prediction_sets = prediction_sets,
    alpha = alpha,
    empirical_coverage = empirical_coverage,
    classes = classes
  ))
}
full_classification <- full_conformal_classification(train_data[,-35], train_data[,35], test_data[,-35], test_data[,35])

full_classification$prediction_sets
full_classification$empirical_coverage
full_classification$classes

# Split conformal prediction

split_conformal_classification <- function(X, y, X_new, y_new = NULL, 
                                              alpha = 0.1, 
                                              train_ratio = 0.6,
                                              ntree = 500,
                                              verbose = TRUE) {
  
  # Input validation
  classes <- levels(y)
  n_classes <- length(classes)
  
  # Convert new data
  if (is.data.frame(X)) {
    X_new <- as.data.frame(X_new)
    colnames(X_new) <- colnames(X)
  } else {
    X_new <- as.matrix(X_new)
  }
  
  # 1. Data Splitting
  set.seed(123) # For reproducibility
  n <- nrow(X)
  train_idx <- sample(1:n, size = floor(train_ratio * n))
  
  X_train <- X[train_idx, , drop = FALSE]
  y_train <- y[train_idx]
  X_cal <- X[-train_idx, , drop = FALSE]
  y_cal <- y[-train_idx]
  
  # 2. Train model on proper training set
  if (verbose) cat("Training random forest with", ntree, "trees...\n")
  model <- randomForest(X_train, y_train, ntree = ntree)
  
  # 3. Calculate scores on calibration set
  cal_probs <- predict(model, X_cal, type = "prob")
  
  # Inverse probability score
  true_class_idx <- match(y_cal, colnames(cal_probs))
  scores <- 1 - cal_probs[cbind(1:nrow(cal_probs), true_class_idx)]

  
  # 4. Compute quantile
  n_cal <- length(scores)
  quantile_level <- min((n_cal + 1) * (1 - alpha) / n_cal, 1)
  q_hat <- quantile(scores, probs = quantile_level, type = 1)
  
  # 5. Create prediction sets for new data
  new_probs <- predict(model, X_new, type = "prob")
  prediction_sets <- lapply(1:nrow(X_new), function(i) {
        names(new_probs[i, ])[1 - new_probs[i, ] <= q_hat]
  })
  
  # 6. Calculate empirical coverage if true labels provided
  empirical_coverage <- NULL
  y_new <- as.character(y_new)
  correct <- sapply(1:length(y_new), function(i) y_new[i] %in% prediction_sets[[i]])
  empirical_coverage <- mean(correct)

  
  return(list(
    prediction_sets = prediction_sets,
    empirical_coverage = empirical_coverage,
    classes = classes
  ))
}

split_classification <- split_conformal_classification(train_data[,-35], train_data[,35], test_data[,-35], test_data[,35])

split_classification$prediction_sets 
split_classification$empirical_coverage
split_classification$classes

rm(list = ls())

# Exercise: use the london bike sharing dataset and apply conformal prediction. Try to use different statistical models and compare results.

london_data <- read.csv("london_bike_sharing.csv")
head(london_data)