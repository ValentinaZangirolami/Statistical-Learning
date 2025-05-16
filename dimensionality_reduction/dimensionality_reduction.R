################################################################################
#                                                                              #
#                         Dimensionality reduction                             #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################


# --> UNSUPERVISED LEARNING!

#load libraries

# you must install: BiocManager::install("Biobase")

library(tidymodels)
library(ggplot2)
library(NMF)
library(tidyverse)
library(h2o)

# android data

android_data <- read.csv("android_data.csv")
head(android_data)

summary(android_data)

#split target and features
label_android <- android_data |> select(Label)
android_data <- android_data |> select(-Label)

# PCA

# Compute PCA with max components
pca_recipe <- recipe(~ ., data = android_data) |>
  step_pca(all_numeric(), num_comp = 56)  # All 40 PCs for evaluation

pca_prepped <- prep(pca_recipe, android_data)
pca_variance <- tidy(pca_prepped, 1, type = "variance")

# Automated selection: Find smallest #PCs reaching 95% variance
optimal_pcs_95 <- pca_variance |>
  filter(terms == "cumulative percent variance") |>
  filter(value >= 95) |>
  slice(1) |>
  pull(component)

optimal_pcs_90 <- pca_variance |>
  filter(terms == "cumulative percent variance") |>
  filter(value >= 90) |>
  slice(1) |>
  pull(component)

optimal_pcs_85 <- pca_variance |>
  filter(terms == "cumulative percent variance") |>
  filter(value >= 85) |>
  slice(1) |>
  pull(component)

cat("Optimal #PCs:", optimal_pcs_85, "(85% variance explained)")
cat("Optimal #PCs:", optimal_pcs_90, "(90% variance explained)")
cat("Optimal #PCs:", optimal_pcs_95, "(95% variance explained)")


pca_variance |>
  filter(terms == "percent variance") |>
  ggplot(aes(as.numeric(component), value)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = optimal_pcs_95, linetype = "dashed", color = "red") +
  geom_vline(xintercept = optimal_pcs_90, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = optimal_pcs_85, linetype = "dashed", color = "violet") +
  labs(title = "PCA Scree Plot", x = "Principal Component", y = "% Variance")

# it seems that we can strongly reduce the dimension of our data
# Maybe in this case would be better to choose also 6 components (given that we still obtain a strong reduction while having good levels of explained variance)

# let's store the PCA component

pca_recipe_90 <- recipe(~ ., data =android_data ) %>%
  step_normalize(all_numeric()) %>%     # Center/scale numeric vars
  step_pca(all_numeric(), num_comp = 3)

pca_recipe_95 <- recipe(~ ., data =android_data) %>%
  step_normalize(all_numeric()) %>%     # Center/scale numeric vars
  step_pca(all_numeric(), num_comp = 5)

# NMF

#it requires only numeric variables (or we can use one-hot encoding for categorical vars) and positive entries 

android_numeric <- android_data |>
  select_if(is.numeric)  # Keeps only numeric columns 

#rescale to obtain positive entries
android_nonneg <- android_numeric |> 
  mutate(across(everything(), ~ .x - min(.x, na.rm = TRUE)))

# Run NMF for multiple ranks (k=1 to 10)

#matrix factorization try to construct the matrix V (matrix of our covariates, dimension n x p) by leveraging WH 
# dimension of W is n x k
# dimension of H is k x p
# k is the rank and it corresponds to the dimension of latent variables we used to reduce dimensionality while obtaining a
# good representation of our features

nmf_results <- nmf(as.matrix(android_nonneg), rank = 2:8, method = "lee", nrun = 10)

# Plot metrics (higher cophenetic = better stability)
plot(nmf_results, main = "NMF Rank Selection")

# cophenetic is a correlation measure about the pairwise distance between points in the original data 
# and the pairwise distance between points in the reduced data
# high cophenetic --> distances are better preserved

# dispersion:  stability of NMF across runs
# evar: variance captured by different ranks
# residuals: based on V - WH
# rss: residual sum of squared 
# silhouette: it measures how well each sample fits its assigned cluster compared to other clusters (then, 
# in terms of cohesion and separation
# sparseness: sparsity in the observations (basis) or in the covariates (coefficients) for each run

nmf_result <- nmf(as.matrix(android_nonneg), rank = 4, method = "lee", nrun = 10)

V.hat <- fitted(nmf_result)

w <- basis(nmf_result) #  W 
dim(w) # n x k

h_matrix <- coef(nmf_result) #  H 
dim(h_matrix) # k x p

#Autoencoder

h2o.init()

# Convert data to H2O frame
data_h2o <- as.h2o(android_data)

# Train autoencoder
autoencoder <- h2o.deeplearning(
  x = names(data_h2o),
  training_frame = data_h2o,
  autoencoder = TRUE,
  hidden = c(32, 8, 32),
  activation = "RectifierWithDropout",
  l1 = 1e-5,
  l2 = 1e-5,
  max_w2 = 10,
  adaptive_rate = TRUE,
  input_dropout_ratio = 0.1,
  hidden_dropout_ratios = c(0.1, 0.1, 0.1),
  epochs = 100
)

# Extract latent features
latent_rep <- h2o.deepfeatures(autoencoder, data_h2o, layer = 2) # we can extract second layer
latent_rep_df <- as.data.frame(latent_rep)


#let's try to compare a model with standard features and feature obtained with PCA, NMF and autoencoder

android_true <- android_data |> mutate(Y = as.factor(label_android$Label))
pca_90 <- bake(prep(pca_recipe_90, training = android_data), new_data = android_data)
android_pca90 <- pca_90 |> mutate(Y = as.factor(label_android$Label))
pca_95 <- bake(prep(pca_recipe_95, training = android_data), new_data = android_data)
android_pca95 <- pca_95 |> mutate(Y = as.factor(label_android$Label))
android_nmf <- as.data.frame(w) |> mutate(Y = as.factor(label_android$Label))
android_autoencoder <- latent_rep_df |> mutate(Y = as.factor(label_android$Label))


set.seed(123)  # For reproducibility
split <- initial_split(android_true, prop = 0.8)
train_data <- training(split)
val_data <- testing(split)

# true features
android_recipe <- recipe(Y~., train_data) |> 
  step_zv(all_predictors())

rf_spec <- rand_forest(mtry = 5, trees = 10, min_n = 8) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

# Create workflow
rf_wf <- workflow() |>
  add_recipe(android_recipe) |>
  add_model(rf_spec)

# Fit model
rf_fit <- parsnip::fit(rf_wf, data = train_data)

# Get predictions on validation data
val_preds_true <- predict(rf_fit, new_data = val_data) |> 
  bind_cols(val_data) 
metrics <- metric_set(accuracy, f_meas)

val_metrics_true <- metrics(val_preds_true, truth = Y, estimate = .pred_class)

round(val_metrics_true$.estimate,4)

#PCA 90%

set.seed(123)  # For reproducibility
split <- initial_split(android_pca90, prop = 0.8)
train_data <- training(split)
val_data <- testing(split)

# true features
android_recipe <- recipe(Y~., train_data) |> 
  step_zv(all_predictors())

rf_spec <- rand_forest(mtry = 5, trees = 10, min_n = 8) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

# Create workflow
rf_wf <- workflow() |>
  add_recipe(android_recipe) |>
  add_model(rf_spec)

# Fit model
rf_fit <- parsnip::fit(rf_wf, data = train_data)

# Get predictions on validation data
val_preds_pca90 <- predict(rf_fit, new_data = val_data) |> 
  bind_cols(val_data) 

val_metrics_pca90 <- metrics(val_preds_pca90, truth = Y, estimate = .pred_class)

round(val_metrics_pca90$.estimate,4)

#PCA 95%

set.seed(123)  # For reproducibility
split <- initial_split(android_pca95, prop = 0.8)
train_data <- training(split)
val_data <- testing(split)

# true features
android_recipe <- recipe(Y~., train_data) |> 
  step_zv(all_predictors())

rf_spec <- rand_forest(mtry = 5, trees = 10, min_n = 8) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

# Create workflow
rf_wf <- workflow() |>
  add_recipe(android_recipe) |>
  add_model(rf_spec)

# Fit model
rf_fit <- parsnip::fit(rf_wf, data = train_data)

# Get predictions on validation data
val_preds_pca95 <- predict(rf_fit, new_data = val_data) |> 
  bind_cols(val_data) 

val_metrics_pca95 <- metrics(val_preds_pca95, truth = Y, estimate = .pred_class)

round(val_metrics_pca95$.estimate,4)

# NMF

set.seed(123)  # For reproducibility
split <- initial_split(android_nmf, prop = 0.8)
train_data <- training(split)
val_data <- testing(split)

# true features
android_recipe <- recipe(Y~., train_data) |> 
  step_zv(all_predictors())

rf_spec <- rand_forest(mtry = 5, trees = 10, min_n = 8) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

# Create workflow
rf_wf <- workflow() |>
  add_recipe(android_recipe) |>
  add_model(rf_spec)

# Fit model
rf_fit <- parsnip::fit(rf_wf, data = train_data)

# Get predictions on validation data
val_preds_nmf <- predict(rf_fit, new_data = val_data) |> 
  bind_cols(val_data) 

val_metrics_nmf <- metrics(val_preds_nmf, truth = Y, estimate = .pred_class)

round(val_metrics_nmf$.estimate,4)

# Autoencoder

set.seed(123)  # For reproducibility
split <- initial_split(android_autoencoder, prop = 0.8)
train_data <- training(split)
val_data <- testing(split)

# true features
android_recipe <- recipe(Y~., train_data) |> 
  step_zv(all_predictors())

rf_spec <- rand_forest(mtry = 5, trees = 10, min_n = 8) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

# Create workflow
rf_wf <- workflow() |>
  add_recipe(android_recipe) |>
  add_model(rf_spec)

# Fit model
rf_fit <- parsnip::fit(rf_wf, data = train_data)

# Get predictions on validation data
val_preds_autoencoder <- predict(rf_fit, new_data = val_data) |> 
  bind_cols(val_data) 

val_metrics_autoencoder <- metrics(val_preds_nmf, truth = Y, estimate = .pred_class)

round(val_metrics_autoencoder$.estimate,4)


#exercise: try to apply dimensionality reduction techniques while testing supervised models with the reduced feature space

london_data <- read.csv("london_bike_sharing.csv")
head(london_data)

