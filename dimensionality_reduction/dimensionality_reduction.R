################################################################################
#                                                                              #
#                         Dimensionality reduction                             #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################


# --> UNSUPERVISED LEARNING!

#load libraries

BiocManager::install("Biobase")

library(tidymodels)
library(ggplot2)
library(NMF)
library(tidyverse)
library(keras)

# android data

android_data <- read.csv("Android_Malware.csv", row.names=1)
head(android_data)

summary(android_data)

# PCA

# Compute PCA with max components
pca_recipe <- recipe(~ ., data = android_data |> select(-Label)) |>
  step_pca(all_numeric(), num_comp = 84)  # All 40 PCs for evaluation

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

# let's store the PCA component

pca_recipe_90 <- recipe(~ ., data =android_data |> select(-Label)) %>%
  step_normalize(all_numeric()) %>%     # Center/scale numeric vars
  step_pca(all_numeric(), num_comp = 3)

pca_recipe_95 <- recipe(~ ., data =android_data |> select(-Label)) %>%
  step_normalize(all_numeric()) %>%     # Center/scale numeric vars
  step_pca(all_numeric(), num_comp = 5)

# NMF

# Run NMF for multiple ranks (k=1 to 4)
nmf_results <- nmf(as.matrix(android_data[, 1:82]), rank = 1:82, nrun = 50, .opt = "v")

# Plot metrics (higher cophenetic = better stability)
plot(nmf_results, main = "NMF Rank Selection")





#exercise: try to apply dimensionality reduction techniques while testing supervised models with the reduced feature space

london_data <- read.csv("london_bike_sharing.csv")
head(london_data)

