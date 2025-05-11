################################################################################
#                                                                              #
#                              Missing values                                  #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################


#load libraries

library(naniar)
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(mice)
library(missForest) 
library(misty)

#load dataset
load('ESG_missing_data.RData')
esg_data <- read.csv("esg_financial_dataset.csv")

# It contains 3 dataset of ESG dataset.

########### Missing values: type 1

summary(dataset_1) # Revenue, ESG_Social, WaterUsage have missing values

# We need to understand better the situation:
# (1) What kind of missing values?
# (2) After analyzing the missing generating mechanisms, we need to find appropriate methods to solve the issue
# (3) Compare different methods

# At the beggining, we can visualize better the missing values

gg_miss_upset(dataset_1)

#45 obs have missing in all three variables
#Water usage is the variable which has higher percentage of NAs

#Check the percentage of missing values in each column

vis_miss(dataset_1) #7.3% of missing values

# WaterUsage 70% of NAs and the other variables 10% and 15%
# Then, we need to remove WaterUsage --> too many missing.

dataset_1 <- dataset_1 |> select(-WaterUsage)

# Check if NAs are MCAR: we can use a test from misty library

# select a subset of covariates

data_test <- dataset_1 |> select(-c(ESG_Overall, ProfitMargin))

na.test(data_test) # not rejection of H0: mcar NAs

# we can use also the test from mice library

mcar_test(data_test) # not rejection of H0: mcar NAs

# We can also try to visualize if missing values have some relationship with other covariates

data_test <- data_test |>
  mutate(
    ESG_Social_missing = ifelse(is.na(ESG_Social), "Missing", "Observed"),
    Revenue_missing = ifelse(is.na(Revenue), "Missing", "Observed"))

#categorical variables

data_test |>
  group_by(Region) |>
  summarize(
    ESG_Social_missing_rate = mean(is.na(ESG_Social)),
    Revenue_missing_rate = mean(is.na(Revenue))
  )

data_test |>
  group_by(Industry) |>
  summarize(
    ESG_Social_missing_rate = mean(is.na(ESG_Social)),
    Revenue_missing_rate = mean(is.na(Revenue))
  )

#quantitative variables

ggplot(data_test, aes(x = GrowthRate)) +
  geom_density(data = subset(data_test, !is.na(Revenue)), 
               fill = "green", alpha = 0.3) +
  geom_density(data = subset(data_test, is.na(Revenue)), 
               fill = "orange", alpha = 0.3) +
  labs(title = "Distribution of GrowthRate\nby Revenue Missingness")

ggplot(data_test, aes(x = GrowthRate)) +
  geom_density(data = subset(data_test, !is.na(ESG_Social)), 
               fill = "green", alpha = 0.3) +
  geom_density(data = subset(data_test, is.na(ESG_Social)), 
               fill = "orange", alpha = 0.3) +
  labs(title = "Distribution of GrowthRate\nby ESG Social Missingness")

ggplot(data_test, aes(x = MarketCap)) +
  geom_density(data = subset(data_test, !is.na(Revenue)), 
               fill = "green", alpha = 0.3) +
  geom_density(data = subset(data_test, is.na(Revenue)), 
               fill = "orange", alpha = 0.3) +
  labs(title = "Distribution of MarketCap\nby Revenue Missingness")

ggplot(data_test, aes(x = MarketCap)) +
  geom_density(data = subset(data_test, !is.na(ESG_Social)), 
               fill = "green", alpha = 0.3) +
  geom_density(data = subset(data_test, is.na(ESG_Social)), 
               fill = "orange", alpha = 0.3) +
  labs(title = "Distribution of MarketCap\nby ESG Social Missingness")

ggplot(data_test, aes(x = EnergyConsumption)) +
  geom_density(data = subset(data_test, !is.na(Revenue)), 
               fill = "green", alpha = 0.3) +
  geom_density(data = subset(data_test, is.na(Revenue)), 
               fill = "orange", alpha = 0.3) +
  labs(title = "Distribution of EnergyConsumption\nby Revenue Missingness")

# we chose the variables that, in our opinion, maybe can have a sort of relation with missingness
# -> the evidence is that NAs are MCAR

rm(data_test)

### Methods to solve MCAR

## mean/median imputation

dataset_1$Revenue_mean <- ifelse(is.na(dataset_1$Revenue), mean(dataset_1$Revenue, na.rm = TRUE), dataset_1$Revenue)
dataset_1$Revenue_med <- ifelse(is.na(dataset_1$Revenue), median(dataset_1$Revenue, na.rm = TRUE), dataset_1$Revenue)

dataset_1$ESG_Social_mean <- ifelse(is.na(dataset_1$ESG_Social), mean(dataset_1$ESG_Social, na.rm = TRUE), dataset_1$ESG_Social)
dataset_1$ESG_Social_med <- ifelse(is.na(dataset_1$ESG_Social), median(dataset_1$ESG_Social, na.rm = TRUE), dataset_1$ESG_Social)

# compare models with true dataset and imputed dataset

esg_recipe <- recipe(MarketCap ~ ., data = esg_data) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

esg_mean <- recipe(MarketCap ~ Industry + Region + Revenue_mean + ProfitMargin + GrowthRate + ESG_Overall + ESG_Environmental + ESG_Social_mean + ESG_Governance +CarbonEmissions + EnergyConsumption, data = dataset_1) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

esg_med <- recipe(MarketCap ~ Industry + Region + Revenue_med + ProfitMargin + GrowthRate + ESG_Overall + ESG_Environmental + ESG_Social_med + ESG_Governance +CarbonEmissions + EnergyConsumption, data = dataset_1) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

rf_spec <- rand_forest(mtry = 8, trees = 10, min_n = 8) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

# Create workflow
rf_wf_esg <- workflow() |>
  add_recipe(esg_recipe) |>
  add_model(rf_spec)

rf_wf_mean <- workflow() |>
  add_recipe(esg_mean) |>
  add_model(rf_spec)

rf_wf_med <- workflow() |>
  add_recipe(esg_med) |>
  add_model(rf_spec)

# Fit model
rf_fit_esg <- fit(rf_wf_esg, data = esg_data)
rf_fit_mean <- fit(rf_wf_mean, data = dataset_1[,-c(3, 14, 16)])
rf_fit_med <- fit(rf_wf_med, data = dataset_1[,-c(3, 13, 15)])

# Get predictions on training data
train_preds_esg <- predict(rf_fit_esg, new_data = esg_data) %>% 
  bind_cols(esg_data)  # Attach true values
train_preds_mean <- predict(rf_fit_mean, new_data = dataset_1[,-c(3, 14, 16)]) %>% 
  bind_cols(esg_data)  # Attach true values
train_preds_med <- predict(rf_fit_med, new_data = dataset_1[,-c(3, 13, 15)]) %>% 
  bind_cols(esg_data)  # Attach true values

# Compute metrics (e.g., RMSE, R-squared for regression)
metrics <- metric_set(rmse, rsq)

metrics(train_preds_esg, truth = MarketCap, estimate = .pred)
metrics(train_preds_mean, truth = MarketCap, estimate = .pred)
metrics(train_preds_med, truth = MarketCap, estimate = .pred)

rm(dataset_1, esg_data, esg_mean, esg_med, esg_recipe, rf_fit_esg, rf_fit_mean, rf_fit_med, train_preds_esg, train_preds_mean, train_preds_med)

########### Missing values: type 2

#let's check what happen in dataset_2

summary(dataset_2) # ProfitMargin, ESG_Overall, ESG_Environmental,CarbonEmissions have NAs

gg_miss_upset(dataset_2) # from the plot we can observe that ESG_Overall is missing because of the ESG_Environmental NAs
# no other important intersections

vis_miss(dataset_2)

# Check if NAs are MCAR: we can use a test from misty library

# select a subset of covariates

data_test <- dataset_2 |> select(-c(ESG_Overall, MarketCap, WaterUsage))

na.test(data_test) # rejection of H0: NO mcar NAs

#let's analyze if we have MAR

# We can explore which kind of variable can affect the missingness 
# NAs maybe can depend on Region or industry or Revenue 
#let's check

dataset_2 |>
  group_by(Region) |>
  summarize(
    ProfitMargin_missing_rate = mean(is.na(ProfitMargin)),
    CarbonEmissions_missing_rate = mean(is.na(CarbonEmissions)),
    ESG_Environmental_missing_rate = mean(is.na(ESG_Environmental))
  )

# ProfitMargin NAs depend on Region

dataset_2 |>
  group_by(Industry) |>
  summarize(
    ProfitMargin_missing_rate = mean(is.na(ProfitMargin)),
    CarbonEmissions_missing_rate = mean(is.na(CarbonEmissions)),
    ESG_Environmental_missing_rate = mean(is.na(ESG_Environmental))
  )
# ESG_Environmental NAs depend on Industry

ggplot(dataset_2, aes(x = Revenue)) +
  geom_density(data = subset(dataset_2, !is.na(CarbonEmissions)), 
               fill = "green", alpha = 0.3) +
  geom_density(data = subset(dataset_2, is.na(CarbonEmissions)), 
               fill = "orange", alpha = 0.3) +
  labs(title = "Distribution of Revenue\nby CarbonEmissions Missingness")

ggplot(dataset_2, aes(x = Revenue)) +
  geom_density(data = subset(dataset_2, !is.na(ESG_Environmental)), 
               fill = "green", alpha = 0.3) +
  geom_density(data = subset(dataset_2, is.na(ESG_Environmental)), 
               fill = "orange", alpha = 0.3) +
  labs(title = "Distribution of Revenue\nby ESG_Environmental Missingness")

# CarbonEmissions NAs depend on Revenue

rm(data_test)

### Methods to solve MAR

## conditional mean/median imputation

dataset_2 <- dataset_2 |>
  group_by(Region) |>
  mutate(ProfitMargin = ifelse(
    is.na(ProfitMargin),
    median(ProfitMargin, na.rm = TRUE),  # Replace NA with group mean
    ProfitMargin # Keep original if not NA
  )) |>
  ungroup()

dataset_2 <- dataset_2 |>
  group_by(Industry) |>
  mutate(ESG_Environmental = ifelse(
    is.na(ESG_Environmental),
    median(ESG_Environmental, na.rm = TRUE),  # Replace NA with group mean
    ESG_Environmental # Keep original if not NA
  )) |>
  ungroup()

# non-parametric imputation for CarbonEmissions and ESG_Overall
# Set seed for reproducibility
set.seed(123)

dataset_2 <- dataset_2 |>  
  mutate(across(where(is.character), as.factor))

# Perform mice imputation
imputed_data <- mice(
  dataset_2[,c(3, 11)],
  method = "norm",
  m = 1,                 # Number of imputations
  maxit = 10
)

imputed_data_2 <- mice(
  dataset_2[,c(7:10)],
  method = "norm",
  ridge = 0.01,          # Penalty term for numeric vars
  m = 1,                 # Number of imputations
  maxit = 10
)

imputed_data <- complete(imputed_data)
imputed_data_2 <- complete(imputed_data_2)

dataset_2$CarbonEmissions <- imputed_data$CarbonEmissions
dataset_2$ESG_Overall <- imputed_data_2$ESG_Overall

#let's see what happen if we want apply a model

esg_recipe <- recipe(MarketCap ~ ., data = dataset_2) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

rf_wf_esg <- workflow() |>
  add_recipe(esg_recipe) |>
  add_model(rf_spec)

rf_fit_esg <- fit(rf_wf_esg, data = dataset_2)

train_preds_esg <- predict(rf_fit_esg, new_data = dataset_2) %>% 
  bind_cols(dataset_2)
metrics <- metric_set(rmse, rsq)
metrics(train_preds_esg, truth = MarketCap, estimate = .pred)

rm(dataset_2, imputed_data, imputed_data_2)

########### Missing values: type 3

summary(dataset_3) # WaterUsage, ESG_Overall, ESG_Social,CarbonEmissions have NAs

gg_miss_upset(dataset_3) # from the plot we can observe that ESG_Overall is missing because of the ESG_Social NAs
# no other important intersections

vis_miss(dataset_3)

# Check if NAs are MCAR: we can use a test from misty library

# select a subset of covariates

data_test <- dataset_3 |> select(-c(ESG_Overall, MarketCap, WaterUsage))

na.test(data_test) # rejection of H0: NO mcar NAs

# let's check graphically

View(dataset_3 |>
  group_by(Region, Industry) |>
  summarize(
    WaterUsage_missing_rate = mean(is.na(WaterUsage)),
    CarbonEmissions_missing_rate = mean(is.na(CarbonEmissions)),
    ESG_Social_missing_rate = mean(is.na(ESG_Social))
  ))

ggplot(dataset_3, aes(x = ESG_Governance)) +
  geom_density(data = subset(dataset_3, !is.na(ESG_Social)), 
               fill = "green", alpha = 0.3) +
  geom_density(data = subset(dataset_3, is.na(ESG_Social)), 
               fill = "orange", alpha = 0.3) +
  labs(title = "Distribution of ESG_Governance\nby ESG_Social Missingness")

ggplot(dataset_3, aes(x = EnergyConsumption)) +
  geom_density(data = subset(dataset_3, !is.na(WaterUsage)), 
               fill = "green", alpha = 0.3) +
  geom_density(data = subset(dataset_3, is.na(WaterUsage)), 
               fill = "orange", alpha = 0.3) +
  labs(title = "Distribution of EnergyConsumption\nby WaterUsage Missingness")  + xlim(0, 1.0e+08)

ggplot(dataset_3, aes(x = EnergyConsumption)) +
  geom_density(data = subset(dataset_3, !is.na(CarbonEmissions)), 
               fill = "green", alpha = 0.3) +
  geom_density(data = subset(dataset_3, is.na(CarbonEmissions)), 
               fill = "orange", alpha = 0.3) +
  labs(title = "Distribution of EnergyConsumption\nby CarbonEmissions Missingness")  + xlim(0, 1.0e+08)

#it's better try to impute
# we can use random forest

dataset_3$Industry <-as.factor(dataset_3$Industry)
dataset_3$Region <-as.factor(dataset_3$Region)

impute <- missForest(dataset_3, maxiter = 10, ntree = 100, variablewise = FALSE)

#let's see what happen if we want apply a model

esg_recipe <- recipe(MarketCap ~ ., data = impute$ximp) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())

rf_wf_esg <- workflow() |>
  add_recipe(esg_recipe) |>
  add_model(rf_spec)

rf_fit_esg <- fit(rf_wf_esg, data = impute$ximp)

train_preds_esg <- predict(rf_fit_esg, new_data = impute$ximp) %>% 
  bind_cols(impute$ximp)
metrics <- metric_set(rmse, rsq)
metrics(train_preds_esg, truth = MarketCap, estimate = .pred)

# Exercise: try to study missing values with credit card dataset