library(tidyverse)

esg_data <- read.csv("esg_financial_dataset.csv")
head(esg_data)

# Generate data with MCAR NA
set.seed(123)

# Create MCAR missingness (random across all variables)
mcar_data <- esg_data

# Add 10% missing to Revenue
mcar_data$Revenue[sample(nrow(mcar_data), nrow(mcar_data)*0.10)] <- NA

# Add 15% missing to ESG_Social
mcar_data$ESG_Social[sample(nrow(mcar_data), nrow(mcar_data)*0.15)] <- NA

# Add 5% missing to WaterUsage
mcar_data$WaterUsage[sample(nrow(mcar_data), nrow(mcar_data)*0.7)] <- NA

# Check missingness
colMeans(is.na(mcar_data))

# Generate data with MAR NA

set.seed(123)
mar_data <- esg_data

# MAR Example 1: Missing ProfitMargin more likely in Asia and Africa
mar_data$ProfitMargin[(mar_data$Region == "Asia" | mar_data$Region == "Africa") & runif(nrow(mar_data)) < 0.3] <- NA

# MAR Example 2: Missing ESG_Environmental more likely in Retail and Healthcare industry
mar_data$ESG_Environmental[(mar_data$Industry == "Retail" | mar_data$Industry == "Healthcare") & runif(nrow(mar_data)) < 0.5] <- NA
mar_data$ESG_Overall[is.na(mar_data$ESG_Environmental)] <- NA

# MAR Example 3: Missing CarbonEmissions more likely when there is high revenue
mar_data$CarbonEmissions[mar_data$Revenue > quantile(mar_data$Revenue, 0.75) & runif(nrow(mar_data)) < 0.5] <- NA

# Check missingness
colMeans(is.na(mar_data))

set.seed(123)
    
# Generate data with MNAR NA

mnar_data <- esg_data

# MNAR Example 1: Companies with low ESG_Overall less likely to report it
mnar_data$ESG_Social[mnar_data$ESG_Social < 60 & runif(nrow(mnar_data)) < 0.4] <- NA
mnar_data$ESG_Overall[is.na(mnar_data$ESG_Social)] <- NA

## MNAR Example 2: High-polluting companies less likely to report CarbonEmissions
# (Missingness depends on the unobserved true pollution levels)
mnar_data$CarbonEmissions[mnar_data$CarbonEmissions > median(mnar_data$CarbonEmissions, na.rm = TRUE) & 
                            runif(nrow(mnar_data)) < 0.4] <- NA

# MNAR Example 3: High water users less likely to report WaterUsage
mnar_data$WaterUsage[mnar_data$WaterUsage > median(mnar_data$WaterUsage, na.rm = TRUE) & 
                       runif(nrow(mnar_data)) < 0.25] <- NA

# Check missingness
colMeans(is.na(mnar_data))

# Save dataset
rm(esg_data)

dataset_1 <- mcar_data
dataset_2 <- mar_data
dataset_3 <- mnar_data

rm(mcar_data, mar_data, mnar_data)

# dataset for the exercise

credit_data <- read.csv("Credit.csv")
head(credit_data)

summary(credit_data)

set.seed(123)  # For reproducibility

## 1. MCAR (Missing Completely At Random)
# 15% missing in Age (completely random)
credit_data$Age[sample(nrow(credit_data), size = round(nrow(credit_data)*0.15))] <- NA

## 2. MAR (Missing At Random)
# Missingness depends on Married (more likely missing for Married=Yes)
credit_data <- credit_data |>
  mutate(Cards = ifelse(Married == "Yes" & runif(n()) < 0.30, NA, Cards))

## 3. MNAR (Missing Not At Random)
# 25% missing in Balance 
# Missingness depends on its own value (higher values more likely missing)
credit_data$Balance <- ifelse(credit_data$Balance > quantile(credit_data$Balance, 0.75, na.rm = TRUE) & runif(nrow(credit_data)) < 0.4, NA, credit_data$Balance)

## 4. Additional MAR example
# Limit missing more often when Region is West
credit_data <- credit_data |>
  mutate(Limit = ifelse(Region == "West" & runif(n()) < 0.35, NA, Limit))

# Check missingness
colMeans(is.na(credit_data))

save(dataset_1, dataset_2, dataset_3, credit_data, file = "ESG_missing_data.RData")
