################################################################################
#                                                                              #
#                     Multiple linear regression models                        #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

# you can find the notes of this lecture in my github: https://github.com/ValentinaZangirolami/Statistical-Learning

#load libraries
library(car)
library(ggplot2)
library(tidyverse)

#load data
esg_data <- read.csv("esg_financial_dataset.csv")

#show data
head(esg_data)

# esclude some variables to: (i) alleviate correlation among independent variables, (ii) simplify the problem
cat_variables <- esg_data |> select(Industry)
esg_data <- esg_data |> select(-c(Industry, Region, ProfitMargin, ESG_Environmental, ESG_Social, ESG_Governance, EnergyConsumption, WaterUsage, CarbonEmissions))

#marginal interpretation

(Scatter_Matrix <- GGally::ggpairs(esg_data, 
                                  title = "Scatter Plot Matrix for ESG data", 
                                  axisLabels = "show"))
#linear regression

mod <- lm(MarketCap~., data=esg_data)
summary(mod)

round(mod$coefficients, 5)

#diagnostic plot
par(mfrow=c(2,2))
plot(mod) # some issues

# adding categorical variable

esg_data <- cbind(esg_data, cat_variables)

mod_2 =lm(MarketCap~., data=esg_data)
summary(mod_2)

round(mod_2$coefficients, 5)

unique(esg_data$Industry) # 8 dummies out of 9 categories

#nested models

anova(mod, mod_2)

#diagnostic plot

par(mfrow=c(2,2))
plot(mod_2)

# Evaluating interactions

ggplot(esg_data, aes(x = GrowthRate, y = MarketCap, color = Industry)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear regression lines
  labs(
    x = "GrowthRate",
    y = "MarketCap",
    color = "Industry"
  ) + ylim(0, 35000) + 
  theme_minimal()

ggplot(esg_data, aes(x = ESG_Overall, y = MarketCap, color = Industry)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear regression lines
  labs(
    x = "ESG Overall",
    y = "MarketCap",
    color = "Industry"
  ) + ylim(0, 35000) + 
  theme_minimal()

#interaction

mod_int <- lm(MarketCap ~ . + GrowthRate:Industry, data = esg_data)
summary(mod_int)

par(mfrow=c(2,2))
plot(mod_int)

round(mod_int$coefficients, 5)

#---------------------
#MODEL SELECTION
#---------------------

#CRITERION-BASED
#---------------

#CRITERION-BASED: considering the criterion AIC=-2sup loglik+2p 
# where p is the number of parameter
#(# explanatory variables + 1, if we estimate also sigma^2 
#the # of parameters increase of 1)
#AIC should be minimized. 
#For linear model is an increasing function of DR and p


#step function
#-------------
#An automatic selection can be done with 
#step(modello, formula, direction, trace, keep)

#direction can be "both", "backward", "forward" 
#(default "backward")
#trace can be 0 o 1 
#(default trace=1, shows all the selection process)
#keep alloos to keep track of the non significative variables

step(mod_int,trace=1,direction="backward")    #default: backward, trace=1
#we start from the full model:
#if we consider all the explanatory variables (none), l'AIC is 81921.36
#if we do not consider GrowthRate, AIC is 81919.59

#the best AIC is obtained with the model without GrowthRate

mod_nullo <- lm(MarketCap ~ 1, data = esg_data)
step(mod_nullo,trace=1,direction="forward",scope = formula(lm(MarketCap ~ . + GrowthRate:Industry, data = esg_data)))